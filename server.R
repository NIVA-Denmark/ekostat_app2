library(shiny)
library(DT)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(prodlim)
library(sparkline)
library(RSQLite)
library(data.table)
library(magrittr)


source("classoutputtable.R")
source("Aggregation.R")
source("Assessment.R")
source("helpfunctions.R")
source("extrapolation.R")

source("IndicatorFunctions.R")
source("read_parameter_files.R")

#source("ReadIndicatorType.R")
#source("ReadVariances.R")
#source("ReadBounds.R")

shinyServer(function(input, output, session) {
  
  # obtain the values of inputs
  
  #shinyValue = function(id, len,chklist){
  #  s<-paste0(id,seq_len(len)) %in% chklist[chklist %like% id]
  #  cat(paste0(id," "),s,"\n")
  #  s
  #}
  listWBindicators <- c("CoastHypoxicArea")
  
  
  # ------------------------ setup -----------------------------------------------
  # Path to the eksostat database
  dbpath_info<-"../efs/ekostat/ekostat_info.db"

  dbselect<-function(wtype){
    dbpath_C<-"../efs/ekostat/ekostat_C.db"
    dbpath_L<-"../efs/ekostat/ekostat_L.db"
    dbpath_R<-"../efs/ekostat/ekostat_R.db"
    if(wtype=="Coast") dbpath_C
    else if(wtype=="Lake")dbpath_L
    else if(wtype=="River")dbpath_R
    else ""
  }  
  #dfind<-ReadIndicatorType()
  #df_var<-ReadVariances()
  #df_bound<-ReadBounds()
  
  
  
  dbpath<-reactive({
    wtype<-input$waterType
    db<-dbselect(wtype)
    return(db)
  })
  
 # ---- Database functions -----------------------------------
  readdb <- function(dbname,strSQL){
    db <- dbConnect(SQLite(), dbname=dbname)
    df <- dbGetQuery(db, strSQL)
    dbDisconnect(db)
    return(df)
  }  
  
  
  dfobs <- function(wblist,periodlist){
    #browser()
    sql<-paste0("SELECT * FROM data WHERE period IN (",periodlist,") AND WB_ID = '",wblist,"'")
    df<-readdb(dbpath(), sql)
    df$date<-as.Date(df$date,origin="1970-01-01")
    return(df)
  } 
  
  # Read list of indicators
 
  values <- reactiveValues(resMC = data.frame())
  values$wbselected <- ""
  values$wbselectedname <- ""
  values$typeselected <- ""
  values$typeselectedname <- ""
  values$df_ind_status<-""
  values$df_ind_extrap<-""
  values$periodselected <- ""
  values$resAvgType <- ""
  values$resMCType <-""
  values$IndSelection<-""
  #values$ClickedWB <- FALSE

  # dfwb_lan <- readdb(dbpath_info, "SELECT * FROM WB_Lan") %>% # matching Län and WB_ID
  #   filter(!is.na(Lan)) %>%
  #   mutate(LanName=trimws(LanName,which="both"),Lan=trimws(Lan,which="both"))
  
  dfwb_info <- readdb(dbpath_info, "SELECT * FROM WB_info") # type info WB_ID
  
  #wb <- readdb(dbpath()), "SELECT * FROM WB")             # available assessments
  
  pressure_list<-function(){
    if(!is.null(values$watertypeselected)){
    list<-c("Nutrient loading","Organic loading","Acidification","Harmful substances","Hydrological changes","Morphological changes","General pressure")
    list<-gsub(" ",".",list)
    df<-df_indicators[,c("Water_type",list)]
    df<-df %>% 
      filter(Water_type==values$watertypeselected)%>% 
      gather(key="Pressure",value="value",list) %>%
      filter(value=="X") %>%
      distinct(Pressure)
    list<-df$Pressure
    list<-gsub("\\."," ",list)
    }else{
      list<-c("")
    }
    return(list)
  }
  


  period_list <- function(){
    c("2007-2012","2013-2018")
  }
  
  
# ------------ sidebar menu --------------------------------------------------  
  output$dy_menu <- renderMenu({ 
    sidebarMenu(id="tabs",
                menuItem("Waterbody", tabName = "waterbody", icon = icon("map-marker")),
                menuItem("Indicators", tabName = "indicators", icon = icon("tasks")),
                menuItem("Extrapolation", tabName = "extrapolation", icon = icon("chart-line")),
                #menuItem("Data", tabName = "data", icon = icon("database"))
                menuItem("Status", tabName = "status", icon = icon("bar-chart"))
                #menuItem("Download", tabName = "download", icon = icon("file"))
                #menuItem("Options", tabName = "options", icon = icon("cog"))#,
    )
  })
  
  # icon file-alt
  
# ------ Output components for the Waterbody selection page -----------------------
  output$selectWaterType <- renderUI({
    tagList(selectInput(
      "waterType",
      "Water type:",
      choices = c("Coast","Lake"),
      #choices = c("Coast","Lake","River"),
      #choices = listWaterType,
      multiple = FALSE,
      width="180px"
    ))
  })

  
  output$selectLan <- renderUI({
    tagList(selectInput(
      "lan",
      "Län:",
      choices = lan_list(),
      multiple = FALSE,
      width="180px"
    ))
  })
  
  output$selectMun <- renderUI({
    tagList(selectInput(
      "mun",
      "Municipality:",
      choices = mun_list(),
      multiple = FALSE,
      width="180px"
    ))
  })
  
  output$selectType <- renderUI({
    tagList(selectInput(
      "type",
      "WB Type:",
      choices = type_list(),
      multiple = FALSE,
      width="180px"
    ))
  })
  

  output$selectPeriod <- renderUI({
    tagList(selectInput(
      "period",
      "Select Period",
      choices = period_list(),
      selected= period_list()[length(period_list())],
      multiple = T#,
      #width="180px"
    ))
  })
  
  # information about the selected waterbody
  output$wb_info<-renderText({
    periodlist<-paste(paste0("'",input$period,"'"),collapse = ",")
    if (length(input$dtwb_rows_selected) > 0) {
      n<-input$dtwb_rows_selected
      df<-wb_list()
      wbidselect<-df[n,"WB_ID"]
      wbnameselect<-df[n,"WB_Name"]
      
      
      
      db <- dbConnect(SQLite(), dbname=dbpath())
      sql<-paste0("SELECT COUNT(*) FROM data WHERE period IN (",periodlist,") AND  WB_ID IN ('",wbidselect,"')")
      nrows <- dbGetQuery(db, sql)
      dbDisconnect(db)
      values$WBinfo <- paste0(wbidselect," ",wbnameselect," (data count = ",nrows,")")
    }else{
      values$WBinfo<-""
    }
    if(typeof(values$WBinfo)!="character"){
      "none selected"
    }else{
      if(values$WBinfo==""){
        "none selected"
      }else{
        values$WBinfo
      }
    }
  }
  )
  
  output$dataButton <- renderUI({
    if (length(input$dtwb_rows_selected) > 0) {
      buttontext <-"Data Status"
      tagList(actionButton("dataButton", buttontext))
    }
  })
  
  # ----------- output DataTable of waterbodies ----------------------------------
  output$dtwb = DT::renderDataTable({
    df <- wb_list() %>% select(WB_ID,WB_Name,District,Lan,Municipality,Type)
    names(df)<-c("WB ID","WB Name","District","Län","Municipality","Type" )
    df
  }, selection = 'single', rownames= F,options = list(lengthMenu = c(5, 10, 20, 50), pageLength = 10))
  
  # ----- reactive data for the waterbody selection
  
  lan_list <- reactive({
    #browser()
    Lan <- c("ALL")
    all <- data.frame(Lan,row.names=F,stringsAsFactors=F)
    df<-df_WB_lan  %>%
      distinct(Lan,LanID,LanName) %>%
      arrange(LanName) %>%
      select(Lan)
    df<-bind_rows(all,df)
    df$Lan
  })
  
  mun_list <- reactive({
    Mun <- c("ALL")
    all <- data.frame(Mun,row.names=F,stringsAsFactors=F)
    df<-df_WB_mun %>% 
      distinct(Mun,MunID,MunName) %>%
      arrange(MunName) %>%
      select(Mun)
    df<-bind_rows(all,df)
    df$Mun
  })
  
  # make list of WB types
  type_list <- reactive({
    #browser()
    Type <- c("ALL")
    all <- data.frame(Type,row.names=F,stringsAsFactors=F)
    df<-dfwb_info
    
    if(!is.null(input$waterType)){
      df <- df %>% filter(CLR==input$waterType)
    }
    
      if (!is.null(input$lan)){
        if(input$lan!="ALL"){
          dfselect<-df_WB_lan %>% 
            filter(Lan==input$lan) %>%
            select(WB_ID)
          df <- df %>% inner_join(dfselect,by="WB_ID")
        }}
    if (!is.null(input$mun)){
      if(input$mun!="ALL"){
        dfselect<-df_WB_mun %>% 
          filter(Mun==input$mun) %>%
          select(WB_ID)
        df <- df %>% inner_join(dfselect,by="WB_ID")
      }}
    
    #browser()
    df <- df %>%
      distinct(CLR,Type) %>%
      mutate(typesort=as.numeric(ifelse(CLR=="Coast",gsub("n","",gsub("s","",Type)),Type)))
     
    df <- df %>%
      arrange(CLR,typesort,Type) %>%
      select(Type=Type)
    df<-bind_rows(all,df)
    df$Type
    
  })

  # ---------------- wb_list: table of WBs matching search criteria  ----------------------
  wb_list<-reactive({
    
    df <- dfwb_info 
    
    values$WBinfo <- ""
    if (!is.null(input$waterType)){
      df <- df %>% filter(CLR==input$waterType)
    }
    
    if (!is.null(input$mun)){
      if(input$mun!="ALL"){
        dfselect<-df_WB_mun %>% 
          filter(Mun==input$mun) %>%
          select(WB_ID)
        df <- df %>% inner_join(dfselect,by="WB_ID")
      }}
        
    if (!is.null(input$lan)){
      if(input$lan!="ALL"){
        dfselect<-df_WB_lan %>% 
          filter(Lan==input$lan) %>%
          select(WB_ID)
        df <- df %>% inner_join(dfselect,by="WB_ID")
      }}
    if (!is.null(input$type)){
      if(input$type!="ALL"){
        df <- df %>% filter(Type==input$type)
        }
    }

    return(df)
  })
  
 
  output$IndicatorsTitle<-renderText({
    "Select Indicators"
  })
  
  output$buttonWB <- renderUI({
    if(!is.null(input$dtwb_rows_selected)){
      if (input$dtwb_rows_selected > 0) {
        df<-wb_list()
        wbidselect<-df[input$dtwb_rows_selected,"WB_ID"]
        buttontext <-paste0("Select ",wbidselect)
        tagList(actionButton("buttonWB", buttontext))
      } }
  })
  
 # function to add leading zero to coastal types 
  TypeLeadingZero<-function(type){
    if(is.numeric(type)){
      type<-as.character(type)
    }
    if(typeof(type)=="character"){
      n = nchar(type)
      ns = substr(type,n,n)
      if(ns %in% c("s","n")){
        value=as.numeric(substr(type,1,n-1))
      }else{
        value=as.numeric(type)
        ns=""
      }
      if(value<10){
        type=paste0("0",as.character(value),ns)
      }
    }
    return(type)
  }

  observeEvent(input$buttonWB, {
    values$wbselected<-wb_list()[input$dtwb_rows_selected,"WB_ID"]
    values$wbselectedname<-wb_list()[input$dtwb_rows_selected,"WB_Name"]
    typeselected<-dfwb_info[dfwb_info$WB_ID==values$wbselected,"Type"]
    values$periodselected<-input$period
    if(input$waterType=="Coast"){
      values$watertypeselected<-"Coastal"
      values$typeselected<-TypeLeadingZero(typeselected)
    }else if(input$waterType=="Lake"){
      values$watertypeselected<-"Lakes"
      values$typeselected<-typeselected
    }else if(input$waterType=="River"){
      values$watertypeselected<-"Rivers"
      values$typeselected<-typeselected
    }
  
    values$IndSelection<-""
    values$toggleIndAll<-T
    values$toggleExtrapAll<-T
    values$toggleExtrapWBs<-T
    
    # ----------------------------------------
    # reset the results tables
    values$res1MC<-""
    values$res2MC<-""
    values$res3MC<-""
    values$res4MC<-""
    values$resInd<-""
    values$resAvg<-data.frame()
    values$resMC<-data.frame()
    
    # ----------------------------------------
    # reset the extrapolation WB selection table
    output$dtextrapstn = renderDataTable(
      datatable(data.frame(),options=list(dom = 't',pageLength = 99,autoWidth=TRUE),
                selection=list(mode='multiple')), 
      server=FALSE)
    #-----------------------------------------
    
    updateTabItems(session, "tabs", "indicators")
    
      
    UpdateIndTable()
 
    }, ignoreInit = T)

  # ------ Output components for the indicator selection / modification page -----------------------
  
  output$SelectedWB<-renderText({
    if(values$wbselected=="") {
      titletext<-"No Waterbody Selected"
    }else{
      titletext<-paste0(values$wbselected," - ",values$wbselectedname)
    }
    titletext
  })
  
  output$SelectedWB2<-renderText({
    if(values$wbselected=="") {
      titletext<-"No Waterbody Selected"
    }else{
      titletext<-paste0(values$wbselected," - ",values$wbselectedname)
    }
    titletext
  })
  
  
  output$SelectedType<-renderText({
    if(values$typeselected==""){
      titletext<-""
    }else{
      titletext<-paste0("Type: ",values$typeselected)
    }   
  })
  
  output$selectPressure <- renderUI({
    tagList(selectInput(
      "pressure",
      "Select Pressure(s)",
      selected = pressure_list()[1],
      choices = pressure_list(),
      multiple = T
    ))
  })
  
  observeEvent(input$pressure, {
    values$IndSelection<-""
    UpdateIndTable()
  }, ignoreInit = T) 
  
  output$buttonExtrap <- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(countIndicators()<0){
        ""
      }else{
        if(length(input$dtindextrap_rows_selected)>0 | countIndicators()<1){
          tagList(actionButton("extrapButton", "Extrapolation"))
        }else{
          tagList(actionButton("goButtonDirect", "Calculate Status"))
        }
      }
    }
  })
  
  output$goButton <- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(countIndicators()<1){
        ""
      }else{
        tagList(actionButton("goButton", "Calculate Status"))
      }
    }
  })
  
  output$txtCheckedRows<-renderText({
    paste(input$dtind_rows_selected,"\n")
  })
  
  output$txtExtrapRows<-renderText({
    paste(input$extrap_rows,"\n")
  })
  
  
 
  output$toggleIndicators<- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(countIndicators()<0){
        ""
      }else{
        tagList(actionButton("allIndicators", "Select/Deselect All"))
      }
    }
  })
  
  output$toggleExtrapolate<- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(countExtrapIndicators()<0){
        ""
      }else{
        tagList(actionButton("allExtrapolate", "Select/Deselect All"))
      }
    }
  })
  
  output$toggleWBs<- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(length(input$dtextrap_rows_selected)>0){
        tagList(actionButton("allExtrapWBs", "Select/Deselect All"))
      }else{
        ""
      }
    }
  })
  
  output$applyWBs<- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(length(input$dtextrap_rows_selected)>0){
        tagList(actionButton("applyExtrapWBs", "Apply"))
      }else{
        ""
      }
    }
  })
  
  
  
  
  # --------------- update indicator information / selection ----------------
  
  #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
  #        Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
  # })"))
  
  # --------------- output$dtind ----------------------------------
    output$dtind = DT::renderDataTable({
      
     input$buttonWB
      
    df<-values$df_ind_status
    if(typeof(df)!="list"){
      df<-data.frame() 
    }else{
      num_col<-ncol(df)
      df$row<-seq(1,nrow(df),1)
      df$row<-NULL
            # reorder_columns
      df<-df[c(num_col-1,seq(2,num_col-2,1))]
      #df<-df %>% rename(Indicator=IndicatorDescription)
      names(df)[names(df)=="IndicatorDescription"]<-"Indicator"
      values$df_ind_select<-df
      df
    }
  },server=F, escape=F,selection ="multiple",rownames=T, 
  options=list(dom = 't',pageLength = 99,autoWidth=TRUE
               ))

  # --------------- output$dtindextrap ----------------------------------
  # generate data on extrapolation for the selected indicator
  output$dtindextrap = DT::renderDataTable({
    dftype<-values$resAvgType
    if(typeof(dftype)!="list"){
      dftype=""
      df<-data.frame(Indicator=character())
    }else{
    dftype<-dftype %>%
      distinct(Period,Indicator,WB_ID)    
    dftype<-dftype %>%
      left_join(select(df_indicators,Indicator,WB_extrapolation),by="Indicator") %>%
      filter(!is.na(WB_extrapolation))
    
    dftype<-dftype %>% 
      group_by(Period,Indicator) %>%
      summarise(WB_count=n())
    #} # xxxxxxxxxxxxxxxxxxxxxxxx
    if(length(input$dtind_rows_selected)==0){
       df<-data.frame()
    }else{
    df<-values$df_ind_status[input$dtind_rows_selected,]
    num_col<-ncol(df)
    if(nrow(df)>0){
      df$id <- 1:nrow(df)
      
    }
    df<-df[,c(1,num_col-1,seq(2,num_col-2,1),num_col+1)] 
    df <- df %>% gather(key="Period","Status",c(seq(3,num_col-1,1)))
    df <- df %>% left_join(dftype,by=c("Period","Indicator"))
    df <- df %>% filter(!is.na(WB_count))
    df <- df %>% filter(Status!="OK") ######### to be checked
    if(input$IgnoreErr){
      df <- df %>% filter(Status=="-")
    }
    df <- df %>% 
      arrange(id) %>% 
      select(-c(id,Status,Indicator)) #%>% 
      #rename(Indicator=IndicatorDescription)
    names(df)[names(df)=="IndicatorDescription"]<-"Indicator"
    }}
    values$df_extrap<-df
    df
  },server=T, escape=FALSE,selection='multiple',rownames=T,
  options=list(dom = 't',pageLength = 99,autoWidth=TRUE  ))
  
  dt_proxy <- DT::dataTableProxy("dtind")
  dt_proxy2 <- DT::dataTableProxy("dtindextrap")
  dt_proxy3 <- DT::dataTableProxy("dtextrapstn")
  
   observeEvent(input$allIndicators,{
     val=values$toggleIndAll
     values$toggleIndAll<-!val
      if(val){
         DT::selectRows(dt_proxy, input$dtind_rows_all)
       } else {
         DT::selectRows(dt_proxy, NULL)
       }
   })
   
   observeEvent(input$allExtrapolate,{
     val=values$toggleExtrapAll
     values$toggleExtrapAll<-!val
     if(val){
       DT::selectRows(dt_proxy2, input$dtindextrap_rows_all)
     } else {
       DT::selectRows(dt_proxy2, NULL)
     }
   })
   
   observeEvent(input$allExtrapWBs,{
     val=values$toggleExtrapWBs
     values$toggleExtrapWBs<-!val
     if(val){
       DT::selectRows(dt_proxy3, input$dtextrapstn_rows_all)
     } else {
       DT::selectRows(dt_proxy3, NULL)
     }
   })
   

   
   # ---------------- Function updateind() --------------------------------

  UpdateIndTable<-function(){
    #cat(paste0("UpdateIndTable\n"))
      # Get the info on the status for the indicators
      bOK<-TRUE
  
      sql<-paste0("SELECT * FROM resAvg WHERE WB_ID ='",values$wbselected,"'")  
      df <- readdb(dbpath(),sql)
      df<-CleanSubTypes(df)
      
      pname<-input$pressure
      
      if(is.null(pname)){
        bOK<-FALSE
      }else if(pname==""){
          bOK<-FALSE
          }
      if(is.null(values$watertypeselected)){bOK<-FALSE}
      if(is.null(values$periodselected)){bOK<-FALSE}
      
      if(bOK){
        df3 <- NULL
        for(i in 1:length(pname)){
          
        pname[i]<-gsub(" ",".",pname[i])
        df2 <- df_indicators %>% filter(Water_type==values$watertypeselected)
        df2 <- df2[df2[,pname[i]]=="X",]
        df2 <- df2 %>% filter(!is.na(Indicator))
        df2 <- df2 %>% select(Indicator)
        if(is.null(df3)){
          df3<-df2
        }else{
          df3<-bind_rows(df3,df2) %>% distinct(Indicator)
        }
        }
        df2<-df3
        if(nrow(df2)==0){bOK<-FALSE}
      }
      
      if(bOK){
        df <- df %>% select(Indicator,Period,Code)
        indlist<-paste(paste0("'",df2$Indicator,"'"),collapse = ",")

        df2$X<-1
        df2$num<-seq(1,nrow(df2),1)
        dforder<-df2 %>% select(Indicator)
        
        dfperiod<-data.frame(values$periodselected,stringsAsFactors=F)
        dfperiod$X<-1
        
        df2<-df2 %>% left_join(dfperiod,by="X") %>% select(-c(X,num))
        names(df2) = c("Indicator","Period")
        
        
        #db <- dbConnect(SQLite(), dbname=dbpath())
        sql<-paste0("SELECT * FROM resAvg WHERE Type ='",values$typeselected,
                    "' and Indicator in (",indlist,") AND WB_ID <>'", values$wbselected,"'")  
        dbname<-dbpath()
        dftypeperiod <- readdb(dbname, sql)
        #dbDisconnect(db)
        
        
        if(nrow(dftypeperiod)>0){
          #------------------------------------
       dftypeperiod<-CleanSubTypes(dftypeperiod)
        
        dfwb_type <- dfwb_info %>% distinct(WB_ID,WB_Name)
        #browser()
        dftypeperiod <- dftypeperiod %>% 
          left_join(dfwb_type,by=c("WB_ID"="WB_ID"))   
        dftypeperiod <- dftypeperiod %>%
          filter(Code==0) 
        dftypeperiod<- df2 %>% left_join(dftypeperiod,by=c("Indicator","Period")) %>%
          filter(!is.na(Mean))
        } #******************
        
        df <- df2 %>% left_join(df,by=c("Indicator","Period")) %>%
          mutate(Code=ifelse(is.na(Code),-99,Code)) %>%
          mutate(Data=ifelse(Code=='0',"OK",ifelse(Code %in% c('-1','-2'),"(OK)","-")),
                 Code=ifelse(Data=="OK",0,1))
          #mutate(Data=ifelse(Code=='0',"OK",
          #                   ifelse(Code %in% c('-1','-2'),"few data","-"))) %>%
          #mutate(Code=ifelse(Data %in% matchcodename,0,1))
        
          dfext <- df %>% 
            group_by(Indicator) %>% 
            summarise(sum=sum(Code,na.rm=T)) %>%
            ungroup() %>%
            mutate(Extrap=ifelse(sum==0,F,T)) %>%
            #mutate(Extrap=F) %>%
            select(-sum) 
          
        df <- df %>% 
          select(-Code) %>%
          spread(key="Period",value="Data") %>%
          left_join(select(df_indicators,Indicator,IndicatorDescription),by="Indicator") %>%
          left_join(dfext,by=c("Indicator")) 

        df<-dforder %>% left_join(df,by="Indicator") #%>% mutate(Selected=F)
        
        values$df_ind_status <- df
        
        if(nrow(dftypeperiod)>0){
          dftypeperiod$Include<-T
          values$resAvgType <-dftypeperiod
        }else{
          values$resAvgType <-""
        }
        }else{
          values$df_ind_status <-""
          values$resAvgType <-""
         }
        #----------------------------------
      #}else{
       # values$df_ind_status <-""
      #  values$resAvgType <-""
      #}

      #updatedtind()
      #browser()
}


  
  # ------------------------------------------------------------------- 
  # -------------  extrapolate button  action -----------------------------
  # ------------------------------------------------------------------- 
  observeEvent(input$extrapButton, {  
      GoExtrap()
    }) 
   
   GoExtrap=function(){
     
 
     output$dtextrap=DT::renderDataTable({
       input$buttonWB
      if(length(input$dtindextrap_rows_selected)>0){
        df<-values$df_extrap[input$dtindextrap_rows_selected,]
        df<- df %>% select(Indicator,Period)
        values$dtextrap<-df
        }else{
        df<-data.frame()
      }
      df
      },server=T, escape=FALSE,selection='single',rownames=T,
      options=list(dom = 't',pageLength = 99,autoWidth=TRUE  ))
    
     dfextrap <- values$resAvgType
     dfextrapWB <-data.frame(WB_ID=character(),Indicator=character())
     if(typeof(dfextrap)=="list"){
       
       dfextrap <- dfextrap %>%
         left_join(select(df_indicators,Indicator,IndicatorDescription),by="Indicator")
       
       if(ncol(values$df_extrap)){
       
       df<-values$df_extrap[input$dtindextrap_rows_selected,] %>% 
         select(Indicator,Period)
       #select(IndicatorDescription=Indicator,Period)
       names(df)[names(df)=="Indicator"]<-"IndicatorDescription"
       
       
       df <- df %>% left_join(dfextrap,by=c("IndicatorDescription","Period"))
       dfextrapWB <- df %>% 
         distinct(Indicator,IndicatorDescription,WB_ID) %>%   
         filter(!is.na(WB_ID)) %>%                            
         mutate(Select=T)
     }}
     
     values$dfextrapWB<-dfextrapWB
     
    #    #rename(IndicatorDescription=Indicator) %>%
   
    updateTabItems(session, "tabs", "extrapolation")
    
    return(0)
    
   } 
  
  # ---------- DataTable with stations for extrapolation ---------------------
  
  
 
  
  observeEvent(input$dtextrap_rows_selected,{
    if(is.null(input$dtextrap_rows_selected)){
        df<-data.frame()
        selected<-NULL
      }else{

        df<-values$dtextrap
        indmatch<-df[input$dtextrap_rows_selected,"Indicator"]
        df<-isolate(values$dfextrapWB) 
          df <- df%>% 
            filter(IndicatorDescription==indmatch)
          if(nrow(df)>0){
            df$id<-1:nrow(df)
           selected <- df[df$Select==T,"id"]
          } else{
            selected<-NULL
          }
        df <- df %>% select(WB_ID)                                                    
        df <- df %>% left_join(select(dfwb_info,WB_ID=WB_ID,Name=WB_Name),by="WB_ID")    
        
        values$current_extrap_WBs <- df
      }
    output$dtextrapstn = renderDataTable(
      datatable(df,options=list(dom = 't',pageLength = 99,autoWidth=TRUE),
                 selection=list(mode='multiple',selected=selected)), 
      server=FALSE)
  })
  
  observeEvent(input$applyExtrapWBs,{
    df<-values$current_extrap_WBs
    if(nrow(df)>1){
    df$SelectNew<-NA
    df1 <- df[input$dtextrapstn_rows_selected,]
    df<-df %>% 
      select(WB_ID)   
    
    if(nrow(df1)>0){
      df1$SelectNew=T
      df<-df %>% left_join(df1,by="WB_ID")       
      dfapply<- df %>% mutate(SelectNew=ifelse(is.na(SelectNew),F,SelectNew))
    }else{
      dfapply<- df %>% mutate(SelectNew=F)
    }


    df<-values$dtextrap
    indmatch<-df[input$dtextrap_rows_selected,"Indicator"]
    dfapply <- dfapply %>% mutate(IndicatorDescription=indmatch)
    
    dfwb<-isolate(values$dfextrapWB)
    
    dfwb <- dfwb %>% 
      left_join(dfapply,by=c("IndicatorDescription","WB_ID"))        
    
    dfwb <- dfwb %>% 
      mutate(Select=ifelse(is.na(SelectNew),Select,SelectNew)) %>%
      select(-SelectNew)

    values$dfextrapWB<- dfwb
    }
  })
  

  output$NoticeExtrapolation<-renderText({
   
    df<-input$dtindextrap_rows_selected
    if(is.integer(df)){
      titletext<-"Indicators using extrapolation"
    }else{
      titletext<-"No indicators are using extrapolation!"
    }
     titletext
  })
  

   
   listIndicators <- function(){
     dfi<-values$df_ind_status
     if(typeof(dfi)!="list"){
       df<-data.frame()
     }else{
       ni<-nrow(dfi)
       if(ni>0){
         df<-data.frame(
           Indicator=dfi$Indicator,
           Select=shinyValue('ind_',ni,input$checked_rows),
           Extrapolate=shinyValue('extrap_',ni,input$extrap_rows)
         )
         
       }else{
         df<-data.frame()
       }
     }
     return(df)   
   }
   
  
   countIndicators<-function(){
     n<-length(input$dtind_rows_selected)
     return(n)
   }
   countExtrapIndicators<-function(){
     n<-length(input$dtindextrap_rows_selected)
     return(n)
   }
   

   
# ------------------------------------------------------------------- 
# ------------- go calculate action -----------------------------
# ------------------------------------------------------------------- 
   
observeEvent(input$goButton, {
  GoCalculation()
}, ignoreInit = T)
   
observeEvent(input$goButtonDirect, {
     GoExtrap()
     GoCalculation()
   }, ignoreInit = T)
   
   
GoCalculation=function(){
     
  start.time <- Sys.time()
  #df<-listIndicators()
  #browser()
  withProgress(message = 'Calculating...', value = 0, {
    df<-values$df_ind_select
    df<-df[input$dtind_rows_selected,]
    
    df2<-df_indicators %>% 
      filter(Water_type==values$watertypeselected) %>% 
      select(Indicator,IndicatorDescription) 
    
    names(df)[names(df)=="Indicator"]<-"IndicatorDescription"
    df<-df %>%
      #rename(IndicatorDescription=Indicator) %>%
      left_join(df2,by="IndicatorDescription")
    
    #df<- listIndicators()
    #df <- df %>% filter(Select==T)
    IndList <- df$Indicator
    #cat(paste0("Indicator list:", paste(paste0("'",IndList,"'"),collapse = ","),"\n"))
    
    nSimMC <- input$n
    
    
    IndList<-paste(paste0("'",IndList,"'"),collapse = ",")
    periodlist<-paste(paste0("'",values$periodselected,"'"),collapse = ",")
    wblist<-paste(paste0("'",values$wbselected,"'"),collapse = ",")


 
  # Get results for the waterbody we are showing results for   
    db <- dbConnect(SQLite(), dbname=dbpath())
    sql<-paste0("SELECT * FROM resAvg WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,  
                ") AND Indicator IN (",IndList,")")      
    resAvg <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resYear WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,
                ") AND Indicator IN (",IndList,")")       
    resYr <- dbGetQuery(db, sql)
    incProgress(0.1)

    #sql<-paste0("SELECT * FROM resMC WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,
    #             ") AND Indicator IN (",IndList,") AND sim <= ",nSimMC)      
    #resMCX <- dbGetQuery(db, sql) 

    sql<-paste0("SELECT WB_ID,Period,Indicator,IndSubtype,sim,Value,ClassID,Class,EQR FROM resMC WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,
                ") AND Indicator IN (",IndList,") AND sim <= ",nSimMC)
    resMC <- dbGetQuery(db, sql) 
    resMC <- resMC %>% 
      left_join(select(resAvg,Type,Typename,WB_ID,Period,Region,Indicator,IndSubtype,Code,QEtype,QualityElement,QualitySubelement,Note,Unit,Months,
                       Worst,PB,MP,GM,HG,Ref,Mean,StdErr,EQRavg=EQR,ClassAvg=Class),
                by=c("WB_ID","Period","Indicator","IndSubtype"))

    
    
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resErr WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,
                ") AND Indicator IN (",IndList,")")         
    resErr <- dbGetQuery(db, sql)
    dbDisconnect(db)
    
    #region<-resAvg$Region[1]
    #typename<-resAvg$Typename[1]
     #-----------------------------------------------------------------------
    incProgress(0.1,message="getting data for extrapolation")
    #find which indicators need to be (and can be extrapolated)
    
    # if we choose to use indicators with less than 3 years data, then these 
    # should be included in the list of indicators NOT to be extrapolated  
    if(isolate(input$IgnoreErr)){
      matchcode<-c(0,-1,-2)
    }else{
      matchcode<-c(0)
    }

    df <- values$dfextrapWB
    df <- df %>% 
      filter(Select==T) %>%
      select(WB_ID,Indicator)        
    
    dfextrap<-values$resAvgType
    
    if(is.null(dfextrap)){dfextrap<-""}
    if(dfextrap!=""){
    dfextrap<- df %>% 
      left_join(dfextrap,by=c("WB_ID","Indicator")) %>%       
      filter(!is.na(WB_ID)) 

    dfmatch<-resAvg %>% 
      filter(Code %in% matchcode) %>% 
      select(Period,Indicator) %>%
      mutate(OK=1) 
    dfextrap<-dfextrap %>% 
      left_join(dfmatch,by=c("Period","Indicator"))
    dfextrap<-dfextrap %>% 
      filter(is.na(OK)) %>%
      select(WB_ID,Indicator,Period,Type) 
    
    # this is the list of indicators available from extrapolation stations which 
    # do NOT have a result for the WB we are considering

    # Get results for the waterbodies used for extrapolation we are showing results for   
    wblisttype<-paste(paste0("'",dfextrap$WB_ID,"'"),collapse = ",")
    #cat(paste0("WBs for extrap: ",wblisttype,"\n"))
    db <- dbConnect(SQLite(), dbname=dbpath())
    sql<-paste0("SELECT * FROM resAvg WHERE period IN (",periodlist,") AND WB_ID IN (",wblisttype,
                ") AND Indicator IN (",IndList,")")
    resAvgtype <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resYear WHERE period IN (",periodlist,") AND WB_ID IN (",wblisttype,
                ") AND Indicator IN (",IndList,")")
    resYrtype <- dbGetQuery(db, sql)
    incProgress(0.1)

    #sql<-paste0("SELECT * FROM resMC WHERE period IN (",periodlist,") AND WB_ID IN (",wblisttype,
    #            ") AND Indicator IN (",IndList,") AND sim <= ",nSimMC)
    #resMCtype <- dbGetQuery(db, sql) 

    sql<-paste0("SELECT WB_ID,Period,Indicator,IndSubtype,sim,Value,ClassID FROM resMC WHERE period IN (",periodlist,") AND WB_ID IN (",wblisttype,
                ") AND Indicator IN (",IndList,") AND sim <= ",nSimMC)
    resMCtype <- dbGetQuery(db, sql) 
    # XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    incProgress(0.1) 
    dbDisconnect(db)
    
    
    resExtrap<-extrapolation(dfextrap,df_bound,input$n,resYrtype,resAvgtype,resMCtype)
    resAvgExtrap<-resExtrap$dfAvg
    
    if(nrow(resAvgExtrap)>0){
      resAvgExtrap<-resAvgExtrap %>% mutate(WB_ID=values$wbselected,Type=values$typeselected,Note="Extrap",Code=0)
      namelist<-paste(paste0("'",names(resAvgExtrap),"'"),collapse = ",")

      
      resAvgExtrap<-resAvgExtrap %>% left_join(select(df_indicators,Indicator,QEtype,QualityElement,QualitySubelement),by=c("Indicator"))
      
      dftemp <- df_bound
      names(dftemp)[names(dftemp)=="Depth_stratum"]<-"IndSubtype"
      resAvgExtrap<-resAvgExtrap %>% left_join(dftemp,by=c("Type","Indicator","IndSubtype"))
      resAvgExtrap<-resAvgExtrap %>%rename(Ref=`RefCond`,HG=`H.G`,GM=`G.M`,MP=`M.P`,PB=`P.B`) 
      resAvgExtrap<-resAvgExtrap %>%select(Water_type,WB_ID,Region,Type,Typename,Period,QEtype,QualityElement,QualitySubelement,Indicator,IndSubtype,
             Months,Unit,Worst,PB,MP,GM,HG,Ref,Mean,StdErr,Code,Note) %>%
        mutate(Worst=as.numeric(Worst),
             PB=as.numeric(PB),
             MP=as.numeric(MP),
             GM=as.numeric(GM),
             HG=as.numeric(HG),
             Ref=as.numeric(Ref))#,
             #Region=region,Typename=typename)

        resMCExtrap<-resExtrap$dfMC
        resMCExtrap<-resMCExtrap %>% 
          mutate(WB_ID=values$wbselected,Type=values$typeselected,Note="Extrap",Code=0)
         resMCExtrap<-resMCExtrap %>% 
           left_join(select(df_indicators,Indicator,QEtype,QualityElement,QualitySubelement),by=c("Indicator"))
        resMCExtrap<-resMCExtrap %>% 
          left_join(dftemp,by=c("Type","Indicator","IndSubtype")) %>%
          rename(Ref=`RefCond`,HG=`H.G`,GM=`G.M`,MP=`M.P`,PB=`P.B`)  %>%
          select(Water_type,WB_ID,Region,Type,Typename,Period,QEtype,QualityElement,QualitySubelement,Indicator,IndSubtype,
             Months,Unit,Worst,PB,MP,GM,HG,Ref,sim,Value,Code,Note) %>%
          mutate(Worst=as.numeric(Worst),
             PB=as.numeric(PB),
             MP=as.numeric(MP),
             GM=as.numeric(GM),
             HG=as.numeric(HG),
             Ref=as.numeric(Ref))
        #browser()
      resAvgExtrap<-resAvgExtrap %>% mutate(Value=Mean)
      resAvgExtrap<-GetClass(resAvgExtrap)
      resMCExtrap<-GetClass(resMCExtrap)
      
      
      freq<-Frequency(resMCExtrap,Groups=c("Period","Indicator","IndSubtype"),varname="ClassID") %>%
        rename(fBad="C1",fPoor="C2",fMod="C3",fGood="C4",fHigh="C5")
      
      resAvgExtrap<-resAvgExtrap %>% left_join(freq,by=c("Period","Indicator","IndSubtype"))
          
      resMCExtrap<-resMCExtrap %>% left_join(select(resAvgExtrap,WB_ID,Period,Indicator,IndSubtype,Mean,StdErr,EQRavg=EQR,ClassAvg=Class),
                                             by=c("WB_ID","Period","Indicator","IndSubtype"))
      
    #filter out the results which will be replaced by extrapolated results
       dfmatch<-resAvgExtrap %>%
         filter(Code==0,!is.na(Mean)) %>%
         select(Period,Indicator) %>%
         mutate(OK=1)
     
       resAvg<-resAvg %>%
         left_join(dfmatch,by=c("Period","Indicator")) %>%
         filter(is.na(OK)) %>%
         select(-OK)
     
       resMC<-resMC %>%
         left_join(dfmatch,by=c("Period","Indicator")) %>%
         filter(is.na(OK)) %>%
         select(-OK)
      
      if(nrow(resAvg)>0){
        resAvg<-resAvg %>% bind_rows(resAvgExtrap)
        resMC<-resMC %>% bind_rows(resMCExtrap)
      }else{
        resAvg<-resAvgExtrap
        resMC<-resMCExtrap
      }
      
    }#if(nrow(resAvgExtrap)>0)
  } #if no extraploation data
    
    incProgress(0.1,message="aggregating results")
    #-----------------------------------------------------------------------
    values$resAvg <- resAvg
    values$resMC <- resMC
    values$resErr <- resErr
    values$res2MC <- ""
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""

  })
  updateTabItems(session, "tabs", "status")
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  #cat(paste0(wblist[1]," time:",time.taken,"\n"))
  
  
  return(0)
}

# --------------------------------------------------------------------
# ------------- RESULTS ----------------------------------------------
# --------------------------------------------------------------------
   # Status page titles
  output$SelectedWBStatus<-renderText({
    if(values$wbselected=="") {
      titletext<-"No Waterbody Selected"
    }else{
      titletext<-paste0(values$wbselected," - ",values$wbselectedname)
    }
    titletext
  })
  
  output$SelectedTypeStatus<-renderText({
    if(values$typeselectedname==""){
      titletext<-""
    }else{
      titletext<-paste0("Type: ",values$typeselectedname)
    }   
  })
  
  
  
  
  observeEvent(input$chkClassBnds, {
    if (nrow(values$resMC) > 0) {
      #str(paste0("dfMC updated n=", nrow(values$resMC)))
      if (input$chkClassBnds == TRUE) {
        grplist <- c(
          "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
          "Months","Unit","Note","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
        )
      } else{
        grplist <- c(
          "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator",
          "IndSubtype","Months","Unit","Note","Mean","StdErr","EQR","Class"
        )
      }
      
      df <-
        values$resMC %>% rename(
          EQRMC = "EQR",
          ClassMC = "Class",
          Class = "ClassAvg",
          EQR = "EQRavg"
        )
      
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 11,
        sDOM = "pl"
      )
    }
  }, ignoreInit = T)
  
  
  observeEvent(values$resMC, {
    #ShowHideDownload()
    if (nrow(values$resMC) > 0) {
      #str(paste0("dfMC updated n=", nrow(values$resMC)))
      #if (input$chkClassBnds == TRUE) {
        grplist <- c(
          "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
          "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
        )
      #} else{
      #  grplist <- c(
      #    "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement",
      #    "Indicator","IndSubtype","Note","Unit","Months","Mean","StdErr","EQR","Class"
      #  )
      #}
      
      df <-
        values$resMC %>% rename(
          EQRMC = "EQR",
          ClassMC = "Class",
          Class = "ClassAvg",
          EQR = "EQRavg"
        )
     
      # output$resTableMC <- ClassOutputTableDT(
      #   df,
      #   Groups = grplist,
      #   ClassVar = "ClassMC",
      #   roundlist = c("Mean", "StdErr", "EQR"),
      #   colOK = 11,
      #   sDOM = "pl"
      # )
      
      resMC <- values$resMC
      resAvg <- values$resAvg
      
      res1MC <-
        Aggregate(
          resMC,
          Groups = c("Region", "WB_ID", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = "Class")
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB_ID", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB_ID, Type, Typename, Period, Class)
      values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Region", "WB_ID", "Type", "Typename", "Period"))
    }
  }, ignoreInit = T)
  
  
  
  ShowHideExtrapErrs<-function(){
    if (nrow(values$resMC) > 0) {
      df <-
        values$resMC %>% rename(
          EQRMC = "EQR",
          ClassMC = "Class",
          Class = "ClassAvg",
          EQR = "EQRavg"
        )

      
      grplist <-
        c(
          "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype","Note","Unit","Months",
          #"Worst","PB","MP","GM","HG","Ref",
          "Mean","StdErr","EQR","Class"
        )
      output$resTableMC <- ClassOutputTableDT(
        df,
        Groups = grplist,
        ClassVar = "ClassMC",
        roundlist = c("Mean", "StdErr", "EQR"),
        colOK = 9,
        sDOM = "pl"
      )
      resMC <- values$resMC
      resAvg <- values$resAvg
      
      res1MC <-
        Aggregate(
          resMC,
          Groups = c("Region", "WB_ID", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = "Class")
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB_ID", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB_ID, Type, Typename, Period, Class)
      #cat("left join res1MC2")
      values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Region", "WB_ID", "Type", "Typename", "Period"))
    }
    values$res2MC <- ""
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  }
  
  
  observeEvent(input$resTable1_rows_selected, {
    df <-
      values$resMC %>% group_by(WB_ID, Period) %>% summarise() %>% ungroup()
    values$sWB <- df$WB_ID[input$resTable1_rows_selected]
    values$sPeriod <- df$Period[input$resTable1_rows_selected]
    df <- filter(values$resMC, WB_ID == values$sWB, Period == values$sPeriod)
    
    
    res2MC <-
      Aggregate(df,
                Groups = c("WB_ID", "Period", "Type", "sim"),
                level = 2) %>%
      rename(ClassMC = "Class", EQRMC = "EQR")
    df <- filter(values$resAvg, WB_ID == values$sWB, Period == values$sPeriod)

    res2Avg <-
      Aggregate(df,
                Groups = c("WB_ID", "Period", "Type"),
                level = 2) %>%
      select(WB_ID, Type, Period, QEtype, EQR, Class)
    values$res2MC <- res2MC %>% left_join(res2Avg,by = c("WB_ID", "Period", "Type", "QEtype"))
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  }, ignoreInit = T)
  
  observeEvent(input$resTable2_rows_selected, {
    n <- input$resTable2_rows_selected
    df <-
      values$res2MC %>% group_by(QEtype) %>% summarise() %>% ungroup()
    values$sQEtype <- df$QEtype[input$resTable2_rows_selected]
    df <-
      filter(values$resMC,
             WB_ID == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)
 
    res3MC <-
      Aggregate(df,
                Groups = c("WB_ID", "Period", "Type", "sim"),
                level = 3) %>%
      rename(ClassMC = "Class", EQRMC = "EQR")
    
    df <-
      filter(values$resAvg,
             WB_ID == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)

    res3Avg <-
      Aggregate(df,
                Groups = c("WB_ID", "Period", "Type"),
                level = 3) %>%
      select(WB_ID, Type, Period, QEtype, QualityElement, EQR, Class)
    values$res3MC <- res3MC %>% left_join(res3Avg,by = c("WB_ID", "Period", "Type", "QualityElement", "QEtype"))
    
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  }, ignoreInit = T)
  
  observeEvent(input$resTable3_rows_selected, {
    df <-
      values$res3MC %>% group_by(QualityElement) %>% summarise() %>% ungroup()
    values$sQualityElement <-
      df$QualityElement[input$resTable3_rows_selected]
    df <- filter(
      values$resMC,
      WB_ID == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )

    res4MC <-
      Aggregate(df,
                Groups = c("WB_ID", "Period", "Type", "sim"),
                level = 4) %>%
      rename(ClassMC = "Class", EQRMC = "EQR")
    
    df <- filter(
      values$resAvg,
      WB_ID == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )

    res4Avg <-
      Aggregate(df,
                Groups = c("WB_ID", "Period", "Type"),
                level = 4) %>%
      select(WB_ID,Type,Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    values$res4MC <- res4MC %>% left_join(res4Avg,by = c("WB_ID", "Period", "Type", "QualityElement", "QEtype", "QualitySubelement"))
    
    values$resInd <- ""
    values$resObs <- ""
  }, ignoreInit = T)
  
  observeEvent(input$resTable4_rows_selected, {
    #n<-input$resTable4_rows_selected
    df <-
      values$res4MC %>% group_by(QualitySubelement) %>% summarise() %>% ungroup()
    values$sQualitySubelement <-
      df$QualitySubelement[input$resTable4_rows_selected]
    
    df <- values$resMC
 
    
    values$resInd <- filter(
      df,
      WB_ID == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement,
      QualitySubelement == values$sQualitySubelement
    ) %>%
      rename(
        EQRMC = "EQR",
        ClassMC = "Class",
        Class = "ClassAvg",
        EQR = "EQRavg"
      )
  }, ignoreInit = T)
  
  observeEvent(input$resTableInd_rows_selected, {
    #browser()
    df <-
      values$resInd %>% group_by(Indicator,IndSubtype) %>% summarise() %>% ungroup()
    values$sIndicator <-
      df$Indicator[input$resTableInd_rows_selected]
    #browser()
    df <- SelectObs(
      dfobs(values$sWB,paste(paste0("'",values$periodselected,"'"),collapse = ",")),
      indicator = values$sIndicator,
      sWB = values$sWB,
      sPeriod = values$sPeriod,
      df_indicators,
      df_var
    )
    if(nrow(df)>0){
      values$resObs <- df %>% arrange(date,station)
    }else{
      values$resObs <- ""
    }
  }, ignoreInit = T)
  
  output$titleTable1 <- renderText({
    if (is.null(values$res1MC)) {
      "<h3>No results</h3>"
    } else{
      if (typeof(values$res1MC)!="list") {
        "<h3>No results</h3>" # style='color:#FF0000';
      } else{
        "<h3>Overall Results:</h3>"
      }
    }
  })
  
  
  observeEvent(values$res1MC, {
    rmlist = c("Region", "WB_ID", "Type", "Typename")
    df<-values$res1MC
    output$resTable1 <-
      ClassOutputTableDT(
        values$res1MC,
        Groups = c("Region", "WB_ID", "Type", "Typename", "Period", "Class"),
        roundlist = c("pGES"),
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
  }, ignoreInit = T)
  
  
  observeEvent(values$res2MC, {
    grplist <- c("WB_ID", "Period", "Type", "QEtype", "EQR", "Class")
    rmlist = c("WB_ID", "Period", "Type")
    
    output$resTable2 <- ClassOutputTableDT(
      values$res2MC,
      Groups = grplist,
      roundlist = c("EQR","pGES"),
      remove = rmlist,
      ClassVar = "ClassMC"
    )
    
    output$titleTable2 <- renderText({
      if (typeof(values$res2MC)!="list") {
        ""
      } else{
        "<h3>Biological/Supporting:</h3>"
      }
    })
    
  }, ignoreInit = T)
  
  
  observeEvent(values$res3MC, {
    grplist <-
      c("WB_ID","Period","Type","QEtype","QualityElement","EQR","Class")
    rmlist = c("WB_ID", "Period", "Type", "QEtype")
    output$resTable3 <-
      ClassOutputTableDT(
        values$res3MC,
        roundlist = c("EQR","pGES"),
        Groups = grplist,
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
    output$titleTable3 <- renderText({
      if (typeof(values$res3MC)!="list") {
        ""
      } else{
        "<h3>QualityElement:</h3>"
      }
    })
  }, ignoreInit = T)
  
  observeEvent(values$res4MC, {
    grplist <-
      c(
        "WB_ID","Period","Type","QEtype","QualityElement","QualitySubelement","EQR","Class"
      )
    rmlist = c("WB_ID", "Period", "Type", "QEtype", "QualityElement")
    output$resTable4 <-
      ClassOutputTableDT(
        values$res4MC,
        roundlist = c("EQR","pGES"),
        Groups = grplist,
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
    output$titleTable4 <- renderText({
      if (typeof(values$res4MC)!="list") {
        ""
      } else{
        "<h3>Subelement:</h3>"
      }
    })
  }, ignoreInit = T)
  
  observeEvent(values$resInd, {
    grplist <- c(
      "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator",
      "IndSubtype","Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
    )
    rmlist <- c("WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement")
    
    output$resTableInd <- ClassOutputTableDT(
      values$resInd,
      Groups = grplist,
      ClassVar = "ClassMC",
      roundlist = c("Mean", "StdErr", "EQR","pGES"),
      remove = rmlist,
      colOK = 3
    )
    output$titleTableInd <- renderText({
      if (typeof(values$resInd)!="list") {
        #if (values$resInd == "") {
        ""
      } else{
        "<h3>Indicators:</h3>"
      }
    })
  }, ignoreInit = T)
  
  
  
  
  observeEvent(values$resObs, {
    #if (typeof(values$sIndicator)=="list") {
    if (!is.null(values$sIndicator)) {
      vars = GetVarNames(values$sIndicator,df_indicators,df_var)
    } else{
      vars <- ""
    }
    dfselect<-NULL
    if (typeof(values$resObs)!="list") {
      plotHeight<-5
      plotWidth<-5
    }else{
      plotHeight<-400
      plotWidth<-600

    }
    #cat(paste0("value$resObs [",typeof(values$resObs),"]\n"))
    
    output$resTableObs <-
      ClassObsTableDT(values$resObs, sDOM = "pl", roundlist = vars)
    
    output$titleTableObs <- renderText({
      if (typeof(values$resObs)!="list") {
        ""
      } else{
        "Observations:"
      }
    })
    
    
    output$plotObs <- renderPlot({
      
      if (typeof(values$resObs)!="list") {
        p <- 0
        #browser()
      } else{
        
        
        yvar <- vars[length(vars)]
        
        df <- values$resObs 

        df$station <- as.factor(df$station)
        indicator<-values$sIndicator
        if(indicator %in% listWBindicators){
          months<-GetIndicatorMonths(indicator,values$typeselected,df_bound_WB)
        }else{
          months<-GetIndicatorMonths(indicator,values$typeselected,df_bound)
        }
        
        df <- df %>% 
          mutate(IndMonth=ifelse(month %in% months,T,F))
        dfselect<-df[input$resTableObs_rows_selected,]
        df1<- filter(df,IndMonth)
        df2<- filter(df,!IndMonth)
        
        # try to catch an error with data not being updated and not having the required variable
        if(yvar %in% names(df)){
          
        p<- ggplot() + geom_point(data=df1, aes_string(x = "date", y = yvar, colour="station"), size=2) +
          geom_point(data=df2, aes_string(x = "date", y = yvar, colour="station"), size=2,alpha=0.3) +
          geom_point(data=dfselect, aes_string(x = "date", y = yvar),size=4,alpha=1,shape=1) 
          p <- p + theme_minimal(base_size = 16) + scale_x_date(date_labels= "%d-%m-%Y") + labs(x = NULL) +
         theme(legend.position="bottom") + guides(col = guide_legend(ncol = 4,title=NULL))
        }else{
          #plotHeight<-5
          #plotWidth<-5
          p<-0
        }
        
        
      }
      return(p)
    }, height = plotHeight, width = plotWidth)
  })  
  

  
  output$download <- renderUI({
    if (nrow(values$resMC) > 0) {
    tagList(downloadButton("downloadButton", "Download Results"))
    }else{
      ""
    }
    })
  
  # Downloadable csv of selected dataset ----
  output$downloadButton <- downloadHandler(
    
    filename = function() {
      paste(values$wbselected, ".csv", sep = "")
    },
    content = function(file) {
      write.table(downloadResults(),file,row.names=F,sep=";", na="")
    }
  )
 
  
  
  downloadResults2<-function(){
    
    resMC <- values$resMC
    grplist <- c(  "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
                   "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class")
    resMC <-resMC %>% rename(EQRMC = "EQR",ClassMC = "Class",Class = "ClassAvg",EQR = "EQRavg")
    resMC <- 
      SummarizeSims(resMC,Groups=grplist,ClassVar="ClassMC")
    
    return(resMC)
  }
  
  
  downloadResults<-function(){

        withProgress(message = 'Preparing download...', value = 0, {
    resMC <- values$resMC
    resAvg <- values$resAvg
    
    
    res1MC <-Aggregate(resMC,Groups=c("WB_ID", "Type", "Period", "sim"),level=1) %>%
      rename(ClassMC = "Class")
    res1Avg <-Aggregate(resAvg,Groups = c("WB_ID", "Type", "Period"),level = 1) %>%
      select(WB_ID, Type, Period, Class)
    res1MC <- res1MC %>% left_join(res1Avg,by=c("WB_ID", "Type", "Period"))
    incProgress(0.1)
   
    res2MC <- Aggregate(resMC,Groups = c("WB_ID", "Period", "Type", "sim"),level = 2) %>%
      rename(ClassMC = "Class", EQRMC = "EQR")
    res2Avg <- Aggregate(resAvg,Groups = c("WB_ID", "Period", "Type"),level = 2) %>% select(WB_ID, Type, Period, QEtype, EQR, Class)
    res2MC <- res2MC %>% left_join(res2Avg,by = c("WB_ID", "Period", "Type", "QEtype"))
    incProgress(0.1)
    
    res3MC <-Aggregate(resMC,Groups = c("WB_ID", "Period", "Type", "sim"),level = 3) %>%
      rename(ClassMC = "Class", EQRMC = "EQR")
    res3Avg <-Aggregate(resAvg,Groups = c("WB_ID", "Period", "Type"),level=3) %>%
      select(WB_ID, Type, Period, QEtype, QualityElement, EQR, Class)
    res3MC <- res3MC %>% left_join(res3Avg,by = c("WB_ID", "Period", "Type", "QualityElement", "QEtype"))
    incProgress(0.1)

    res4MC <-Aggregate(resMC,Groups = c("WB_ID", "Period", "Type", "sim"),level = 4) %>%
      rename(ClassMC = "Class", EQRMC = "EQR")
    res4Avg <-Aggregate(resAvg,Groups = c("WB_ID", "Period", "Type"),level = 4) %>%
      select(WB_ID,Type,Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    res4MC <- res4MC %>% left_join(res4Avg,by = c("WB_ID", "Period", "Type", "QualityElement", "QEtype", "QualitySubelement"))
    incProgress(0.1)
    
    
    grplist <- c(  "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
                   "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class")
    resMC <-resMC %>% rename(EQRMC = "EQR",ClassMC = "Class",Class = "ClassAvg",EQR = "EQRavg")
    
    resMC <- 
      SummarizeSims(resMC,Groups=grplist,ClassVar="ClassMC")
    resMC <- resMC %>% mutate(id=as.numeric(rownames(resMC)))
   
    nr<-nrow(resMC)
    incProgress(0.1)
    
    grplist <-c("WB_ID", "Type", "Period", "Class")
    rmlist = c("WB_ID", "Type")
    res1MC <-
      SummarizeSims(res1MC,Groups=grplist , roundlist = c("pGES"),remove = rmlist,ClassVar = "ClassMC") 
    res1MC <- res1MC %>% mutate(id1=as.numeric(rownames(res1MC)))
    nr1<-nrow(res1MC)
    incProgress(0.1)
    
    grplist <- c("WB_ID", "Period", "Type", "QEtype", "EQR", "Class")
    rmlist = c("WB_ID", "Period", "Type")
    
    res2MC <- 
      SummarizeSims(res2MC,Groups = grplist,remove = rmlist, ClassVar = "ClassMC")  
    
    res2MC <- res2MC %>% mutate(id2=as.numeric(rownames(res2MC)))
    nr2<-nrow(res2MC)
    
    incProgress(0.1)
    
    grplist <-
      c("WB_ID","Period","Type","QEtype","QualityElement","EQR","Class")
    rmlist = c("WB_ID", "Period", "Type", "QEtype")
    
    res3MC <-
      SummarizeSims(res3MC,roundlist = c("EQR","pGES"),Groups = grplist,remove = rmlist,ClassVar = "ClassMC")  
    res3MC <- res3MC %>% mutate(id3=as.numeric(rownames(res3MC)))
    nr3<-nrow(res3MC)
    incProgress(0.1)
    
    
    grplist <-
      c("WB_ID","Period","Type","QEtype","QualityElement","QualitySubelement","EQR","Class")
    rmlist = c("WB_ID", "Period", "Type", "QEtype", "QualityElement")
    
    
    res4MC <-
      SummarizeSims(res4MC,roundlist = c("EQR","pGES"),Groups = grplist,remove = rmlist,ClassVar = "ClassMC")  
    res4MC <- res4MC %>% mutate(id4=as.numeric(rownames(res4MC)))
    nr4<-nrow(res4MC)
    
    res4MC$id4<-res4MC$id4*nr
    res3MC$id3<-res3MC$id3*nr*nr4
    res2MC$id2<-res2MC$id2*nr*nr4*nr3
    res1MC$id1<-res1MC$id1*nr*nr4*nr3*nr2
    
    res2MC <- res2MC %>% left_join(select(res1MC,Period,id1))
    res3MC <- res3MC %>% left_join(select(res2MC,Period,QEtype,id1,id2))
    res4MC <- res4MC %>% left_join(select(res3MC,Period,QEtype,QualityElement,id1,id2,id3))
    resMC <- resMC %>% left_join(select(res4MC,Period,QEtype,QualityElement,QualitySubelement,id1,id2,id3,id4))
    
    resMC <- resMC %>% rename(EQR_ind="EQR")
    res3MC <- res3MC %>% rename(EQR_QE="EQR")
    res4MC <- res4MC %>% rename(EQR_subQE="EQR")

    
    resMC <- bind_rows(resMC,res1MC,res2MC,res3MC,res4MC)
    resMC$sortorder<-rowSums(resMC[,c("id1","id2","id3","id4","id")],na.rm=T)
    resMC <- resMC %>% arrange(sortorder) %>%
      select(-c(id,id1,id2,id3,id4,sortorder))
    
    incProgress(0.1,message="done")
    })
    return(resMC)
    
  }
  
})


