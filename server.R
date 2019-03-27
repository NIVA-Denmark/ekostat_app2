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
source("download.R")

source("IndicatorFunctions.R")
source("read_parameter_files.R")

#source("ReadIndicatorType.R")
#source("ReadVariances.R")
#source("ReadBounds.R")
sessionInfo()

shinyServer(function(input, output, session) {
  
 
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
 
  dbinfo<-function(){
    return(dbpath_info)
  }
  
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
    sql<-paste0("SELECT * FROM data WHERE period IN (",periodlist,") AND WB_ID = '",wblist,"'")
    df<-readdb(dbpath(), sql)
    df$date<-as.Date(df$date,origin="1970-01-01")
    return(df)
  } 
  
  dfobs2 <- function(wblist,periodlist){
    sql<-paste0("SELECT * FROM data WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,")")
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
  
  listdefault<-c("Area affected by hypoxia (%)","Benthic Quality Index (BQI)","BenthicDiatomsIPS (EQR)","BenthicDiatomsPctPT","BenthicDiatomsTDI","BenthicInvertebratesBQI","BenthicInvertebratesDJ (EQR)","Chlorophyll a (EQR)","EindexW3","EQR8","FishVIX","FishVIXh","FishVIXsm","Multi-Species Maximum Depth Index (MSMDI)","Oxygen","Oxygen Concentration","Phytoplankton biovolume (EQR)","Phytoplankton Trophic Index (EQR)","Secchi depth","Secchi Depth (EQR)","Summer TP conc. (EQR)","Total Nitrogen","Total Phosphorus (EQR)","TrophicMacrophyteIndex (EQR)","Winter DIN conc. (EQR)","Winter DIP conc. (EQR)","Summer TN conc. (EQR)","Winter TN conc. (EQR)","Winter TP conc. (EQR)")
  
  
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
  
  regions<- function(){
    c("Södra Sverige","Norra Sverige <200 m","Norra Sverige 200-800 m","Norra Sverige >800 m")
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
                menuItem("Status", tabName = "status", icon = icon("bar-chart"))
    )
  })
  

  
# ------ Output components for the Waterbody selection page -----------------------

  output$selectRegion <- renderUI({
    bShow<-T
    if(!is.null(input$waterType)){
      if(input$waterType=="Coast"){
        bShow<-F
      }
    }
    if(bShow){
      tagList(selectInput(
        "region",
        "Region:",
        choices = region_list(),
        multiple = FALSE,
        width="180px"
      ))
    }else{
      ""
    }
  })
  
    output$selectWaterType <- renderUI({
    tagList(selectInput(
      "waterType",
      "Water type:",
      #choices = c("Coast","Lake"),
      choices = c("Coast","Lake","River"),
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
      width="180px"))
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
      sql<-paste0("SELECT COUNT(*) FROM resAvg WHERE period IN (",periodlist,") AND  WB_ID IN ('",wbidselect,"') AND Code>-3")

      nrows <- dbGetQuery(db, sql)
      dbDisconnect(db)
      values$WBinfo <- paste0(wbidselect," ",wbnameselect," (indicator count = ",nrows,")")
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
  }, selection = 'single', rownames= F,options = list(lengthMenu = c(5, 10, 20, 50), pageLength = 5))
  
  # ----- reactive data for the waterbody selection
  
  lan_list <- reactive({

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
  
  region_list <- reactive({
    Region <- c("ALL")
    all <- data.frame(Region,row.names=F,stringsAsFactors=F)
    df<-df_WB
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
    df <- df %>%
      distinct(CLR,Type_region) 
    
    df <- df %>%
      arrange(CLR,Type_region) %>%
      select(Region=Type_region) %>% 
      filter(!is.na(Region)) %>%
      distinct(Region)
    df$Region<-as.numeric(df$Region)
    
    df <- df %>% 
      mutate(RegionName=regions()[Region]) %>%
      mutate(Region=paste0(Region," ",RegionName))
    df<-bind_rows(all,df) 
    df$Region 
    
    
  })
  
  
  # make list of WB types
  type_list <- reactive({

    Type <- c("ALL")
    all <- data.frame(Type,row.names=F,stringsAsFactors=F)
    df<-df_WB
    
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
    if (!is.null(input$region)){
      if(input$region!="ALL"){
        region<-substr(input$region,1,1)
        dfselect<-df_WB %>% 
          filter(Type_region==region) %>%
          select(WB_ID)
        df <- df %>% inner_join(dfselect,by="WB_ID")
      }}    

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
    
    df <- df_WB 
    
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
    
    if (!is.null(input$region)){
      if(input$region!="ALL"){
        region <- substr(input$region,1,1)
        df <- df %>% filter(Type_region==region)
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
  

  observeEvent(input$buttonWB, {
    values$wbselected<-wb_list()[input$dtwb_rows_selected,"WB_ID"]
    values$wbselectedname<-wb_list()[input$dtwb_rows_selected,"WB_Name"]
    typeselected<-df_WB[df_WB$WB_ID==values$wbselected,"Type"]
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
      datatable(data.frame(),options=list(dom = 'tp',pageLength=20,autoWidth=TRUE),
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
  
  
  output$selectDefault<- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(countExtrapIndicators()<0){
        ""
      }else{
        tagList(actionButton("selectDefault", "Select default"))
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
      df<-df %>% rename(Indicator=IndicatorDescription)
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
      group_by(Period,Indicator,WB_ID)  %>%
      summarise(Code=max(Code,na.rm=T)) %>%
      ungroup() #%>%
      #filter(Code>-3)
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
      select(-c(id,Status,Indicator)) %>% 
      rename(Indicator=IndicatorDescription) 
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
   
   #------------------------------------------------------------
   #------------------------------------------------------------
   #------------------------------------------------------------
   
   observeEvent(input$selectDefault,{
     df<-values$df_ind_select
     rownos<-which(df$Indicator %in% listdefault)
     DT::selectRows(dt_proxy, rownos)

   })
   
   #------------------------------------------------------------
   #------------------------------------------------------------
   #------------------------------------------------------------
   
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
    
      # Get the info on the status for the indicators in the selected WB
      bOK<-TRUE
      pname<-input$pressure
      
      if(is.null(pname)){
        bOK<-FALSE
      }else if(pname==""){
          bOK<-FALSE
          }
      if(is.null(values$watertypeselected)){bOK<-FALSE}
      if(is.null(values$periodselected)){bOK<-FALSE}
      
      if(bOK){
        
        
        sql<-paste0("SELECT * FROM resAvg WHERE WB_ID ='",values$wbselected,"'")  
        df <- readdb(dbpath(),sql)

        sql<-paste0("SELECT * FROM data WHERE WB_ID ='",values$wbselected,"'")
        dfobs <- readdb(dbpath(),sql)
        df<-CleanSubTypes(df,dfobs)
        
        values$subtypes<- df %>% 
          filter(!is.na(IndSubtype)) %>%
          select(WB_ID,Period,Indicator,IndSubtype)
        
        #get list of indicators for the selected pressures (and coastal/lake/river type)
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
        #df <- df %>% select(Indicator,IndSubtype,Period,Code)
        df <- df %>% select(Indicator,Period,Code)
        indlist<-paste(paste0("'",df2$Indicator,"'"),collapse = ",")

        df2$X<-1
        df2$num<-seq(1,nrow(df2),1)
        dforder<-df2 %>% select(Indicator)
        
        dfperiod<-data.frame(values$periodselected,stringsAsFactors=F)
        dfperiod$X<-1
        
        df2<-df2 %>% left_join(dfperiod,by="X") %>% select(-c(X,num))
        names(df2) = c("Indicator","Period")
        
        
        #get list of WBs with same type
        dbpath_info
        sql<-paste0("SELECT WB_ID FROM WB_info WHERE Type ='",TypeLeadingZero(values$typeselected,addzero=F),
                    "' AND WB_ID <>'", values$wbselected,"'")  
        dfwblist <- readdb(dbinfo(), sql)
        wblist <-paste(paste0("'",dfwblist$WB_ID,"'"),collapse = ",")
        
        # get results from the WBs with the same type
        sql<-paste0("SELECT * FROM resAvg WHERE Indicator in (",indlist,
                    ") AND WB_ID in (", wblist,")")  
                
         dbname<-dbpath()
        dftypeperiod <- readdb(dbname, sql)
        #dbDisconnect(db)
        
        
        if(nrow(dftypeperiod)>0){
          #------------------------------------
       dftypeperiod<-CleanSubTypes(dftypeperiod)
        
        dfwb_type <- df_WB %>% distinct(WB_ID,WB_Name)
    
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
     dfextrapWB <-data.frame(WB_ID=character(),Period=character(),Indicator=character())
     if(typeof(dfextrap)=="list"){
       
       dfextrap <- dfextrap %>%
         left_join(select(df_indicators,Indicator,IndicatorDescription),by="Indicator")
       
       if(ncol(values$df_extrap)){
       
       df<-values$df_extrap[input$dtindextrap_rows_selected,] %>% 
         select(IndicatorDescription=Indicator,Period)
       
       df <- df %>% left_join(dfextrap,by=c("IndicatorDescription","Period"))
       dfextrapWB <- df %>% 
         distinct(Indicator,IndicatorDescription,Period,WB_ID) %>%   
         filter(!is.na(WB_ID)) %>%                            
         mutate(Select=T)
     }}
     
     values$dfextrapWB<-dfextrapWB
     
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
        periodmatch<-df[input$dtextrap_rows_selected,"Period"]
        df<-isolate(values$dfextrapWB) 
          df <- df%>% 
            filter(IndicatorDescription==indmatch,Period==periodmatch)
          if(nrow(df)>0){
            df$id<-1:nrow(df)
           selected <- df[df$Select==T,"id"]
          } else{
            selected<-NULL
          }
        df <- df %>% select(WB_ID)                                                    
        df <- df %>% left_join(select(df_WB,WB_ID=WB_ID,Name=WB_Name,Municipality),by="WB_ID")    
        
        values$current_extrap_WBs <- df %>% select(-Municipality)
        
        
      }
    output$dtextrapstn = renderDataTable(
      datatable(df,options=list(dom = 'tp',pageLength = 20,autoWidth=TRUE),
                 selection=list(mode='multiple',selected=selected)), 
      server=FALSE)
  })
  
  observeEvent(input$applyExtrapWBs,{
    df<-values$current_extrap_WBs
    if(nrow(df)>0){
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
    periodmatch<-df[input$dtextrap_rows_selected,"Period"]
    dfapply <- dfapply %>% mutate(IndicatorDescription=indmatch,Period=periodmatch)
    
    dfwb<-isolate(values$dfextrapWB)
    
    dfwb <- dfwb %>% 
      left_join(dfapply,by=c("IndicatorDescription","WB_ID","Period"))        
    
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

    withProgress(message = 'Calculating...', value = 0, {
    df<-values$df_ind_select
    df<-df[input$dtind_rows_selected,]

    df2<-df_indicators %>% 
      filter(Water_type==values$watertypeselected) %>% 
      select(Indicator,IndicatorDescription)

    df<-df %>%
      rename(IndicatorDescription=Indicator) %>%
      left_join(df2,by="IndicatorDescription")

    IndList <- df$Indicator

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

    sql<-paste0("SELECT WB_ID,Period,Indicator,IndSubtype,sim,Value,ClassID,Class,EQR FROM resMC WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,
                ") AND Indicator IN (",IndList,") AND sim <= ",nSimMC)
    resMC <- dbGetQuery(db, sql) 
    resMC <- resMC %>% 
      left_join(select(resAvg,Type,Typename,WB_ID,Period,Region,Indicator,IndSubtype,Code,QEtype,QualityElement,QualitySubelement,Note,Unit,Months,
                       Worst,PB,MP,GM,HG,Ref,Mean,StdErr,EQRavg=EQR,ClassAvg=Class,nobs,stns),
                by=c("WB_ID","Period","Indicator","IndSubtype"))

    subtypes<-values$subtypes %>%
      mutate(SubtypeOK=1)
    
    resMC <- resMC %>% 
      left_join(subtypes,by=c("WB_ID","Period","Indicator","IndSubtype")) %>%
      mutate(SubtypeOK=ifelse(is.na(IndSubtype),1,SubtypeOK)) %>%
      filter(SubtypeOK==1) %>%
      select(-SubtypeOK)
    resAvg <- resAvg %>% 
      left_join(subtypes,by=c("WB_ID","Period","Indicator","IndSubtype")) %>%
      mutate(SubtypeOK=ifelse(is.na(IndSubtype),1,SubtypeOK)) %>%
      filter(SubtypeOK==1) %>%
      select(-SubtypeOK)
    
    
    
    
    
    if(isolate(!input$IgnoreErr)){
      resMC <- resMC %>%
        mutate(EQR=ifelse(Code==0,EQR,NA),
               EQRavg=ifelse(Code==0,EQRavg,NA),
               Value=ifelse(Code==0,Value,NA),
               ClassID=ifelse(Code==0,ClassID,NA),
               ClassAvg=ifelse(Code==0,ClassAvg,NA),
               Class=ifelse(Code==0,Class,NA))
      resAvg <- resAvg %>%
        mutate(EQR=ifelse(Code==0,EQR,NA),
               Mean=ifelse(Code==0,Mean,NA),
               ClassID=ifelse(Code==0,ClassID,NA),
               Class=ifelse(Code==0,Class,NA))
    }
    

    incProgress(0.1)
    sql<-paste0("SELECT * FROM resErr WHERE period IN (",periodlist,") AND WB_ID IN (",wblist,
                ") AND Indicator IN (",IndList,")")         
    resErr <- dbGetQuery(db, sql)
    dbDisconnect(db)
    
    
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
    
    db <- dbConnect(SQLite(), dbname=dbpath())
    sql<-paste0("SELECT * FROM resAvg WHERE period IN (",periodlist,") AND WB_ID IN (",wblisttype,
                ") AND Indicator IN (",IndList,")")
    resAvgtype <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resYear WHERE period IN (",periodlist,") AND WB_ID IN (",wblisttype,
                ") AND Indicator IN (",IndList,")")
    resYrtype <- dbGetQuery(db, sql)
    incProgress(0.1)

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
      resAvgExtrap<-resAvgExtrap %>% left_join(rename(df_bound,IndSubtype=Depth_stratum),by=c("Type","Indicator","IndSubtype"))
      resAvgExtrap<-resAvgExtrap %>%rename(Ref=RefCond,HG="H.G",GM="G.M",MP="M.P",PB="P.B") 
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
          left_join(rename(df_bound,IndSubtype=Depth_stratum),by=c("Type","Indicator","IndSubtype")) %>%
          rename(Ref=RefCond,HG="H.G",GM="G.M",MP="M.P",PB="P.B") %>%
          select(Water_type,WB_ID,Region,Type,Typename,Period,QEtype,QualityElement,QualitySubelement,Indicator,IndSubtype,
             Months,Unit,Worst,PB,MP,GM,HG,Ref,sim,Value,Code,Note) %>%
          mutate(Worst=as.numeric(Worst),
             PB=as.numeric(PB),
             MP=as.numeric(MP),
             GM=as.numeric(GM),
             HG=as.numeric(HG),
             Ref=as.numeric(Ref))

      resAvgExtrap<-resAvgExtrap %>% mutate(Value=Mean)
      resAvgExtrap<-GetClass(resAvgExtrap)
      resMCExtrap<-GetClass(resMCExtrap)
      
      
      freq<-Frequency(resMCExtrap,Groups=c("Period","Indicator","IndSubtype"),varname="ClassID") %>%
        rename(fBad=C1,fPoor=C2,fMod=C3,fGood=C4,fHigh=C5)
      
      resAvgExtrap<-resAvgExtrap %>% left_join(freq,by=c("Period","Indicator","IndSubtype"))
      
      resAvgExtrap<-GetObsCountExtrap(resAvgExtrap,resAvgtype) 
      
      resMCExtrap<-resMCExtrap %>% left_join(select(resAvgExtrap,WB_ID,Period,Indicator,IndSubtype,Mean,StdErr,EQRavg=EQR,ClassAvg=Class,nobs,WBlist),
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
        resAvg <- GetStationIDs(resAvg)
        resAvg<-resAvgExtrap %>% bind_rows(resAvg)
        resMC<-resMCExtrap %>% bind_rows(resMC)
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
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
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
  
  # ------------- show overall results -----------------------------------------------------------------
  observeEvent(values$resMC, {
    #ShowHideDownload()
    if (nrow(values$resMC) > 0) {
        grplist <- c(
          "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
          "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
        )
       
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )
      
      resMC <- values$resMC
      resAvg <- values$resAvg

      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Period"),
          level = 1
        ) 
      
       res1MC <- AggregateMCovr(resMC,resAvg,res1Avg)
       res1MC <- res1MC %>%
         select(Period,sim,ClassID,ClassMC)
       
       res1Avg <- res1Avg %>%
         select(Period, Class)
       
      # TO DO 
      # for each WB/period combination, we need to calculate the worst QE for biological
      # then we need to find which is worst Biological or Supporting and use that for  
      
      values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Period"))
    }
  }, ignoreInit = T)
  
  
  # ------------- User selected Period from Table 1 (Overall Results) - Now show Biological/Supporting (Table 2) ----------------
  observeEvent(input$resTable1_rows_selected, {

    df <-values$resMC %>% 
      group_by(WB_ID, Period) %>% 
      summarise() %>% 
      ungroup()
    
    values$sWB <- df$WB_ID[input$resTable1_rows_selected]
    values$sPeriod <- df$Period[input$resTable1_rows_selected]
    dfMC <- filter(values$resMC, WB_ID == values$sWB, Period == values$sPeriod)
    dfAvg <- filter(values$resAvg, WB_ID == values$sWB, Period == values$sPeriod)
    
    res2MC<-AggregateMC(dfMC,dfAvg)
    
    res2Avg <-
      Aggregate(dfAvg,
                Groups = c("Period"),
                level = 2) %>%
      select(Period, QEtype, EQR, Class)
    
    values$res2MC <- res2MC %>% left_join(res2Avg,by = c("Period", "QEtype"))
 
    
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  }, ignoreInit = T)
  
  # ------------- User selected Biological or Supporting from Table 2 - Now show Quality Elements ------------- 
  observeEvent(input$resTable2_rows_selected, {
    n <- input$resTable2_rows_selected
    df <-
      values$res2MC %>% group_by(QEtype) %>% summarise() %>% ungroup()
    values$sQEtype <- df$QEtype[input$resTable2_rows_selected]
    df <-
      filter(values$resMC,
             #WB_ID == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)
 
    res3MC <-
      Aggregate(df,
                Groups = c("Period", "sim"),
                level = 3) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <-
      filter(values$resAvg,
             #WB_ID == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)

    res3Avg <-
      Aggregate(df,
                Groups = c("Period"),
                level = 3) %>%
      select(Period, QEtype, QualityElement, EQR, Class)
    values$res3MC <- res3MC %>% left_join(res3Avg,by = c("Period","QualityElement", "QEtype"))
    
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  }, ignoreInit = T)
  
  # ------------- User selected Quality Element from Table 3 - now show subelements ------------- 
  observeEvent(input$resTable3_rows_selected, {
    df <-
      values$res3MC %>% group_by(QualityElement) %>% summarise() %>% ungroup()
    values$sQualityElement <-
      df$QualityElement[input$resTable3_rows_selected]
    df <- filter(
      values$resMC,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )

    res4MC <-
      Aggregate(df,
                Groups = c("Period","sim"),
                level = 4) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <- filter(
      values$resAvg,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )

    res4Avg <-
      Aggregate(df,
                Groups = c("Period"),
                level = 4) %>%
      select(Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    values$res4MC <- res4MC %>% left_join(res4Avg,by = c("Period", "QualityElement", "QEtype", "QualitySubelement"))
    
    values$resInd <- ""
    values$resObs <- ""
  }, ignoreInit = T)
  
  # ------------- User selected Quality Subelement from Table 4 - now show indicators ------------- 
  observeEvent(input$resTable4_rows_selected, {
    df <-
      values$res4MC %>% group_by(QualitySubelement) %>% summarise() %>% ungroup()
    values$sQualitySubelement <-
      df$QualitySubelement[input$resTable4_rows_selected]
    
    df <- values$resMC
 
    
    values$resInd <- filter(
      df,
      #WB_ID == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement,
      QualitySubelement == values$sQualitySubelement
    ) %>%
      rename(
        EQRMC = EQR,
        ClassMC = Class,
        Class = ClassAvg,
        EQR = EQRavg
      )
  }, ignoreInit = T)
  
  # ------------- User selected Indicator from Indicator Table - now show observations ------------- 
  observeEvent(input$resTableInd_rows_selected, {
    df <-
      values$resInd %>% group_by(Indicator,Months,IndSubtype) %>% summarise() %>% ungroup()
    values$sIndicator <-
      df$Indicator[input$resTableInd_rows_selected]
    values$sIndSubtype <-
      df$IndSubtype[input$resTableInd_rows_selected]
    values$sMonths<-
      df$Months[input$resTableInd_rows_selected]
    df <- SelectObs(
      dfobs(values$sWB,paste(paste0("'",values$periodselected,"'"),collapse = ",")),
      indicator = values$sIndicator,
      indSubType = values$sIndSubtype,
      sWB = values$sWB,
      sPeriod = values$sPeriod,
      sMonths=values$sMonths,
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
    grplist <- c("Period","Class")
    rmlist = c("")
    df<-values$res1MC
    output$resTable1 <-
      ClassOutputTableDT(
        values$res1MC,
        Groups = grplist,
        roundlist = c("pGES"),
        remove = "",#rmlist,
        ClassVar = "ClassMC"
      )
    
  }, ignoreInit = T)
  
  
  observeEvent(values$res2MC, {
    grplist <- c("Period","QEtype", "EQR", "Class")
    rmlist = c("Period")
    
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
      c("Period","QEtype","QualityElement","EQR","Class")
    rmlist = c("Period", "QEtype")
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
      c("Period","QEtype","QualityElement","QualitySubelement","EQR","Class")
    rmlist = c("Period", "QEtype", "QualityElement")
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
    grplist <- c("Period","QEtype","QualityElement","QualitySubelement","Indicator",
      "IndSubtype","Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
    )
    rmlist <- c("Period","QEtype","QualityElement","QualitySubelement")
    
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
  output$viss<- renderUI({
    if (nrow(values$resMC) > 0) {
      tagList(checkboxInput("visscolumns", "Use VISS column names",value = T))
    }else{
      ""
    }
  })
  output$decimalsymbol<- renderUI({
    if (nrow(values$resMC) > 0) {
      tagList(checkboxInput("decimalcomma", "Use comma decimal",value = FALSE))
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
      values$wbselected
      if(isolate(input$decimalcomma)==T){
        decsymbol<-","
      }else{
        decsymbol<-"."
      }
      write.table(downloadResults(values$resMC,values$resAvg,VISScolumns=input$visscolumns),file,row.names=F,sep=";", na="",fileEncoding="Windows-1252",dec=decsymbol)
    }
  )
 
  
  
  downloadResults2<-function(){
    
    resMC <- values$resMC
    grplist <- c(  "WB_ID","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
                   "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class")
    resMC <-resMC %>% rename(EQRMC = EQR,ClassMC = Class,Class = ClassAvg,EQR = EQRavg)
    resMC <- 
      SummarizeSims(resMC,Groups=grplist,ClassVar="ClassMC")
    
    return(resMC)
  }
  
 
  
})


