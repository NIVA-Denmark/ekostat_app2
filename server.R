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


source("ReadIndicatorType.R")
source("classoutputtable.R")
source("Aggregation.R")
source("Assessment.R")
source("helpfunctions.R")
source("extrapolation.R")

source("IndicatorFunctions.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")
source("ReadBounds.R")

shinyServer(function(input, output, session) {
  
  # obtain the values of inputs
  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }
  
  # ------------------------ setup -----------------------------------------------
  # Path to the eksostat database
  dbpath<-"../efs/ekostat/ekostat2.db"

  dfind<-ReadIndicatorType()
  dfvar<-ReadVariances()
  dfbounds<-ReadBounds()
  
 # ---- Database functions -----------------------------------
  readdb <- function(dbname,strSQL){
    db <- dbConnect(SQLite(), dbname=dbname)
    df <- dbGetQuery(db, strSQL)
    dbDisconnect(db)
    return(df)
  }  
  
  
  dfobs <- function(wblist,periodlist){
    #browser()
    sql<-paste0("SELECT * FROM data WHERE period IN (",periodlist,") AND WB = '",wblist,"'")
    df<-readdb(dbpath, sql)
    #df <- df %>% filter(typology==values$typeselected) # do we still need to filter by tyopolgy?
    df$date<-as.Date(df$date,origin="1970-01-01")
    return(df)
  } 
  
  # Read list of indicators
  dfind<-ReadIndicatorType()
  dfperiod <- readdb(dbpath, "SELECT DISTINCT(Period) FROM resAvg")
  
  values <- reactiveValues(resMC = data.frame())
  values$wbselected <- ""
  values$wbselectedname <- ""
  values$typeselected <- ""
  values$typeselectedname <- ""
  values$df_ind_status<-""
  values$periodselected <- ""
  values$resAvgType <- ""
  values$resMCType <-""
  values$IndSelection<-""
  
  dfwb_lan <- readdb(dbpath, "SELECT * FROM WB_Lan") %>% # matching Län and WB_ID
    filter(!is.na(Lan)) %>%
    mutate(LanName=trimws(LanName,which="both"),Lan=trimws(Lan,which="both"))
  
  dfwb_mun <- readdb(dbpath, "SELECT * FROM WB_Mun") %>%  # matching Kommune and WB_ID
    filter(!is.na(Mun)) %>%
    mutate(MunName=trimws(MunName,which="both"),Mun=trimws(Mun,which="both"))
  
  dfwb_info <- readdb(dbpath, "SELECT * FROM WB_info") # type info WB_ID
  wb <- readdb(dbpath, "SELECT * FROM WB")             # available assessments
  
  pressure_list<-function(){
    if(!is.null(values$watertypeselected)){
    list<-c("Nutrient loading","Organic loading","Acidification","Harmful substances","Hydrological changes","Morphological changes","General pressure")
    list<-gsub(" ",".",list)
    df<-dfind[,c("Water_type",list)]
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
  

  
  # listWaterType<- dfind %>% 
  #   distinct(Water_type) %>%
  #   rename(Water=Water_type)
  
  period_list <- function(){
    c("2007-2012","2013-2018")
  }
  
  
# ------------ sidebar menu --------------------------------------------------  
  output$dy_menu <- renderMenu({ 
    sidebarMenu(id="tabs",
                menuItem("Waterbody", tabName = "waterbody", icon = icon("map-marker")),
                menuItem("Indicators", tabName = "indicators", icon = icon("tasks")),
                #menuItem("Data", tabName = "data", icon = icon("database"))
                menuItem("Status", tabName = "status", icon = icon("bar-chart"))
                #menuItem("Download", tabName = "download", icon = icon("file"))
                #menuItem("Options", tabName = "options", icon = icon("cog"))#,
    )
  })
  
# ------ Output components for the Waterbody selection page -----------------------
  output$selectWaterType <- renderUI({
    tagList(selectInput(
      "waterType",
      "Water type:",
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
      db <- dbConnect(SQLite(), dbname=dbpath)
      sql<-paste0("SELECT COUNT(*) FROM data WHERE period IN (",periodlist,") AND  WB IN ('",wbidselect,"')")
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
    df <- wb_list() %>% select(WB_ID,WB_Name,District,Lan,Municipality,typology)
    names(df)<-c("WB ID","WB Name","District","Län","Municipality","Type" )
    df
  }, selection = 'single', rownames= F,options = list(lengthMenu = c(5, 10, 20, 50), pageLength = 5))
  
  # ----- reactive data for the waterbody selection
  
  lan_list <- reactive({
    #browser()
    Lan <- c("ALL")
    all <- data.frame(Lan,row.names=F,stringsAsFactors=F)
    df<-dfwb_lan  %>%
      distinct(Lan,LanID,LanName) %>%
      arrange(LanName) %>%
      select(Lan)
    df<-bind_rows(all,df)
    df$Lan
  })
  
  mun_list <- reactive({
    Mun <- c("ALL")
    all <- data.frame(Mun,row.names=F,stringsAsFactors=F)
    df<-dfwb_mun  %>%
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
          dfselect<-dfwb_lan %>% 
            filter(Lan==input$lan) %>%
            select(WB_ID)
          df <- df %>% inner_join(dfselect,by="WB_ID")
        }}
    if (!is.null(input$mun)){
      if(input$mun!="ALL"){
        dfselect<-dfwb_mun %>% 
          filter(Mun==input$mun) %>%
          select(WB_ID)
        df <- df %>% inner_join(dfselect,by="WB_ID")
      }}
    
    #browser()
    df <- df %>%
      distinct(CLR,typology) %>%
      mutate(typesort=as.numeric(ifelse(CLR=="Coast",gsub("n","",gsub("s","",typology)),typology)))
     
    df <- df %>%
      arrange(CLR,typesort,typology) %>%
      select(Type=typology)
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
        dfselect<-dfwb_mun %>% 
          filter(Mun==input$mun) %>%
          select(WB_ID)
        df <- df %>% inner_join(dfselect,by="WB_ID")
      }}
        
    if (!is.null(input$lan)){
      if(input$lan!="ALL"){
        dfselect<-dfwb_lan %>% 
          filter(Lan==input$lan) %>%
          select(WB_ID)
        df <- df %>% inner_join(dfselect,by="WB_ID")
      }}
    if (!is.null(input$type)){
      if(input$type!="ALL"){
        df <- df %>% filter(typology==input$type)
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
    values$typeselected<-dfwb_info[dfwb_info$WB_ID==values$wbselected,"typology"]
    values$periodselected<-input$period
    if(input$waterType=="Coast"){
      values$watertypeselected<-"Coastal"
    }else if(input$waterType=="Lake"){
      values$watertypeselected<-"Lakes"
    }else if(input$waterType=="River"){
      values$watertypeselected<-"Rivers"
    }
  
    values$IndSelection<-""
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
      "Select Pressure",
      selected = pressure_list()[1],
      choices = pressure_list(),
      multiple = FALSE
    ))
  })
  
  observeEvent(input$pressure, {
    values$IndSelection<-""
    UpdateIndTable()
  }, ignoreInit = T)
  
  output$goButton <- renderUI({
    if(values$wbselected==""){
      ""
    }else{
      if(countIndicators()==0){
        ""
      }else{
        tagList(actionButton("goButton", "Calculate Status"))
      }
    }
  })
  

  
  # --------------- update indicator information / selection ----------------
  
  #tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
  #        Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
  # })"))
  
  
  updatedtind<-function(reset=0){
    
  output$dtind = DT::renderDataTable({
    df<-values$df_ind_status
    if(typeof(df)!="list"){
      df<-data.frame() 
    }else{
      num_col<-ncol(df)
      
      df$row<-seq(1,nrow(df),1)
      df$row<-NULL

      df$Check<-shinyInput(checkboxInput, nrow(df), 'ind_', value = df$Selected,labels=df[,"IndicatorDescription"])
      df$Extrapolate<-shinyInput(checkboxInput, nrow(df), 'extrap_', value=F,labels="",width = '30px')

            # reorder_columns
      df<-df[c(num_col+1,seq(2,num_col-3,1),num_col+2)]
      df %>% rename(Indicator=Check)
    }
  },server=FALSE, escape=FALSE,selection='single',rownames=F, 
  options=list(dom = 't',pageLength = 99,autoWidth=TRUE,
               preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')))
}
  
   observeEvent(input$extrapAll, {
     cat(paste0("extrapAll:",input$extrapAll,"\n"))
   }, ignoreInit = T)
  

  UpdateIndTable<-function(){
    
      # Get the info on the status for the indicators
      bOK<-TRUE
      
      #db <- dbConnect(SQLite(), dbname=dbpath)
      sql<-paste0("SELECT * FROM resAvg WHERE WB ='",values$wbselected,"'")
      #df <- dbGetQuery(db, sql)
      #dbDisconnect(db)
      #browser()
      df <- readdb(dbpath,sql)
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
        pname<-gsub(" ",".",pname)
        df2 <- dfind %>% filter(Water_type==values$watertypeselected)
        df2 <- df2[df2[,pname]=="X",]
        df2 <- df2 %>% filter(!is.na(Indicator))
        df2 <- df2 %>% select(Indicator)
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
        
        
        db <- dbConnect(SQLite(), dbname=dbpath)
        sql<-paste0("SELECT * FROM resAvg WHERE Type ='",values$typeselected,
                    "' and Indicator in (",indlist,") AND WB <>'", values$wbselected,"'")
        dftypeperiod <- readdb(dbpath, sql)
        dbDisconnect(db)
        
        if(nrow(dftypeperiod)>0){
          
       dftypeperiod<-CleanSubTypes(dftypeperiod)
        
        dfwb_type <- dfwb_info %>% distinct(WB_ID,WB_Name)
        #browser()
        dftypeperiod <- dftypeperiod %>% 
          left_join(dfwb_type,by=c("WB"="WB_ID")) 
        dftypeperiod <- dftypeperiod %>%
          filter(Code==0) 
        dftypeperiod<- df2 %>% left_join(dftypeperiod,by=c("Indicator","Period")) %>%
          filter(!is.na(Mean))
        
        
        
        df <- df2 %>% left_join(df,by=c("Indicator","Period")) %>%
          mutate(Code=ifelse(is.na(Code),-99,Code)) %>%
          mutate(Data=ifelse(Code=='0',"OK",ifelse(Code=='-1',"<3yrs","-")),
                 Code=ifelse(Data=="OK",0,1))

          dfext <- df %>% 
            group_by(Indicator) %>% 
            summarise(sum=sum(Code,na.rm=T)) %>%
            ungroup() %>%
            mutate(Extrap=ifelse(sum==0,F,T)) %>%
            select(-sum) 
          
        df <- df %>% 
          select(-Code) %>%
          spread(key="Period",value="Data") %>%
          left_join(select(dfind,Indicator,IndicatorDescription),by="Indicator") %>%
          left_join(dfext,by=c("Indicator")) 

        df<-dforder %>% left_join(df,by="Indicator") %>% mutate(Selected=TRUE)
        
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
        
      }else{
        values$df_ind_status <-""
        values$resAvgType <-""
      }

      updatedtind()
      #browser()
      }

  # ---------- DataTable with stations for extrapolation ---------------------
  output$dtextrap = DT::renderDataTable({

    df<-values$resAvgType
    if(typeof(df)!="list"){
      df<-data.frame()
    }else{
        i<-input$dtind_rows_selected
        if(is.null(i)){
          df<-data.frame()
        }else{
        dfind<-values$df_ind_status
        indicator<-dfind$Indicator[i]
        values$dtcurrentindicator<-indicator
        df <- df %>% filter(Indicator==indicator)
      if(nrow(df)>0){
        #dfsave<-df
        #save(dfsave,file="test.Rda")
        
        df <- df %>% distinct(WB,WB_Name,Include)

        df$Use<-shinyInput(checkboxInput, nrow(df), 'usestn_', value = df$Include, labels="",width='30px')# labels=df[,"WB"])
        df<-df %>% select(WB,WB_Name)#,Use
        values$n_stn_extrap<-nrow(df)
        #output$btnExtrap <- renderUI({
        #  tagList(actionButton("btnExtrap", "Update"))
        #})

      }else{
        df<-data.frame(WB=c("No data"))
      }}
    }
    df
  },server=FALSE, escape=FALSE,selection='single',rownames=F,
  options=list(dom = 't',pageLength = 99,autoWidth=TRUE,
               preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')))

   
   
   listIndicators <- function(){
     dfi<-values$df_ind_status
     if(typeof(dfi)!="list"){
       df<-data.frame()
     }else{
       ni<-nrow(dfi)
       if(ni>0){
         df<-data.frame(
           Indicator=dfi$Indicator,
           Select=shinyValue('ind_',ni),
           Extrapolate=shinyValue('extrap_',ni)
         )
         
       }else{
         df<-data.frame()
       }
     }
     return(df)   
   }
   
   countIndicators<-function(){
     dfi<-values$df_ind_status
     if(typeof(dfi)!="list"){
       n<-0
     }else{
       ni<-nrow(dfi)
       if(ni>0){
         df<-data.frame(
           Indicator=dfi$Indicator,
           Select=shinyValue('ind_',ni))
        df<-df %>% filter(Select==T)
        n<-nrow(df)
       }else{
         n<-0
       }
     }
     return(n)   
   }
   
   
   # listStations <- function(){
   #   indicator<-values$dtcurrentindicator
   #   df<-values$resAvgType
   #   if(typeof(df)!="list"){
   #     df<-data.frame()
   #   }else{
   #     df<-df %>% filter(Indicator==indicator)
   #     ni<-nrow(df)
   #     if(ni>0){
   #       df<-data.frame(
   #         WB=df$Indicator,
   #         Select=shinyValue('usestn_',ni)
   #       )
   #       #cat(paste0("usestn_",ni,")=",shinyValue('usestn_',ni),"\n"))
   #     }else{
   #       df<-data.frame()
   #     }
   #   }
   #   return(df)   
   # }

  # observeEvent(input$btnExtrap, {
  #   df<-listStations()
  #   cat(paste0(df,"\n"))
  # })
   

   
# ------------------------------------------------------------------- 
# ------------- go calculate action -----------------------------
# ------------------------------------------------------------------- 
   
observeEvent(input$goButton, {
  start.time <- Sys.time()
  #df<-listIndicators()
  #browser()
  withProgress(message = 'Calculating...', value = 0, {
    df<- listIndicators()
    df <- df %>% filter(Select==T)
    IndList <- df$Indicator
    df <- df %>% filter(Extrapolate==T) # df now contains only indicators to be extrapolated
    ExtrapList <- df$Indicator
    
    cat(paste0("Indicator list:", paste(paste0("'",IndList,"'"),collapse = ","),"\n"))
    cat(paste0("Extrap list:", paste(paste0("'",ExtrapList,"'"),collapse = ","),"\n"))
    
    nSimMC <- input$n
    
    
    IndList<-paste(paste0("'",IndList,"'"),collapse = ",")
    periodlist<-paste(paste0("'",values$periodselected,"'"),collapse = ",")
    wblist<-paste(paste0("'",values$wbselected,"'"),collapse = ",")


 
  # Get results for the waterbody we are showing results for   
    db <- dbConnect(SQLite(), dbname=dbpath)
    sql<-paste0("SELECT * FROM resAvg WHERE period IN (",periodlist,") AND WB IN (",wblist,
                ") AND Indicator IN (",IndList,")")
    resAvg <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resYear WHERE period IN (",periodlist,") AND WB IN (",wblist,
                ") AND Indicator IN (",IndList,")")
    resYr <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resMC WHERE period IN (",periodlist,") AND WB IN (",wblist,
                ") AND Indicator IN (",IndList,") AND sim <= ",nSimMC)
    resMC <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resErr WHERE period IN (",periodlist,") AND WB IN (",wblist,
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
      matchcode<-c(0,-1)
    }else{
      matchcode<-c(0)
    }

    dfextrap<-values$resAvgType
    if(is.null(dfextrap)){dfextrap<-""}
    if(dfextrap!=""){
    dfextrap<- df %>% 
      left_join(dfextrap,by="Indicator") %>%
      filter(!is.na(WB))
    #? filter where 
    
    dfmatch<-resAvg %>% 
      filter(Code %in% matchcode) %>% 
      select(Period,Indicator) %>%
      mutate(OK=1) 
    dfextrap<-dfextrap %>% 
      left_join(dfmatch,by=c("Period","Indicator"))
    dfextrap<-dfextrap %>% 
      filter(is.na(OK)) %>%
      select(WB,Indicator,Period,Type)
    
    
    # this is the list of indicators available from extrapolation stations which 
    # do NOT have a result for the WB we are considering

    # Get results for the waterbodies used for extrapolation we are showing results for   
    wblisttype<-paste(paste0("'",dfextrap$WB,"'"),collapse = ",")
    db <- dbConnect(SQLite(), dbname=dbpath)
    sql<-paste0("SELECT * FROM resAvg WHERE period IN (",periodlist,") AND WB IN (",wblisttype,
                ") AND Indicator IN (",IndList,")")
    resAvgtype <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resYear WHERE period IN (",periodlist,") AND WB IN (",wblisttype,
                ") AND Indicator IN (",IndList,")")
    resYrtype <- dbGetQuery(db, sql)
    incProgress(0.1)
    sql<-paste0("SELECT * FROM resMC WHERE period IN (",periodlist,") AND WB IN (",wblisttype,
                ") AND Indicator IN (",IndList,") AND sim <= ",nSimMC)
    resMCtype <- dbGetQuery(db, sql)
    incProgress(0.1)
    dbDisconnect(db)
    
    #function(dfextrap,dfbnds,nsim,resYr,resAvg,resMC){
    resExtrap<-extrapolation(dfextrap,dfbounds,input$n,resYrtype,resAvgtype,resMCtype)
    
    resAvgExtrap<-resExtrap$dfAvg
    
    if(nrow(resAvgExtrap)>0){
      resAvgExtrap<-resAvgExtrap %>% mutate(WB=values$wbselected,Type=values$typeselected,Note="Extrap",Code=0)
      namelist<-paste(paste0("'",names(resAvgExtrap),"'"),collapse = ",")

      resAvgExtrap<-resAvgExtrap %>% left_join(select(dfind,Indicator,QEtype,QualityElement,QualitySubelement),by=c("Indicator"))
      resAvgExtrap<-resAvgExtrap %>% left_join(rename(dfbounds,IndSubtype=Depth_stratum),by=c("Type","Indicator","IndSubtype"))
      resAvgExtrap<-resAvgExtrap %>%rename(Ref=RefCond,HG="H.G",GM="G.M",MP="M.P",PB="P.B") 
      resAvgExtrap<-resAvgExtrap %>%select(Water_type,WB,Region,Type,Typename,Period,QEtype,QualityElement,QualitySubelement,Indicator,IndSubtype,
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
          mutate(WB=values$wbselected,Type=values$typeselected,Note="Extrap",Code=0)
         resMCExtrap<-resMCExtrap %>% 
           left_join(select(dfind,Indicator,QEtype,QualityElement,QualitySubelement),by=c("Indicator"))
        resMCExtrap<-resMCExtrap %>% 
          left_join(rename(dfbounds,IndSubtype=Depth_stratum),by=c("Type","Indicator","IndSubtype")) %>%
          rename(Ref=RefCond,HG="H.G",GM="G.M",MP="M.P",PB="P.B") %>%
          select(Water_type,WB,Region,Type,Typename,Period,QEtype,QualityElement,QualitySubelement,Indicator,IndSubtype,
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
        rename(fBad=C1,fPoor=C2,fMod=C3,fGood=C4,fHigh=C5)
      resAvgExtrap<-resAvgExtrap %>% left_join(freq,by=c("Period","Indicator","IndSubtype"))
          
      resMCExtrap<-resMCExtrap %>% left_join(select(resAvgExtrap,WB,Period,Indicator,IndSubtype,Mean,StdErr,EQRavg=EQR,ClassAvg=Class),
                                             by=c("WB","Period","Indicator","IndSubtype"))
   
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
    
      resAvg<-resAvg %>% bind_rows(resAvgExtrap)
      resMC<-resMC %>% bind_rows(resMCExtrap)
    
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
  cat(paste0(wblist[1]," time:",time.taken,"\n"))
  
  
}, ignoreInit = T)
  

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
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
          "Months","Unit","Note","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
        )
      } else{
        grplist <- c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator",
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
  
  
  observeEvent(values$resMC, {
    #ShowHideDownload()
    if (nrow(values$resMC) > 0) {
      #str(paste0("dfMC updated n=", nrow(values$resMC)))
      #if (input$chkClassBnds == TRUE) {
        grplist <- c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
          "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
        )
      #} else{
      #  grplist <- c(
      #    "WB","Type","Period","QEtype","QualityElement","QualitySubelement",
      #    "Indicator","IndSubtype","Note","Unit","Months","Mean","StdErr","EQR","Class"
      #  )
      #}
      
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
      
      resMC <- values$resMC
      resAvg <- values$resAvg
      
      res1MC <-
        Aggregate(
          resMC,
          Groups = c("Region", "WB", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = Class)
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB, Type, Typename, Period, Class)
      values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Region", "WB", "Type", "Typename", "Period"))
    }
  }, ignoreInit = T)
  
  
  
  ShowHideExtrapErrs<-function(){
    if (nrow(values$resMC) > 0) {
      df <-
        values$resMC %>% rename(
          EQRMC = EQR,
          ClassMC = Class,
          Class = ClassAvg,
          EQR = EQRavg
        )

      
      grplist <-
        c(
          "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype","Note","Unit","Months",
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
          Groups = c("Region", "WB", "Type", "Typename", "Period", "sim"),
          level = 1
        ) %>% rename(ClassMC = Class)
      res1Avg <-
        Aggregate(
          resAvg,
          Groups = c("Region", "WB", "Type", "Typename", "Period"),
          level = 1
        ) %>%
        select(Region, WB, Type, Typename, Period, Class)
      #cat("left join res1MC2")
      values$res1MC <- res1MC %>% left_join(res1Avg,by=c("Region", "WB", "Type", "Typename", "Period"))
    }
    values$res2MC <- ""
    values$res3MC <- ""
    values$res4MC <- ""
    values$resInd <- ""
    values$resObs <- ""
  }
  
  
  observeEvent(input$resTable1_rows_selected, {
    df <-
      values$resMC %>% group_by(WB, Period) %>% summarise() %>% ungroup()
    values$sWB <- df$WB[input$resTable1_rows_selected]
    values$sPeriod <- df$Period[input$resTable1_rows_selected]
    df <- filter(values$resMC, WB == values$sWB, Period == values$sPeriod)
    
    
    res2MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 2) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    df <- filter(values$resAvg, WB == values$sWB, Period == values$sPeriod)

    res2Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 2) %>%
      select(WB, Type, Period, QEtype, EQR, Class)
    values$res2MC <- res2MC %>% left_join(res2Avg,by = c("WB", "Period", "Type", "QEtype"))
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
             WB == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)
 
    res3MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 3) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <-
      filter(values$resAvg,
             WB == values$sWB,
             Period == values$sPeriod,
             QEtype == values$sQEtype)

    res3Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 3) %>%
      select(WB, Type, Period, QEtype, QualityElement, EQR, Class)
    values$res3MC <- res3MC %>% left_join(res3Avg,by = c("WB", "Period", "Type", "QualityElement", "QEtype"))
    
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
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )

    res4MC <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type", "sim"),
                level = 4) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    df <- filter(
      values$resAvg,
      WB == values$sWB,
      Period == values$sPeriod,
      QEtype == values$sQEtype,
      QualityElement == values$sQualityElement
    )

    res4Avg <-
      Aggregate(df,
                Groups = c("WB", "Period", "Type"),
                level = 4) %>%
      select(WB,Type,Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    values$res4MC <- res4MC %>% left_join(res4Avg,by = c("WB", "Period", "Type", "QualityElement", "QEtype", "QualitySubelement"))
    
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
      WB == values$sWB,
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
      sPeriod = values$sPeriod
    )
    if(nrow(df)>0){
      values$resObs <- df
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
    rmlist = c("Region", "WB", "Type", "Typename")
    df<-values$res1MC
    output$resTable1 <-
      ClassOutputTableDT(
        values$res1MC,
        Groups = c("Region", "WB", "Type", "Typename", "Period", "Class"),
        roundlist = c("pGES"),
        remove = rmlist,
        ClassVar = "ClassMC"
      )
    
  }, ignoreInit = T)
  
  
  observeEvent(values$res2MC, {
    grplist <- c("WB", "Period", "Type", "QEtype", "EQR", "Class")
    rmlist = c("WB", "Period", "Type")
    
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
      c("WB","Period","Type","QEtype","QualityElement","EQR","Class")
    rmlist = c("WB", "Period", "Type", "QEtype")
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
        "WB","Period","Type","QEtype","QualityElement","QualitySubelement","EQR","Class"
      )
    rmlist = c("WB", "Period", "Type", "QEtype", "QualityElement")
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
      "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator",
      "IndSubtype","Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class"
    )
    rmlist <- c("WB","Type","Period","QEtype","QualityElement","QualitySubelement")
    
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
      vars = GetVarNames(values$sIndicator)
    } else{
      vars <- ""
    }
    
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
        
        p <- ggplot(df, aes_string(x = "date", y = yvar, colour="station")) + geom_point(size=2) 
        p <- p + theme_minimal(base_size = 16) + scale_x_date(date_labels= "%d-%m-%Y") + xlab("Date") 
        
      }
      return(p)
    }, height = plotHeight, width = plotWidth)
  })  
  
  # ShowHideDownload<-function(){
  #   cat("ShowHideDownload\n")
  # output$downloadButton <- renderUI({
  #   if (nrow(values$resMC) > 0) {
  #     ""
  #   }else{
  #       tagList(actionButton("downloadButton", "Download Results"))
  #     }
  # })
  # }  
  
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
    grplist <- c(  "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
                   "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class")
    resMC <-resMC %>% rename(EQRMC = EQR,ClassMC = Class,Class = ClassAvg,EQR = EQRavg)
    resMC <- 
      SummarizeSims(resMC,Groups=grplist,ClassVar="ClassMC")
    
    return(resMC)
  }
  
  
  downloadResults<-function(){
    withProgress(message = 'Preparing download...', value = 0, {
    resMC <- values$resMC
    resAvg <- values$resAvg
    
    
    res1MC <-Aggregate(resMC,Groups=c("WB", "Type", "Period", "sim"),level=1) %>%
      rename(ClassMC = Class)
    res1Avg <-Aggregate(resAvg,Groups = c("WB", "Type", "Period"),level = 1) %>%
      select(WB, Type, Period, Class)
    res1MC <- res1MC %>% left_join(res1Avg,by=c("WB", "Type", "Period"))
    incProgress(0.1)
   
    res2MC <- Aggregate(resMC,Groups = c("WB", "Period", "Type", "sim"),level = 2) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    res2Avg <- Aggregate(resAvg,Groups = c("WB", "Period", "Type"),level = 2) %>% select(WB, Type, Period, QEtype, EQR, Class)
    res2MC <- res2MC %>% left_join(res2Avg,by = c("WB", "Period", "Type", "QEtype"))
    incProgress(0.1)
    
    res3MC <-Aggregate(resMC,Groups = c("WB", "Period", "Type", "sim"),level = 3) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    res3Avg <-Aggregate(resAvg,Groups = c("WB", "Period", "Type"),level=3) %>%
      select(WB, Type, Period, QEtype, QualityElement, EQR, Class)
    res3MC <- res3MC %>% left_join(res3Avg,by = c("WB", "Period", "Type", "QualityElement", "QEtype"))
    incProgress(0.1)

    res4MC <-Aggregate(resMC,Groups = c("WB", "Period", "Type", "sim"),level = 4) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    res4Avg <-Aggregate(resAvg,Groups = c("WB", "Period", "Type"),level = 4) %>%
      select(WB,Type,Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    res4MC <- res4MC %>% left_join(res4Avg,by = c("WB", "Period", "Type", "QualityElement", "QEtype", "QualitySubelement"))
    incProgress(0.1)
    
    
    grplist <- c(  "WB","Type","Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
                   "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class")
    resMC <-resMC %>% rename(EQRMC = EQR,ClassMC = Class,Class = ClassAvg,EQR = EQRavg)
    
    resMC <- 
      SummarizeSims(resMC,Groups=grplist,ClassVar="ClassMC")
    resMC <- resMC %>% mutate(id=as.numeric(rownames(resMC)))
   
    nr<-nrow(resMC)
    incProgress(0.1)
    
    grplist <-c("WB", "Type", "Period", "Class")
    rmlist = c("WB", "Type")
    res1MC <-
      SummarizeSims(res1MC,Groups=grplist , roundlist = c("pGES"),remove = rmlist,ClassVar = "ClassMC") 
    res1MC <- res1MC %>% mutate(id1=as.numeric(rownames(res1MC)))
    nr1<-nrow(res1MC)
    incProgress(0.1)
    
    grplist <- c("WB", "Period", "Type", "QEtype", "EQR", "Class")
    rmlist = c("WB", "Period", "Type")
    
    res2MC <- 
      SummarizeSims(res2MC,Groups = grplist,remove = rmlist, ClassVar = "ClassMC")  
    
    res2MC <- res2MC %>% mutate(id2=as.numeric(rownames(res2MC)))
    nr2<-nrow(res2MC)
    
    incProgress(0.1)
    
    grplist <-
      c("WB","Period","Type","QEtype","QualityElement","EQR","Class")
    rmlist = c("WB", "Period", "Type", "QEtype")
    
    res3MC <-
      SummarizeSims(res3MC,roundlist = c("EQR","pGES"),Groups = grplist,remove = rmlist,ClassVar = "ClassMC")  
    res3MC <- res3MC %>% mutate(id3=as.numeric(rownames(res3MC)))
    nr3<-nrow(res3MC)
    incProgress(0.1)
    
    
    grplist <-
      c("WB","Period","Type","QEtype","QualityElement","QualitySubelement","EQR","Class")
    rmlist = c("WB", "Period", "Type", "QEtype", "QualityElement")
    
    
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
    
    resMC <- resMC %>% rename(EQR_ind=EQR)
    res3MC <- res3MC %>% rename(EQR_QE=EQR)
    res4MC <- res4MC %>% rename(EQR_subQE=EQR)

    
    resMC <- bind_rows(resMC,res1MC,res2MC,res3MC,res4MC)
    resMC$sortorder<-rowSums(resMC[,c("id1","id2","id3","id4","id")],na.rm=T)
    resMC <- resMC %>% arrange(sortorder) %>%
      select(-c(id,id1,id2,id3,id4,sortorder))
    
    incProgress(0.1,message="done")
    })
    return(resMC)
    
  }
  
})


