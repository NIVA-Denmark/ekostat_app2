
#' Assessment
#' 
#' 
#' @param nsim Number of iterations for Monte Carlo simulation 
#'   
#' @param df.all 
#' @param IndicatorList 
#' 
#' @examples
Assessment <-
  function(df.all,nsim=1000,IndicatorList,df.bounds,df.bounds.hypox,df.bathy,df.indicators,df.variances) {
    
    # df.bounds<-ReadBounds()
    # df.bounds.hypox<-ReadBoundsHypoxicArea()
    # df.bathy<-ReadBathymetry()
    # df.indicators<-ReadIndicatorType()
    # df.variances<-ReadVariances()
    
    df.all$typology<-gsub("SE_", "", df.all$typology)

    df.months<- df.bounds %>% distinct(Indicator,Type,Months)
    
    wblist<-distinct(df.all,WB,typology)
    wbcount<-nrow(wblist)
    progfrac= 1/(wbcount*3*length(IndicatorList))
    
    # Loop through distinct waterbodies and periods in the data

    for(iWB in 1:wbcount){
      #cat(paste0("WB: ",wblist$WB[iWB]," (",iWB," of ",wbcount ,")\n"))
      df.temp<-df.all %>% filter(WB == wblist$WB[iWB])
      plist<-distinct(df.all,period)
      pcount<-nrow(plist)
      typology<-as.character(df.temp[1,"typology"])
      
      for(iPeriod in 1:pcount){
        
        dfp <- df.all %>% filter(WB == wblist$WB[iWB],period == plist$period[iPeriod])
        #cat(paste0("WB: ",wblist$WB[iWB]," (",iWB," of ",wbcount ,")  Period: ",plist$period[iPeriod],"\n"))
        cat(paste0("  Period: ",plist$period[iPeriod]," \n"))
        
        # Get start and end years from the period text (e.g. "2001-2006")
        startyear<-as.numeric(substr(as.character(plist$period[iPeriod]),1,4))
        endyear<-as.numeric(substr(as.character(plist$period[iPeriod]),6,9))

        # Loop through selected indicators
        for(iInd in IndicatorList){
          BoundsList<-df.bounds %>% filter(Type==typology,Indicator==iInd)
          IndSubtypes<-distinct(BoundsList,Depth_stratum)
          subcount<-nrow(IndSubtypes)
          dfsubs<-dfp
          #cat(paste0("Indicator: ",iInd," "))
              #browser()
          for(iSub in 1:subcount){
            df<-dfsubs
            subtype<-IndSubtypes[iSub,1]
            if(!is.na(subtype)){
              if(subtype!=""){
                df<-FilterDepth(dfsubs,subtype)
              }
            }
            
            # The oxygen indicator refers to two global dataframes: WB_bathymetry and BoundariesHypoxicArea
            # We need to set these before calling the O2 indicator
            # Indicator functions need to be modified so that thus information is sent as parameters in the function call!
            
            if (grepl("Oxygen",iInd,fixed=TRUE)) {
              BoundariesHypoxicArea <<- df.bounds.hypox %>% filter(WB==wblist$WB[iWB]) %>% select(Worst,P.B,M.P,G.M,H.G,RefCond) %>% as.list()
              WB_bathymetry <<- df.bathy %>% filter(WB==wblist$WB[iWB]) %>% select(area_pct,depth)
              if(!nrow(WB_bathymetry)>0){
                #cat(paste0("No bathymetry information for ",wblist$WB[iWB],"\n"))
                # the following 3 lines should be removed - they create a false bathymetry dataset 
                area_pct<-seq(1,100,by=1) 
                depth<-area_pct
                WB_bathymetry<<-data.frame(area_pct,depth)
              }
              if(!nrow(df.bounds.hypox %>% filter(WB==wblist$WB[iWB]))>0){
                #cat(paste0("No Hypoxic Area boundaries for ",wblist$WB[iWB],"\n"))
                # the following line should be removed - it creates a false hypoxic area boundaries dataset  
                BoundariesHypoxicArea <<- df.bounds.hypox %>% filter(WB=="SE582000-115270") %>% select(Worst,P.B,M.P,G.M,H.G,RefCond) %>% as.list()
              }
            }
            
            
            res<-IndicatorResults(df,typology,df.bounds,df.indicators,df.variances,iInd,startyear,endyear,nsim)
            #cat(paste0("Indicator: ",iInd,"  res=",res$result_code,"\n"))
            #browser()
            if(res$result_code %in% c(0,-1)){
              
              #Period average results
              rm(df.temp)
              df.temp<-data.frame(Mean=res$period$mean,StdErr=res$period$stderr,Code=res$result_code)
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$Code<-res$result_code
              #cat(paste0("Indicator: ",iInd,"  Result: ",res$result_code,"\n"))
              
              if(exists("res.ind")){
                res.ind<-bind_rows(res.ind,df.temp)
              }else{
                res.ind<-df.temp
              }

              #Year average results
              rm(df.temp)
              
              df.temp<-data.frame(Year=res$annual$year,Mean=res$annual$mean,StdErr=res$annual$stderr)
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$Code<-res$result_code
              #browser()
              if(exists("res.year")){
                res.year<-bind_rows(res.year,df.temp)
              }else{
                res.year<-df.temp
              }
              
              #Monte Carlo results
              rm(df.temp)
              df.temp<-data.frame(Estimate=res$indicator_sim,Code=res$result_code)
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$sim<-1:nsim
              df.temp$Code<-res$result_code
              
              if(exists("res.rnd")){
                res.rnd<-bind_rows(res.rnd,df.temp)
              }else{
                res.rnd<-df.temp
              }
              
              if(res$result_code==-1){
                ErrDesc<-"data <3years"
                df.temp<-data.frame(WB=wblist$WB[iWB],
                                    Type=wblist$typology[iWB],
                                    Period=plist$period[iPeriod],
                                    Indicator=iInd,
                                    IndSubtype=subtype,
                                    Code=res$result_code,
                                    Error=ErrDesc)
                if(exists("res.err")){
                  res.err<-bind_rows(res.err,df.temp)
                }else{
                  res.err<-df.temp
                }
              }
              
            }else{ #res$result_code!=0
              #Add to the list of errors
              ErrDesc <- "unspecified"
              if(res$result_code==-1) ErrDesc<-"data <3years"
              if(res$result_code==-90) ErrDesc<-"no data"
              if(res$result_code==-91) ErrDesc<-"insufficent data"
              
              rm(df.temp)
              df.temp<-data.frame(Mean=NA,StdErr=NA,Code=res$result_code)
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$Code<-res$result_code
              
              if(exists("res.ind")){
                res.ind<-bind_rows(res.ind,df.temp)
              }else{
                res.ind<-df.temp
              }
              #Year average results
              rm(df.temp)
              
              ps<-plist$period[iPeriod]
              y1<-as.numeric(substr(ps,1,4))
              y2<-as.numeric(substr(ps,6,9))
              years<-seq(y1,y2,1)
              
              df.temp<-data.frame(Year=years)
              df.temp$Mean<-NA
              df.temp$StdErr<-NA
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$Code<-res$result_code
              
              if(exists("res.year")){
                res.year<-bind_rows(res.year,df.temp)
              }else{
                res.year<-df.temp
              }
              
              
              df.temp<-data.frame(WB=wblist$WB[iWB],
                                  Type=wblist$typology[iWB],
                                  Period=plist$period[iPeriod],
                                  Indicator=iInd,
                                  IndSubtype=subtype,
                                  Code=res$result_code,
                                  Error=ErrDesc)
              if(exists("res.err")){
                res.err<-bind_rows(res.err,df.temp)
              }else{
                res.err<-df.temp
              }
              
              #Add empty lines to the MC results for the indicators with no data
              rm(df.temp)
              Estimate <- rep(NA,nsim)
              df.temp<-data.frame(Estimate)
              df.temp$Code=res$result_code
              df.temp$Indicator<-iInd
              df.temp$IndSubtype<-subtype
              df.temp$WB<-wblist$WB[iWB]
              df.temp$Type<-wblist$typology[iWB]
              df.temp$Period<-plist$period[iPeriod]
              df.temp$sim<-1:nsim
              df.temp$Code<-res$result_code
              
              if(exists("res.rnd")){
                res.rnd<-bind_rows(res.rnd,df.temp)
              }else{
                res.rnd<-df.temp
              }
              
            }
          } #for(iSub in 1:subcount)
          
          #incProgress(progfrac,detail=paste(wblist$WB[iWB],plist$period[iPeriod]))
        } #for(iInd in IndicatorList)
      }  #for(iPeriod in 1:pcount) 
      #cat(paste0(" Time elapsed: ",hms_span(start_time, Sys.time()) , "\n"))
      
      #cat(paste0("\n"))
      }    #for(iWB in 1:wbcount)
    #---------------------- Summarise results --------------------------
    # Get indicator categories based on mean values
    
    res.ind<- res.ind %>% select(WB,Type,Period,Indicator,IndSubtype,Mean,StdErr,Code)
    res.ind<- res.ind %>% left_join(df.bounds, by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    res.ind$Value<-res.ind$Mean
    
    #We now have some duplicates for BQI because there are different
    
    # Do we show mean concentrations where the indicator is EQR value?
    #res.ind$Value<-ifelse(res.ind$UseEQR==1,(res.ind$Mean/res.ind$Ref),res.ind$Mean)
    res.ind<-GetClass(res.ind)
    
    # Get indicator categories for MC results
    res.rnd<- res.rnd %>% select(WB,Type,Period,Indicator,IndSubtype,sim,Estimate,Code)
    
    res.rnd<- res.rnd %>% left_join(df.bounds, by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    names(res.rnd)[names(res.rnd)=="Estimate"]<-"Value"
    
    #res.rnd$Value<-ifelse(res.rnd$UseEQR==1,(res.rnd$Estimate/res.rnd$Ref),res.rnd$Estimate)
    res.rnd<-GetClass(res.rnd)
    #cat(paste0("Sim results: ",nrow(res.rnd),"\n"))
    
    res.rnd <- res.rnd %>% left_join(select(df.indicators,Indicator,QualityElement,QualitySubelement,QEtype),
                                     by=c("Indicator"))
    
    #Find counts for each category
    res.rnd.count <- res.rnd %>% filter(!is.na(ClassID)) %>%
      group_by(WB,Period,Type,Indicator,IndSubtype,ClassID) %>% summarise(n=n())
    
    # Here we add zeros for ClassIDs which don't occur
    # this ensures that all 5 columns are present after transposing
    
    ClassID<-c(1,2,3,4,5)
    ClassID<-data.frame(ClassID)
    ClassID$X<-1
    #if(nrow(res.rnd.count)==0){
    #  browser()
    #}
    #res.rnd.count$X<-1
      
    res.rnd.distinct <- res.rnd %>% 
      group_by(WB,Period,Type,Indicator,IndSubtype) %>% 
      summarise() %>%
      ungroup() %>%
      mutate(X=1) %>%
      left_join(ClassID,by=c("X")) %>%
      select(-X)
    
    if(nrow(res.rnd.count)==0){
      res.rnd.count <- res.rnd.distinct %>% mutate(n=NA)
    }else{
      res.rnd.count <- res.rnd.distinct %>%
        left_join(res.rnd.count,by = c("WB", "Period", "Type", "Indicator", "IndSubtype", "ClassID"))
    }
    
    res.rnd.count$ClassID<-paste0("C",res.rnd.count$ClassID)
    res.rnd.count$n <- res.rnd.count$n/nsim
    #browser()
    
    res.rnd.count<-spread(res.rnd.count, ClassID, n, fill = NA)
  
    #res.rnd<-res.rnd %>% left_join(res.ind, by=c("Indicator","QualityElement","QualitySubelement","QEtype")) %>% select(-c(Sali_0:Sali_36))
    
    res.rnd<-res.rnd %>% left_join(select(res.ind,WB,Type,Period,Indicator,IndSubtype,Mean,StdErr,EQRavg=EQR,ClassAvg=Class), 
                                   by=c("WB"="WB","Type"="Type","Period"="Period",
                                     "Indicator"="Indicator","IndSubtype"="IndSubtype")) %>% 
      select(-c(Sali_0:Sali_36))
    Categories<-c("Bad","Poor","Mod","Good","High","Ref")
    res.rnd$Class<-Categories[res.rnd$ClassID]
    
    res.ind<-left_join(res.ind,res.rnd.count,by = c("WB", "Type", "Period", "Indicator", "IndSubtype"))
    
    res.ind <- res.ind %>% left_join(select(df.indicators,Indicator,QualityElement,QualitySubelement,QEtype),by = "Indicator")
    
    names(res.ind)[names(res.ind)=="C1"]<-"fBad"
    names(res.ind)[names(res.ind)=="C2"]<-"fPoor"
    names(res.ind)[names(res.ind)=="C3"]<-"fMod"
    names(res.ind)[names(res.ind)=="C4"]<-"fGood"
    names(res.ind)[names(res.ind)=="C5"]<-"fHigh"

    res<-list(data.frame)
    
    Note<-c("OK","data<3yrs","missing","missing")
    Code<-c(0,-1,-90,-91)
    Note<-data.frame(Code,Note,stringsAsFactors=FALSE)
  
    #Indicators
    res[[1]] <-res.ind %>% left_join(Note, by="Code")#%>% select(WB,Type,Period,QualityElement,QualitySubelement,Indicator,Mean,StdErr,EQR,Class,fBad,fPoor,fMod,fGood,fHigh )
    res[[2]]<-res.rnd %>% left_join(Note, by="Code")
    if(!exists("res.err")){
      res.err<-data.frame(WB=NA,Type=NA,Period=NA,Indicator=NA,
                          IndSubtype=NA,Code=NA,Error=NA)
    }
    res[[3]]<-res.err
    res[[4]]<-res.year
    
    return(res)
    
  }

#' GetClass
#' 
#' 
#' @param df A dataframe 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \item{Value} 
#'   \item{Ref}{ } 
#'   \item{HG}{ } 
#'   \item{GM}{ } 
#'   \item{MP}{ } 
#'   \item{PB}{ } 
#'   \item{Worst}{ } 
#'   \item{Resp}{ } 
#'   #'   
#' 
#' @examples
GetClass<-function(df){
  Categories<-c("Bad","Poor","Mod","Good","High","Ref")
  names(df)[names(df)=="RefCond"]<-"Ref"
  names(df)[names(df)=="H.G"]<-"HG"
  names(df)[names(df)=="G.M"]<-"GM"
  names(df)[names(df)=="M.P"]<-"MP"
  names(df)[names(df)=="P.B"]<-"PB"

  
  df$Resp<-ifelse(df$HG > df$GM,-1,1)
  df$class1<-ifelse(df$Resp==1,df$Value<df$Ref,df$Value>df$Ref)
  df$class2<-ifelse(df$Resp==1,df$Value<df$HG,df$Value>df$HG)
  df$class3<-ifelse(df$Resp==1,df$Value<df$GM,df$Value>df$GM)
  df$class4<-ifelse(df$Resp==1,df$Value<df$MP,df$Value>df$MP)
  df$class5<-ifelse(df$Resp==1,df$Value<df$PB,df$Value>df$PB)
  df$ClassID<-df$class1+df$class2+df$class3+df$class4+df$class5+1
  df$Bnd1<-df$Worst
  df$Bnd2<-df$PB
  df$Bnd1<-ifelse(df$ClassID==2,df$PB,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==2,df$MP,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==3,df$MP,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==3,df$GM,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==4,df$GM,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==4,df$HG,df$Bnd2)
  df$Bnd1<-ifelse(df$ClassID==5,df$HG,df$Bnd1)
  df$Bnd2<-ifelse(df$ClassID==5,df$Ref,df$Bnd2)
  df$EQR<-0.2*((df$ClassID-1)+(df$Value-df$Bnd1)/(df$Bnd2-df$Bnd1))
  df$EQR<-ifelse(df$ClassID>5,1,df$EQR)
  #Class cannot be better than "High":
  df$ClassID<-ifelse(df$ClassID>5,5,df$ClassID)
  df$Class<-ifelse(is.na(df$ClassID),NA,Categories[df$ClassID])
  df<-select(df,-c(Resp,class1,class2,class3,class4,class5,Bnd1,Bnd2))
  return(df)
}

#' SalinityReferenceValues
#' 
#' 
SalinityReferenceValues <- function(df.bounds,typology,indicator,missing=1){
  refcond<-filter(df.bounds,Type==typology,Indicator==indicator)
  refcond<-refcond[,grep("Sali_", names(refcond), value=TRUE)]
  refcond<-as.numeric(refcond[1,])
  refcond[is.na(refcond)]<-missing
  return(refcond)
}

#' IndicatorMonths
#' 
#' 
IndicatorMonths <- function(df.months,typology,indicator){
  
  df.months<-df.months %>% filter(Indicator==indicator)
  #Are there different combinations of months for this indicator
  n <- nrow(summarise(group_by(df.months,Months),n=n()))
  if(n>1){
    #If so, then filter the boundary data by typology
    df.months<-df.months %>% filter(Indicator==indicator,Type==typology)
  }
  months<-df.months[1,"Months"]
  if(is.na(months)){months<-"1,2,3,4,5,6,7,8,9,10,11,12"}
  if(months=="1,2,..,12"){months<-"1,2,3,4,5,6,7,8,9,10,11,12"}
  months<-lapply(strsplit(months, ","),function(x) as.numeric(x))[[1]]
  return(months)
}

#' IndicatorMonths
#' 
#' 
VarianceComponents<-function(df.indicators,df.variances,typology,indicator){
  measurement<-df.indicators[df.indicators$Indicator==indicator,"Measurement"]
  df.variances<-df.variances %>% filter(Type==typology, Measurement==measurement)
  
  variance_list <- list(V_station=df.variances$V_station[1],
                        V_obspoint=df.variances$V_station[1],
                        V_year=df.variances$V_year[1],
                        V_yearmonth=df.variances$V_yearmonth[1],
                        V_stationdate=df.variances$V_stationdate[1],
                        V_stationyear=df.variances$V_stationyear[1],
                        V_stationmonth=df.variances$V_stationmonth[1],
                        V_institution=df.variances$V_institution[1],
                        V_replication=df.variances$V_replication[1])
  variance_list <- lapply(variance_list, function(x) ifelse(is.na(x),0,x))
  return(variance_list)
}



#' IndicatorResults
#' 
#' 
IndicatorResults<-function(df,typology,df.bounds,df.indicators,df.variances,indicator,startyear,endyear,nsim){
  missing <- switch(indicator,0,
                    ChlaEQR      = 0.9,
                    TNsummer     = 50,
                    TNwinter     = 50
  )
  df.months<- df.bounds %>% distinct(Indicator,Type,Months)
  RefCond_sali<-SalinityReferenceValues(df.bounds,typology,indicator,missing)
  MonthInclude <- IndicatorMonths(df.months,typology,indicator)
  
  variance_list<- VarianceComponents(df.indicators,df.variances,typology,indicator)
  #cat(paste0(indicator,"\n"))
  
  res<-CalculateIndicator(indicator,df,RefCond_sali,variance_list,MonthInclude,startyear,endyear,n_iter=nsim)

}

#' FilterDepth
#' 
#'
FilterDepth<-function(df,depths){
  # e.g. >20 m, 5-20 m, 5-60 m,  >5 m
  depths<-gsub(' m', '', depths)
  pos = regexpr('-', depths)
  if(pos>0){
    z1<-as.numeric(substr(depths,1,pos-1))
    z2<-as.numeric(substr(depths,pos+1,99))
    df<-df %>% filter(station_depth > z1) %>% filter(station_depth < z2)
  }else{
    z<-as.numeric(substr(depths,2,99))
    if(substr(depths,1,1)=="<"){
      df<-df %>% filter(station_depth <= z)
    }
    if(substr(depths,1,1)==">"){
      df<-df %>% filter(station_depth >= z)
    }
  }
  return(df)
}


hms_span <- function(start, end) {
  dsec <- as.numeric(difftime(end, start, unit = "secs"))
  hours <- floor(dsec / 3600)
  minutes <- floor((dsec - 3600 * hours) / 60)
  seconds <- dsec - 3600*hours - 60*minutes
  paste0(
    sapply(c(hours, minutes, seconds), function(x) {
      formatC(x, width = 2, format = "d", flag = "0")
    }), collapse = ":")
}
