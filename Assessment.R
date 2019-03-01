#  -------------------------------------------------------------------
#' ------ AssessmentMultiple ----------------------------------------
#' -------------------------------------------------------------------
#' 
#' Loop through Waterbodies and Call individual Assessment routine for each WB
#'

                            
AssessmentMultiple<-function(wblist,df_periods,df,outputdb,IndList,df_bounds,df_bounds_WB,df_indicators,df_variances,nSimMC=1000,bReplaceResults=T,logfile="",iStart=1){
  
  start_time <- Sys.time()
  if(logfile!=""){
    cat(paste0("Time: ",Sys.time(),"  Start\n"),file=logfile,append=F)
  }
  cat(paste0("Time: ",Sys.time(),"  Start\n"))
  
  if(bReplaceResults){
    bOVR<-TRUE
    bAPP<-FALSE
  }else{
    bOVR<-FALSE
    bAPP<-TRUE
  }
  
  wbcount<-nrow(wblist)
  
  for(iWB in iStart:wbcount){
    #for(iWB in 1:1){
    WB<-wblist$WB_ID[iWB]
    dfselect<-df %>% filter(WB_ID == WB)
    
    if(logfile!=""){
      cat(paste0(wblist$Category[iWB],"WB: ",WB," (",iWB," of ",wbcount ,")"),file=logfile,append=T)
    }
    cat(paste0(wblist$Category[iWB],"WB: ",WB," (",iWB," of ",wbcount ,")"))
    
    CLR<-wblist$CLR[iWB]
    typology<-wblist$Type[iWB]
    if(CLR=="Coast"){
      typology<-TypeLeadingZero(typology) # Add leading zero to typology
      typology_varcomp<-typology
    }else{
      typology_varcomp<-substr(typology,1,1)
    }

        AssessmentResults <- Assessment(CLR,WB,df_periods,dfselect, nsim = nSimMC, IndList,df_bounds,df_bounds_WB,df_indicators,df_variances,typology,typology_varcomp)
    
    ETA <- Sys.time() + (Sys.time() - start_time)*(wbcount-iWB) /(1+iWB-iStart)
    cat(paste0(" done at: ",Sys.time(),"  (elapsed: ",round(Sys.time() - start_time,4),") ETA=",ETA,"\n"))
    
    if(logfile!=""){
      cat(paste0("   done at: ",Sys.time(),"  (elapsed: ",round(Sys.time() - start_time,4),") ETA=",ETA,"\n"),file=logfile,append=T)
    }

    resAvg <- AssessmentResults[[1]]
    resMC <- AssessmentResults[[2]]
    resErr <- AssessmentResults[[3]]
    resYear <- AssessmentResults[[4]]
    db <- dbConnect(SQLite(), dbname=outputdb)
    
    if(!is.na(resAvg)){
      WB <- resAvg %>% group_by(WB_ID,Type,Period,Region,Typename) %>% summarise()
      dbWriteTable(conn = db, name = "resMC", resMC, overwrite=bOVR,append=bAPP, row.names=FALSE)
      dbWriteTable(conn = db, name = "resYear", resYear, overwrite=bOVR,append=bAPP, row.names=FALSE)
      dbWriteTable(conn = db, name = "WB", WB, overwrite=bOVR,append=bAPP, row.names=FALSE)
      dbWriteTable(conn = db, name = "data", dfselect, overwrite=bOVR,append=bAPP, row.names=FALSE)
      dbWriteTable(conn = db, name = "resAvg", resAvg, overwrite=bOVR,append=bAPP, row.names=FALSE)
    }
    dbWriteTable(conn = db, name = "resErr", resErr, overwrite=bOVR,append=bAPP, row.names=FALSE)
    dbDisconnect(db)
    
    bOVR<-FALSE
    bAPP<-TRUE
    
  }
  # if(logfile!=""){
  #   writeLines(paste0("Time: ",Sys.time(),"  (elapsed: ",round(Sys.time() - start_time,4),") Finished"), fileConn)
  #   close(fileConn)
  # }
  
  
  }
#-------------------------------------------------------------------
#' ------ Assessment ------------------------------------------------
#'------------------------------------------------------------------

#' Run Indicator Calculations for a single waterbody 
#' 
#' 
#' @param nsim Number of iterations for Monte Carlo simulation 
#' @param df_all 
#' @param IndicatorList 
#' 
#' @examples
#' 
#' 
#'             
Assessment <-
  function(CLR,WB,plist,df_all,nsim=1000,IndicatorList,df_bounds,df_bounds_WB,df_indicators,df_variances,typology,typology_varcomp) {
    #browser()
    pcount<-nrow(plist)
    df_months<- df_bounds %>% distinct(Indicator,Type,Months)
      
      for(iPeriod in 1:pcount){
        #dfp <- df_all %>% filter(WB_ID == wblist$WB_ID[iWB],Period == plist$Period[iPeriod])
        dfp <- df_all %>% filter(Period == plist$Period[iPeriod])
        cat(paste0(" [",plist$Period[iPeriod],"] "))
        
        # Get start and end years from the period text (e.g. "2001-2006")
        startyear<-as.numeric(substr(as.character(plist$Period[iPeriod]),1,4))
        endyear<-as.numeric(substr(as.character(plist$Period[iPeriod]),6,9))
        
        #Get the relevant subset of indicators (Coast/Lake/River)
        IndicatorListSubset <- IndicatorList[grep(CLR,IndicatorList)]

        # Global variable used to pass Lake Chl to Lake Secchi calculation
        LakeChlaGlobal<<-NA
        
        # Loop through selected indicators
        for(iInd in IndicatorListSubset){
          if(iInd=="CoastHypoxicArea"){
            BoundsList<-df_bounds_WB %>% filter(MS_CD==WB,Indicator==iInd)
          }else{
            # For LakeBiovol, LakeBiovolEQR, LakeChla, LakeChlaEQR use Gony boundaries, if biovol Gony >5% of biovol total
            if(iInd %in% c("LakeBiovol", "LakeBiovolEQR", "LakeChla", "LakeChlaEQR")){
              biovolmean<-mean(dfp$biovol,na.rm=TRUE)
              Gonytest<-0
              if(!is.nan(biovolmean)){
                if(biovolmean>0){
                  Gonytest <- mean(dfp$biovolGony,na.rm=TRUE)/biovolmean
                }
              }
              if(Gonytest>0.05){
                BoundsList<-df_bounds %>% filter(Type==paste0(typology,"Gony"),Indicator==iInd)
              }else{
                BoundsList<-df_bounds %>% filter(Type==typology,Indicator==iInd)
              }
            }else{
              BoundsList<-df_bounds %>% filter(Type==typology,Indicator==iInd)
            }
          }
          
          if(nrow(BoundsList)>0){ 
          IndSubtypes<-distinct(BoundsList,Depth_stratum)
          subcount<-nrow(IndSubtypes)
          dfsubs<-dfp

          for(iSub in 1:subcount){
            df<-dfsubs
            subtype<-IndSubtypes[iSub,1]
            if(!is.na(subtype)){
              if(subtype!=""){
                df<-FilterDepth(dfsubs,subtype)
              }
            }
            
            res<-IndicatorResults(df,typology,typology_varcomp,df_bounds,df_indicators,df_variances,iInd,startyear,endyear,nsim)
            #cat(paste0("    Indicator: ",iInd,"  res=",res$result_code,"\n"))
            
            ErrDesc<-ErrorDescription(res$result_code,BoundsList$MinYear[1],BoundsList$MinPerYear[1])
            
            if(res$result_code %in% c(0,-1,-2)){
              if(iInd=="LakeChla"){
                #save Chl to a global variable, used later by LakeSecchi
                LakeChlaGlobal<<-res$period$mean
              }
              #Period average results
              rm(df_temp)
              df_temp<-data.frame(Mean=res$period$mean,StdErr=res$period$stderr,Code=res$result_code)
              df_temp$Indicator<-iInd
              df_temp$IndSubtype<-subtype
              df_temp$WB_ID<-WB
              df_temp$Type<-typology
              df_temp$Period<-plist$Period[iPeriod]
              df_temp$Code<-res$result_code
              df_temp$Note<-ErrDesc
              
              if(exists("res_ind")){
                res_ind<-bind_rows(res_ind,df_temp)
              }else{
                res_ind<-df_temp
              }

              #Year average results
              rm(df_temp)
              
              df_temp<-data.frame(Year=res$annual$year,Mean=res$annual$mean,StdErr=res$annual$stderr)
              df_temp$Indicator<-iInd
              df_temp$IndSubtype<-subtype
              df_temp$WB_ID<-WB
              df_temp$Type<-typology
              df_temp$Period<-plist$Period[iPeriod]
              df_temp$Code<-res$result_code
              df_temp$Note<-ErrDesc
              
              if(exists("res_year")){
                res_year<-bind_rows(res_year,df_temp)
              }else{
                res_year<-df_temp
              }
              
              #Monte Carlo results
              rm(df_temp)
              df_temp<-data.frame(Estimate=res$indicator_sim,Code=res$result_code)
              df_temp$Indicator<-iInd
              df_temp$IndSubtype<-subtype
              df_temp$WB_ID<-WB
              df_temp$Type<-typology
              df_temp$Period<-plist$Period[iPeriod]
              df_temp$sim<-1:nsim
              df_temp$Code<-res$result_code
              
              if(exists("res_rnd")){
                res_rnd<-bind_rows(res_rnd,df_temp)
              }else{
                res_rnd<-df_temp
              }
              
              if(res$result_code %in% c(-1,-2)){
                
                #ErrDesc<-ErrorDescription(res$result_code,res$MinYear,res$MinPerYear)
                df_temp<-data.frame(WB_ID=WB,
                                    Type=typology,
                                    Period=plist$Period[iPeriod],
                                    Indicator=iInd,
                                    IndSubtype=subtype,
                                    Code=res$result_code,
                                    Error=ErrDesc)
                if(exists("res_err")){
                  res_err<-bind_rows(res_err,df_temp)
                }else{
                  res_err<-df_temp
                }
              }
              
            }else{ #res$result_code!=0
              #Add to the list of errors
              rm(df_temp)
              df_temp<-data.frame(Mean=NA,
                                  StdErr=NA,
                                  Code=res$result_code,
                                  Indicator=iInd,
                                  IndSubtype=subtype,
                                  WB_ID=WB,
                                  Type=typology,
                                  Period=plist$Period[iPeriod],
                                  Code=res$result_code,
                                  Note=ErrDesc)
              
              if(exists("res_ind")){
                res_ind<-bind_rows(res_ind,df_temp)
              }else{
                res_ind<-df_temp
              }
              #Year average results
              rm(df_temp)
              
              ps<-plist$Period[iPeriod]
              y1<-as.numeric(substr(ps,1,4))
              y2<-as.numeric(substr(ps,6,9))
              years<-seq(y1,y2,1)
              
              df_temp<-data.frame(Year=years)
              df_temp$Mean<-NA
              df_temp$StdErr<-NA
              df_temp$Indicator<-iInd
              df_temp$IndSubtype<-subtype
              df_temp$WB_ID<-WB
              df_temp$Type<-typology
              df_temp$Period<-plist$Period[iPeriod]
              df_temp$Code<-res$result_code
              df_temp$Note<-ErrDesc
              
              if(exists("res_year")){
                res_year<-bind_rows(res_year,df_temp)
              }else{
                res_year<-df_temp
              }
              
              df_temp<-data.frame(WB_ID=WB,
                                  Type=typology,
                                  Period=plist$Period[iPeriod],
                                  Indicator=iInd,
                                  IndSubtype=subtype,
                                  Code=res$result_code,
                                  Error=ErrDesc)
              if(exists("res_err")){
                res_err<-bind_rows(res_err,df_temp)
              }else{
                res_err<-df_temp
              }
              
              #Add empty lines to the MC results for the indicators with no data 
              
              rm(df_temp)
              Estimate <- rep(NA,1)
              df_temp<-data.frame(Estimate)
              df_temp$Code=res$result_code
              df_temp$Indicator<-iInd
              df_temp$IndSubtype<-subtype
              df_temp$WB_ID<-WB
              df_temp$Type<-typology
              df_temp$Period<-plist$Period[iPeriod]
              df_temp$sim<-NA #1:nsim
              #df_temp$Code<-res$result_code
              
              if(exists("res_rnd")){
                res_rnd<-bind_rows(res_rnd,df_temp)
              }else{
                res_rnd<-df_temp
              }
              #FALSE
              
            }
          } #for(iSub in 1:subcount)
          
        }else{
          # no matching indicator boundaries - nrow(BoundsList)>0
          #cat(paste0(" -> no boundary values (Indicator: ",iInd,", type:",typology,")\n"))
          #ErrDesc<-"missing boundary values"
          df_temp<-data.frame(WB_ID=WB,
                              Type=typology,
                              Period=plist$Period[iPeriod],
                              Indicator=iInd,
                              IndSubtype="",
                              Code=-96,
                              Error=ErrorDescription(-96))
          if(exists("res_err")){
            res_err<-bind_rows(res_err,df_temp)
          }else{
            res_err<-df_temp
          }
          
        }
        } #for(iInd in IndicatorList)
      }  #for(iPeriod in 1:pcount) 

    #---------------------- Summarise results --------------------------
    # Get indicator categories based on mean values

    if(exists("res_ind")){
    res_ind<- res_ind %>% select(WB_ID,Type,Period,Indicator,IndSubtype,Mean,StdErr,Code,Note)
    #res_ind<- res_ind %>% left_join(df_bounds, by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    res_ind<- res_ind %>% left_join(select(df_bounds,Water_type,Indicator,Unit,Months,Depth_stratum,Region,Type,Typename,MinYear,MinPerYear,RefCond,H.G,G.M,M.P,P.B,Worst),
                                    by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    #res_ind<- res_ind %>% 
    #  select(-c(ParameterVector_1:ParameterVector_10,V_WBperiod,V_WBannual))
                                    
    
    res_ind$Value<-res_ind$Mean
    
    #We now have some duplicates for BQI because there are different
    # Do we show mean concentrations where the indicator is EQR value?
    res_ind<-GetClass(res_ind)
    
    # Get indicator categories for MC results
    res_rnd<- res_rnd %>% select(WB_ID,Type,Period,Indicator,IndSubtype,sim,Estimate,Code)
    
    
    res_rnd<- res_rnd %>% left_join(select(df_bounds,Water_type,Indicator,Unit,Months,Depth_stratum,Region,Type,Typename,MinYear,MinPerYear,RefCond,H.G,G.M,M.P,P.B,Worst), 
                                    by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    
    #res_rnd<- res_rnd %>% left_join(df_bounds, 
    #                                by=c("Indicator"="Indicator","Type"="Type","IndSubtype"="Depth_stratum"))
    names(res_rnd)[names(res_rnd)=="Estimate"]<-"Value"
    
    res_rnd<-GetClass(res_rnd)

    res_rnd <- res_rnd %>% left_join(select(df_indicators,Indicator,QualityElement,QualitySubelement,QEtype),
                                     by=c("Indicator"))
    
    #Find counts for each category
    res_rnd_count <- res_rnd %>% filter(!is.na(ClassID)) %>%
      group_by(WB_ID,Period,Type,Indicator,IndSubtype,ClassID) %>% summarise(n=n())
    
    # Here we add zeros for ClassIDs which don't occur
    # this ensures that all 5 columns are present after transposing
    
    ClassID<-c(1,2,3,4,5)
    ClassID<-data.frame(ClassID)
    ClassID$X<-1
    
      
    res_rnd_distinct <- res_rnd %>% 
      group_by(WB_ID,Period,Type,Indicator,IndSubtype) %>% 
      summarise() %>%
      ungroup() %>%
      mutate(X=1) %>%
      left_join(ClassID,by=c("X")) %>%
      select(-X)
    
    if(nrow(res_rnd_count)==0){
      res_rnd_count <- res_rnd_distinct %>% mutate(n=NA)
    }else{
      res_rnd_count <- res_rnd_distinct %>%
        left_join(res_rnd_count,by = c("WB_ID", "Period", "Type", "Indicator", "IndSubtype", "ClassID"))
    }
    
    res_rnd_count$ClassID<-paste0("C",res_rnd_count$ClassID)
    res_rnd_count$n <- res_rnd_count$n/nsim

    res_rnd_count<-spread(res_rnd_count, ClassID, n, fill = NA)
  
    #res_rnd<-res_rnd %>% left_join(select(res_ind,WB_ID,Type,Period,Indicator,IndSubtype,Mean,StdErr,EQRavg=EQR,ClassAvg=Class), 
    #                               by=c("WB_ID"="WB_ID","Type"="Type","Period"="Period",
    #                                 "Indicator"="Indicator","IndSubtype"="IndSubtype")) 
    
    Categories<-c("Bad","Poor","Mod","Good","High","Ref")
    res_rnd$Class<-Categories[res_rnd$ClassID]
    
    res_rnd<-res_rnd %>% select(WB_ID,Period,Indicator,IndSubtype,sim,Value,ClassID,Class,EQR)
    
    res_ind<-left_join(res_ind,res_rnd_count,by = c("WB_ID", "Type", "Period", "Indicator", "IndSubtype"))
    
    res_ind <- res_ind %>% left_join(select(df_indicators,Indicator,QualityElement,QualitySubelement,QEtype),by = "Indicator")
    
    names(res_ind)[names(res_ind)=="C1"]<-"fBad"
    names(res_ind)[names(res_ind)=="C2"]<-"fPoor"
    names(res_ind)[names(res_ind)=="C3"]<-"fMod"
    names(res_ind)[names(res_ind)=="C4"]<-"fGood"
    names(res_ind)[names(res_ind)=="C5"]<-"fHigh"

    res<-list(data.frame)
    
    res[[1]]<-res_ind 
    res[[2]]<-res_rnd 
    if(!exists("res_err")){
      res_err<-data.frame(WB_ID=NA,Type=NA,Period=NA,Indicator=NA,
                          IndSubtype=NA,Code=NA,Error=NA)
    }
    res[[3]]<-res_err
    res[[4]]<-res_year
    }else{
      # no results
      res<-list(data.frame)
      res[[1]]<-NA
      res[[2]]<-NA
      res[[3]]<-res_err
      res[[4]]<-NA
    }
    
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
  #names(df)[names(df)==""]<-""
  df$Ref<-as.numeric(df$Ref)
  df$HG<-as.numeric(df$HG)
  df$GM<-as.numeric(df$GM)
  df$MP<-as.numeric(df$MP)
  df$PB<-as.numeric(df$PB)
  df$Worst<-as.numeric(df$Worst)
  
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
  # truncate EQR values <0 or >1
  df$EQR<-ifelse(df$EQR>1,1,df$EQR)
  df$EQR<-ifelse(df$EQR<0,0,df$EQR)
  #Class cannot be better than "High":
  df$ClassID<-ifelse(df$ClassID>5,5,df$ClassID)
  df$Class<-ifelse(is.na(df$ClassID),NA,Categories[df$ClassID])
  df<-select(df,-c(Resp,class1,class2,class3,class4,class5,Bnd1,Bnd2))
  return(df)
}

#' SalinityReferenceValues
#' 
#' 
SalinityReferenceValues <- function(df_bounds,typology,indicator,missing=1){
  refcond<-filter(df_bounds,Type==typology,Indicator==indicator)
  refcond<-refcond[,grep("Sali_", names(refcond), value=TRUE)]
  refcond<-as.numeric(refcond[1,])
  refcond[is.na(refcond)]<-missing
  return(refcond)
}

GetParameterVector<- function(df_bounds,typology,indicator,missing=NA){
  refcond<-filter(df_bounds,Type==typology,Indicator==indicator)
  refcond<-refcond[,grep("ParameterVector_", names(refcond), value=TRUE)]
  refcond<-as.numeric(refcond[1,])
  refcond[is.na(refcond)]<-missing
  return(refcond)
}

GetMinObs<-function(df_bounds,typology,indicator,missing=0){
  df<-filter(df_bounds,Type==typology,Indicator==indicator)
  MinYear<-df[1,"MinYear"]
  MinYear<-ifelse(is.na(MinYear),missing,MinYear)
  MinPerYear<-df[1,"MinPerYear"]
  MinPerYear<-ifelse(is.na(MinPerYear),missing,MinPerYear)
  return(list(MinYear=MinYear,MinObsPerYear=MinPerYear))
}

#' IndicatorMonths
#' 
#' 
IndicatorMonths <- function(df_months,typology,indicator){
  
  df_months<-df_months %>% filter(Indicator==indicator)
  #Are there different combinations of months for this indicator
  n <- nrow(summarise(group_by(df_months,Months),n=n()))
  if(n>1){
    #If so, then filter the boundary data by typology
    df_months<-df_months %>% filter(Indicator==indicator,Type==typology)
  }
  months<-df_months[1,"Months"]
  if(is.na(months)){months<-"1,2,3,4,5,6,7,8,9,10,11,12"}
  if(months=="1,2,..,12"){months<-"1,2,3,4,5,6,7,8,9,10,11,12"}
  months<-lapply(strsplit(months, ","),function(x) as.numeric(x))[[1]]
  return(months)
}

#' IndicatorMonths
#' 
#' 
VarianceComponents<-function(df_indicators,df_variances,typology,indicator){
  measurement<-df_indicators[df_indicators$Indicator==indicator,"Measurement"]
  
  wtype<-substr(indicator,1,5)
  if(substr(indicator,1,4)=="Lake"){
    wtype<-"Lakes"
  }
  if(substr(indicator,1,5)=="Coast"){
    wtype<-"Coastal"
    df_variances$Type<-lapply(df_variances$Type,function(x) TypeLeadingZero(x))  
  }
  if(substr(indicator,1,5)=="River"){
    wtype<-"Rivers"
  }

  # typology is NULL at this point - need to find out why it hasn't been set!
  df_variances<-df_variances %>% filter(Water_type==wtype,Type==typology, Measurement==measurement)
  variance_list <- list(V_station=df_variances$V_station[1],
                        V_obspoint=df_variances$V_station[1],
                        V_year=df_variances$V_year[1],
                        V_yearmonth=df_variances$V_yearmonth[1],
                        V_monthhour=df_variances$V_monthhour[1],
                        V_yearmonthhour=df_variances$V_yearmonthhour[1],
                        V_stationdate=df_variances$V_stationdate[1],
                        V_stationyear=df_variances$V_stationyear[1],
                        V_stationmonth=df_variances$V_stationmonth[1],
                        V_institution=df_variances$V_institution[1],
                        V_replication=df_variances$V_replication[1])
  variance_list <- lapply(variance_list, function(x) ifelse(is.na(x),0,x))
  
  return(variance_list)
}



#' IndicatorResults
#' 
#' 
IndicatorResults<-function(df,typology,typology_varcomp,df_bounds,df_indicators,df_variances,indicator,startyear,endyear,nsim){
  missing <- switch(indicator,0,
                    ChlaEQR      = 0.9,
                    TNsummer     = 50,
                    TNwinter     = 50
  )
  df_months<- df_bounds %>% distinct(Indicator,Type,Months)
  #RefCond_sali<-SalinityReferenceValues(df_bounds,typology,indicator,missing)
  ParameterVector<-GetParameterVector(df_bounds,typology,indicator,missing)
  
  # RefCond for TP: Function depends on availability of data (autumn circulation, annual or August)
  if(indicator %in% c("LakeTP","LakeTPEQR")){
    AugustOnly <- length(unique(df$month)) == 1 && unique(df$month)[1] == 8
    ParameterVector <- c(RefCond_LakeTP(mean(df$Abs_F420,na.rm=TRUE),100,0.6,AugustOnly))
  }
  # RefCond for Secchi depth: LakeChla should be second parameter in ParameterVector
  if(indicator %in% c("LakeSecchi","LakeSecchiEQR")){
    refcond<-RefCond_LakeSecchiDepth(mean(df$Abs_F420,na.rm=TRUE),LakeChlaGlobal)
    ParameterVector <- c(refcond,LakeChlaGlobal)
  }
  if(indicator %in% c("RiverTP","RiverTPEQR")){
    ParameterVector <- c(RefCond_RiverTP(mean(df$Abs_F420,na.rm=TRUE),100,1),rep(0,35))
  }
  
  
  
  MinObsList<-GetMinObs(df_bounds,typology,indicator,missing)
  MonthInclude <- IndicatorMonths(df_months,typology,indicator)
  #browser()
  variance_list<- VarianceComponents(df_indicators,df_variances,typology_varcomp,indicator)

  res<-CalculateIndicator(indicator,df,ParameterVector,MinObsList,variance_list,MonthInclude,startyear,endyear,n_iter=nsim)
  #                                    RefCondSali
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

# TypologyFixCoastal<-function(typology){
#   n<-nchar(typology)
#   if(substr(typology,n,n) %in% c("n","s")){
#     ns<-substr(typology,n,n)
#     typology<-substr(typology,1,n-1)
#   }else{
#     ns<-""
#   }
#   typology<-as.character(typology)
#   n<-nchar(typology)
#   if(n<2){
#     typology<-paste0("0",typology)
#   }
#   typology<-paste0(typology,ns)
#   return(typology)
# }

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

ErrorDescription<-function(ErrCode,nyear=0,nobs=0){
  if(ErrCode==0) return("OK")
  else if(ErrCode==-1) return(paste0("obs <",nyear," years"))
  else if (ErrCode==-2) return(paste0("<",nobs," obs per year"))
  else if (ErrCode==-90) return("no data")
  else if (ErrCode==-91) return("insufficient data")
  else if (ErrCode==-95) return("missing boundary values")
  else if (ErrCode==-96) return("missing boundary values")
  else return("")
} 
