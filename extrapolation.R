
extrapolation_single<-function(dfavg,dfyr,dfMC,dfbnds,nsim){
  require(dplyr)

  resMC<-data.frame(Indicator=c(NA),IndSubtype=c(NA),Period=c(NA),sim=c(NA),Value=c(NA),stringsAsFactors=F)
  resAvg<-data.frame(Indicator=c(NA),IndSubtype=c(NA),Period=c(NA),Mean=c(NA),stringsAsFactors=F)
  
  # we can't combine indicator subtypes
  # check if there are more than 1
  subtypes <- dfavg %>% 
    group_by(IndSubtype,Code) %>% 
    summarise(n=n()) %>%
    arrange(desc(Code),desc(n))
  
  nsubtype<-nrow(subtypes)
  subtype<-subtypes$IndSubtype[1]
  
  if(is.list(dfavg)){
    if(is.na(subtype)){
      dfavg <- dfavg %>% filter(Code>-3,is.na(IndSubtype))
      dfMC <- dfavg %>% 
        select(WB_ID,Indicator,Period,IndSubtype) %>%
        left_join(dfMC,by=c("WB_ID","Indicator","Period","IndSubtype"))
      dfyr <- dfyr %>% filter(Code>-3,is.na(IndSubtype))
    }else{
      dfavg <- dfavg %>% filter(Code>-3,IndSubtype==subtype)
      dfMC <- dfavg %>% 
        select(WB_ID,Indicator,Period,IndSubtype) %>%
        left_join(dfMC,by=c("WB_ID","Indicator","Period","IndSubtype"))
      dfyr <- dfyr %>% filter(Code>-3,IndSubtype==subtype)
    }
    
    if(nrow(dfavg)>0){
      indicator<-dfavg$Indicator[1]
      period<-dfavg$Period[1]
      yrfrom<-as.numeric(substr(period,1,4))
      yrto<-as.numeric(substr(period,6,9))
      type<-dfavg$Type[1]

      #dfvar<-dfbnds %>% filter(Indicator==indicator,Depth_stratum==subtype,Type==type) 
      dfvar<-dfbnds %>% filter(Indicator==indicator,Type==type) 
      if(is.na(subtype)){
        dfvar<-dfvar %>% filter(is.na(Depth_stratum))
      }else{
         dfvar<-dfvar %>% filter(Depth_stratum==subtype)
     }
      varper<-dfvar$V_WBperiod[1]
      varyr<-dfvar$V_WBannual[1]
      var_list<-list(V_WBperiod=varper,V_WBannual=varyr)
      ntype<-dfvar$ntype_WB
      if(is.na(ntype)){
        ntype<-10 # this is to stop errors if no number of WBs of same type is specified
       }
      
      ix<-0
      wbs<-dfavg %>% distinct(WB_ID)
      #ntype<-nrow(wbs)
      un<-NULL
      for(wb in wbs$WB_ID){
        ix<-ix+1
        datperiod<-dfavg %>% filter(WB_ID==wb) %>%
          select(mean=Mean,stderr=StdErr) 
        datannual<-dfyr %>% filter(WB_ID==wb) %>%
          select(year=Year,mean=Mean,stderr=StdErr)
        indicator_sim<-dfMC %>% filter(WB_ID==wb) %>%
          select(Value) %>% as.list()
        uni<-list(period=datperiod,annual=datannual,indicator_sim=indicator_sim$Value,result_code=0)
        if(ix==1){
          un<-list()
        }#else{
          un[[ix]]<-uni
        #}
        }
      
      df<-CalculateIndicatorType(indicator,un,var_list,ntype,yrfrom,yrto,n_iter=nsim)

      resMC<-data.frame(Indicator=indicator,IndSubtype=subtype,Period=period,sim=seq(1,nsim),Value=df$indicator_sim,stringsAsFactors=F)
      resAvg<-data.frame(Indicator=indicator,IndSubtype=subtype,Period=period,Mean=df$period$mean,StdErr=df$period$stderr,stringsAsFactors=F)
      df<-list(resAvg=resAvg,resMC=resMC)
    }else{
      df<-list(resAvg=resAvg,resMC=resMC)
    }
  }else{
    df<-list(resAvg=resAvg,resMC=resMC)
  }
  return(df)
}

extrapolation<-function(dfextrap,dfbnds,nsim,resYr,resAvg,resMC){
  require(purrr)
  # WB,Indicator,Period,Type
  # dfextrap contains distinct WB,Indicator,Period,Type to be used in extrapolation
  # WB here is the id of those used in extrapolation - NOT the one for the results
  
  #incProgress(0.1,message="doing extrapolation calculations")
  dfperiod <- dfextrap %>% left_join(resAvg,by=c("WB_ID","Indicator","Type","Period"))
  dfyear <- dfextrap %>% left_join(resYr,by=c("WB_ID","Indicator","Type","Period"))
  dfMC <- dfextrap %>% left_join(resMC,by=c("WB_ID","Indicator","Period"))
  
  listperiod<-dfperiod %>% split(list(.$Indicator,.$Period))
  listyr<-dfyear %>% split(list(.$Indicator,.$Period))
  listMC<-dfMC %>% split(list(.$Indicator,.$Period))
  listres<-pmap(list(listperiod,listyr,listMC),extrapolation_single,dfbnds,nsim)
  
  dfAvg<-map_dfr(listres, "resAvg")
  dfAvg <- dfAvg %>% 
    filter(!is.na(Mean))
  dfMC<-map_dfr(listres, "resMC")
  dfMC<-dfMC %>%
    filter(!is.na(Value))
  res<-list(dfAvg=dfAvg,dfMC=dfMC)
  return(res)
}

