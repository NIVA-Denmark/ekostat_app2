
extrapolation_single<-function(dfavg,dfyr,dfMC,dfbnds,nsim){
  require(dplyr)
  #browser()
  
  resMC<-data.frame(Indicator=c(NA),IndSubtype=c(NA),Period=c(NA),sim=c(NA),Value=c(NA),stringsAsFactors=F)
  resAvg<-data.frame(Indicator=c(NA),IndSubtype=c(NA),Period=c(NA),Mean=c(NA),stringsAsFactors=F)
  
  # we can't combine indicator subtypes
  # check if there are more than 1
  subtypes <- dfavg %>% 
    group_by(IndSubtype) %>% 
    summarise(n=n()) %>%
    arrange(desc(n))
  
  nsubtype<-nrow(subtypes)
  subtype<-subtypes$IndSubtype[1]
  
  if(is.list(dfavg)){
    if(nrow(dfavg)>0){
      indicator<-dfavg$Indicator[1]
      period<-dfavg$Period[1]
      yrfrom<-as.numeric(substr(period,1,4))
      yrto<-as.numeric(substr(period,6,9))
      type<-dfavg$Type[1]

      dfvar<-dfbnds %>% filter(Indicator==indicator,Depth_stratum==subtype,Type==type) 
      varper<-dfvar$V_WBperiod[1]
      varyr<-dfvar$V_WBannual[1]
      var_list<-list(V_WBperiod=varper,V_WBannual=varyr)
      
      ix<-0
      wbs<-dfavg %>% distinct(WB)
      ntype<-nrow(wbs)
      un<-NULL
      for(wb in wbs$WB){
        ix<-ix+1
        datperiod<-dfavg %>% filter(WB==wb) %>%
          select(mean=Mean,stderr=StdErr) 
        datannual<-dfyr %>% filter(WB==wb) %>%
          select(year=Year,mean=Mean,stderr=StdErr)
        indicator_sim<-dfMC %>% filter(WB==wb) %>%
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
  
  incProgress(0.1,message="doing extrapolation calculations")
  dfperiod <- dfextrap %>% left_join(resAvg,by=c("WB","Indicator","Type","Period"))
  dfyear <- dfextrap %>% left_join(resYr,by=c("WB","Indicator","Type","Period"))
  dfMC <- dfextrap %>% left_join(resMC,by=c("WB","Indicator","Type","Period"))
  
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

