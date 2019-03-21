CleanSubTypes <- function(df,dfobs=data.frame()){
  if(sum(names(df)=="Year")>0) {
    oksubtypes <- df %>% distinct(WB_ID,Indicator,IndSubtype,Period,Code,Year) %>% 
      filter(Code>-10,IndSubtype!="")
    
    subtypes <- df %>% distinct(WB_ID,Indicator,IndSubtype,Period,Year) %>%
      filter(IndSubtype!="")
    
    subtypes <- subtypes %>% 
      left_join(oksubtypes,by=c("WB_ID","Indicator","IndSubtype","Period","Year")) %>%
      filter(is.na(Code)) %>%
      select(-Code) %>%
      mutate(drop=1)
    
    df <- df %>% left_join(subtypes,by=c("WB_ID","Indicator","IndSubtype","Period","Year")) %>%
      filter(is.na(drop)) %>%
      select(-drop)
  }else{
    oksubtypes <- df %>% distinct(WB_ID,Indicator,IndSubtype,Period,Code) %>% 
      filter(Code>-10,IndSubtype!="")
    
    subtypes <- df %>% distinct(WB_ID,Indicator,IndSubtype,Period) %>%
      filter(IndSubtype!="")
    
    subtypes <- subtypes %>% 
      left_join(oksubtypes,by=c("WB_ID","Indicator","IndSubtype","Period")) %>%
      filter(is.na(Code)) %>%
      select(-Code) %>%
      mutate(drop=1)
    
    df <- df %>% left_join(subtypes,by=c("WB_ID","Indicator","IndSubtype","Period")) %>%
      filter(is.na(drop)) %>%
      select(-drop)
    
    wbdistinct<-df %>% 
      distinct(WB_ID)
    if(nrow(wbdistinct)==1){
    if("CoastBQI" %in% df$Indicator){
      #browser()
      df <- SelectBQISubType(df,dfobs)
      }}
    }
  return(df)
}


SelectBQISubType<-function(df,dfdata){
dfBQI<-df %>%
  filter(Indicator=="CoastBQI")
dfBQI$z0 <- lapply(dfBQI$IndSubtype,function(x) DepthLimits(x,1))
dfBQI$z1 <- lapply(dfBQI$IndSubtype,function(x) DepthLimits(x,2))
dfBQI <- dfBQI %>% 
  select(WB_ID,Period,IndSubtype,z0,z1)

dfBQIobs <- dfdata %>% 
  filter(!is.na(BQI)) %>%
  select(WB_ID,Period,station_depth,BQI)

dfBQI <- dfBQI %>% 
  left_join(dfBQIobs, by=c("WB_ID","Period")) 

dfBQI<-dfBQI %>%
  mutate(OK=ifelse(station_depth>=z0 & station_depth<=z1,1,NA)) %>%
  filter(OK==1) %>%
  group_by(WB_ID,Period,IndSubtype) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  arrange(desc(n))

dfBQI <- dfBQI %>%
  group_by(WB_ID,Period) %>%
  slice(1) %>% 
  ungroup() %>%
  select(-n)

dfBQI<-dfBQI %>% 
  left_join(df,by=c("WB_ID","Period","IndSubtype"))

dfNotBQI <- df %>%
  filter(Indicator!="CoastBQI")

df2 <- dfNotBQI %>% 
  bind_rows(dfBQI)
return(df2)
}


shinyInput = function(FUN, len, id, labels,...) {
  inputs = character(len)
  if(typeof(labels)=="character"){
    labels<-rep(labels, len)
  }
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN(paste0(id, i),label = labels[i], ...))# NULL,
  }
  inputs
}
buttonInput = function(FUN, len, id, labels, actions, ...) {
  inputs = character(len)
  if(typeof(labels)=="character"){
    labels<-rep(labels, len)
  }
  for (i in seq_len(len)) {
    inputs[i] = as.character(FUN( inputId=paste0(id,i), label=labels[i], icon=NULL,width=NULL, onclick=actions[i], ...)) #paste0(id, i),
  }
  inputs
}



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



# function to add (or remove) leading zero for coastal types 
TypeLeadingZero<-function(type,addzero=T){
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
      if(addzero){
        type=paste0("0",as.character(value),ns)
      }else{
        type=paste0(as.character(value),ns)
      }
    }
  }
  return(type)
}


# SelectObs
#
#
#filter observation data based on the selected indicator, WB and period
SelectObs<-function(df,indicator,indSubType="",sWB,sPeriod,df_indicators,df_var){
  #browser()
  varlist<-GetVarNames(indicator,df_indicators,df_var)
  obsvar<-varlist[length(varlist)]
  
  #varlist<-c("station","obspoint","date","year","month",varlist)
  varlist<-c("station","date","year","month",varlist)
  df <- df %>% filter(WB_ID==sWB,Period==sPeriod)
  df <- df[!is.na(df[,obsvar]),]
  df <- df[,varlist]
  if(!is.na(indSubType)){
    depthlimits<-DepthLimits(indSubType)
    df <- df %>% filter(depth>=depthlimits[1],depth<=depthlimits[2])
  }
  
  return(df)
  
}




DepthLimits<-function(s,n=NA){
  m<-regexpr("m",s,fixed=T)[1] 
  from<-NA
  to<-NA
  if(m>0){
    s<-substr(s,1,m-1)
  }
  if(substr(s,1,1)=="<"){
    from=0
    to=as.numeric(substr(s,2,nchar(s)))
  }else if(substr(s,1,1)==">"){
    from=as.numeric(substr(s,2,nchar(s)))
    to = 99999
  }else{
    nchar<-regexpr("-",s,fixed=T)[1]
    if(nchar>0){
      from=as.numeric(substr(s,1,nchar-1))
      to=as.numeric(substr(s,nchar+1,nchar(s)))
    }
  }
  if(is.na(n)){
    return(c(from,to))
  }else{
    if(n==1){
      return(from)
    }else{
      return(to)
    }
  }
}

# GetIndicatorMonths
GetIndicatorMonths<-function(indicator,type,df_bound){
  df_bound <- df_bound %>% 
    filter(Indicator==indicator,Type==type) %>%
    distinct(Months)
  if(nrow(df_bound)>1){
    cat(paste0("Indicator ",indicator," has more than one combination of months!\n"))
  }
  Months<-df_bound[1,"Months"]
  if(Months %in% c("1,2,..,12","1,2,...,12")){
    Months<-1:12
  }else{
    Months<-unlist(strsplit(Months,","))
    Months<-as.numeric(Months)
  }
  return(Months)
}

# GetVarNames
#
#

GetVarNames<-function(indicator,df_indicators,df_var){
  if(indicator!=""){
    
    df_indicators<-df_indicators %>% 
      filter(Indicator==indicator) %>%
      left_join(select(df_var,Indicator,Parameter=var),by="Indicator")
    
    obsvar<-as.character(df_indicators[1,"Parameter"])
    if(substr(indicator,1,5)=="Coast"){
      if(indicator %in% c("CoastOxygen","CoastBQI")){
        varlist<-c("depth","sali",obsvar)
      }else{
        varlist<-c("sali",obsvar)
      } 
    }else{
      varlist<-c(obsvar)
    }
  }else{
    varlist<-""
  }
  return(varlist)
}


GetObsCountExtrap<-function(resAvgExtrap,resAvgtype){
  
  df <- resAvgtype %>%
    group_by(Period,Indicator,IndSubtype) %>%
    summarise(nobs=sum(nobs,na.rm=T)) 
  
  df2 <- resAvgtype %>%
    distinct(Period,Indicator,IndSubtype,WB_ID) %>%
    group_by(Period,Indicator,IndSubtype) %>%
    mutate(WBlist = paste0(WB_ID, collapse = ","))
  
  df2 <- distinct(df2,Period,Indicator,IndSubtype,WBlist)
  
  df <- df %>%
    left_join(df2,by=c("Period","Indicator","IndSubtype"))
  
  resAvgExtrap<-resAvgExtrap %>% 
    left_join(df,by=c("Period","Indicator","IndSubtype"))
  
  return(resAvgExtrap)
}
