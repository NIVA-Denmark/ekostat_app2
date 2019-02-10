CleanSubTypes <- function(df){
  if(sum(names(df)=="Year")>0) {
    oksubtypes <- df %>% distinct(WB,Indicator,IndSubtype,Period,Code,Year) %>% 
      filter(Code>-10,IndSubtype!="")
    
    subtypes <- df %>% distinct(WB,Indicator,IndSubtype,Period,Year) %>%
      filter(IndSubtype!="")
    
    subtypes <- subtypes %>% 
      left_join(oksubtypes,by=c("WB","Indicator","IndSubtype","Period","Year")) %>%
      filter(is.na(Code)) %>%
      select(-Code) %>%
      mutate(drop=1)
    
    df <- df %>% left_join(subtypes,by=c("WB","Indicator","IndSubtype","Period","Year")) %>%
      filter(is.na(drop)) %>%
      select(-drop)
  }else{
    oksubtypes <- df %>% distinct(WB,Indicator,IndSubtype,Period,Code) %>% 
      filter(Code>-10,IndSubtype!="")
    
    subtypes <- df %>% distinct(WB,Indicator,IndSubtype,Period) %>%
      filter(IndSubtype!="")
    
    subtypes <- subtypes %>% 
      left_join(oksubtypes,by=c("WB","Indicator","IndSubtype","Period")) %>%
      filter(is.na(Code)) %>%
      select(-Code) %>%
      mutate(drop=1)
    
    df <- df %>% left_join(subtypes,by=c("WB","Indicator","IndSubtype","Period")) %>%
      filter(is.na(drop)) %>%
      select(-drop)
  }
  return(df)
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
