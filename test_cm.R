library(tidyverse)
library(RSQLite)

source("IndicatorFunctions.R")
source("helpFunctions.R")
source("ReadIndicatorType.R")
source("ReadVariances.R")
source("ReadBounds.R")

#---------------------------------------------------------------------------------
dbpath<-"data/ekostat.db"
values <- list()
#values$wbselected<-"SE584695-175315"
values$wbselected<-"SE584870-174310" #Asköfjärden
values$watertypeselected<-"Coastal"
values$typeselected <- "12n"
values$periodselected<-"2004-2009" #"2010-2015"
ind<-"CoastSecchiEQR"
ind<-"CoastBQI"
ind<-"CoastChlaEQR"

#---------------------------------------------------------------------------------
readdb <- function(dbname,strSQL){
  db <- dbConnect(SQLite(), dbname=dbname)
  df <- dbGetQuery(db, strSQL)
  dbDisconnect(db)
  return(df)
} 
#---------------------------------------------------------------------------------
dfwb_lan <- readdb(dbpath, "SELECT * FROM WB_Lan") %>%
  mutate(TypeName = Type) %>%
  mutate(Type = substr(Type,1,3)) %>%
  mutate(Type = gsub(" ","",Type)) %>%
  mutate(Type = gsub("\\.","",Type)) %>%
  mutate(Type = gsub(":","",Type)) %>%
  mutate(TypeNum = gsub("n",".0",Type)) %>%
  mutate(TypeNum = gsub("s",".5",TypeNum))
dfwb_lan$TypeNum <- as.numeric(dfwb_lan$TypeNum)

dfind<-ReadIndicatorType()
dfvar<-ReadVariances()
dfbounds<-ReadBounds()
dfmonths<- dfbounds %>% distinct(Indicator,Type,Months)

pressure_list<-function(){
  c("Nutrient loading","Organic loading","Acidification","Harmful substances","Hydrological changes","Morphological changes","General pressure")
}
#---------------------------------------------------------------------------------
# exisiting code in UpdateIndTable()
# updates the list of WBs having the same type as the selected WB
sql<-paste0("SELECT * FROM resAvg WHERE Type ='",values$typeselected,"' AND Period=='",values$periodselected,"'")
dftypeperiod <- readdb(dbpath, sql)
dftypeperiod<-CleanSubTypes(dftypeperiod)
sql<-paste0("SELECT * FROM resYear WHERE Type ='",values$typeselected,"' AND Period=='",values$periodselected,"'")
dftypeyear <- readdb(dbpath, sql)
dftypeyear2<-CleanSubTypes(dftypeyear)
sql<-paste0("SELECT * FROM resMC WHERE Type ='",values$typeselected,"' AND Period=='",values$periodselected,"'")
dftypeMC <- readdb(dbpath, sql)
dftypeMC<-CleanSubTypes(dftypeMC)

pname<-pressure_list()[1]
pname<-gsub(" ",".",pname)
df2 <- dfind %>% filter(Water_type==values$watertypeselected)
df2 <- df2[df2[,pname]=="X",]
df2<-df2 %>% select(Indicator)


dfwb_type <- dfwb_lan %>% distinct(WB_ID,Name)

dftypeperiod <- dftypeperiod %>% left_join(dfwb_type,by=c("WB"="WB_ID")) %>%
  filter(Code==0) 
dftypeperiod<- df2 %>% left_join(dftypeperiod,by="Indicator")

dftypeyear <- dftypeyear %>% left_join(dfwb_type,by=c("WB"="WB_ID")) %>%
  filter(Code==0) 
dftypeyear<- df2 %>% left_join(dftypeyear,by="Indicator")

dftypeMC <- dftypeMC %>% left_join(dfwb_type,by=c("WB"="WB_ID")) %>%
  filter(Code==0) 
dftypeMC<- df2 %>% left_join(dftypeMC,by="Indicator")


#---------------------------------------------------------------------------------

# List of WB's is changed manually by selecting from the available list




WB_type_list<-dftypeperiod %>%
  filter(Indicator==ind) %>%
  distinct(WB) %>%
  filter(!is.na(WB))
ntype<-WB_type_list %>% nrow()

# Get list of variance components from water type (e.g. Coastal) and waterbody type (e.g. 12n)
variance_list<-dfind %>% 
  filter(Indicator==ind) %>%
  select(Measurement) %>% 
  left_join(dfvar,by="Measurement") %>%
  filter(Water_type==values$watertypeselected,Type==values$typeselected) %>%
  #select(V_station,V_obspoint,V_year,V_yearmonth,V_stationdate,V_stationyear,V_stationmonth,V_institution,V_replication,V_WB,V_WBperiod,V_WBannual) %>%
  select(V_WBperiod,V_WBannual) %>%
  lapply(function(x) ifelse (is.na(x),0,x))


MonthInclude<-IndicatorMonths(dfmonths,values$typeselected,ind)



get_un<-function(dfp,dfa,dfmc,wblist,i){
  ix<-0
  for(wb in wblist){
    period<-dfp %>% 
      filter(WB==wb,Indicator==i) %>%
      select(mean=Mean,stderr=StdErr) 
    annual<-dfa %>% 
      filter(WB==wb,Indicator==i) %>%
      select(year=Year,mean=Mean,stderr=StdErr) 
    indicator_sim<-dfmc %>% 
      filter(WB==wb,Indicator==i) %>%
      select(Value)
    uni<-list(period=period,annual=annual,indicator_sim=indicator_sim$Value,result_code=0)
    if(nrow(dfp)>0){
      ix<-ix+1
      if(ix==1){
        un<-list(uni)
      }else{
        un[[ix]]<-uni
      }
    }
  }
  return(un)
}

wblist<-WB_type_list$WB
wblist<-wblist[1:2]

un<-get_un(dftypeperiod,dftypeyear,dftypeMC,wblist,ind)
un<-list(un[[1]])

yrfrom <- as.numeric(substr(values$periodselected,1,4))
yrto <- as.numeric(substr(values$periodselected,6,9))
test<-CalculateIndicatorType(ind,un,variance_list,ntype,yrfrom,yrto,n_iter=500)


ind_typ_input<-list(Indicator=ind,unc_list=un,var_list=variance_list,ntype_WB=ntype,startyear=yrfrom,endyear=yrto,n_iter=500)

# -------------------------------------------------------------------------
load("../ekostat_calc/from_JAC/20180508/test_cm/un.Rda")
un1<-un[[1]]
# Testing indicator calculation for non-monitored water bodies
# variance_list <- list(V_station=0.0909487321117623,V_obspoint=0,
#                       V_year=0.001168302,V_yearmonth=0,
#                       V_stationdate=0.278888006,
#                       V_stationyear=0,V_stationmonth=0.092962407,
#                       V_institution=0.062103034,V_replication=0)
#MonthInclude <- c(11,12,1,2)
#RefCond_sali <- c(rep(0.9,36))
xx<-un
CalculateIndicatorType("CoastTNwinter",un,list(V_WBperiod=9,V_WBannual=12),30,2007,2012)
# -------------------------------------------------------------------------



period<-dftypeperiod %>% 
  filter(WB==wblist[1],Indicator==ind)
annual<-dftypeyear %>% 
  filter(WB==wblist[2],Indicator==ind)

dftypeperiod %>% filter(Indicator==ind) %>% group_by(WB) %>% summarise(n=n()) %>% arrange(desc(n))

cat(paste0("ok2\n\n"))


load("extrap_input.Rda")
listperiod<-extrap_input[[1]]
listyr<-extrap_input[[2]]
listMC<-extrap_input[[3]]




