

# ultimately, all parameters should be in a database

df_bound<-read.table("parameters/boundaries.txt", sep="\t", fileEncoding="UTF-8",
                     stringsAsFactors=F,header=T,comment.char="") 
df_bound_WB<-read.table("parameters/boundaries_WB.txt", sep="\t",fileEncoding="UTF-8",
                        stringsAsFactors=F,header=T,comment.char="") 
df_indicators<-read.table("parameters/indicators.txt", sep="\t", fileEncoding="UTF-8",
                          stringsAsFactors=F,header=T,comment.char="")
df_varcomp<-read.table("parameters/varcomp.txt", sep="\t", fileEncoding="UTF-8",
                       stringsAsFactors=F,header=T,comment.char="") 
df_var<-read.table("parameters/variables.txt", sep="\t", fileEncoding="UTF-8",
                   stringsAsFactors=F,header=T,comment.char="") 


df_indicators<- df_indicators %>% filter(Indicator!="LakepHchange")

names(df_indicators)[names(df_indicators)=="Qetype"]<-"QEtype"
names(df_indicators)[names(df_indicators)=="Quality.subelement"]<-"QualitySubelement"
names(df_indicators)[names(df_indicators)=="Quality.element"]<-"QualityElement"

names(df_bound)[names(df_bound)=="Min..year"]<-"MinYear"
names(df_bound)[names(df_bound)=="Min.per.year"]<-"MinPerYear"
names(df_bound_WB)[names(df_bound_WB)=="Min..year"]<-"MinYear"
names(df_bound_WB)[names(df_bound_WB)=="Min.per.year"]<-"MinPerYear"

df_bound <- df_bound %>% 
  mutate(V_WBannual=ifelse(is.na(V_WBannual),0.1,V_WBannual),
         V_WBperiod=ifelse(is.na(V_WBperiod),0.1,V_WBperiod))   

df_bound_WB <- df_bound_WB %>% 
  mutate(V_WBannual=ifelse(is.na(V_WBannual),0.1,V_WBannual),
         V_WBperiod=ifelse(is.na(V_WBperiod),0.1,V_WBperiod))   



dbpath<-"../efs/ekostat/ekostat_info.db"
db <- dbConnect(SQLite(), dbname=dbpath)
df_WB<-dbGetQuery(conn=db,"Select * from WB_info")
df_WB_mun<-dbGetQuery(conn=db,"Select * from WB_mun")
df_WB_lan<-dbGetQuery(conn=db,"Select * from WB_lan")
df_WB_EU<-dbGetQuery(conn=db,"Select * from WB_EU")
dbDisconnect(db)

