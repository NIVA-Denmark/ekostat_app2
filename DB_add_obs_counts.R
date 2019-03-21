library(tidyverse)
library(RSQLite)


readdb <- function(dbname,strSQL){
  db <- dbConnect(SQLite(), dbname=dbname)
  df <- dbGetQuery(db, strSQL)
  dbDisconnect(db)
  return(df)
}  

db<-"../efs/ekostat/ekostat_C_nobs.db"
sql<-paste0("SELECT WB_ID,Type,Period,Indicator,IndSubtype,nobs,stns FROM resAvg")
dfobscount <- readdb(db, sql)
dfobscount <- dfobscount %>%
  mutate(IndSubtype=ifelse(IndSubtype=="",NA,IndSubtype))

db<-"../efs/ekostat/ekostat_C.db"
sql<-paste0("SELECT * FROM resAvg")
df <- readdb(db, sql)

df <- df %>% left_join(dfobscount,by=c("WB_ID","Type","Period","Indicator","IndSubtype")) #

db <- dbConnect(SQLite(), dbname="../efs/ekostat/ekostat_C.db")
dbWriteTable(conn=db,name="resAvg",df,overwrite=T,append=F,row.names=FALSE)
dbDisconnect(db)






