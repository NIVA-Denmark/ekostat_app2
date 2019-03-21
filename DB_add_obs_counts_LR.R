
library(RSQLite)

dbpath<-"../efs/ekostat/ekostat_L.db"
db <- dbConnect(SQLite(), dbname=dbpath)
sql<-"ALTER TABLE resAvg ADD nobs INTEGER;"
rs <- dbSendStatement(db,sql)

sql<-"ALTER TABLE resAvg ADD stns TEXT;"
rs <- dbSendStatement(db,sql)
dbDisconnect(db)

dbpath<-"../efs/ekostat/ekostat_R.db"
db <- dbConnect(SQLite(), dbname=dbpath)
sql<-"ALTER TABLE resAvg ADD nobs INTEGER;"
rs <- dbSendStatement(db,sql)

sql<-"ALTER TABLE resAvg ADD stns TEXT;"
rs <- dbSendStatement(db,sql)
dbDisconnect(db)

print("done")