library(RSQLite)
library(tidyverse)

df<-read.table("data/wblist.txt",  sep="\t", 
               stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")

names(df)<-c("WB_ID","Name","Lan","Kommun","District","Type")

df <- df %>% select(WB_ID,Lan,Name,District,Type) %>%
  separate(Lan,into=c("Lan1","Lan2"),sep=", ") %>%
  gather(key=ID,value=Lan, -c(WB_ID,Name,District,Type)) %>% arrange(WB_ID) %>%
  filter(!is.na(Lan)) %>%
  separate(Lan,into=c("Lan_name","Lan_ID"),sep=" - ",remove=F) %>%
  filter(Lan_ID!="N1") %>%
  select(Lan_ID,Lan_name,WB_ID,Name,District,Type) %>%
  arrange(Lan_ID,WB_ID)

db <- dbConnect(SQLite(), dbname="data/ekostat.db")
dbWriteTable(conn=db,name="WB_Lan", df, overwrite=T,append=F, row.names=F)
dbDisconnect(db)
