df <- df_bound %>% 
  filter(Water_type=="Rivers") %>%
  rename(HG="H.G",GM="G.M",MP="M.P",PB="P.B") %>%
  mutate(Type_reg=substr(Type,1,1))

dfreg<- df %>% 
  distinct(Type_reg) %>% 
  mutate(Type=paste0(Type_reg,"--"))

df <- df %>% distinct(Water_type,Indicator,Unit,Months,Depth_stratum,Region,Type_reg,MinYear,MinPerYear,RefCond,HG,GM,MP,PB,
                      Worst,ParameterVector_1,ParameterVector_2,ParameterVector_3,ParameterVector_4,
                      ParameterVector_5,ParameterVector_6,ParameterVector_7,ParameterVector_8,ParameterVector_9,
                      ParameterVector_10,V_WBperiod,V_WBannual)

df <- df %>% 
  left_join(dfreg,by="Type_reg") %>%
  select(-Type_reg)

names(df)[names(df)=="HG"]<-"H.G"
names(df)[names(df)=="GM"]<-"G.M"
names(df)[names(df)=="MP"]<-"M.P"
names(df)[names(df)=="PB"]<-"P.B"

df_bound <- df_bound %>%
  bind_rows(df)

# add missing variance components for extrapolation
df_bound$GM<-df_bound[,"G.M"]

df_bound <- df_bound %>% 
  mutate(GM=as.numeric(GM)) %>%
  mutate(V_WBannual=ifelse(is.na(V_WBannual),0.1*GM,V_WBannual)) %>%
  mutate(V_WBperiod=ifelse(is.na(V_WBperiod),0.1*GM,V_WBperiod)) %>%
  select(-GM)

