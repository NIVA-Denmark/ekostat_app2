#' Aggregate
#' 
#' 
#' @param df A dataframe 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \item{EQR} 
#'   \item{Indicator} 
#'   \item{QualityElement}
#'   \item{QualitySubelement}
#'   #'   
#' 
#' @param string specifiying the aggregation level: 
#' @param string list of columns to group by : e.g. c("WB","Period") 
#'  
#' 

Aggregate<-function(df,level=1,Groups="",QE_use_mean=c("Supporting")){
  # level=1 overall result
  # level=2 QE type (biological / supporting)
  # level=3 QE result
  # level=4 subelement result
  
  # QE_use_mean - list of QE "groups" e.g. "Supporting"
  #               within this group, the status is calculated from the mean of EQR for Quality Elements
  #               for other groups (e.g. "Biological"), status is calculated from the lowest EQR of Quality Elements (OOAO)
  
  GroupsType<-c(Groups,"QEtype")
  GroupsQE<-c(Groups,"QualityElement","QEtype")
  GroupsQSE<-c(Groups,"QualityElement","QEtype","QualitySubelement")
  df_res <-df %>% 
    group_by_(.dots=GroupsQSE) %>% 
    summarise(nInd=n(),EQR=mean(EQR,na.rm=TRUE)) %>%
    ungroup()
  #if aggregation to subelement level is chosen then stop here
  if(level > 3){
    df_res <- EQRclass(df_res)
  }else{
    df_res <-df_res %>% 
      group_by_(.dots=GroupsQE) %>% 
      summarise(nInd=sum(nInd,na.rm=TRUE),nSubE=n(),EQR=mean(EQR,na.rm=TRUE)) %>%
      ungroup()
    # if aggregating to overall level
    if(level==3){
      df_res <- EQRclass(df_res)
    }else{
      #browser()
      df_res$EQR<-ifelse(is.nan(df_res$EQR),NA,df_res$EQR)
      df_min <- df_res %>%
        group_by_(.dots=GroupsType) %>%
        summarise(nInd=sum(nInd,na.rm=TRUE),nSubE=sum(nSubE,na.rm=TRUE),EQRmean=mean(EQR,na.rm=TRUE),EQR=min(EQR,na.rm=TRUE)) %>%
        ungroup()
      df_min$EQR<-ifelse(is.infinite(df_min$EQR),NA,df_min$EQR)
      
      #Join the dataframe with minimum EQR values back to the QE results to get the name of
      # the QE having the lowest EQR.
      # if there are two QE sharing the same minimum value, then we will get two QEs per Group
      df_min <- df_min %>% left_join(select(df_res,-c(nInd,nSubE)),by=c(GroupsType,"EQR")) 
      df_min <- df_min %>% rename(Worst=QualityElement)
      
      # select only one QE per group 
      df_min <- df_min %>% group_by_(.dots=GroupsType) %>% 
        mutate(id = row_number()) %>% filter(id==1) %>%
        select(-id)
      
      df_min$EQR<-ifelse(df_min$QEtype %in% QE_use_mean,df_min$EQRmean,df_min$EQR) 
      df_min <- df_min %>% select(-EQRmean)

      df_res <- EQRclass(df_min)
      if(level==1){
        QEtype<-c("Biological","Supporting")
        QEtype<-data.frame(QEtype,stringsAsFactors=FALSE)
        QEtype$X<-1
        df_gp<-df_res %>% 
          group_by_(.dots=Groups) %>% 
          summarise(n=n()) %>% 
          ungroup() %>%
          select(-n) %>%
          mutate(X=1) %>%
          left_join(QEtype,by="X") %>%
          select(-X)
          
        df_res1 <- df_gp %>%
         left_join(df_res,by=GroupsType) %>% 
          select(-c(nInd,nSubE,Worst,Class,EQR)) %>% 
          spread(key=QEtype,value=ClassID) %>%
          mutate(Supporting2=ifelse(is.na(Supporting),5,ifelse(Supporting<3,3,Supporting))) %>%
          mutate(ClassID=ifelse(Biological<Supporting2,Biological,Supporting2)) %>%
          select(-c(Supporting2)) %>%
          rename(ClassIDBio=Biological,ClassIDSup=Supporting)
        
        df_res2 <- df_gp %>%
          left_join(df_res,by=GroupsType) %>% 
          mutate(nInd=ifelse(is.na(nInd),0,nInd)) %>%
          select(-c(ClassID,nSubE,Worst,Class,EQR)) %>% 
          spread(key=QEtype,value=nInd) %>%
          rename(nIndBio=Biological,nIndSup=Supporting)
        
        #cat(paste0("left_join df_res  ",unlist(Groups),"\n"))
        df_res <- left_join(df_res1,df_res2,by=Groups)
        
        Categories<-c("Bad","Poor","Mod","Good","High","Ref")
        df_res$Class<-ifelse(is.na(df_res$ClassID),NA,Categories[df_res$ClassID])
        
      }
    }
  }
  return(df_res)
}

#' EQRclass
#' 
#'  df must contain a column EQR
#' 
EQRclass<-function(df,varname="EQR"){
  if(length(names(df)[names(df)==varname])>0){
    
    df$ClassID<-ifelse(is.na(df[,varname]),NA,
                       ifelse(df[,varname]<0.2,1,
                     ifelse(df[,varname]<0.4,2,
                            ifelse(df[,varname]<0.6,3,
                                   ifelse(df[,varname]<0.8,4,5)))))
    Categories<-c("Bad","Poor","Mod","Good","High","Ref")
    #browser()
    df$Class<-ifelse(is.na(df$ClassID),NA,Categories[df$ClassID])
    
  }
  return(df)
}


#' Frequency
#' 
#' 
#' @param df A dataframe 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \item{EQR} 
#'   \item{Indicator} 
#'   \item{QualityElement}
#'   \item{QualitySubelement}
#'   #'   
#' 
#' @param string specifiying the aggregation level: 
#' @param string list of columns to group by : e.g. c("WB","Period") 
#'  
#' 

Frequency<-function(df,Groups="",varname="Class"){
  #browser()
  names(df)[names(df)==varname]<-"ClassID"
  GroupsSum <- c(Groups,"ClassID")
  
  ClassID<-c(0,1,2,3,4,5)
  ClassID<-data.frame(ClassID)
  ClassID$X<-1
  
  dfn <- df %>% 
    group_by_(.dots=GroupsSum) %>% 
    summarise(n=n()) %>%
    ungroup()
  
  #cat(paste0("left_join df_res  ",unlist(Groups),"\n"))
  cat("left_join dft\n")
  dft <- df %>% 
    group_by_(.dots=Groups) %>% 
    summarise(t=n()) %>%
    ungroup() %>% 
    mutate(X=1) %>%
    left_join(ClassID) %>%
    select(-X)
  
  dfn <- dfn %>% mutate(ClassID=ifelse(is.na(ClassID),0,ClassID))
  cat("left_join dfn\n")
  dfn<-dft %>% 
    left_join(dfn) 
  dfn<-dfn %>%
    filter(ClassID!=0)
  dfn<-dfn %>%
    mutate(f=n/t) 
  dfn<-dfn %>%
    select(-c(n,t)) 
  dfn<-dfn %>%
    ungroup() 
  dfn<-dfn %>%
    mutate(f=ifelse(is.na(f),0,f),prefix="C") 
  dfn<-dfn %>%
    mutate(ClassID=paste0(prefix,ClassID)) 
  dfn<-dfn %>%
    select(-prefix)
  
  #browser()
  df<-dfn %>% spread(key=ClassID,value=f)
  
  return(df)
}
