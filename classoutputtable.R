


# ClassOutputTableDT
#
#
ClassOutputTableDT<-function(df,ClassVar="Class",Groups="",
                             ymin=0,ymax=0,
                             barwidthpx=c("20px","20px","20px","20px","20px","0px"),
                             barcolours=c('#FF0000','#FFC000','#FFFF00','#92D050','#00B0F0',"#FFFFFF"),
                             remove="",roundlist=NULL,colOK=0,
                             sDOM="t"){
  
  
  
  
  
  
  
  if(is.data.frame(df)){
    return(DT::renderDataTable({
      
      df[,"X"] <- df[,ClassVar]
      df$X <- factor(df$X,levels=c("Bad","Poor","Mod","Good","High"))
      
      GroupsClass<-c(Groups,"X")
      df.count<- df %>% 
        group_by_(.dots=GroupsClass) %>% 
        summarize(n=n()) %>% mutate(f = n / sum(n)) %>% 
        complete(X, fill = list(f = 0))
      df.count$f[is.na(df.count$X)]<-0

      ########
      df.count.GH <- df.count %>% ungroup() %>%
        mutate(ok=ifelse(is.na(f),0,1)) %>%
        filter(X %in% c("Good","High")) %>%
        group_by_(.dots=Groups) %>% 
        summarize(pGES=sum(f,na.rm=T),ok=sum(ok,na.rm=T)) %>%
        mutate(pGES=ifelse(ok>0,pGES,NA)) %>%
        select(-ok)
      df.count <- df.count %>% left_join(df.count.GH,by=Groups)
      
      Groups<-c(Groups,"pGES")   
      #######
            
      dt<-df.count %>% 
        group_by_(.dots=Groups) %>% 
        summarize(Classes = spk_chr(f,type='bar',barWidth=barwidthpx,chartRangeMin=ymin, chartRangeMax=ymax,
                                    colorMap=barcolours)) %>% 
        ungroup() %>%
        mutate(pGES=ifelse(Class=='','',pGES)) %>%
        select(-one_of(remove)) %>%
        datatable(escape = F,rownames = F,selection = 'single',
                  options = list(dom=sDOM,fnDrawCallback = htmlwidgets::JS('function(){HTMLWidgets.staticRender();}')))
      if(!is.null(roundlist)){
        dt<-dt %>% formatRound(columns=roundlist, digits=3)
      }
      
      
      if(colOK){
        dt<-dt %>% # Style cells with max_val vector
          formatStyle(
            columns = colOK,
            #backgroundColor = styleEqual(c(0, -1,-90,-91), c('', 'yellow', 'yellow', 'yellow'))
            backgroundColor = styleEqual(c("Extrap","OK","data<3yrs","missing","missing"), c('grey20','', 'yellow', 'red', 'red'))
          )
      }
      dt<-dt %>% spk_add_deps()
      dt            
    }))
  }else{
    return(DT::renderDataTable({
      dt<-data.frame() %>% 
        datatable()
    }))
  }
  
}


# ClearErrorValues
#
#
ClearErrorValues <- function(df,checkvar="Code",OKvalue=0,varList=c("EQR","Class","pGES")){
  for(var in varList){
    df[df[,checkvar]!=OKvalue,var]<-NA
    #df[!df[,checkvar] %in% OKvalue,var]<-NA
  }
  return(df)
}

# ClearExtrapValues
#
#
ClearExtrapValues <- function(df,checkvar="Note",matchvalue="Extrap",varList=c("EQR","Class","pGES")){
  for(var in varList){
    df[df[,checkvar] %in% matchvalue,var]<-NA
  }
  return(df)
}

# ClassObsTableDT
#
#
ClassObsTableDT<-function(df,roundlist=NULL,sDOM="t"){
  if(is.data.frame(df)){
    return(DT::renderDataTable({
      dt<-df %>% datatable(escape = F,rownames = F,selection = 'none',
                           options = list(dom=sDOM))
      if(!is.null(roundlist)){
        dt<-dt %>% formatRound(columns=roundlist, digits=3)
      }
      dt
    })
    )
  } else{
    return(DT::renderDataTable({
      dt<-data.frame() %>% 
        datatable()
    }))
  }
  
}


# SelectObs
#
#
#filter observation data based on the selected indicator, WB and period
SelectObs<-function(df,indicator,sWB,sPeriod){
  varlist<-GetVarNames(indicator)
  obsvar<-varlist[length(varlist)]

  varlist<-c("station","obspoint","date","year","month",varlist)

  df <- df %>% filter(WB==sWB,period==sPeriod)
  df <- df[!is.na(df[,obsvar]),]
  df <- df[,varlist]
  
  return(df)
  
}

# GetVarNames
#
#

GetVarNames<-function(indicator){
  if(indicator!=""){
    
    df.indicators<-ReadIndicatorType()
    df.indicators<-df.indicators %>% filter(Indicator==indicator)
    obsvar<-as.character(df.indicators[1,"Parameter"])
    if(substr(indicator,1,5)=="Coast"){
      if(indicator %in% c("CoastOxygen")){
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

# SummarizeSims
#
#
SummarizeSims<-function(df,ClassVar="Class",Groups="",remove="",roundlist=NULL){
  df[,"X"] <- df[,ClassVar]
  df$X <- factor(df$X,levels=c("Bad","Poor","Mod","Good","High"))
  
  GroupsClass<-c(Groups,"X")
  df.count<- df %>% 
    group_by_(.dots=GroupsClass) %>% 
    summarize(n=n()) %>% mutate(f = n / sum(n)) %>% 
    complete(X, fill = list(f = 0))
  df.count$f[is.na(df.count$X)]<-0
  
  ########
  df.count.GH <- df.count %>% ungroup() %>%
    mutate(ok=ifelse(is.na(f),0,1)) %>%
    filter(X %in% c("Good","High")) %>%
    group_by_(.dots=Groups) %>% 
    summarize(pGES=sum(f,na.rm=T),ok=sum(ok,na.rm=T)) %>%
    mutate(pGES=ifelse(ok>0,pGES,NA)) %>%
    select(-ok)
  df.count <- df.count %>% left_join(df.count.GH,by=Groups) %>%  
    ungroup() %>%
    select(-n) %>%
    spread(X,f,drop=T,sep="f")
  names(df.count)[substr(names(df.count),1,2)=="Xf"]<-substr(names(df.count)[substr(names(df.count),1,2)=="Xf"],2,99)
  
  if(length(names(df.count)[names(df.count)=="fNA"])){
    df.count <- df.count %>% filter(is.na(fNA))
    df.count$fNA<-NULL
  }
  
  return(df.count)
  
}
