
downloadResults<-function(resMC,resAvg,nsigdig=3,VISScolumns=T){

      withProgress(message = 'Preparing download...', value = 0, {
    
      # if only extrapolation indicators are used, the columns 'stns' will be missing
      if(!"stns" %in% names(resMC)){
        resMC$stns<-""
      }
      # if no extrapolation indicators are used, the columns 'WBlist' will be missing
      if(!"WBlist" %in% names(resMC)){
        resMC$WBlist<-""
      }
      
      
    wb <- resAvg$WB_ID[1]
    
    res1Avg <-
      Aggregate(
        resAvg,
        Groups = c("Period"),
        level = 1
      ) 
    
    res1MC <- AggregateMCovr(resMC,resAvg,res1Avg)
    res1MC <- res1MC %>%
      select(Period,sim,ClassIDMC=ClassID,ClassMC)
    
    res1MC <- res1MC %>% left_join(res1Avg,by = c("Period"))
    
    res1Avg <- res1Avg %>%
      select(Period, Class)
    incProgress(0.1)
    
    
    res2MC<-AggregateMC(resMC,resAvg)
    
    res2Avg <-
      Aggregate(resAvg,
                Groups = c("Period"),
                level = 2) %>%
      select(Period, QEtype, EQR, Class)
    
    res2MC <- res2MC %>% left_join(res2Avg,by = c("Period", "QEtype"))
    incProgress(0.1)
    
    res3Avg <-
      Aggregate(resAvg,
                Groups = c("Period"),
                level = 3) %>%
      select(Period, QEtype, QualityElement, EQR, Class)
    
    res3MC <- Aggregate(resMC,
                        Groups = c("Period", "sim"),
                        level = 3) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    res3MC <- res3MC %>% left_join(res3Avg,by = c("Period", "QEtype","QualityElement"))
    incProgress(0.1)
    
    
    res4MC <-Aggregate(resMC,Groups = c("Period", "sim"),level = 4) %>%
      rename(ClassMC = Class, EQRMC = EQR)
    
    res4Avg <-Aggregate(resAvg,Groups = c("Period"),level = 4) %>%
      select(Period,QEtype,QualityElement,QualitySubelement,EQR,Class)
    
    res4MC <- res4MC %>% left_join(res4Avg,by = c("Period", "QualityElement", "QEtype", "QualitySubelement"))
    incProgress(0.1)
    
    
    
    
    grplist <- c("Period","QEtype","QualityElement","QualitySubelement","Indicator","IndSubtype",
                 "Note","Unit","Months","Worst","PB","MP","GM","HG","Ref","Mean","StdErr","EQR","Class")
    
      infolist<- c("nobs","stns","WBlist","RefCondAvg")

    
    resMC <-resMC %>% rename(EQRMC = EQR,ClassMC = Class,Class = ClassAvg,EQR = EQRavg)
    
    resMC <- 
      SummarizeSims(resMC,Groups=c(grplist,infolist),ClassVar="ClassMC")
    resMC <- resMC %>% mutate(id=as.numeric(rownames(resMC)))
    incProgress(0.1)
    
    nr<-nrow(resMC)
    
    rmlist <- ""
    grplist <-c("Period", "Class")

    res1MC <-
      SummarizeSims(res1MC,Groups=grplist , roundlist = c("pGES"),remove = rmlist,ClassVar = "ClassMC") %>%
      select(-Class)
    res1MC <- res1Avg %>% 
      left_join(res1MC,by="Period") %>% 
      arrange(Period) 
    res1MC <-res1MC %>%
      mutate(id1=as.numeric(rownames(res1MC)))
    nr1<-nrow(res1MC)
    incProgress(0.1)
    
    grplist <- c("Period", "QEtype", "EQR", "Class")
    rmlist = c("Period")
    
    res2MC <- 
      SummarizeSims(res2MC,Groups = grplist,remove = rmlist, ClassVar = "ClassMC")  
    
    res2MC <- res2MC %>% arrange(Period,QEtype) %>% 
      mutate(id2=as.numeric(rownames(res2MC)))
    nr2<-nrow(res2MC)
    
    incProgress(0.1)
    
    grplist <-
      c("Period","QEtype","QualityElement","EQR","Class")
    rmlist = c("Period", "QEtype")
    
    res3MC <-
      SummarizeSims(res3MC,roundlist = c("EQR","pGES"),Groups = grplist,remove = rmlist,ClassVar = "ClassMC")  
    res3MC <- res3MC %>% mutate(id3=as.numeric(rownames(res3MC)))
    nr3<-nrow(res3MC)
    incProgress(0.1)
    
    
    grplist <-
      c("Period","QEtype","QualityElement","QualitySubelement","EQR","Class")
    rmlist = c("Period", "QEtype", "QualityElement")
    
    
    res4MC <-
      SummarizeSims(res4MC,roundlist = c("EQR","pGES"),Groups = grplist,remove = rmlist,ClassVar = "ClassMC")  
    res4MC <- res4MC %>% mutate(id4=as.numeric(rownames(res4MC)))
    nr4<-nrow(res4MC)
  
    res4MC$id4<-res4MC$id4*nr
    res3MC$id3<-res3MC$id3*nr*nr4
    res2MC$id2<-res2MC$id2*nr*nr4*nr3
    res1MC$id1<-res1MC$id1*nr*nr4*nr3*nr2
    
    res2MC <- res2MC %>%
      left_join(select(res1MC,Period,id1),
                by="Period")
    res3MC <- res3MC %>%
      left_join(select(res2MC,Period,QEtype,id1,id2),
                by=c("Period","QEtype"))
    res4MC <- res4MC %>%
      left_join(select(res3MC,Period,QEtype,QualityElement,id1,id2,id3),
                by=c("Period","QEtype","QualityElement"))
    resMC <- resMC %>%
      left_join(select(res4MC,Period,QEtype,QualityElement,QualitySubelement,id1,id2,id3,id4),
                by=c("Period","QEtype","QualityElement","QualitySubelement"))
    
    #resMC <- resMC %>% rename(EQR_ind=EQR)
    #res3MC <- res3MC %>% rename(EQR_QE=EQR)
    #res4MC <- res4MC %>% rename(EQR_subQE=EQR)
    
    res1MC <- res1MC %>% mutate(Level="1. Overall",Name="")
    res2MC <- res2MC %>% mutate(Level="2. Biological/Supporting",Name=QEtype)
    res3MC <- res3MC %>% mutate(Level="3. Quality Element",Name=QualityElement)
    res4MC <- res4MC %>% mutate(Level="4. Quality Subelement",Name=QualitySubelement)
    resMC <- resMC %>% mutate(Level="5. Indicator",
                              Name=ifelse(is.na(IndSubtype),Indicator,paste0(Indicator," [",IndSubtype,"]")))

    resMC <- bind_rows(resMC,res1MC,res2MC,res3MC,res4MC)
    resMC$sortorder<-rowSums(resMC[,c("id1","id2","id3","id4","id")],na.rm=T)
    
    resMC <- resMC %>% arrange(sortorder) %>%
      left_join(df_viss,by=c("Indicator")) %>%
      separate(Period,into=c("YearFrom","YearTo"),sep="-") %>%
      mutate(WB_ID = wb,Mean=round(Mean,nsigdig),StdErr=round(StdErr,nsigdig),
             EQR=round(EQR,nsigdig),RefCondAvg=round(RefCondAvg,nsigdig)) %>%
      #select(-c(id,id1,id2,id3,id4,sortorder))
      select(WB_ID,YearFrom,YearTo,VISS_parameter,Level,Name,Note,Unit,Months,Worst,PB,MP,GM,HG,Ref,Mean,StdErr,EQR,Class,nobs,stns,WBlist,RefCondAvg,pGES,fBad,fPoor,fMod,fGood,fHigh)
      
    # add VISS parameters for QEs etc.
    resMC <- resMC %>%
      left_join(select(df_viss,Indicator,VISS_parameter2=VISS_parameter),by=c("Name"="Indicator")) %>%
      mutate(VISS_parameter=ifelse(is.na(VISS_parameter),VISS_parameter2,VISS_parameter)) %>%
      select(-VISS_parameter2)

    # Vatten-ID
    # Parameternamn
    # Klassificeringsnamn
    # Versionsnamn
    # Motiveringstext
    # Referenser
    # Tillförlitlighetsklassning
    # Typ av bedömning
    # År från
    # År till
    # Ekologisk kvot
    # Jämförvärde ?
    # Enhet för jämförvärde ?
    # Antal mätningar
    # Använda stationer
    # Observerad halt
    
    if(VISScolumns){
      resMC <- resMC %>% 
        rename("Vatten-ID"=WB_ID,
               "År från"=YearFrom,
               "År till"=YearTo,
               Parameternamn=VISS_parameter,
               "Referensvärde/Bakgrundshalt"=RefCondAvg,
               "Antal mätningar"=nobs,
               "Använda stationer"=stns,
               Waterbodies=WBlist) %>%
        mutate("Ekologisk kvot"=ifelse(grepl("EQR",Name),Mean,EQR),
               "Tillförlitlighetsklassning"=NA,
               Versionsnamn=NA,
               Motiveringstext=NA,
               Referenser=NA,
               "Typ av bedömning"=ifelse(grepl("Extrap",Note),"WATERS tool (Extrapolated)","WATERS tool"),
               Klassificeringsnamn=substr(Class,1,1)) %>%
        select("Vatten-ID",Parameternamn,Klassificeringsnamn,Versionsnamn,
               "Referensvärde/Bakgrundshalt",
               Motiveringstext,Referenser,"Typ av bedömning",
               "År från","År till",
               "Ekologisk kvot",
               "Antal mätningar",
               "Använda stationer",
               Level,Name,pGES,Note,Waterbodies,Unit,Months,Worst,PB,MP,GM,HG,Ref,Mean,StdErr,EQR,fBad,fPoor,fMod,fGood,fHigh)
                 
    }

    
    incProgress(0.1,message="done")
  })
  return(resMC)
  
}
