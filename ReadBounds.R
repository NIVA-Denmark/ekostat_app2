ReadBounds<-function(){
  df<-read.table("data/boundaries.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  return(df)
}

ReadBoundsHypoxicArea<-function(){
  df<-read.table("data/BoundsHypoxicArea.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A")
  #df$WB<-paste0(df$WaterbodyID," ",df$WB_name)
  df$WB<-df$WaterbodyID
  return(df)
}

