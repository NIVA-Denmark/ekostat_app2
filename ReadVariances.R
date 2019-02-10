ReadVariances<-function(){
  df<-read.table("data/varcomp.txt", fileEncoding = "UTF-8", sep="\t", 
                 stringsAsFactors=F, header=T, comment.char="",na.string = "#N/A" )
  return(df)
}


