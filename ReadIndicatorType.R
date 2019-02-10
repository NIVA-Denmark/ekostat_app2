
ReadIndicatorType<-function(){
  df<-read.table("data/IndicatorList.txt", fileEncoding = "UTF-8", sep="\t", stringsAsFactors=F, header=T)
  return(df)
}
