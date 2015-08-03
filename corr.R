corr <- function(directory, threshold = 0) {
  
  # directory<-"specdata"
  id<-1:length(dir(paste(getwd(),"/",directory,sep='')))
# threshold<-5000
  str_pad(id,3,side="left",pad="0")->id_new
  filenames=character()
  for(i in 1:length(id_new))
  {
    filenames[i]<-paste(paste(directory,"/",id_new[i],sep=''),'.csv',sep='') 
    
  }
  sapply(filenames,read.csv,simplify=F)->simp_cor
  
  corelation<-numeric()
  val<-0
  for( j in 1:length(simp_cor))
  {
    as.data.frame(simp_cor[j])->dat
    names(dat)<-c("Date","sulphate","nitrate","ID")
    if(sum(complete.cases(dat)=='TRUE')>threshold)
    {
      val<-val+1
      dat[complete.cases(dat),]->cor_frame
      
      cor(cor_frame$sulphate,cor_frame$nitrate)->corelation[val]
      
    }
  }
  return(corelation)
}