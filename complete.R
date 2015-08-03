complete <- function(directory, id = 1:332) {

  str_pad(id,3,side="left",pad="0")->id_new
  filenames=character()
  for(i in 1:length(id_new))
  {
    filenames[i]<-paste("specdata\\",directory,'\\',id_new[i],'.csv',sep='') 
   
   }

 nobs<-numeric()
  sapply(filenames,read.csv,simplify=F)->simp
  for(i in 1:length(simp)){
 sum(complete.cases(as.data.frame(simp[i]))=='TRUE')->nobs[i]
    }

  as.data.frame(cbind(id,nobs))
  
}