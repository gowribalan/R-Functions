library(stringr)
pollutantmean <- function(directory, pollutant, id = 1:332) {
#  print(id) 
#   id<-1:10
  str_pad(id,3,side="left",pad="0")->id

  ####### Reading files
  filenames=character()
  for(i in 1:length(id))
  {
    filenames[i]<-paste("specdata\\",directory,'\\',id[i],'.csv',sep='') }
  
  final<-read.csv(filenames[1],header=T)
 
  if(length(filenames)>1){
    for(k in 2:length(filenames))
    {
      print(filenames[k])
      rbind(read.csv(filenames[k],header=T),final)->final
    }
  }
  
  ##############
 print(mean(final[,pollutant],na.rm=T))
 
  
}

