
setwd('C:\\Users\\Guest\\Downloads\\R\\Coursera assign')
getwd()
rankall <- function(outcome, num = "best") {
 
  # changing names
  which(names(outcome1) %in%c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
  
  #Reading File
  outcome1 <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=F)
 
  # checking values of the outcome 
  if(!toupper(outcome) %in% (c('HEART ATTACK','HEART FAILURE','PNEUMONIA'))){stop("invalid outcome")}
  outcome1[,which(names(outcome1) %in%c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))]->sub
  # renaming Not applicable to NA's
  sub[which(sub[,3]=='Not Available'),3]<-NA
  sub[which(sub[,4]=='Not Available'),4]<-NA
  sub[which(sub[,5]=='Not Available'),5]<-NA
  as.numeric(sub[,3])->sub[,3]
  as.numeric(sub[,4])->sub[,4]
  as.numeric(sub[,5])->sub[,5]
  #Renaming columns
  names(sub)<-c("Hospital_name","state","HEART ATTACK","HEART FAILURE","PNEUMONIA")
  grep(toupper(outcome),names(sub))->col
  
  sub[order(sub$Hospital_name),]->sub
  sub[!is.na(sub[,col]),]->sub
  #Ranking
  transform(sub,rank=ave(sub[,col],sub$state,FUN=function(x) rank(x,na.last = T,ties.method = "first"))) ->ranked
  
  sort(unique(ranked$state))->states
  
  
  if (num=="best")
  {
    
    ranked[which(ranked$rank==1),c(1,2)]->bestrank
    bestrank[order(bestrank$state),c(1,2)]->bestrank
    rownames(bestrank)<-states
    names(bestrank)<-c("hospital","state")
    return(bestrank)
    
  }else if(num=="worst")
  {
    aggregate(ranked$rank~ranked$state,FUN=function(x) max(x))->maxrank
    # names(rabked)
    names(maxrank)<-c("state","rank")
    merge(maxrank,ranked,by=c("state","rank"))->worstrank
    worstrank[,c(3,1)]->worstrank
    rownames(worstrank)<-states
    names(worstrank)<-c("hospital","state")
    return(worstrank)
  }else  {
    
    val<-numeric(54)
    
    final1<-data.frame(row.names=c("Hospital","state"))
    for(i in 1:length(states))
    {
      
      if(length(which(ranked$state==states[i] & ranked$rank==num))==1)
      {
        
        ranked[which(ranked$state==states[i] & ranked$rank==num),c(1,2)]->final
        
        final1[i,1]<-final$Hospital_name
        final1[i,2]<-final$state
        
      }else {final1[i,1]<-NA
      final1[i,2]<-states[i]}
    }
    rownames(final1)<-states
    colnames(final1)<-c("hospital","state")
    return(final1)
    
  }
  
}





