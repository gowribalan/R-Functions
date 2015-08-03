setwd('C:\\Users\\Guest\\Downloads\\R\\Coursera assign')
getwd()
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcome1 <- read.csv("outcome-of-care-measures.csv",stringsAsFactors=F)
  #   nrow(outcome1)->cnt
  #   num<-5
  # state<-'MD'
  #    outcome<-'heart attack'
  #    if(!num %in% c("best","worst")) 
  # {
  #      if(!regexp("[:xdigit:]+","worse"))
  #      {
  #        stop("invalid")
  #      }   
  #    }   
  
  st<-unique(outcome1$State)
  if(!state %in% st){ stop("invalid state")}
  if(!toupper(outcome) %in% (c('HEART ATTACK','HEART FAILURE','PNEUMONIA'))){stop("invalid outcome")}
  subset(outcome1,State==state,c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))->sub
  # unique(sub$State)
  summary(sub)
  sub[which(sub[,3]=='Not Available'),3]<-NA
  sub[which(sub[,4]=='Not Available'),4]<-NA
  sub[which(sub[,5]=='Not Available'),5]<-NA
  as.numeric(sub[,3])->sub[,3]
  as.numeric(sub[,4])->sub[,4]
  as.numeric(sub[,5])->sub[,5]
  names(sub)<-c("Hospital_name","state","HEART ATTACK","HEART FAILURE","PNEUMONIA")
  # 'Pneumonia'->outcome
  
  # names(sub)[grep(toupper(outcome),names(sub))]->col
  grep(toupper(outcome),names(sub))->col
  sub[order(sub[,col],sub[,1]),]->ordered
  which(is.na(ordered[,col]))->ind
  clean<-ordered[!is.na(ordered[,col]),]
  # num=5
  # View(ordered)
  if (num=="best")
  {
    print(clean[1,1])
  }else if(num=="worst")
  {
    print(clean[nrow(clean),1])
  }else if(is.numeric(num) & num<nrow(clean))
  {
    print(clean[num,1])
    
  }else
  {
    print(NA)
  }
  
}