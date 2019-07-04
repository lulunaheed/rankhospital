rankall<- function (outcome,num="best"){
  loc <- "/Users/lilafakharzadeh/Desktop/Coursera/rprog_data_ProgAssignment3-data"
  setwd(loc)
  ## Read outcome data
  outcomedata<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  
  statesused<-unique(outcomedata[,7])
  statenames<-statesused[order(statesused)]
  numstates<-length(statenames)
  hs<-character()
  sts<-character()
  
  for (j in 1:numstates){
    state<-statenames[j]
    sts[j]<-as.character(state)
    vals<-(outcomedata[,7]==state)
    newstate<-outcomedata[vals,7]
    if ((sum(vals))==0){
    stop(geterrmessage="invalid state")
    }
    if (outcome=="heart attack"){
      newoutcome<-outcomedata[vals,11]
      newhospital<-outcomedata[vals,2]
      n<-order(state=newstate,outcome=newoutcome,hospital=newhospital)
      newmat<-data.frame(state=newstate[n],outcome=newoutcome[n],hospital=newhospital[n])
      good<-complete.cases(newmat)
      newmat<-data.frame(state=newmat$state[good],outcome=newmat$outcome[good],hospital=newmat$hospital[good])
    } else if (outcome=="heart failure"){
      newoutcome<-outcomedata[,17][vals]
      newhospital<-outcomedata[vals,2]
      n<-order(state=newstate,outcome=newoutcome,hospital=newhospital)
      newmat<-data.frame(state=newstate[n],outcome=newoutcome[n],hospital=newhospital[n])
      good<-complete.cases(newmat)
      newmat<-data.frame(state=newmat$state[good],outcome=newmat$outcome[good],hospital=newmat$hospital[good])
    } else if (outcome=="pneumonia"){
      newoutcome<-outcomedata[,23][vals]
      newhospital<-outcomedata[vals,2]
      n<-order(state=newstate,outcome=newoutcome,hospital=newhospital)
      newmat<-data.frame(state=newstate[n],outcome=newoutcome[n],hospital=newhospital[n])
      good<-complete.cases(newmat)
      newmat<-data.frame(state=newmat$state[good],outcome=newmat$outcome[good],hospital=newmat$hospital[good])
    } else {
      stop(geterrmessage="invalid outcome")
    }
  
    if (num=="best"){
    a<-1
    hs[j]<-as.character(newmat$hospital[a])
    }else if (num=="worst"){
    a<-length(newmat$hospital[good])
    hs[j]<-as.character(newmat$hospital[a])
    }else {
     hs[j]<-as.character(newmat$hospital[num])
    }
}
  
final<-data.frame(HospitalName=hs,StateName=sts)
 
}