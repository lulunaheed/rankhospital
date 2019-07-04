best<- function(state,outcome){
  loc <- "/Users/lilafakharzadeh/Desktop/Coursera/rprog_data_ProgAssignment3-data"
  setwd(loc)
  ## Read outcome data
  outcomedata<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  
  ##Split data from different States
  
  vals<-(outcomedata[,7]==state)
  if ((sum(vals))==0){
    stop(geterrmessage="invalid state")
  }
  newstate<-outcomedata[,7][vals]
  
  if (outcome=="heart attack"){
   newoutcome<-outcomedata[,11][vals]
   newhospital<-outcomedata[,2][vals]
   newmat<-data.frame(state=newstate,outcome=newoutcome,hospital=newhospital)
   good<-complete.cases(newmat)
   newmat<-data.frame(state=newstate[good],outcome=newoutcome[good],hospital=newhospital[good])
  }else if (outcome=="heart failure"){
    newoutcome<-outcomedata[,17][vals]
    newhospital<-outcomedata[,2][vals]
    newmat<-data.frame(state=newstate,outcome=newoutcome,hospital=newhospital)
    good<-complete.cases(newmat)
    newmat<-data.frame(state=newstate[good],outcome=newoutcome[good],hospital=newhospital[good])
  }else if (outcome=="pneumonia"){
    newoutcome<-outcomedata[,23][vals]
    newhospital<-outcomedata[,2][vals]
    newmat<-data.frame(state=newstate,outcome=newoutcome,hospital=newhospital)
    good<-complete.cases(newmat)
    newmat<-data.frame(state=newstate[good],outcome=newoutcome[good],hospital=newhospital[good])
  }else {
    stop(geterrmessage="invalid outcome")
  }
  
  lowestdeaths<-min(newmat[,2])
  inds<-newmat[,3][lowestdeaths==newmat[,2]]
  besthospitals<-hospitalsinstate[inds]
  
  if (length(besthospitals)>1){
    firstletters<-vector()
    o<-vector()
    noms<-vector()
    ordering<-LETTERS
    for (t in (1:length(inds))){
      hello<-inds[t]
      noms[t]<-besthospitals[hello]
      firstletters[t]<-substr(besthospitals[hello],1,1)
      now<-firstletters[t]
      n<-1:26
      o[t]<-n[now==ordering]
    }
    first<-min(o)
    yes<-LETTERS[first]==firstletters
    finalname<-noms[yes]
  } else {
    
    besthospitals
  }
  
  
  
  