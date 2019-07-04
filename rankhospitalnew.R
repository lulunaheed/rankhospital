rankhospital<- function (state, outcome,num){
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
    rankedinds<-order(newoutcome[good])
    newmat<-data.frame(state=newstate[rankedinds],outcome=newoutcome[rankedinds],Rank=order(newoutcome[rankedinds],hospital=newhospital[rankedinds])
    } else if{
    newoutcome<-outcomedata[,17][vals]
    newhospital<-outcomedata[,2][vals]
    newmat<-data.frame(state=newstate,outcome=newoutcome,hospital=newhospital)
    good<-complete.cases(newmat)
    rankedinds<-order(newoutcome[good])
    newmat<-data.frame(state=newstate[rankedinds],outcome=newoutcome[rankedinds],Rank=order(newoutcome[rankedinds],hospital=newhospital[rankedinds])
    } else if (outcome=="pneumonia"){
    newoutcome<-outcomedata[,23][vals]
    newhospital<-outcomedata[,2][vals]
    newmat<-data.frame(state=newstate,outcome=newoutcome,hospital=newhospital)
    good<-complete.cases(newmat)
    rankedinds<-order(newoutcome[good])
    newmat<-data.frame(state=newstate[rankedinds],outcome=newoutcome[rankedinds],Rank=order(newoutcome[rankedinds],hospital=newhospital[rankedinds])
    } else {
      stop(geterrmessage="invalid outcome")
    }
  
  if (num=="best"){
    numb<-min(newmat[,2])
    hosp<-newmat[,4][newmat[,2]==numb]
    l<-length(hosp)
      if (l>1){
        newhosps<-sort(hosp)
        prefhosp<-newhosps[1]
        return(prefhosp)
      }else{
      return(hosp)
      }
  }else if(num=="worst"){
    numb<-max(newmat[,2])
    hosp<-newmat[,4][newmat[,2]==numb]
    l<-length(hosp)
      if (l>1){
        newhosps<-sort(hosp)
        prefhosp<-newhosps[1]
        return(prefhosp)
      }else {
        return(hosp)
      }
  }else if (num>max(rankedinds)){
    return(NA)
  }else{
    hel<-newmat[,2][num]
    hosp<-newmat[,4][newmat[,2]==hel]
    l<-length(hosp)
      if (l>1){
        newhosps<-sort(hosp)
        prefhosp<-newhosps[1]
        return(prefhosp)
      }else{
      return(hosp)
      }
  }
  
}