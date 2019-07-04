outcome<-read.csv("outcome-of-care-measures.csv")
head(outcome)

numcolumns<- ncol(outcome)
numrows<- nrow(outcome)

nameofcolumns<-names(outcome)

outcome[,11]<- as.numeric(outcome[,11])
hist(outcome[,11])