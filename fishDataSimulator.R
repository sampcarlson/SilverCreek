simEventDate=function(){
  startDate=as.POSIXct(strptime("2022-02-10 00:00:00", "%Y-%m-%d %H:%M:%S"))
  period=as.numeric(diff.Date(c(startDate,Sys.Date())))
  addDays=runif(1)*period
  return(startDate+(addDays*86400))
}

simFish=data.frame(fishID=sample(1:10,100,replace = T),
                   arrayID=sample(1:8,100,replace=T))
simFish$dateTime=replicate(nrow(simFish),simEventDate())
simFish$dateTime=as.POSIXct(simFish$dateTime,origin = '1970-01-01 00:00.00')
write.csv(simFish,"simFish.csv")
