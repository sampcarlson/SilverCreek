getAllRecords_read=function(hexID){
  thisRD=readData[readData$HEX.Tag.ID==hexID & !is.na(readData$HEX.Tag.ID),]
  thisFG=fgData[fgData$hexID==hexID & !is.na(fgData$hexID),]
  thisMob=mobData[mobData$Tag_ID==hexID & !is.na(mobData$Tag_ID),]
  list(reader=thisRD,FG=thisFG,mobile=thisMob)
}

getAllRecords=function(anyID,allObs=allFishObs){
  r1=allObs[allObs$l4dec==anyID & !is.na(allObs$l4dec),]
  r2=allObs[allObs$l4hex==anyID & !is.na(allObs$l4hex),]
  r3=allObs[allObs$hexID==anyID& !is.na(allObs$hexID),]
  r4=allObs[allObs$decID==anyID& !is.na(allObs$decID),]
  r0=rbind(r1,r2,r3,r4)
  r0=r0[order(r0$dateTime),]
  return(r0)
}
