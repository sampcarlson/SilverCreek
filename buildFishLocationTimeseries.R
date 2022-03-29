library(sf)


fishObs=read.csv("simFish.csv")
routes=st_read("allRoutes.gpkg")

#
observations=fishObs
allRoutes=routes

calculateRoutePoints=function(startLoc,endLoc,startTime,endTime,thisRoutes,timestep="hours"){
  thisTimes=round(
    seq(from=as.POSIXct(startTime),to=as.POSIXct(endTime),by=timestep),
    units=timestep  
  )
  
  if(startLoc == endLoc){
    #find coords of this location
    
    thisStartLocRoute=thisRoutes[
      thisRoutes$routeName %in% c(paste0(startLoc,"_to_",endLoc+1),paste0(startLoc,"_to_",endLoc-1))
      ,][1,]
    
    locationDF=data.frame(st_coordinates(thisStartLocRoute))[1,] #grab the first coord, all others are erronious
    locationDF=locationDF[rep(1,length(thisTimes)),]
  }else{ #find coords along route
    thisRouteName=paste0(startLoc,"_to_",endLoc)
    thisRoute=thisRoutes[thisRoutes$routeName==thisRouteName,]
    location=st_line_sample(thisRoute,n=length(thisTimes))
    locationDF=data.frame(st_coordinates(location))
  }
  locationDF$dateTime=thisTimes
  locationDF$L1=NULL
  #location=st_cast(location,"POINT")
  #location=st_sf(location)
  #location$dateTime=thisTimes
  return(locationDF)
}


buildLocationTimeseries=function(observations,allRoutes){ #must have a 'fishID' field, an 'arrayID' field, and a 'dateTime' field
  fish=unique(observations$fishID)
  
  for(f in fish){
    print(paste("fish",f))
    thisFishObs=observations[observations$fishID==fish[f],]
    thisFishObs=thisFishObs[order(thisFishObs$dateTime),]
    for(o in 2:nrow(thisFishObs)){
      #print(o)
      thisStartLoc=thisFishObs[o-1,"arrayID"]
      thisEndLoc=thisFishObs[o,"arrayID"]
      thisStartTime=thisFishObs[o-1,"dateTime"]
      thisEndTime=thisFishObs[o,"dateTime"]
      thisRoutePoints=calculateRoutePoints(startLoc = thisStartLoc, endLoc = thisEndLoc, 
                                           startTime = thisStartTime, endTime = thisEndTime,
                                           thisRoutes=allRoutes,timestep = "hours")
      thisRoutePoints$fishID=fish[f]
      if(exists("allLocations")){
        allLocations=rbind(allLocations,thisRoutePoints)
      }else{
        allLocations=thisRoutePoints
      }
    }
  }
  return(allLocations)
  
}

locationTimeseries=buildLocationTimeseries(observations=fishObs,allRoutes=routes)

locationTimeseries$dateTimeNumeric=as.numeric(locationTimeseries$dateTime)

format(object.size(locationTimeseries),units="Mb")
