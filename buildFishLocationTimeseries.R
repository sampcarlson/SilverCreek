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
  
  thisRouteName=paste0(startLoc,"_to_",endLoc)
  thisRoute=thisRoutes[thisRoutes$routeName==thisRouteName,]
  
  if(startLoc == endLoc){
    #find coords of this location
    locationDF=data.frame(st_coordinates(thisRoute))[1,] #grab the first coord
    locationDF=locationDF[rep(1,length(thisTimes)),]
  }else{ #find coords along route
    location=st_line_sample(thisRoute,n=length(thisTimes))
    locationDF=data.frame(st_coordinates(location))
  }
  locationDF$time=thisTimes
  locationDF$L1=NULL
  locationDF$routeName=thisRouteName

  return(locationDF)
}


buildLocationTimeseries=function(observations,allRoutes){ #must have a 'fishID' field, an 'arrayID' field, and a 'time' field
  fish=unique(observations$fishID)
  
  for(f in fish){
    print(paste("fish",f))
    thisFishObs=observations[observations$fishID==fish[f],]
    thisFishObs=thisFishObs[order(thisFishObs$time),]
    for(o in 2:nrow(thisFishObs)){
      #print(o)
      thisStartLoc=thisFishObs[o-1,"arrayID"]
      thisEndLoc=thisFishObs[o,"arrayID"]
      thisStartTime=thisFishObs[o-1,"time"]
      thisEndTime=thisFishObs[o,"time"]
      
      thisRoutePoints=calculateRoutePoints(startLoc = thisStartLoc, endLoc = thisEndLoc, 
                                           startTime = thisStartTime, endTime = thisEndTime,
                                           thisRoutes=allRoutes,timestep = "hours")
      #print(paste0(thisStartLoc," to ",thisEndLoc))
      
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

#locationTimeseries$timeNumeric=as.numeric(locationTimeseries$time)

locationTimeseries=st_as_sf(locationTimeseries,coords=c("X","Y"))
st_crs(locationTimeseries)=st_crs(32611)

locationTimeseries=st_transform(locationTimeseries,crs=st_crs(4326))

format(object.size(locationTimeseries),units="Mb")

st_write(locationTimeseries,"C:\\Users\\sam\\Documents\\SilverCreek\\R\\SilverCreekApp\\fishLocationTimeseries.gpkg",append=FALSE)

#plot(st_geometry(locationTimeseries))
plot(st_geometry(locationTimeseries[locationTimeseries$fishID==1,]),type="l")