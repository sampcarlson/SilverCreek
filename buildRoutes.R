library(sf)
library(terra)
library(gdistance)

library(raster) #still needed for type conversions within gdistance::transition

#sources
netDSN="C:/Users/sam/Documents/spatial/SilverCreek/SilverCreekNet.gpkg"
netName="SilverCreek"

aryDSN="C:/Users/sam/Documents/spatial/SilverCreek/ArraysTest.gpkg"
aryName="arrays"

#load data
network=st_read(dsn=netDSN,layer=netName)
arrays=st_read(dsn=aryDSN,layer=aryName)

#rm M (order) for GEOS compatibility
network=st_zm(network)

snapPointsToLines=function(points_to_snap,target_lines){
  #First define points along lines to snap to...
  snapPoints=st_line_sample(st_cast(target_lines,"LINESTRING"),density=1)#'density' snap points per meter
  
  #snap array pts to pts along network lines:
  #st_snap to the line directly doesn't work, st_snap to all sampled points also fails.  
  
  for(i in 1:nrow(points_to_snap)){ #iterate through points
    near=st_nearest_points(points_to_snap[i,],snapPoints)  #find line to all snapPoints
    thisLocation=st_geometry( 
      st_cast(near[which.min(st_length(near))],"POINT")[2] #find geometry of nearest snap point. [2] returns 'to' point of st_nearest line
    )  
    print(paste("Snapped point",points_to_snap$Name[i],"to target line", round(st_distance(points_to_snap[i,],thisLocation),1), "meters"))
    points_to_snap[i,]$geom=thisLocation
  }
  return(points_to_snap)
}

arrays=snapPointsToLines(points_to_snap = arrays,target_lines = network)


network$weight=1 
rastRes=5#reducing resolution vastly speeds up process
nr=terra::rast(vect(network),res=rastRes,crs="EPSG:32611",vals=NA) 

netBuff=st_buffer(network,dist=2*rastRes)
netBuff$weight=0.1

# not sure why, but only works w/ update = T

nr=terra::rasterize(vect(netBuff),nr,field="weight",update=T)
nr=terra::rasterize(vect(network),nr,field="weight",update=T)
#check...
#plot(nr)

costSurface=transition(raster::raster(nr),transitionFunction=prod,directions=8)

#make df of all route ids
arrays$ID=row.names(arrays)
allRouteIDs=expand.grid(arrays$ID,arrays$ID)
names(allRouteIDs)=c("from","to")

buildRoute=function(from,to,costSurf,arySites){
  routeName=paste0(from,"_to_",to)
  print(routeName)
  
  if(from==to){ #return a 'linestring' from/to the same point
    coords=st_coordinates(arySites[from,])
    routePoint=st_point(coords)
    route=st_linestring(c(routePoint,routePoint))
    route=st_sfc(route,crs="EPSG:32611")
    #route=data.frame(route)
    route=st_as_sf(route)
    route$geometry=st_geometry(route)
    route$x=NULL
  }else{
    
    route=shortestPath(costSurf, 
                       origin=st_coordinates(arySites[from,]),
                       goal=st_coordinates(arySites[to,]),
                       output="SpatialLines")
    route=st_as_sf(route)
  }
  route$routeName=routeName
  return(route)
}

#could apply, but then I get a list of sf objects rather than a 'multi' sf object
for(i in 1:nrow(allRouteIDs)){
  thisRoute=buildRoute(from=allRouteIDs$from[i], to=allRouteIDs$to[i],
                       costSurf = costSurface, arySites = arrays)
  if(i==1){
    allRoutes=thisRoute
  } else { allRoutes=rbind(allRoutes,thisRoute) }
}
row.names(allRoutes)=allRoutes$routeName
allRoutes=st_as_sf(allRoutes)

plot(st_geometry(network),col="blue")
plot(st_geometry(arrays),add=T,pch=as.character(arrays$ID))
plot(st_geometry(allRoutes),add=T,lwd=4)

plot(st_geometry(allRoutes[29,]),add=T,lwd=4)

st_write(allRoutes,"allRoutes.gpkg",append=F,overwrite=T)

#writeRaster(nr,"NetRast.tif",overwrite=T)
#st_write(arrays,"arrays.gpkg",overwrite=T)

