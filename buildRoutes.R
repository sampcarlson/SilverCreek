library(sf)
library(terra)

library(leastcostpath)

library(raster)#needed for type conversions within leastcostpath

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
      st_cast(near[which.min(st_length(near))],"POINT")[2] 
    )  #find geometry of nearest snap point. [2] returns 'to' point of st_nearest line
    print(paste("Snapped point",points_to_snap$Name[i],"to target line", round(st_distance(points_to_snap[i,],thisLocation),1), "meters"))
    points_to_snap[i,]$geom=thisLocation
  }
  return(points_to_snap)
}

arrays=snapPointsToLines(points_to_snap = arrays,target_lines = network)



network$weight=1 #for barrier raster, network = 1, upland = 0
# high res here can cause some issues w/ the route finder if using barrier cost method...
networkRaster=terra::rast(vect(network),res=10,crs="EPSG:32611",vals=90)

# not sure why, but only works w/ update = T
nr=terra::rasterize(vect(network),networkRaster,field="weight",update=T)

#check...
plot(nr)

#barrier cost surface
#costSurface=create_barrier_cs(raster=raster(nr),barrier=raster(nr),field="mask")

#sometimes throws 'Error in x$.self$finalize() : attempt to apply non-function' but works anyway????
costSurface=create_slope_cs(dem=raster(nr),cost_function="wheeled transport",max_slope=10)


#make df of all route ids
arrays$ID=row.names(arrays)
allRouteIDs=expand.grid(arrays$ID,arrays$ID)
names(allRouteIDs)=c("from","to")

buildRoute=function(from,to,costSurf,arySites){
  routeName=paste0(from,"_to_",to)
  route=create_lcp(cost_surface = costSurf,
                   origin=as(arySites[to,],"Spatial"),
                   destination=as(arySites[from,],"Spatial"),
                   directional=T)
  route=st_as_sf(route)
  route$routeName=routeName
  return(route)
}

#could use sapply, but then I get a list of sf objects rather than a 'multi' sf object
for(i in 1:nrow(allRouteIDs)){
  print(i)
  thisRoute=buildRoute(from=allRouteIDs$from[i], to=allRouteIDs$to[i],
                       costSurf = costSurface, arySites = arrays)
  if(i==1){
    allRoutes=thisRoute
  } else { allRoutes=rbind(allRoutes,thisRoute) }
}
row.names(allRoutes)=allRoutes$routeName


plot(st_geometry(network),col="blue")
plot(st_geometry(arrays),add=T,pch=as.character(arrays$ID))
plot(st_geometry(allRoutes["8_to_2",]),add=T,lwd=4)

st_write(allRoutes,"allRoutes.gpkg")
