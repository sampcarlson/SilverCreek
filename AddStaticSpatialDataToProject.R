library(sf)

netDSN="C:/Users/sam/Documents/spatial/SilverCreek/SilverCreekNet.gpkg"
netName="SilverCreek"

#load data
network=st_read(dsn=netDSN,layer=netName)

#remove m, simplify to xy:
network=st_zm(network)

plot(network$geom,col="blue")


##################Leaflet app is in 4326.  Reproject for easier use there:
network=st_transform(network,crs=st_crs(4326))
#save to .rds file for simple access
saveRDS(network,file="C:/Users/sam/Documents/SilverCreek/R/SilverCreekApp/network.rds")
