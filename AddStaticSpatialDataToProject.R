library(sf)

netDSN="C:/Users/sam/Documents/spatial/SilverCreek/SilverCreekNet.gpkg"
netName="SilverCreek"



#load data
network=st_read(dsn=netDSN,layer=netName)

#remove m, simplify to xy:
network=st_zm(network)

plot(network$geom,col="blue")

#save to .rds file for simple access
saveRDS(network,file="C:/Users/sam/Documents/R/projects/SilverCreekApp/network.rds")
