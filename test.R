library(adehabitatHR) #for home range calculations
library(data.table) #manipulate S3 and S4 data tables
library(tidyverse) #for graphic output
library(ggfortify) #to allow ggplot2 to read spatial data
library(grid) #to add annotations to the output
library(move) #create movement data
library(moveVis) #visualize the movement data
library(OpenStreetMap) #for obtaining raster images
library(pbapply) #needed for progress bar
library(plotly) #for interactive xy plot
library(rgdal) #for converting spatial data
library(sp) #for converting spatial data

torts<-read.csv("Data/AllTortsUTM.csv", header=TRUE)
torts_blank <- torts[-c(6:9,20:21,25:33)]
data <- na.omit(torts_blank)

Mary <- data %>% subset(individual.local.identifier=="Mary")

#Mary <- read.csv("Mary .csv")
date <- as.POSIXct(strptime(as.character(Mary$timestamp),"%Y-%m-%d %H:%M:%S", tz="Asia/Bangkok"))
Mary$date <- date
Mary.reloc <- cbind.data.frame(Mary$utm.easting, Mary$utm.northing,
                               as.vector(Mary$individual.local.identifier),
                               as.POSIXct(date))
colnames(Mary.reloc) <- c("x","y","id","date")
trajectory <- as.ltraj(Mary.reloc, date=date, id="Mary")
sig1 <- liker(trajectory, sig2 = 58, rangesig1 = c(0, 5), plotit = FALSE)
opha.traj <- kernelbb(trajectory, sig1 = .1351, sig2 = 58, grid = 100)
bb_ver <- getverticeshr(opha.traj, 95)
bb_poly <- fortify(bb_ver, region = "id", 
                   proj4string = CRS("+proj=utm +zone=15 +south
                                     +ellps=WGS84 +units=m +no_defs"))
colnames(bb_poly) <- c("x","y","order","hole","piece","id","group")
bb_image <- crop(opha.traj, bb_ver, 
                 proj4string = CRS("+proj=utm +zone=15 +south 
                                   +ellps=WGS84 +units=m +no_defs"))
bb_units <- grid.text(paste(round(bb_ver$area,2)," ha"), x=0.85,  y=0.95,
                      gp=gpar(fontface=4, col="white", cex=0.9), draw = FALSE)
bb.plot <- autoplot(raster_utm, expand = TRUE) + theme_bw() + theme(legend.position="none") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  geom_tile(data=bb_image, 
            aes(x=bb_image@coords[,1], y=bb_image@coords[,2],
                fill = bb_image@data$ud)) +
  geom_polygon(data=bb_poly, aes(x=x, y=y, group = group), color = "black", fill = NA) +
  scale_fill_viridis_c(option = "inferno") + annotation_custom(bb_units) +
  labs(x="Easting (m)", y="Northing (m)", title="Mary") +
  theme(legend.position="none", plot.title = element_text(face = "bold", hjust = 0.5))
bb.plot