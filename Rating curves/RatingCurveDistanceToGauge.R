##Author: Ryan Riggs
##Date: 9/12/2022
##Calculate distances from best node to gauge. 
#################################################################################################
##Libraries
#################################################################################################
library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(BBmisc)
################################################################################################
##Read in nodes. 
################################################################################################
files = list.files("path\\to\\RatingCurveResultsV6", full.names=TRUE)
all = st_read("path\\to\\validQ_1984.shp")
nodeShp = st_read("path\\to\\nodeShp10kmV2.shp")
nds10km = fread("path\\to\\nodeNetwork10km_allv2.csv")

library(sfnetworks)
nodeShp = nodeShp%>%st_set_crs(4326)
all = all%>%st_set_crs(4326)
tab = list()
for(i in 1:length(files)){
print(i)
df = fread(files[i])
if(nrow(df)==0){next}
df$Sttn_Nm = gsub("Gauge__", "", df$agency)
df$Sttn_Nm = gsub("grdc", "GRDC", df$Sttn_Nm)

##Filer to gauge location. 
allFilt = all[all$Sttn_Nm==df$Sttn_Nm[1],]
allFilt = allFilt

##Filter to nodes within 10km of gauge location. 
nearestNd = which(nds10km$Sttn_Nm==df$Sttn_Nm[1])
nd = nds10km$node_id[nearestNd]
nearestNd = nodeShp[nodeShp$node_id%in%as.character(nd),]
if(nrow(nearestNd)==0){  tab[[i]] = data.frame(NA)
}else{

##Create network. 
net = as_sfnetwork(nearestNd)

##Filter to nodes that have valid cross sections. 
nodeShpFilt = nearestNd#nodeShp[nodeShp$node_id%in%as.character(df$node_id),]

##Measure distance along SWORD network. 
networkDistance = try(st_network_cost(net,from=allFilt,to=nodeShpFilt))
if(nrow(networkDistance)==0|is.error(networkDistance)){
  tab[[i]] = data.frame(NA)
}else{
  nodeShpFilt$distance = networkDistance[1,]
  nodeShpFilt = nodeShpFilt[order(nodeShpFilt$distance),]
  
  ##Using the closest node and SWORD Id, assign upstream or downstream to each distance from the gauge. 
  baseLevel = as.numeric(nodeShpFilt$node_id[1])
  nodeShpFilt$flow = ifelse(as.numeric(nodeShpFilt$node_id)<baseLevel, nodeShpFilt$distance, nodeShpFilt$distance*-1)
  nodeShpFilt$flow = as.numeric(nodeShpFilt$flow)
  df$node_id = as.character(df$node_id)
  df$flow = nodeShpFilt$flow[match(df$node_id, nodeShpFilt$node_id)]
  df$node_width = nodeShpFilt$width[match(df$node_id, nodeShpFilt$node_id)]
  df$node_width_var = nodeShpFilt$wth_var[match(df$node_id, nodeShpFilt$node_id)]
  tab[[i]] = df
}
}
}
for(i in 1:length(tab)){
  print(i)
  df = tab[[i]]
  fwrite(df, paste0("path\\out\\RatingCurveResultsV6wDistance\\", df$Sttn_Nm[1],".csv"))
}
