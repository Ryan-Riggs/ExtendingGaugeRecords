##Calculate distances from best node to gauge. 
library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(BBmisc)

##Redundant data to remove.
rm = fread("E:\\research\\RatingCurveAnalysis\\stats\\removeThese.csv")
'%!in%' <- function(x,y)!('%in%'(x,y))
################################################################################################
##Read in nodes. 
################################################################################################
files = list.files("E:\\research\\RatingCurveAnalysis\\RatingCurveResultsV6", full.names=TRUE)
#all = st_read("E:\\research\\GlobalGaugeData\\Stations\\LocationsGaugesGRWLfilt.shp")
#nodeShp = st_read("E:\\research\\RatingCurveAnalysis\\stats\\nodeShp10km.shp")
#nds10km = fread("E:\\research\\RatingCurveAnalysis\\stats\\nodes_10km_Sttn_Nm.csv")
all = st_read("E:\\research\\GlobalGaugeData\\Stations\\validQ_1984.shp")
nodeShp = st_read("E:\\research\\RatingCurveAnalysis\\stats\\nodeShp10kmV2.shp")
nds10km = fread("E:\\research\\RatingCurveAnalysis\\stats\\nodeNetwork10km_allv2.csv")

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
  fwrite(df, paste0("E:\\research\\RatingCurveAnalysis\\RatingCurveResultsV6wDistance\\", df$Sttn_Nm[1],".csv"))
}






outputList = list()
for(j in 1:length(tab)){
  print(j)
  df = tab[[j]]
  if(ncol(df)==1){next}
  
  ##Limit to nodes within 10 km. 
  df = df[abs(flow)<10000]
  
  ##Limit to nodes on river reaches. No lakes/obstructions, etc. 
  df$node_id_end = stringr::str_sub(df$node_id, -1)
  df = df[node_id_end=="1"]
  df = df[df[,.I[which.max(nse)],]]
  df$Sttn_Nm = as.character(df$Sttn_Nm)
  df$node_id = as.character(df$node_id)
  df$model = as.character(df$model)
  df$nse = as.numeric(df$nse)
  df$rrmse = as.numeric(df$rrmse)
  df$rbias = as.numeric(df$rbias)
  df$kge = as.numeric(df$kge)
  df$flow = as.numeric(df$flow)
  outputList[[j]] = df%>%select(-Site_number)
}
output = rbindlist(outputList)
output = output[output$Sttn_Nm%!in%rm$Sttn_Nm,]
summary(output)



library(tmap)
tmap_mode("view")
tm_basemap("Esri.WorldImagery")+
  tm_shape(nodeShpFilt)+
  tm_bubbles(size=0.05, col="flow")+
  tm_shape(allFilt)+
  tm_bubbles(size=0.1, col="purple")


h = ggplot(output)+
  geom_histogram(aes(flow),
                 #binwidth=200,
                 breaks=seq(-5000,5000,400),
                 color="black",
                 fill="red",
                 alpha=0.5,
                 position="stack")+
  coord_cartesian(xlim=c(-5000,5000))+
  ggtitle("SWORD distance to rating curve")+
  xlab("Distance (m)")+
  ylab("Count")+
  annotate("text",                        # Add text for mean
           x = c(-4500,-4500, 4000,4000),
           y = c(450,500,450,500),
           label = c(paste("Mean =", round(mean(output$flow))), 
                     paste("Median =", round(median(output$flow))),
                     paste("abs(Mean) =", round(mean(abs(output$flow)))), 
                     paste("abs(Median) =", round(median(abs(output$flow))))),
           col = "red",
           size = 4)+
  theme_classic()
h
ggplotly(h)




ggplot(output)+
  geom_histogram(aes(model),
                 stat="count",
                 binwidth=200,
                 color="black",
                 fill="red",
                 alpha=0.5,
                 position="stack")+
  #ggtitle("Distance to rating curve")+
  xlab("Distance (m)")+
  ylab("Count")+
  theme_classic()


#############################################################
##Investigate. 
#############################################################
filesShort = list.files("E:\\research\\RatingCurveAnalysis\\RatingCurveResultsV2")
filesShort = gsub("grdc", "GRDC", filesShort)

i = grep("13180000_ANA", filesShort)
df = fread(files[i])
df$Sttn_Nm = gsub("Gauge__", "", df$agency)
df$Sttn_Nm = gsub("grdc", "GRDC", df$Sttn_Nm)

##Filer to gauge location. 
allFilt = all[all$Sttn_Nm==df$Sttn_Nm[1],]
allFilt = allFilt

##Filter to nodes within 10km of gauge location. 
nearestNd = which(nds10km$Sttn_Nm==df$Sttn_Nm[1])
nd = nds10km$node_id[nearestNd]
nearestNd = nodeShp[nodeShp$node_id%in%as.character(nd),]

##Create network. 
net = as_sfnetwork(nearestNd)

##Filter to nodes that have valid cross sections. 
nodeShpFilt = nearestNd#nodeShp[nodeShp$node_id%in%as.character(df$node_id),]

##Measure distance along SWORD network. 
networkDistance = try(st_network_cost(net,from=allFilt,to=nodeShpFilt))
nodeShpFilt$distance = networkDistance[1,]
nodeShpFilt = nodeShpFilt[order(nodeShpFilt$distance),]
  
##Using the closest node and SWORD Id, assign upstream or downstream to each distance from the gauge. 
baseLevel = as.numeric(nodeShpFilt$node_id[1])
nodeShpFilt$flow = ifelse(as.numeric(nodeShpFilt$node_id)<baseLevel, nodeShpFilt$distance, nodeShpFilt$distance*-1)
nodeShpFilt$flow = as.numeric(nodeShpFilt$flow)
best = nodeShpFilt[nodeShpFilt$node_id==output$node_id[output$Sttn_Nm==allFilt$Sttn_Nm],]
print(best$flow)
output$nse[output$Sttn_Nm==allFilt$Sttn_Nm]

tmap_mode("view")
tm_basemap("Esri.WorldImagery")+
  tm_shape(nodeShpFilt)+
  tm_bubbles(size=0.05, col="flow")+
  tm_shape(allFilt)+
  tm_bubbles(size=0.1, col="purple")+
  tm_shape(best)+
  tm_bubbles(size=0.1, col="white")



output$model = factor(output$model, levels=c("hg", "Spl", "Pcw", "RF"))
p = ggplot(output)+
  geom_histogram(aes(x=abs(flow), fill=model),alpha=0.5,binwidth = 200)+
  #coord_cartesian(xlim=c(-5000,5000))+
  facet_wrap(~model, nrow=1)+
  theme_classic()

library(plotly)
ggplotly(p)





#################################################################
##Export images. 
#################################################################
#############################################################
filesShort = list.files("E:\\research\\RatingCurveAnalysis\\RatingCurveResultsV2")
filesShort = gsub("grdc", "GRDC", filesShort)
filesShort = gsub("Gauge__", "", filesShort)
filesShort = gsub(".csv", "", filesShort)
filesIn = filesShort%in%output$Sttn_Nm

pdf("E:\\research\\RatingCurveAnalysis\\Figures\\spatialPlots.pdf", width=7, height=5)
for(i in 1:length(files[filesIn])){
print(i)
df = fread(files[filesIn][i])
df$Sttn_Nm = gsub("Gauge__", "", df$agency)
df$Sttn_Nm = gsub("grdc", "GRDC", df$Sttn_Nm)

##Filer to gauge location. 
allFilt = all[all$Sttn_Nm==df$Sttn_Nm[1],]
allFilt = allFilt

##Filter to nodes within 10km of gauge location. 
nearestNd = which(nds10km$Sttn_Nm==df$Sttn_Nm[1])
nd = nds10km$node_id[nearestNd]
nearestNd = nodeShp[nodeShp$node_id%in%as.character(nd),]

##Create network. 
net = as_sfnetwork(nearestNd)

##Filter to nodes that have valid cross sections. 
nodeShpFilt = nearestNd#nodeShp[nodeShp$node_id%in%as.character(df$node_id),]

##Measure distance along SWORD network. 
networkDistance = try(st_network_cost(net,from=allFilt,to=nodeShpFilt))
nodeShpFilt$distance = networkDistance[1,]
nodeShpFilt = nodeShpFilt[order(nodeShpFilt$distance),]

##Using the closest node and SWORD Id, assign upstream or downstream to each distance from the gauge. 
baseLevel = as.numeric(nodeShpFilt$node_id[1])
nodeShpFilt$flow = ifelse(as.numeric(nodeShpFilt$node_id)<baseLevel, nodeShpFilt$distance, nodeShpFilt$distance*-1)
nodeShpFilt$flow = as.numeric(nodeShpFilt$flow)
best = nodeShpFilt[nodeShpFilt$node_id==output$node_id[output$Sttn_Nm==allFilt$Sttn_Nm],]
print(best$flow)
output$nse[output$Sttn_Nm==allFilt$Sttn_Nm]

tmap_mode("plot")
print(tm_basemap("Esri.WorldImagery")+
  tm_shape(nodeShpFilt)+
  tm_bubbles(size=0.05, col="flow")+
  tm_shape(allFilt)+
  tm_bubbles(size=0.1, col="purple")+
  tm_shape(best)+
  tm_bubbles(size=0.1, col="white")+
  tm_layout(title=paste(allFilt$Sttn_Nm, round(best$flow))))

}

dev.off()
