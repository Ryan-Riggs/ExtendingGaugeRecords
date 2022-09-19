##Author: Ryan Riggs
##Date: 9/12/2022
################################################################################
##Libraries.
################################################################################
library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(BBmisc)
################################################################################
shp = st_read("path\\to\\allUpdated_endYear.shp")
rm = fread("path\\to\\redundant\\removeTheseV5.csv")
'%!in%' <- function(x,y)!('%in%'(x,y))
shp = shp[shp$Sttn_Nm%!in%rm$Sttn_Nm,]
shp = shp[!is.na(shp$year),]##Author: Ryan Riggs
##Date: 9/12/2022
################################################################################
##Libraries.
################################################################################
library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(BBmisc)
################################################################################
shp = st_read("E:\\research\\GlobalGaugeData\\Stations\\allUpdated_endYear.shp")
rm = fread("E:\\research\\RatingCurveAnalysis\\stats\\removeTheseV5.csv")
'%!in%' <- function(x,y)!('%in%'(x,y))
shp = shp[shp$Sttn_Nm%!in%rm$Sttn_Nm,]
shp = shp[!is.na(shp$year),]
shp = shp%>%distinct(Sttn_Nm, .keep_all = TRUE)
dt = data.table(Date = seq.Date(as.Date("1700-01-01"), 
                                as.Date("2022-12-31"),1))
################################################################################
##Assign gauges to continents. 
################################################################################
library(purrr)
shp$Station_Num = shp$Statin_Nm
##list of dataframes of each continent. 
##split arcitcnet into Europe and Asia. 
arcticnet = shp[shp$agency=='arcticnet',]
arcticnet = arcticnet%>%
  mutate(long = unlist(map(arcticnet$geometry,1)),
         lat = unlist(map(arcticnet$geometry,2)))
arc_eu = arcticnet[arcticnet$long<58,]
arc_as = arcticnet[arcticnet$long>=58,]
ggplot(data=NULL)+geom_sf(data=arc_eu, col='blue')+
  geom_sf(data=arc_as, col='red')

##Europe. 
eu = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="6",]
eu = bind_rows(eu, shp[shp$agency=="AFD",], shp[shp$Sttn_Nm%in%arc_eu$Sttn_Nm,])
nrow(eu)/nrow(shp)
eu$continent = "Europe"

##Asia. 
as = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="2",]
as = bind_rows(as, shp[shp$agency=="CHP"|shp$agency=="RID"|shp$Sttn_Nm%in%arc_as$Sttn_Nm|shp$agency=="MLIT"|shp$agency=="IWRIS",])
nrow(as)/nrow(shp)
as$continent = "Asia"

##Oceania. 
au = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="5",]
au = bind_rows(au, shp[shp$agency=="BOM",])
nrow(au)/nrow(shp)
au$continent = "Oceania"

##Africa. 
af = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="1",]
nrow(af)/nrow(shp)
af$continent = "Africa"

##South America.
sa = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="3",]
sa = bind_rows(sa, shp[shp$agency=="CCRR"|shp$agency=="ANA",])
nrow(sa)/nrow(shp)
sa$continent = "South America"

##North America
na = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="4",]
na = bind_rows(na, shp[shp$agency=="USGS"|shp$agency=="HYDAT",])
nrow(na)/nrow(shp)
na$continent = "North America"
continent = bind_rows(na, sa, af, au, as, eu)
rm(af)
rm(as)
rm(au)
rm(eu)
rm(na)
rm(sa)
rm(files)
rm(arcticnet)
rm(arc_as)
rm(arc_eu)
rm(all)
################################################################################
##Functions to open and read in gauge records. 
################################################################################
path = "E:\\research\\GlobalGaugeData\\combined\\"
files = list.files(path, full.names=TRUE)

##Read in all gage records to a list. 
readFiles = function(f){
  file = paste0(path, f, ".csv")
  file = fread(file)
  file$Sttn_Nm = f
  file$continent = continent$continent[match(file$Sttn_Nm[1], continent$Sttn_Nm)]
  return(file)
}

##Split gage records into observations and missing lists. 
obsFun = function(f){
  df = f
  mx = max(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  mn = min(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  df = df%>%distinct(Date,.keep_all=TRUE)
  all = merge(dt, df, all.x=TRUE)
  disc = all[Date<=mx&Date>=mn]
  disc$month = lubridate::month(disc$Date)
  disc$year = lubridate::year(disc$Date)
  missing = disc[disc$Q<0|is.na(disc$Q)]
  observed = disc[disc$Q>=0&!is.na(disc$Q)]
  ##Added month. 
  agg = observed[,.N,by=list(year,month, Sttn_Nm,continent)]
  agg$type = 'observed'
  ##Added month. 
  agg2 = missing[,.N,by=list(year,month, Sttn_Nm, continent)]
  agg2$type = 'missed'
  comb = bind_rows(agg, agg2)
  comb$Sttn_Nm = na.omit(unique(comb$Sttn_Nm))
  comb$continent = na.omit(unique(comb$continent))
  return(comb)
}
combinedFun = function(x){
  a = readFiles(x)
  b = obsFun(a)
  return(b)
}
################################################################################
##Parallel. 
################################################################################
cFun = function(f){
  file = paste0(path, f, ".csv")
  file = fread(file)
  file$Sttn_Nm = f
  file$continent = continent$continent[match(file$Sttn_Nm[1], continent$Sttn_Nm)]
  df = file
  mx = max(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  mn = min(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  all = merge(dt, df, all.x=TRUE)
  disc = all[Date<=mx&Date>=mn]
  disc$month = lubridate::month(disc$Date)
  disc$year = lubridate::year(disc$Date)
  missing = disc[disc$Q<0|is.na(disc$Q)]
  observed = disc[disc$Q>=0&!is.na(disc$Q)]
  total=disc
  total = total[order(total$Date)]
  total$missed=FALSE
  total$missed=fifelse(total$Q<0|is.na(total$Q), TRUE,total$missed)
  
  ddt = total[ , .(start = .I[1], end = .I[.N]), by = .(missed,rleid(missed))][, rleid := NULL][]
  ddt$startDate=total$Date[ddt$start]
  ddt$endDate=total$Date[ddt$end]
  ddt$duration=(ddt$end-ddt$start)+1
  
  agg = observed[,.N,by=list(year,month, Sttn_Nm,continent)]
  agg$type = 'observed'
  agg2 = missing[,.N,by=list(year,month, Sttn_Nm, continent)]
  agg2$type = 'missed'
  comb = bind_rows(agg, agg2)
  comb$Sttn_Nm = na.omit(unique(comb$Sttn_Nm))
  comb$continent = na.omit(unique(comb$continent))
  #return(comb)
  return(list(comb,ddt))
}


library(parallel)
clust = makeCluster(6)
clusterExport(clust, c('path', 'continent', 'shp', 'cFun', 'dt'))
clusterEvalQ(clust, c(library(data.table), library(dplyr)))
st=Sys.time()
openFiles = parLapply(clust,shp$Sttn_Nm,cFun)
ryan=openFiles
stopCluster(clust)
en=Sys.time()
en-st

counts=list()
duration=list()
for(i in 1:length(openFiles)){
  ls=openFiles[[i]]
  counts[[i]]=ls[[1]]
  dur=ls[[2]]
  dur$continent=ls[[1]]$continent[1]
  dur$Sttn_Nm=ls[[1]]$Sttn_Nm[1]
  duration[[i]]=dur
}
openFiles = counts
################################################################################
##Add agency to record.
################################################################################
for(i in 1:nrow(shp)){
  print(i)
  d=openFiles[[i]]
  if(any(is.na(d$continent))){break}
}

addAgency = function(x){
  x$agency = gsub('^.*\\_','', x$Sttn_Nm)
  return(x)
}
openFiles = lapply(openFiles, addAgency)
################################################################################
##t test
################################################################################
unSites=rbindlist(openFiles)[,length(unique(Sttn_Nm)),by=continent]
out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,continent,year)]
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year,continent)]
agg$sites=unSites$V1[match(agg$continent,unSites$continent)]
agg$perc = (agg$V1/agg$sites)
agg$perc = agg$perc*100
##%complete and proportion of operating gauges
out = rbindlist(openFiles)[,list(sum(N)), by=list(continent,year, type)]#[!is.na(continent)]
miss = out[type=='missed']
obs = out[type=='observed']
# comb = merge(obs, miss, by=c('continent','year'))
comb = full_join(obs, miss, by=c('continent','year'))
comb$V1.x = fifelse(is.na(comb$V1.x), 0, comb$V1.x)
comb$V1.y = fifelse(is.na(comb$V1.y), 0, comb$V1.y)
comb$total = comb$V1.x+comb$V1.y
comb$perc = (comb$V1.x/(comb$V1.y+comb$V1.x))*100

tFun = function(cont, years){
  chaos = comb[continent==cont&year>=years[1]&year<=years[2]]
  normal = comb[continent==cont&year%!in%chaos$year]
  t.test(chaos$perc, normal$perc)
}
##two sample, one tailed with 'less'.
tFun = function(cont, years){
  chaos = comb[continent==cont&year>=years[1]&year<=years[2]]
  normal = comb[continent==cont&year%!in%chaos$year]
  t.test(chaos$perc, normal$perc,alternative='less')
}
tFun('Africa', c(1939,1945))
tFun('Africa',c(1988,1991))
tFun('Asia',c(1939,1945))
tFun('Asia',c(1988,1991))
tFun('Europe',c(1914,1918))
tFun('Europe',c(1936,1945))
tFun('Europe',c(1988,1991))
tFun('South America', c(1988,1991))
#Note the 'chaotic' period is higher than normal for below.
tFun('North America', c(1929,1939))
tFun('South America',c(1929,1939))
tFun('Europe',c(1929,1945))

################################################################################
##Duration.
################################################################################
out=rbindlist(duration)
out = out[out$missed==TRUE]
##Median length gauges are offline. 
median(out$duration)
##Median times gauges go offline total and by continent.
median(out[,.N,by=list(Sttn_Nm,continent)]$N)
out[,.N,by=list(Sttn_Nm,continent)][,median(N),by=continent]
################################################################################
##Map final year. 
################################################################################
out = rbindlist(openFiles)[type=='observed',max(year),by=Sttn_Nm]
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
world = ne_countries(scale = "small", returnclass="sf")
world = world[world$sovereignt!="Antarctica",]
st_crs(shp) = st_crs(world)
#shp$year = output$max[match(shp$Sttn_Nm, output$Sttn_Nm)]
'%!in%' <- function(x,y)!('%in%'(x,y))
shp$year = out$V1[match(shp$Sttn_Nm, out$Sttn_Nm)]
shp = shp[!is.na(shp$year),]
shp = shp%>%distinct(Sttn_Nm, .keep_all=TRUE)
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
library(scales)
mn = 1980
mx = 2020
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(mn, mx))
pal <- wes_palette("Zissou1",type = "discrete")
pal <- colorRampPalette(pal)
sc1 <- scale_colour_gradientn(colours = pal(100), limits=c(mn, mx))


filt = shp
filt$Qcap = filt$year
filt$Qcap = ifelse(filt$Qcap<mn, mn, filt$Qcap)
filt$Qcap = ifelse(filt$Qcap>mx, mx, filt$Qcap)
set.seed(7)
rnd = sample(nrow(filt), nrow(filt))
filt = filt[rnd,]#[order(filt$Qcap),]
filt$size = 0.005

filt$endYear = cut(filt$year, breaks=c(1900,1980, 1990,2000,2010,2020, 2022))

map=ggplot(world)+
  geom_sf(color="black", fill = "black")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank(),
        legend.title =element_text(size=11),
        legend.text=element_text(size=9))+
  geom_sf(data = filt, aes(color=Qcap), pch=19, alpha = 0.9, size=0.05)+
  labs(color="Last year on record")+
  scale_color_distiller(palette='RdYlBu', direction = 1)+
  coord_sf(crs ="+proj=wag5")
map
ggsave("path\\out\\Figure2_map.png",map,
       dpi=1000,units="in",width=18,height=8)
################################################################################
##Gauge availability across all gauges and all GRDC gauges.
################################################################################
grdcFun = function(f){
  file = paste0(path, f, ".csv")
  file = fread(file)
  file$Sttn_Nm = f
  df = file
  mx = max(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  mn = min(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  all = merge(dt, df, all.x=TRUE)
  disc = all[Date<=mx&Date>=mn]
  disc$month = lubridate::month(disc$Date)
  disc$year = lubridate::year(disc$Date)
  missing = disc[disc$Q<0|is.na(disc$Q)]
  observed = disc[disc$Q>=0&!is.na(disc$Q)]
  total=disc
  total = total[order(total$Date)]
  total$missed=FALSE
  total$missed=fifelse(total$Q<0|is.na(total$Q), TRUE,total$missed)
  
  ddt = total[ , .(start = .I[1], end = .I[.N]), by = .(missed,rleid(missed))][, rleid := NULL][]
  ddt$startDate=total$Date[ddt$start]
  ddt$endDate=total$Date[ddt$end]
  ddt$duration=(ddt$end-ddt$start)+1
  
  agg = observed[,.N,by=list(year,month, Sttn_Nm)]
  agg$type = 'observed'
  agg2 = missing[,.N,by=list(year,month, Sttn_Nm)]
  agg2$type = 'missed'
  comb = bind_rows(agg, agg2)
  comb$Sttn_Nm = na.omit(unique(comb$Sttn_Nm))
  #return(comb)
  return(list(comb,ddt))
}


library(parallel)
grdcFiles = list.files(path, pattern='GRDC')
grdcFiles = gsub('.csv', '', grdcFiles)
clust = makeCluster(6)
clusterExport(clust, c('path', 'continent', 'grdcFiles', 'grdcFun', 'dt'))
clusterEvalQ(clust, c(library(data.table), library(dplyr)))
st=Sys.time()
grdcFiles = parLapply(clust,grdcFiles,grdcFun)
grdc=grdcFiles
stopCluster(clust)
en=Sys.time()
en-st

counts=list()
duration=list()
for(i in 1:length(grdcFiles)){
  ls=grdcFiles[[i]]
  counts[[i]]=ls[[1]]
  dur=ls[[2]]
  dur$continent=ls[[1]]$continent[1]
  dur$Sttn_Nm=ls[[1]]$Sttn_Nm[1]
  duration[[i]]=dur
}
grdcFiles = counts


out = rbindlist(grdcFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,year)]
tot = out[,sum(V1),by=list(year)]
tot$method='Daily observations'
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year)]
all = out[,length(unique(Sttn_Nm)),by=list(year)]
agg$method='N operating gauges'
comb = bind_rows(agg, tot)
grdcLines=comb

out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,continent,year)]
tot = out[,sum(V1),by=list(continent, year)]
tot$method='Daily observations'
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year,continent)]
all = out[,length(unique(Sttn_Nm)),by=list(year,continent)]
agg$method='N operating gauges'
comb = bind_rows(agg, tot)

g=ggplot(comb) + 
  geom_bar(aes(fill=continent, y=V1, x=year, color=continent),position="stack", stat="identity", width=1)+
  facet_wrap(~method, scales="free",
             strip.position="left")+
  ylab(NULL)+
  xlab(NULL)+
  scale_fill_viridis_d(option="D", direction=-1)+
  scale_color_viridis_d(option="D", direction=-1)+
  coord_cartesian(xlim=c(1900,2021))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  theme_classic()+theme(legend.title = element_blank(),
                        legend.position = "top",
                        axis.title=element_text(size=12, color="black"),
                        axis.text=element_text(size=11, color="black"),
                        legend.text=element_text(size=11, color="black"),
                        strip.background = element_blank(),
                        strip.placement = "outside",
                        strip.text = element_text(size=12, color="black"))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  geom_line(data=grdcLines,aes(x=year,y=V1),col='black')
g

ggsave("path\\out\\Figure2_b.pdf",g,
       dpi=1000,units="in",width=7,height=2.5)
################################################################################
##Gauge record completeness and proportion of gauges. 
###############################################################################
unSites=rbindlist(openFiles)[,length(unique(Sttn_Nm)),by=continent]
out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,continent,year)]
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year,continent)]
agg$sites=unSites$V1[match(agg$continent,unSites$continent)]
agg$perc = (agg$V1/agg$sites)
agg$perc = agg$perc*100
##%complete and proportion of operating gauges
out = rbindlist(openFiles)[,list(sum(N)), by=list(continent,year, type)]#[!is.na(continent)]
miss = out[type=='missed']
obs = out[type=='observed']
# comb = merge(obs, miss, by=c('continent','year'))
comb = full_join(obs, miss, by=c('continent','year'))
comb$V1.x = fifelse(is.na(comb$V1.x), 0, comb$V1.x)
comb$V1.y = fifelse(is.na(comb$V1.y), 0, comb$V1.y)
comb$total = comb$V1.x+comb$V1.y
comb$perc = (comb$V1.x/(comb$V1.y+comb$V1.x))*100
completeness=comb
completeness$type='Gauge record completeness (%)'
agg$type = 'Proportion of operating gauges (%)'
dual = bind_rows(completeness, agg)
dualPlot=ggplot(data=NULL)+ 
  geom_line(data=dual, aes(lty=type, y=perc, x=year,color=continent))+
  ylab('Percent (%)')+
  xlab(NULL)+
  facet_wrap(~continent)+
  scale_color_viridis_d(option="D",direction=-1)+
  coord_cartesian(xlim=c(1900,2021),ylim=c(0,100))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,25,50,75,100))+
  scale_x_continuous(expand = c(0, 0))+
  theme_classic()+theme(legend.title = element_blank(),
                        legend.position = "top",
                        axis.title=element_text(size=12, color='black'),
                        axis.text=element_text(size=11, color='black'),
                        legend.text=element_text(size=11, color='black'),
                        strip.background = element_blank(),
                        strip.placement = "outside",
                        strip.text = element_text(size=12, color='black'),
                        plot.margin = margin(10, 20, 10, 10),
                        panel.grid.major = element_line(color = "grey80", size=0.5),
                        panel.grid.minor = element_line(color = "grey80", size=0.4),
                        panel.border = element_rect(colour='black', fill=NA)
  )+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))
dualPlot
ggsave("path\\out\\Figure2_c.pdf",dualPlot,
       dpi=1000,units="in",width=6.9,height=4)
##################################################################################
##Stats for paper. 
##################################################################################
##Europe and Africa decline
unSites=rbindlist(openFiles)[,length(unique(Sttn_Nm)),by=continent]
out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,continent,year)]
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year,continent)]
agg$sites=unSites$V1[match(agg$continent,unSites$continent)]
agg$perc = (agg$V1/agg$sites)
agg$perc = agg$perc*100
(agg[year==2015&continent=='Africa']$perc-agg[year==1980&continent=='Africa']$perc)/agg[year==1980&continent=='Africa']$perc
(agg[year==2015&continent=='Europe']$perc-agg[year==1980&continent=='Europe']$perc)/agg[year==1980&continent=='Europe']$perc

##Gauge record completeness overall
out = rbindlist(openFiles)[,list(sum(N)), by=list(continent,type)]
miss = out[type=='missed']
obs = out[type=='observed']
comb = full_join(obs, miss, by=c('continent'))
comb$V1.x = fifelse(is.na(comb$V1.x), 0, comb$V1.x)
comb$V1.y = fifelse(is.na(comb$V1.y), 0, comb$V1.y)
comb$total = comb$V1.x+comb$V1.y
comb$perc = (comb$V1.x/(comb$V1.y+comb$V1.x))*100
comb

out = rbindlist(openFiles)[,list(sum(N)), by=list(type)]#[!is.na(continent)]
miss = out[type=='missed']
obs = out[type=='observed']
(obs$V1/(miss$V1+obs$V1))*100

##Russia and China
library(raster)
border=getData('GADM',country='Russia',level=0)
border=st_as_sf(border)
as = continent[continent$continent=='Asia',]
st_crs(as) = st_crs(border)
sf::sf_use_s2(FALSE)
sub = st_join(as,border,join=st_intersects)
sub=sub[!is.na(sub$NAME_0),]
Russia=sub
Russia$country='Russia'
border=getData('GADM',country='China',level=0)
border=st_as_sf(border)
st_crs(as) = st_crs(border)
sf::sf_use_s2(FALSE)
sub = st_join(as,border,join=st_intersects)
China=sub[!is.na(sub$NAME_0),]
China$country='China'
CR=bind_rows(China, Russia)

##% Russian and Chinese gauges.
nrow(CR)/nrow(as)

##Last year on record for each country.
out = rbindlist(openFiles)[,list(sum(N)), by=list(type, year,Sttn_Nm)]#[!is.na(continent)]
out = out[out$Sttn_Nm%in%CR$Sttn_Nm,]
out$country=CR$country[match(out$Sttn_Nm,CR$Sttn_Nm)]
out[,max(year),by=country]
################################################################################
##latency. 
################################################################################
filledRecords=list.files('E:/research/RatingCurveAnalysis/FilledRecords/allobs_wFlagsV6/', full.names=TRUE)
readFilled = function(f){
  df = fread(f)
  df = df[Q<0|is.na(Q)]
  if(nrow(df)==0){return(NULL)}
  df = df[RC>0&!is.na(RC)]
  if(nrow(df)==0){return(NULL)}
  df$year=lubridate::year(df$Date)
  agg = df[,.N,by=year]
  agg$Sttn_Nm = gsub('E:/research/RatingCurveAnalysis/FilledRecords/allobs_wFlagsV6/', '', f)
  agg$Sttn_Nm = gsub('.csv', '', agg$Sttn_Nm)
  return(agg)
}
filledRecordsList=lapply(filledRecords, readFilled)
out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,year)]
agg = out[V1>=335&year>=2020]
filledOut = rbindlist(filledRecordsList)[,sum(N),by=list(Sttn_Nm,year)]
#filledOut = filledOut[Sttn_Nm%in%agg$Sttn_Nm&year>=2019]
filledOut = filledOut[,sum(V1),by=year]
sum(filledOut$V1)/451037#length(unique(agg$Sttn_Nm))
sum(filledOut[year>=2020]$V1)/451037

##Extended number of months. 
out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,year)]
agg = out[V1>=335&year>=2015]
filledOut = rbindlist(filledRecordsList)[,sum(N),by=list(Sttn_Nm,year)]
filledOut = filledOut[Sttn_Nm%in%agg$Sttn_Nm&year>=2015]
filledOut = filledOut[,sum(V1),by=year]
sum(filledOut$V1)/length(unique(agg$Sttn_Nm))











shp = shp%>%distinct(Sttn_Nm, .keep_all = TRUE)
dt = data.table(Date = seq.Date(as.Date("1700-01-01"), 
                                as.Date("2022-12-31"),1))
################################################################################
##Assign gauges to continents. 
################################################################################
library(purrr)
shp$Station_Num = shp$Statin_Nm
##list of dataframes of each continent. 
##split arcitcnet into Europe and Asia. 
arcticnet = shp[shp$agency=='arcticnet',]
arcticnet = arcticnet%>%
  mutate(long = unlist(map(arcticnet$geometry,1)),
         lat = unlist(map(arcticnet$geometry,2)))
arc_eu = arcticnet[arcticnet$long<58,]
arc_as = arcticnet[arcticnet$long>=58,]
ggplot(data=NULL)+geom_sf(data=arc_eu, col='blue')+
  geom_sf(data=arc_as, col='red')

##Europe. 
eu = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="6",]
eu = bind_rows(eu, shp[shp$agency=="AFD",], shp[shp$Sttn_Nm%in%arc_eu$Sttn_Nm,])
nrow(eu)/nrow(shp)
eu$continent = "Europe"

##Asia. 
as = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="2",]
as = bind_rows(as, shp[shp$agency=="CHP"|shp$agency=="RID"|shp$Sttn_Nm%in%arc_as$Sttn_Nm|shp$agency=="MLIT"|shp$agency=="IWRIS",])
nrow(as)/nrow(shp)
as$continent = "Asia"

##Oceania. 
au = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="5",]
au = bind_rows(au, shp[shp$agency=="BOM",])
nrow(au)/nrow(shp)
au$continent = "Oceania"

##Africa. 
af = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="1",]
nrow(af)/nrow(shp)
af$continent = "Africa"

##South America.
sa = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="3",]
sa = bind_rows(sa, shp[shp$agency=="CCRR"|shp$agency=="ANA",])
nrow(sa)/nrow(shp)
sa$continent = "South America"

##North America
na = shp[shp$agency=="GRDC"&substr(shp$Station_Num, 1,1)=="4",]
na = bind_rows(na, shp[shp$agency=="USGS"|shp$agency=="HYDAT",])
nrow(na)/nrow(shp)
na$continent = "North America"
continent = bind_rows(na, sa, af, au, as, eu)
rm(af)
rm(as)
rm(au)
rm(eu)
rm(na)
rm(sa)
rm(files)
rm(arcticnet)
rm(arc_as)
rm(arc_eu)
rm(all)
################################################################################
##Functions to open and read in gauge records. 
################################################################################
path = "path\\to\\gauge\\data\\"
files = list.files(path, full.names=TRUE)

##Read in all gage records to a list. 
readFiles = function(f){
  file = paste0(path, f, ".csv")
  file = fread(file)
  file$Sttn_Nm = f
  file$continent = continent$continent[match(file$Sttn_Nm[1], continent$Sttn_Nm)]
  return(file)
}

##Split gage records into observations and missing lists. 
obsFun = function(f){
  df = f
  mx = max(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  mn = min(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  df = df%>%distinct(Date,.keep_all=TRUE)
  all = merge(dt, df, all.x=TRUE)
  disc = all[Date<=mx&Date>=mn]
  disc$month = lubridate::month(disc$Date)
  disc$year = lubridate::year(disc$Date)
  missing = disc[disc$Q<0|is.na(disc$Q)]
  observed = disc[disc$Q>=0&!is.na(disc$Q)]
  ##Added month. 
  agg = observed[,.N,by=list(year,month, Sttn_Nm,continent)]
  agg$type = 'observed'
  ##Added month. 
  agg2 = missing[,.N,by=list(year,month, Sttn_Nm, continent)]
  agg2$type = 'missed'
  comb = bind_rows(agg, agg2)
  comb$Sttn_Nm = na.omit(unique(comb$Sttn_Nm))
  comb$continent = na.omit(unique(comb$continent))
  return(comb)
}
combinedFun = function(x){
  a = readFiles(x)
  b = obsFun(a)
  return(b)
}
################################################################################
##Parallel. 
################################################################################
cFun = function(f){
  file = paste0(path, f, ".csv")
  file = fread(file)
  file$Sttn_Nm = f
  file$continent = continent$continent[match(file$Sttn_Nm[1], continent$Sttn_Nm)]
  df = file
  mx = max(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  mn = min(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  all = merge(dt, df, all.x=TRUE)
  disc = all[Date<=mx&Date>=mn]
  disc$month = lubridate::month(disc$Date)
  disc$year = lubridate::year(disc$Date)
  missing = disc[disc$Q<0|is.na(disc$Q)]
  observed = disc[disc$Q>=0&!is.na(disc$Q)]
  total=disc
  total = total[order(total$Date)]
  total$missed=FALSE
  total$missed=fifelse(total$Q<0|is.na(total$Q), TRUE,total$missed)
  
  ddt = total[ , .(start = .I[1], end = .I[.N]), by = .(missed,rleid(missed))][, rleid := NULL][]
  ddt$startDate=total$Date[ddt$start]
  ddt$endDate=total$Date[ddt$end]
  ddt$duration=(ddt$end-ddt$start)+1
  
  agg = observed[,.N,by=list(year,month, Sttn_Nm,continent)]
  agg$type = 'observed'
  agg2 = missing[,.N,by=list(year,month, Sttn_Nm, continent)]
  agg2$type = 'missed'
  comb = bind_rows(agg, agg2)
  comb$Sttn_Nm = na.omit(unique(comb$Sttn_Nm))
  comb$continent = na.omit(unique(comb$continent))
  #return(comb)
  return(list(comb,ddt))
}


library(parallel)
clust = makeCluster(6)
clusterExport(clust, c('path', 'continent', 'shp', 'cFun', 'dt'))
clusterEvalQ(clust, c(library(data.table), library(dplyr)))
st=Sys.time()
openFiles = parLapply(clust,shp$Sttn_Nm,cFun)
ryan=openFiles
stopCluster(clust)
en=Sys.time()
en-st

counts=list()
duration=list()
for(i in 1:length(openFiles)){
  ls=openFiles[[i]]
  counts[[i]]=ls[[1]]
  dur=ls[[2]]
  dur$continent=ls[[1]]$continent[1]
  dur$Sttn_Nm=ls[[1]]$Sttn_Nm[1]
  duration[[i]]=dur
}
openFiles = counts
################################################################################
##Add agency to record.
################################################################################
for(i in 1:nrow(shp)){
  print(i)
  d=openFiles[[i]]
  if(any(is.na(d$continent))){break}
}

addAgency = function(x){
  x$agency = gsub('^.*\\_','', x$Sttn_Nm)
  return(x)
}
openFiles = lapply(openFiles, addAgency)
################################################################################
##t test
################################################################################
tFun = function(cont, years){
  chaos = comb[continent==cont&year>=years[1]&year<=years[2]]
  normal = comb[continent==cont&year%!in%chaos$year]
  t.test(chaos$perc, normal$perc)
}
tFun('Africa', c(1939,1945))
tFun('Africa',c(1988,1991))
tFun('Asia',c(1939,1945))
tFun('Asia',c(1988,1991))
tFun('Europe',c(1914,1918))
tFun('Europe',c(1936,1945))
tFun('Europe',c(1988,1991))
tFun('South America', c(1988,1991))
#Note the 'chaotic' period is higher than normal for below.
tFun('North America', c(1988,1991))
tFun('Oceania', c(1988,1991))
################################################################################
##Duration.
################################################################################
out=rbindlist(duration)
out = out[out$missed==TRUE]
##Median length gauges are offline. 
median(out$duration)
##Median times gauges go offline total and by continent.
median(out[,.N,by=list(Sttn_Nm,continent)]$N)
out[,.N,by=list(Sttn_Nm,continent)][,median(N),by=continent]
################################################################################
##Map final year. 
################################################################################
out = rbindlist(openFiles)[type=='observed',max(year),by=Sttn_Nm]
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
world = ne_countries(scale = "small", returnclass="sf")
world = world[world$sovereignt!="Antarctica",]
st_crs(shp) = st_crs(world)
#shp$year = output$max[match(shp$Sttn_Nm, output$Sttn_Nm)]
'%!in%' <- function(x,y)!('%in%'(x,y))
shp$year = out$V1[match(shp$Sttn_Nm, out$Sttn_Nm)]
shp = shp[!is.na(shp$year),]
shp = shp%>%distinct(Sttn_Nm, .keep_all=TRUE)
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
library(scales)
mn = 1980
mx = 2020
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(mn, mx))
pal <- wes_palette("Zissou1",type = "discrete")
pal <- colorRampPalette(pal)
sc1 <- scale_colour_gradientn(colours = pal(100), limits=c(mn, mx))


filt = shp
filt$Qcap = filt$year
filt$Qcap = ifelse(filt$Qcap<mn, mn, filt$Qcap)
filt$Qcap = ifelse(filt$Qcap>mx, mx, filt$Qcap)
set.seed(7)
rnd = sample(nrow(filt), nrow(filt))
filt = filt[rnd,]#[order(filt$Qcap),]
filt$size = 0.005

filt$endYear = cut(filt$year, breaks=c(1900,1980, 1990,2000,2010,2020, 2022))

map=ggplot(world)+
  geom_sf(color="black", fill = "black")+
  coord_sf(crs = "+proj=wag5")+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank(),
        legend.title =element_text(size=11),
        legend.text=element_text(size=9))+
  geom_sf(data = filt, aes(color=Qcap), pch=19, alpha = 0.9, size=0.05)+
  labs(color="Last year on record")+
  scale_color_distiller(palette='RdYlBu', direction = 1)+
  coord_sf(crs ="+proj=wag5")
map
ggsave("path\\out\\Figure2_map.png",map,
       dpi=1000,units="in",width=18,height=8)
################################################################################
##Gauge availability across all gauges and all GRDC gauges.
################################################################################
grdcFun = function(f){
  file = paste0(path, f, ".csv")
  file = fread(file)
  file$Sttn_Nm = f
  df = file
  mx = max(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  mn = min(df$Date[!is.na(df$Q)&df$Q>=0], na.rm=TRUE)
  all = merge(dt, df, all.x=TRUE)
  disc = all[Date<=mx&Date>=mn]
  disc$month = lubridate::month(disc$Date)
  disc$year = lubridate::year(disc$Date)
  missing = disc[disc$Q<0|is.na(disc$Q)]
  observed = disc[disc$Q>=0&!is.na(disc$Q)]
  total=disc
  total = total[order(total$Date)]
  total$missed=FALSE
  total$missed=fifelse(total$Q<0|is.na(total$Q), TRUE,total$missed)
  
  ddt = total[ , .(start = .I[1], end = .I[.N]), by = .(missed,rleid(missed))][, rleid := NULL][]
  ddt$startDate=total$Date[ddt$start]
  ddt$endDate=total$Date[ddt$end]
  ddt$duration=(ddt$end-ddt$start)+1
  
  agg = observed[,.N,by=list(year,month, Sttn_Nm)]
  agg$type = 'observed'
  agg2 = missing[,.N,by=list(year,month, Sttn_Nm)]
  agg2$type = 'missed'
  comb = bind_rows(agg, agg2)
  comb$Sttn_Nm = na.omit(unique(comb$Sttn_Nm))
  #return(comb)
  return(list(comb,ddt))
}


library(parallel)
grdcFiles = list.files(path, pattern='GRDC')
grdcFiles = gsub('.csv', '', grdcFiles)
clust = makeCluster(6)
clusterExport(clust, c('path', 'continent', 'grdcFiles', 'grdcFun', 'dt'))
clusterEvalQ(clust, c(library(data.table), library(dplyr)))
st=Sys.time()
grdcFiles = parLapply(clust,grdcFiles,grdcFun)
grdc=grdcFiles
stopCluster(clust)
en=Sys.time()
en-st

counts=list()
duration=list()
for(i in 1:length(grdcFiles)){
  ls=grdcFiles[[i]]
  counts[[i]]=ls[[1]]
  dur=ls[[2]]
  dur$continent=ls[[1]]$continent[1]
  dur$Sttn_Nm=ls[[1]]$Sttn_Nm[1]
  duration[[i]]=dur
}
grdcFiles = counts


out = rbindlist(grdcFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,year)]
tot = out[,sum(V1),by=list(year)]
tot$method='Daily observations'
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year)]
all = out[,length(unique(Sttn_Nm)),by=list(year)]
agg$method='N operating gauges'
comb = bind_rows(agg, tot)
grdcLines=comb

out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,continent,year)]
tot = out[,sum(V1),by=list(continent, year)]
tot$method='Daily observations'
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year,continent)]
all = out[,length(unique(Sttn_Nm)),by=list(year,continent)]
agg$method='N operating gauges'
comb = bind_rows(agg, tot)

g=ggplot(comb) + 
  geom_bar(aes(fill=continent, y=V1, x=year, color=continent),position="stack", stat="identity", width=1)+
  facet_wrap(~method, scales="free",
             strip.position="left")+
  ylab(NULL)+
  xlab(NULL)+
  scale_fill_viridis_d(option="D", direction=-1)+
  scale_color_viridis_d(option="D", direction=-1)+
  coord_cartesian(xlim=c(1900,2021))+
  scale_y_continuous(expand = c(0, 0))+
  scale_x_continuous(expand = c(0, 0))+
  theme_classic()+theme(legend.title = element_blank(),
                        legend.position = "top",
                        axis.title=element_text(size=12, color="black"),
                        axis.text=element_text(size=11, color="black"),
                        legend.text=element_text(size=11, color="black"),
                        strip.background = element_blank(),
                        strip.placement = "outside",
                        strip.text = element_text(size=12, color="black"))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  geom_line(data=grdcLines,aes(x=year,y=V1),col='black')
g

ggsave("path\\out\\Figure2_b.pdf",g,
       dpi=1000,units="in",width=7,height=2.5)
################################################################################
##Gauge record completeness and proportion of gauges. 
###############################################################################
unSites=rbindlist(openFiles)[,length(unique(Sttn_Nm)),by=continent]
out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,continent,year)]
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year,continent)]
agg$sites=unSites$V1[match(agg$continent,unSites$continent)]
agg$perc = (agg$V1/agg$sites)
agg$perc = agg$perc*100
##%complete and proportion of operating gauges
out = rbindlist(openFiles)[,list(sum(N)), by=list(continent,year, type)]#[!is.na(continent)]
miss = out[type=='missed']
obs = out[type=='observed']
# comb = merge(obs, miss, by=c('continent','year'))
comb = full_join(obs, miss, by=c('continent','year'))
comb$V1.x = fifelse(is.na(comb$V1.x), 0, comb$V1.x)
comb$V1.y = fifelse(is.na(comb$V1.y), 0, comb$V1.y)
comb$total = comb$V1.x+comb$V1.y
comb$perc = (comb$V1.x/(comb$V1.y+comb$V1.x))*100
completeness=comb
completeness$type='Gauge record completeness (%)'
agg$type = 'Proportion of operating gauges (%)'
dual = bind_rows(completeness, agg)
dualPlot=ggplot(data=NULL)+ 
  geom_line(data=dual, aes(lty=type, y=perc, x=year,color=continent))+
  ylab('Percent (%)')+
  xlab(NULL)+
  facet_wrap(~continent)+
  scale_color_viridis_d(option="D",direction=-1)+
  coord_cartesian(xlim=c(1900,2021),ylim=c(0,100))+
  scale_y_continuous(expand = c(0, 0),breaks=c(0,25,50,75,100))+
  scale_x_continuous(expand = c(0, 0))+
  theme_classic()+theme(legend.title = element_blank(),
                        legend.position = "top",
                        axis.title=element_text(size=12, color='black'),
                        axis.text=element_text(size=11, color='black'),
                        legend.text=element_text(size=11, color='black'),
                        strip.background = element_blank(),
                        strip.placement = "outside",
                        strip.text = element_text(size=12, color='black'),
                        plot.margin = margin(10, 20, 10, 10),
                        panel.grid.major = element_line(color = "grey80", size=0.5),
                        panel.grid.minor = element_line(color = "grey80", size=0.4),
                        panel.border = element_rect(colour='black', fill=NA)
  )+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))
dualPlot
ggsave("path\\out\\Figure2_c.pdf",dualPlot,
       dpi=1000,units="in",width=6.9,height=4)
##################################################################################
##Stats for paper. 
##################################################################################
##Europe and Africa decline
unSites=rbindlist(openFiles)[,length(unique(Sttn_Nm)),by=continent]
out = rbindlist(openFiles)[type=='observed',sum(N),by=list(type,Sttn_Nm,continent,year)]
agg = out[V1>=335,length(unique(Sttn_Nm)),by=list(year,continent)]
agg$sites=unSites$V1[match(agg$continent,unSites$continent)]
agg$perc = (agg$V1/agg$sites)
agg$perc = agg$perc*100
(agg[year==2015&continent=='Africa']$perc-agg[year==1980&continent=='Africa']$perc)/agg[year==1980&continent=='Africa']$perc
(agg[year==2015&continent=='Europe']$perc-agg[year==1980&continent=='Europe']$perc)/agg[year==1980&continent=='Europe']$perc

##Gauge record completeness overall
out = rbindlist(openFiles)[,list(sum(N)), by=list(continent,type)]
miss = out[type=='missed']
obs = out[type=='observed']
comb = full_join(obs, miss, by=c('continent'))
comb$V1.x = fifelse(is.na(comb$V1.x), 0, comb$V1.x)
comb$V1.y = fifelse(is.na(comb$V1.y), 0, comb$V1.y)
comb$total = comb$V1.x+comb$V1.y
comb$perc = (comb$V1.x/(comb$V1.y+comb$V1.x))*100
comb

out = rbindlist(openFiles)[,list(sum(N)), by=list(type)]#[!is.na(continent)]
miss = out[type=='missed']
obs = out[type=='observed']
(obs$V1/(miss$V1+obs$V1))*100

##Russia and China
library(raster)
border=getData('GADM',country='Russia',level=0)
border=st_as_sf(border)
as = continent[continent$continent=='Asia',]
st_crs(as) = st_crs(border)
sf::sf_use_s2(FALSE)
sub = st_join(as,border,join=st_intersects)
sub=sub[!is.na(sub$NAME_0),]
Russia=sub
Russia$country='Russia'
border=getData('GADM',country='China',level=0)
border=st_as_sf(border)
st_crs(as) = st_crs(border)
sf::sf_use_s2(FALSE)
sub = st_join(as,border,join=st_intersects)
China=sub[!is.na(sub$NAME_0),]
China$country='China'
CR=bind_rows(China, Russia)

##% Russian and Chinese gauges.
nrow(CR)/nrow(as)

##Last year on record for each country.
out = rbindlist(openFiles)[,list(sum(N)), by=list(type, year,Sttn_Nm)]#[!is.na(continent)]
out = out[out$Sttn_Nm%in%CR$Sttn_Nm,]
out$country=CR$country[match(out$Sttn_Nm,CR$Sttn_Nm)]
out[,max(year),by=country]

