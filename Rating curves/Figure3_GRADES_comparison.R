##Author: Ryan Riggs
##Date: 9/12/2022
##comparing RC with GRADES.
############################################################################################
##Libraries.
############################################################################################
library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(ncdf4)
library(hydroGOF)
#############################################################################################
##Assign MERIT to each gage locations. 
#############################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))
gage_file = st_read("path\\to\\validQ_1984.shp")
gage_stats_vals = gage_file
gage_stats_vals = distinct(gage_stats_vals, gage_stats_vals$Sttn_Nm, .keep_all=TRUE)
viable = list.files('path/to/crossection/widths')
viable = gsub('Gauge__', '', viable)
viable = gsub('.csv', '', viable)
viable = gsub('grdc', 'GRDC', viable)

gage_stats_vals = gage_stats_vals[gage_stats_vals$Sttn_Nm%in%viable,]
ms = viable[viable%!in%gage_stats_vals$Sttn_Nm]


##Read in Merit. 
meritFiles= "path/to/MERIT/HYDRO/"
meritFiles = list.files(meritFiles, full.names=TRUE, ".shp")
meritFiles = meritFiles[grep("riv", meritFiles)]
meritFiles = meritFiles[!grepl("Copy", meritFiles)]
meritFiles = meritFiles[!grepl("lock", meritFiles)]

tab = list()
for(i in 1:length(meritFiles)){
print(i)
merit = st_read(meritFiles[i])
st_crs(gage_stats_vals) = st_crs(merit)
gage_dist = st_nearest_feature(gage_stats_vals, merit)
dist = st_distance(gage_stats_vals, merit[gage_dist,], by_element=TRUE)
gage_stats_vals$dist = dist
gage_stats_vals$COMID = merit$COMID[gage_dist]
tab[[i]] = gage_stats_vals
}
all = rbindlist(tab)
allFilt = all%>%group_by(Sttn_Nm)%>%filter(dist==min(dist, na.rm=TRUE))
fwrite(allFilt, "path\\out\\gageComidSample.csv")



##################################################################################
##Rating curve results. 
##################################################################################
##Read in files and filter to max nse value. 
files = list.files("path/to/ratingcurveparallelresults/", full.names=TRUE)
processing = function(i, n, distance, wd){
  x = fread(i)
  x = x[obs>=n&abs(flow)<distance&node_width>=wd]
  x$node_id_end = stringr::str_sub(x$node_id, -1)
  x = x[node_id_end=="1"]
  nd = x[x[,.I[which.max(kge)],]]
  return(nd)
}

out = list()
for(j in 1:length(files)){
  df = processing(files[j], 33, 10000, 120)
  df$node_id = as.character(df$node_id)
  df$Site_number = as.character(df$Site_number)
  out[[j]] = df
}
out = rbindlist(out)
out$Sttn_Nm = out$agency
out$Sttn_Nm = gsub("Gauge__", "", out$Sttn_Nm)
out$Sttn_Nm = gsub("grdc", "GRDC", out$Sttn_Nm)
stats = out#%>%select(-model, -Site_number, -node_id, -agency, -Sttn_Nm)
stats = sapply(stats, as.numeric)
stats = as.data.frame(stats)
summary(stats)
stats$Sttn_Nm = out$Sttn_Nm
stats$node_id = out$node_id
stats$model = out$model
out = stats
out$nse = as.numeric(out$nse)
rm = fread("path/to/redundant/removeTheseV5.csv")
out = out[out$Sttn_Nm%!in%rm$Sttn_Nm,]
##########################################################################
##GRADES Data. 
##########################################################################
#########################################################################################################################
allFilt=fread("path\\out\\gageComidSample.csv")
allFilt = allFilt[allFilt$Sttn_Nm%in%out$Sttn_Nm,]
# read in netCDF file:
ncIn = nc_open("E:\\research\\GRADES\\GRADES_Q_v01_pfaf_07_19790101_20131231.nc")
ncFiles = list.files("path\\to\\GRADES\\", full.names=TRUE)
ncFiles = ncFiles[grep("v01", ncFiles)]
idsAll = substr(allFilt$COMID, 1,1)
allFilt$begin = idsAll
ids = unique(substr(allFilt$COMID, 1,1))



##Processing function. 
processingGRADES = function(ncFile, comids){
ncIn = nc_open(ncFile)
allComid = ncvar_get(ncIn, "COMID")
roi = allComid%in%comids
start = 1
end = ncIn$var$Q$varsize[2]
j = 1
interval  = 50 
year = interval # assuming no leap years
output=list()
while (start < end){
  print(paste("chunk", j))
  # for last year: 
  if ((start+interval) > ncIn$var$Q$varsize[2]){
    interval = ncIn$var$Q$varsize[2] - start
  }
  nc = ncvar_get(ncIn, "Q", 
                 start=c(1, start), # starting index of netCDF to read in 
                 count=c(ncIn$var$Q$varsize[1], interval)) # ending index of netCDF to read in 
  nc = cbind(allComid, nc)
  timeSeries = seq(start, start +interval-1,1)
  nc_filt = nc[roi,]
  nc_filt = as.data.frame(nc_filt)
  colnames(nc_filt) = c("comid", timeSeries)
  tall = melt(nc_filt, id.var ="comid")
  output[[j]] = tall
  start = start+interval
  j = j+1
}
out7 = rbindlist(output)
return(out7)
}


tab = list()
for(k in 1:length(ids)){
  print(k)
  numb = ids[k]
  df = allFilt[allFilt$begin==numb,]
  nc = ncFiles[k]
  tab[[k]] = processingGRADES(nc, unique(df$COMID))
}
grades = rbindlist(tab)
grades = as.data.frame(grades)
grades$time = as.numeric(as.character(grades$variable))
grades$time = grades$time-1
start = as.Date("1979-01-01")
grades$Date = start+grades$time
grades$Q = as.numeric(grades$value)
grades$COMID = grades$comid
fwrite(grades, "path\\out\\subsetSampleV5_updated.csv")


###############################################################################
##Join to actual record. 
###############################################################################
library(hydroGOF)
gagePath = "path/to/gauge/data/"
grades = fread("path\\out\\subsetSampleV5.csv")
grades1 = fread("path\\out\\subsetSampleV5_updated.csv")
setkey(grades, COMID)
allFilt = fread("path\\out\\gageComidSample.csv")
allFilt = allFilt[allFilt$Sttn_Nm%in%out$Sttn_Nm,]

source("E:/research/2019_08_30_rivObs/git/src/Error_stats_functions.R")
validation = function(sim, obs){
  rrmse = RRMSE(sim, obs)
  nse = NSE(sim, obs)
  kge = KGE(sim, obs)
  nrmse = NRMSE(sim, obs)
  rbias = rBias(sim, obs)
  bias = mean(sim-obs)
  error = obs-sim
  rmse = sqrt(mean((error^2),na.rm=TRUE))
  df = data.frame(rrmse=rrmse, nse=nse, kge=kge, nrmse=nrmse, rbias = rbias, bias=bias, rmse=rmse)
  return(df)
}

gradesPerformance = list()
for(i in 1:nrow(allFilt)){
  sub = allFilt[i]
  print(i)
  gr = grades[.(allFilt$COMID[i])]
  gr$year = year(gr$Date)
  annual = gr[,mean(Q), by=year]
  annual = mean(annual$V1, na.rm=TRUE)
  file = paste0(gagePath, allFilt$Sttn_Nm[i], ".csv")
  gage = fread(file)
  gage_full = gage
  gage$Date = as.Date(gage$Date)
  gage = gage[!is.na(Q)&Q>=0]
  combined = merge(gage, gr, by = "Date")
  val = validation(combined$Q.y, combined$Q.x)
  val$Sttn_Nm = sub$Sttn_Nm
  val$mnQ = annual
  gradesPerformance[[i]] = val 
  
}
gradesOut = rbindlist(gradesPerformance)
gradesOut = gradesOut[gradesOut$Sttn_Nm%in%out$Sttn_Nm,]
stats=out

###########################################################################################################
##Join to hydrobasins level 3 tp assign aridity. 
###########################################################################################################
basins = st_read("E:\\research\\HydroAtlas\\BasinATLAS_v10_shp\\BasinATLAS_v10_lev03.shp")
st_crs(basins) = crs(gage_stats_vals)
sf::sf_use_s2(FALSE)
#basin_pts = st_join(filt, basins, join=st_within)
basin_pts = st_join(gage_stats_vals[gage_stats_vals$Sttn_Nm%in%out$Sttn_Nm,],basins,join=st_intersects)
basin_pts$aridity = NA
basin_pts$aridity[basin_pts$ari_ix_sav<50] = "Arid"
basin_pts$aridity#[basin_pts$ari_ix_sav>=20&basin_pts$ari_ix_sav<50] = "Semiarid"
basin_pts$aridity#[basin_pts$ari_ix_sav>=50&basin_pts$ari_ix_sav<65] = "Subhumid"
basin_pts$aridity[basin_pts$ari_ix_sav>=50] = "Humid"
missing = basin_pts[is.na(basin_pts$aridity),]
missingDist = basin_pts[st_nearest_feature(missing[1,], basin_pts[basin_pts$Sttn_Nm!=missing$Sttn_Nm,]),]
missing$HYBAS_ID = missingDist$HYBAS_ID
missing$aridity = missingDist$aridity
basin_pts = basin_pts[basin_pts$Sttn_Nm!=missing$Sttn_Nm,]
basin_pts = bind_rows(basin_pts, missing)
gradesOut$aridity = basin_pts$aridity[match(gradesOut$Sttn_Nm, basin_pts$Sttn_Nm)]
stats$aridity = basin_pts$aridity[match(stats$Sttn_Nm, basin_pts$Sttn_Nm)]
gradesOut = gradesOut[!is.na(kge)]
#stats = stats[stats$Sttn_Nm%in%gradesOut$Sttn_Nm,]

library(ggplot2)
rcDt = data.table(kge = stats$kge,
                  nse=stats$nse,
                  rrmse=stats$rrmse,
                  nrmse=stats$nrmse,
                  rbias=stats$rbias, 
                  model="RC",
                  aridity = stats$aridity)
rcDt_total = rcDt
rcDt_total$aridity='Total'
rcDt = bind_rows(rcDt, rcDt_total)
gradesDt = data.table(kge = gradesOut$kge,nse = gradesOut$nse,
                      rrmse = gradesOut$rrmse,
                      nrmse = gradesOut$nrmse,
                      rbias = gradesOut$rbias,
                      model="GRADES",
                      aridity=gradesOut$aridity)
gradesDt_total = gradesDt
gradesDt_total$aridity='Total'
gradesDt = bind_rows(gradesDt, gradesDt_total)

tall = bind_rows(rcDt, gradesDt)
mlt = melt(tall)
mlt$value = as.numeric(mlt$value)
mlt$variable = as.character(mlt$variable)
mlt$model = factor(mlt$model, levels=c('RC', 'GRADES'))



library(ggpubr)
pal = RColorBrewer::brewer.pal(3, 'Dark2')
pal = c(pal[2], pal[1], pal[3])
pal = c('#F21A00', '#3B9AB2', 'black')
wd = 0.75
alp = 1#0.5
nrmse=ggplot(mlt[variable=='nrmse'])+
  stat_ecdf(aes(x=value, color=aridity, lty=model), lwd=wd, alpha=alp)+
  theme_classic()+
  ylab('CDF')+
  xlab('NRMSE (%)')+
  scale_color_manual(values=pal)+
  coord_cartesian(xlim=c(0,300))+
  theme(legend.position = 'none', legend.title = element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11),
        legend.text=element_text(size=11), 
        panel.grid.major = element_line(color = "grey80", size=0.5),
        panel.grid.minor = element_line(color = "grey80", size=0.4),
        panel.border = element_rect(colour='black', fill=NA)
  )
nrmse



rbias=ggplot(mlt[variable=='rbias'])+
  stat_ecdf(aes(x=value, color=aridity, lty=model), lwd=wd, alpha=alp)+
  theme_classic()+
  ylab('CDF')+
  xlab('rBias (%)')+
  scale_color_manual(values=pal)+
  coord_cartesian(xlim=c(-100,100))+
  theme(legend.position = "none", legend.title = element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11),
        legend.text=element_text(size=11), 
        panel.grid.major = element_line(color = "grey80", size=0.5),
        panel.grid.minor = element_line(color = "grey80", size=0.4),
        panel.border = element_rect(colour='black', fill=NA)
  )
rbias

kge=ggplot(mlt[variable=='kge'])+
  stat_ecdf(aes(x=value, color=aridity, lty=model), lwd=wd, alpha=alp)+
  theme_classic()+
  ylab('CDF')+
  xlab('KGE')+
  scale_color_manual(values=pal)+
  coord_cartesian(xlim=c(-1,1))+
  theme(legend.position = "none", legend.title = element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11),
        legend.text=element_text(size=11), 
        panel.grid.major = element_line(color = "grey80", size=0.5),
        panel.grid.minor = element_line(color = "grey80", size=0.4),
        panel.border = element_rect(colour='black', fill=NA)
  )
kge

ggpubr::ggarrange(rbias,nrmse, kge, nrow=1, common.legend = TRUE)
#######################################################################
##Observations per gauges. 
#######################################################################
countNA = function(f){
  na = f[is.na(f)|f<=0]
  ct = length(na)
  return(ct)
}

path = "path\\to\\allobs_wFlagsV6\\"
files = list.files(path, full.names=TRUE)
sites = list.files(path)


dts = seq.Date(as.Date("1984-01-01"), as.Date(Sys.time()), by = "day")
dfDf = data.frame(Date=dts)
tab = list()
filledList=list()
supplemented = list()
extended=list()
for(i in 1:length(files)){
  print(i)
  q = fread(files[i])
  q$Date = as.Date(q$Date)
  gage_full = q
  mx=max(q$Date[q$Q>=0&!is.na(q$Q)])
  mn=min(q$Date[q$Q>=0&!is.na(q$Q)])
  #q = full_join(q, dfDf)
  q = q[Date>as.Date('1984-03-01')]
  lf = q[is.na(Q)|Q<0]
  lf = lf[!is.na(RC)]
  lf$obs = fifelse(lf$RC>0, "Filled", "NotFilled")
  #lf$obs = fifelse(!is.na(lf$RC), "Filled", "NotFilled")
  lf$Sttn_Nm = sites[i]
  tab[[i]] = lf
  supplemented[[i]] = lf[lf$Date<=mx&lf$Date>=mn]
  extended[[i]] = lf[lf$Date<mn|lf$Date>mx]
  
  ##GRADES Filled in. 
  q = fread(paste0('path/to/gauge/data/', sites[i]))
  q = q[Date>as.Date('1984-03-01')]
  cd = allFilt[allFilt$Sttn_Nm==gsub('.csv','', sites[i]),]
  cd = cd$COMID
  gr = grades[.(cd)]
  gr$grades = as.numeric(gr$Q)
  #lf = q[is.na(Q)|Q<0]
  comb_grades = right_join(q, gr, by='Date')
  comb_grades = comb_grades[!is.na(grades)]

  comb_grades = comb_grades[is.na(Q.x)|Q.x<0]
  comb_grades$obs = fifelse(comb_grades$grades>0, "Filled", "NotFilled")
  comb_grades$Sttn_Nm = gsub('.csv','', sites[i])
  filledList[[i]] = comb_grades
  
}
perc = rbindlist(tab,fill=TRUE)
#perc = perc[Date<as.Date('2021-08-27')]
perc$Sttn_Nm = gsub(".csv", "", perc$Sttn_Nm)
perc$Sttn_Nm = gsub('grdc', 'GRDC',perc$Sttn_Nm)
gage_file = st_read("path\\to\\allUpdated_endYear.shp")

perc$Sttn_Nm = gsub("grdc", "GRDC", perc$Sttn_Nm)
perc$agency = gage_file$agency[match(perc$Sttn_Nm, gage_file$Sttn_Nm)]
gage_file = gage_file[!is.na(gage_file$year),]
rm = fread("path\\to\\redundant\\removeTheseV5.csv")
'%!in%' <- function(x,y)!('%in%'(x,y))
gage_file = gage_file[gage_file$Sttn_Nm%!in%rm$Sttn_Nm,]
perc$Sttn_Nm = gsub('grdc', 'GRDC',perc$Sttn_Nm)
perc$aridity = basin_pts$aridity[match(perc$Sttn_Nm, basin_pts$Sttn_Nm)]
perc$year = lubridate::year(perc$Date)
notFilled = data.table(perc)[obs=='NotFilled',list(.N),by=list(aridity, year)]



colnames(notFilled) = c("aridity","year","missing")
aridRegions = data.table(perc)[obs=='Filled',list(.N),by=list(aridity)]
yrs = min(perc$year):max(perc$year)

combined = data.table(perc)[obs=='Filled',list(.N),by=list(obs, aridity, year)]
combined$aridity = factor(combined$aridity, levels=c("Arid","Humid", "Total"))
tot = combined[,sum(N), by=list(year)]
colnames(tot) = c('year', 'N')
tot$aridity='Total'
combined = bind_rows(combined, tot)
gages = data.table(basin_pts)[,.N, by=aridity]
gages = bind_rows(gages, data.table(aridity='Total', N=nrow(basin_pts)))
colnames(gages) = c('aridity', 'gages')
jnd = full_join(combined, gages)
jnd$perc = (jnd$N/jnd$gages)
jnd$model ='RC'
##Stats for paper. 
jnd[,mean(perc),by=list(aridity, model)]
jndRC = jnd

##GRADES Filled
gradesFilled = rbindlist(filledList, use.names=TRUE)
gradesFilled$aridity = basin_pts$aridity[match(gradesFilled$Sttn_Nm, basin_pts$Sttn_Nm)]
gradesFilled$year = lubridate::year(gradesFilled$Date)
#gradesFilled = gradesFilled[gradesFilled$Sttn_Nm%in%gradesOut$Sttn_Nm,]

combined = data.table(gradesFilled)[obs=='Filled',list(.N),by=list(obs, aridity, year)]


combined$aridity = factor(combined$aridity, levels=c("Arid","Humid", "Total"))
tot = combined[,sum(N), by=list(year)]
colnames(tot) = c('year', 'N')
tot$aridity='Total'
combined = bind_rows(combined, tot)
gages = data.table(basin_pts)[,.N, by=aridity]
gages = bind_rows(gages, data.table(aridity='Total', N=nrow(basin_pts)))
colnames(gages) = c('aridity', 'gages')
jndGRADES = full_join(combined, gages)
jndGRADES$perc = (jndGRADES$N/jndGRADES$gages)
jndGRADES$model='GRADES'
jnd = bind_rows(jndRC, jndGRADES)
jnd$model=factor(jnd$model, levels=c('RC','GRADES'))
percPlot = ggplot(jnd)+geom_line(aes(x=year, y=perc, color=aridity, lty=model),lwd=wd,alpha=alp)+
  theme_classic()+
  xlab("Year")+
  ylab("Mean N records supplemented per gauge")+
  coord_cartesian(xlim=c(1984, 2022))+
  scale_color_manual(values=pal)+
  scale_y_log10()+
  theme(legend.position = "none", legend.title = element_blank(),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11),
        legend.text=element_text(size=11), 
        panel.grid.major = element_line(color = "grey80", size=0.5),
        panel.grid.minor = element_line(color = "grey80", size=0.4),
        panel.border = element_rect(colour='black', fill=NA)
  )
percPlot
jnd[,mean(perc),by=list(aridity, model)]

cb = ggarrange(rbias, nrmse,kge, labels=c('b', 'c', 'd', 'e'), legend=NULL, nrow=1)
cb
p=ggarrange(percPlot,labels=c('a'),legend=NULL)
p
cb=ggarrange(p,cb,ncol=1)
cb
ggsave("path\\out\\Figure3b_e.pdf", cb, dpi=1000, 
       width=6.5, height=3.7)

##3.7
##supplemented vs extended. 
sup = rbindlist(supplemented,use.names=TRUE)[obs=='Filled']
ext = rbindlist(extended,use.names=TRUE)[obs=='Filled']

addAgency = function(x){
  x$agency = gsub('^.*\\_','', x$Sttn_Nm)
  return(x)
}

sup = addAgency(sup)
ext = addAgency(ext)

sup[,.N,by=agency]
ext[,.N,by=agency]

############################################################################
#################################################################################
##Results stats. 
#################################################################################
##By satellite timeseries. 
#################################################################################
#L5 only 
sub = perc[perc$Date<as.Date('1999-05-28'),]
sub$year = year(sub$Date)
statsFilled = data.table(sub)[obs=='Filled',list(.N),by=list(obs,year)]
mean(statsFilled$N)


#L5 and L7
sub = perc[perc$Date>=as.Date('1999-05-28')&perc$Date<as.Date('2003-05-31'),]
sub$year = year(sub$Date)
statsFilled = data.table(sub)[obs=='Filled',list(.N),by=list(obs, year)]
mean(statsFilled$N)

#L8 and S2
sub = perc[perc$Date>=as.Date('2017-03-28'),]#&out$Date<as.Date('2003-05-31'),]
sub$year = year(sub$Date)
statsFilled = data.table(sub)[obs=='Filled',list(.N),by=list(obs, year)]
mean(statsFilled$N)

################################################################################
##Map figure 3
################################################################################
##Colored by basins.  
basins = st_read("E:\\research\\HydroAtlas\\BasinATLAS_v10_shp\\BasinATLAS_v10_lev03.shp")
st_crs(basins) = crs(gage_stats_vals)
sf::sf_use_s2(FALSE)
shp = gage_stats_vals[gage_stats_vals$Sttn_Nm%in%out$Sttn_Nm,]
#basin_pts = st_join(filt, basins, join=st_within)
basin_pts = st_join(shp,basins,join=st_intersects)
basin_pts$aridity = NA
basin_pts$aridity[basin_pts$ari_ix_sav<50] = "Arid"
basin_pts$aridity#[basin_pts$ari_ix_sav>=20&basin_pts$ari_ix_sav<50] = "Semiarid"
basin_pts$aridity#[basin_pts$ari_ix_sav>=50&basin_pts$ari_ix_sav<65] = "Subhumid"
basin_pts$aridity[basin_pts$ari_ix_sav>=50] = "Humid"
missing = basin_pts[is.na(basin_pts$aridity),]
missingDist = basin_pts[st_nearest_feature(missing[1,], basin_pts[basin_pts$Sttn_Nm!=missing$Sttn_Nm,]),]
missing$HYBAS_ID = missingDist$HYBAS_ID
missing$aridity = missingDist$aridity
basin_pts = basin_pts[basin_pts$Sttn_Nm!=missing$Sttn_Nm,]
basin_pts = bind_rows(basin_pts, missing)
basin_mx = bind_rows(sup,ext)[,.N,by=Sttn_Nm]
basin_mx$Sttn_Nm = gsub('.csv', '', basin_mx$Sttn_Nm)
basin_mx$obs = basin_mx$N
#basin_mx = aggregate(obs~Sttn_Nm, length, data = out[out$obs=="Filled",])
basin_pts$obs = basin_mx$obs[match(basin_pts$Sttn_Nm, basin_mx$Sttn_Nm)]
basin_agg = aggregate(obs~HYBAS_ID, sum, data = basin_pts)
basins$obs = basin_agg$obs[match(basins$HYBAS_ID, basin_agg$HYBAS_ID)]

library(scales)
library(RColorBrewer)
mn = 1980
mx = 2020
myPalette <- colorRampPalette(brewer.pal(11, "Spectral"))
sc <- scale_color_gradientn(colors = myPalette(100), limits=c(mn, mx),labels=comma,
                            na.value = "black")

library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
world = ne_countries(scale = "small", returnclass="sf")
world = world[world$sovereignt!="Antarctica",]
st_crs(basins) = st_crs(world)
st_crs(shp)=st_crs(world)
st_crs(basin_pts) = st_crs(world)
basin_pts$kge = out$kge[match(basin_pts$Sttn_Nm,out$Sttn_Nm)]
basin_pts$kge = as.numeric(basin_pts$kge)
v = -1
basin_pts$cap = ifelse(basin_pts$kge<=v,-1, basin_pts$kge)
basin_pts$cap = cut(basin_pts$kge, c(-Inf,-0.41,0,0.41,1))
basin_pts$obsCap = ifelse(is.na(basin_pts$obs), 0.1, basin_pts$obs)
basin_pts$obsCap = cut(basin_pts$obsCap, c(-Inf,10,100,Inf))
pal=wesanderson::wes_palette('Zissou1')
mp=ggplot(world)+
  geom_sf(color="black", fill = "black")+
  coord_sf(crs = "+proj=wag5", xlim=c(-180,180))+
  theme_classic()+
  theme(axis.title=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.line.x = element_blank(),
        legend.title =element_text(size=12),
        legend.text=element_text(size=11))+
  geom_sf(data=basin_pts,aes(color=obs),pch=19, alpha = 0.9, size=0.75)+
  #scale_color_gradient(trans='log10', low='orange', high='green')+
  #scale_color_gradient2(low='red', high='blue',mid='yellow',midpoint=1,trans='log10')+
  #scale_color_brewer(palette='RdYlBu')+
  #scale_color_manual(values=c('red', 'gold', 'dodgerblue'))+
  #scale_color_manual(values=c(pal[5],pal[3],pal[1]))+
  #scale_color_manual(values=c('orange','deeppink','chartreuse'))+
  # scale_color_manual(values=c('#DBB316','#DB00CF','#00DBC0'))+
  #scale_color_manual(values=c('#ED1E24','#FBF172','#3A52A4'))+
  scale_color_distiller(palette='RdYlBu', direction = 1,trans='log10')+
  coord_sf(crs='+proj=wag5')
mp
ggsave("path\\out\\Figure3a.png", mp,
       dpi=1000,units="in",width=18,height=8)
