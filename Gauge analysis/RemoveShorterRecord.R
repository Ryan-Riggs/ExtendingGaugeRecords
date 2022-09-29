##Author: Ryan Riggs
##Date: 9/12/2022
################################################################################
##Libraries
################################################################################
library(data.table)
library(dplyr)
library(tidyr)
################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))
new = fread("path/to/gauge/distance/file.csv")
sub = new[new$Sttn_Nm!=new$Redundant_Sttn_Nm]
fl =unique(sub[,c('Redundant_Sttn_Nm', 'Sttn_Nm')])

##Processing function. Determine length of record, meanQ and most recent date.
proc = function(f){
  closest = try(fread(paste0(path, f, ".csv")))
  if(!is.error(closest)){
  rec = nrow(closest[!is.na(Q)&Q>0])
  meanQ = mean(closest[!is.na(Q)&Q>0]$Q)
  dt =max(closest[!is.na(Q)&Q>0]$Date)
  return(data.table(Sttn_Nm=f, obs=rec, Q=meanQ, Date = as.Date(dt)))##Author: Ryan Riggs
##Date: 9/12/2022
################################################################################
##Libraries
################################################################################
library(data.table)
library(dplyr)
library(tidyr)
################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))
new = fread("path/to/gauge/distance/file.csv")
sub = new[new$Sttn_Nm!=new$Redundant_Sttn_Nm]
fl =unique(sub[,c('Redundant_Sttn_Nm', 'Sttn_Nm')])

##Processing function. Determine length of record, meanQ and most recent date.
proc = function(f){
  closest = try(fread(paste0(path, f, ".csv")))
  if(!is.error(closest)){
  rec = nrow(closest[!is.na(Q)&Q>0])
  meanQ = mean(closest[!is.na(Q)&Q>0]$Q)
  dt =max(closest[!is.na(Q)&Q>0]$Date)
  return(data.table(Sttn_Nm=f, obs=rec, Q=meanQ, Date = as.Date(dt)))
  }else{
    return(data.table(Sttn_Nm=f, obs=0, Q=0, Date = as.Date('1900-01-01')))
  }
}

tab = list()
for(i in 1:nrow(fl)){
  st = fl$Sttn_Nm[i]
  rd = fl$Redundant_Sttn_Nm[i]
  sub = which(fl$Redundant_Sttn_Nm==st&fl$Sttn_Nm==rd)
  if(length(sub)==0){
    sub=NA
    fl = fl
  }else{
    fl = fl[-sub,]
  }
  tab[[i]] = sub
}
tab = unlist(tab)

fl = fl
library(BBmisc)
redundantList = list()
for(i in 1:nrow(fl)){
  id = fl$Sttn_Nm[i]
  df = fl[Redundant_Sttn_Nm==id|Sttn_Nm==id]
  other = c(df$Redundant_Sttn_Nm, df$Sttn_Nm)
  df = fl[Redundant_Sttn_Nm%in%other|Sttn_Nm%in%other]
  id = unique(c(df$Redundant_Sttn_Nm, df$Sttn_Nm))
  redundantList[[i]] = id
}
length(unique(redundantList))
path = "path/to/gauge/files/"
redundantList = unique(redundantList)
rm = list()
kp = list()
for(i in 1:length(redundantList)){
  id = redundantList[[i]]
  
  closest = lapply(id, proc)
  closest = rbindlist(closest)
  
  mx = which(closest$obs==max(closest$obs, na.rm=TRUE))
  grdc = grep('GRDC', closest$Sttn_Nm)
  
  closest2 = closest[-mx]
  sub = closest[closest$Sttn_Nm%!in%closest2$Sttn_Nm,]
  sub = sub[sub$Date==max(sub$Date, na.rm=TRUE),]
  sub = sub[sub$Q==max(sub$Q, na.rm=TRUE),]
  sub = sub[1,]
  kp[[i]] = unique(sub$Sttn_Nm)
  rm[[i]] = closest$Sttn_Nm[closest$Sttn_Nm%!in%sub$Sttn_Nm]
}
r=lapply(rm, length)
r=unlist(r)
k=lapply(kp, length)
k=unlist(k)

filtered = unique(unlist(rm))
keep = unique(unlist(kp))

keep[keep%in%filtered]
filtered[filtered%in%keep]
ryan = data.table(filtered)
kp = data.table(keep)
rm = data.table(Sttn_Nm = filtered)
fwrite(rm, "outpath/redundantSites.csv")
