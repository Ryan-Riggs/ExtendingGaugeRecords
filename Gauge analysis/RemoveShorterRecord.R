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
new = fread("path\\to\\dist_Sttn_Nm_validQ.csv")
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
path = "path/to/gauge/data/"
rm = list()
kp = list()
for(i in 1:nrow(fl)){
  print(i)
  id = fl$Sttn_Nm[i]
  df = fl[Redundant_Sttn_Nm==id|Sttn_Nm==id]
  other = c(df$Redundant_Sttn_Nm, df$Sttn_Nm)
  df = fl[Redundant_Sttn_Nm%in%other|Sttn_Nm%in%other]
  id = unique(c(df$Redundant_Sttn_Nm, df$Sttn_Nm))
  
  closest = lapply(id, proc)
  closest = rbindlist(closest)
  
  mx = which(closest$obs==max(closest$obs, na.rm=TRUE))
  grdc = grep('GRDC', closest$Sttn_Nm)
  
  closest2 = closest[-mx]
  ##If length of record is the same, keep most recent record. 
  if(nrow(closest2)==0){
  d = which(closest$Date==max(closest$Date, na.rm=TRUE))
  filtOut = closest$Sttn_Nm[-d]

  ##If recent records match, take record with largest mean Q. 
  if(length(filtOut)==0){
    r = which(closest$Q==max(closest$Q, na.rm=TRUE))
    filtOut = closest$Sttn_Nm[r]
    rm[[i]] = filtOut
    kp[[i]] = closest$Sttn_Nm[closest$Sttn_Nm%!in%filtOut]
  }
  rm[[i]] = filtOut
  kp[[i]] = closest$Sttn_Nm[closest$Sttn_Nm%!in%filtOut]
  }
  ##Take the longest record. 
  else{
  filtOut = unique(closest2$Sttn_Nm)
  rm[[i]] = filtOut
  kp[[i]] = closest$Sttn_Nm[closest$Sttn_Nm%!in%filtOut]
  }
}


filtered = unique(unlist(rm))
keep = unique(unlist(kp))

keep[keep%in%filtered]
filtered[filtered%in%keep]
ryan = data.table(filtered)
kp = data.table(keep)
rm = data.table(Sttn_Nm = filtered)
fwrite(rm, "path\\out\\removeTheseV5.csv")



