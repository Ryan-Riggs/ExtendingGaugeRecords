##Author: Ryan Riggs
##Date: 9/20/2022.
###################################################
##add in functions to determine best overall approach for each node using entire timeseries. 
##No visualizations to increase efficiency. 
###################################################
library(caret)
library(segmented)
library(GA)
library(mgcv)
library(BBmisc)
library(data.table)
library(dplyr)
library(plyr)
library(tidyr)
library(randomForest)
library(hydroGOF)


names = c("rrmse", "nse", "kge", "nrmse", "rbias", "bias", "rmse", "stde")
pal = rainbow(8)

sd.p=function(x){sd(na.omit(x))*sqrt((length(na.omit(x))-1)/length(na.omit(x)))}

source("path/to/Error_stats_functions.R")

validation = function(sim, obs){
  sim = as.numeric(sim)
  obs = as.numeric(obs)
  rrmse = RRMSE(sim, obs)
  nse = NSE(sim, obs)
  kge = KGE(sim, obs)
  nrmse = NRMSE(sim, obs)
  rbias = rBias(sim, obs)
  bias = mean(sim-obs)
  error = obs-sim
  rmse = sqrt(mean((error^2),na.rm=TRUE))
  stde = sd.p(error)
  return(c(rrmse, nse, kge, nrmse, rbias, bias, rmse, stde))
}

mspline<-function(x,y,k=10,lower=NA,upper=NA){
  #fits a monotonic spline to data
  #small values of k= more smoothing (flatter curves)
  #large values of k= more flexible (wiggly curves)
  #k is related to effective degrees of freedom and number of knots
  #use unconstrained gam to get rough parameter estimates
  #lower, upper optional bounds on the function
  #basically a slight modification of an example in the mgcv::pcls documentation
  dat<-data.frame(x=x,y=y)
  init_gam <- gam(y~s(x,k=k,bs="cr"))
  # Create Design matrix, constraints etc. for monotonic spline....
  sm <- smoothCon(s(x,k=k,bs="cr"),dat,knots=NULL)[[1]]
  mc <- mono.con(sm$xp,lower=lower,upper=upper) # monotonicity constraints
  M <- list(X=sm$X, y=y, #design matrix, outcome
            C=matrix(0,0,0), #equality constraints (none)
            Ain=mc$A, bin=mc$b, #inequality constraints
            sp=init_gam$sp, p=sm$xp, #initial guesses for param estimates
            S=sm$S, #smoothness penalty matrix
            w=y*0+1, off=0 #weights, offset
  )
  #fit spine using penalized constrained least squares
  p<-pcls(M)
  return(list(sm=sm,p=p))
}

predict.mspline<-function(msp,x){
  #using the monotone spline msp, predict values for the vector x
  Predict.matrix(msp$sm,data.frame(x=x))%*%msp$p
}




rc_function = function(Train, Valid){
  rn = range(c(Train$width, Valid$width), na.rm=TRUE)
  rn = data.table(width=seq(rn[1], rn[2], 0.01))
  ##RF:We added in max node argument to try and limit overfitting. 
  #rf = randomForest(Q ~width,data = Train, ntree= nrow(Train),maxnodes = round(nrow(Train)*.05), mtry = 1)
  rf = randomForest(Q~width, data=Train, nodesize=nrow(Train)*0.75,sampsize=round(nrow(Train)*0.1),nPerm=round(nrow(Train)*0.1))
  out = predict(rf, Valid)
  rfrn = predict(rf, rn)
  
  ##SPline. 
  #spline_test = try(smooth.spline(Train$width, Train$Q, spar = 0.9))
  #spl_preds = predict(spline_test, Valid$width)
  kn = round(nrow(Train)*.05)
  kn = ifelse(kn>3, kn, 4)
  fv = mspline(Train$width, Train$Q,k=kn)
  spl_preds = predict.mspline(fv, Valid$width)
  splrn = predict.mspline(fv, rn$width)
  
  
  
  
  
  
  ##Linear segmentation. 
  # segmented.mod = try(segmented(lm(Q~width, Train), seg.Z = ~width))
  # s = slope(segmented.mod)
  # s = data.frame(s)
  # numb = as.vector(as.numeric(sign(s$width.Est.)))
  # seg_preds = predict(segmented.mod, Valid)
  
  splitFun = function(a){
    reg = lm(Q~width+I((width-a)*(width>=a)),
             data=Train)
    
    firstDf = predict(reg, Train[Train$width<a,])
    secondDf = predict(reg, Train[Train$width>=a,])
    
    if(min(secondDf)>max(firstDf)){
      return(KGE(c(firstDf, secondDf), c(Train$Q[Train$width<a],
                                         Train$Q[Train$width>=a])))
    }else{return(-1e6)}
  }
  quants = Train$width#quantile(Train$width, seq(.1,.9,.01))
  t = list()
  for(k in 1:length(quants)){
    print(k)
    dt = try(splitFun(quants[k]))
    if(is.error(dt)){
      n = NA
    }else{
      n = dt
    }
    t[[k]] = n
  }
  t = unlist(t)
  outputs = data.table(width = quants, kge=t)
  
  ##Result. 
  applyFun = function(a){
    reg = lm(Q~width+I((width-a)*(width>=a)),
             data=Train)
    
    firstDf = predict(reg, Train[Train$width<a,])
    secondDf = predict(reg, Train[Train$width>=a,])
    
    return(c(firstDf, secondDf))
    
  }
  #app = approxfun(Train$width, applyFun(outputs[,.SD[which.max(nse)]]$width))
  app = approxfun(c(Train$width[Train$width<outputs[,.SD[which.max(kge)]]$width], 
                    Train$width[Train$width>=outputs[,.SD[which.max(kge)]]$width]),
                  applyFun(outputs[,.SD[which.max(kge)]]$width))
  seg_preds = app(Valid$width)
  segrn = app(rn$width)
  
  ##Hydraulic Geometry. 
  pwrFun = function(a, b){
    kge = KGE(((Train$width/a)^(1/b)), Train$Q)
    return(kge)
  }
  
  ga = ga(type="real-valued",fitness=function(x) pwrFun(x[1], x[2]),lower = c(0, 0),  upper = c(max(Train$Q, na.rm=TRUE),1), 
          popSize = 1000, maxiter = 50, run = 10, monitor = FALSE, seed=1)
  df1 = summary(ga)$solution
  gaOut = (Valid$width/df1[[1]])^(1/df1[[2]])
  garn = (rn$width/df1[[1]])^(1/df1[[2]])
  
  ##combine them. 
  RF = out
  Spl = as.vector(spl_preds)#$y
  Pcw = as.numeric(seg_preds)
  hg = gaOut
  tab = list(RF, Spl, Pcw, hg)
  models = c("RF","Spl", "Pcw", "hg")
  
  tab = lapply(tab, FUN = function(x){ifelse(x<0, NA, x)})
  
  out= lapply(tab, validation, obs = Valid$Q)
  out = rbindlist(lapply(out, as.data.frame.list), use.names = FALSE)
  colnames(out) = names
  
  lreg = list(data.table(width=rn$width,Q=rfrn, model='RF'), data.table(width=rn$width,Q=as.vector(splrn), model='Spl'),
              data.table(width=rn$width,Q=segrn, model='Pcw'), data.table(width=rn$width,Q=garn, model='hg'))
  lreg = rbindlist(lreg)[,list(slope=lm(Q~width)$coefficients[[2]]),by=model]
  
  dv=diff(rfrn)/diff(rn$width)
  splRF = spline(rn$width, rfrn, n=10)
  percChange = (diff(splRF$y)/splRF$y[1:9])*100

  #t = which(out$nse==max(out$nse))
  output = out#[t,]
  output$model = models#[t]
  output$slope = lreg$slope[match(output$model, lreg$model)]
  output$meanDv = mean(abs(dv))
  output$maxDv = max(abs(dv))
  output$meanDv = ifelse(output$model=='RF', output$meanDv, NA)
  output$maxDv = ifelse(output$model=='RF', output$maxDv, NA)
  output$minPC = min(percChange, na.rm=TRUE)
  output$maxPC=max(percChange, na.rm=TRUE)
  output$minPC=ifelse(output$model=='RF', output$minPC, NA)
  output$maxPC=ifelse(output$model=='RF', output$maxPC, NA)
  output$a = df1[1]
  output$a = ifelse(output$model=='hg', output$a, NA)
  output$b=df1[2]  
  output$b = ifelse(output$model=='hg', output$b, NA)
  return(output)
}


sites = fread("E:\\research\\GlobalGaugeData\\Stations\\FilteredStationsbyRecord.csv")
grdc_path = "E:\\research\\GlobalGaugeData\\GRDC\\"
gages = list.files("path/to/gauge/data/")
gages = gsub("Gauge__", "", gages)
Site_number_xsections = gsub("_.*.csv$", "", gages)


xsection_files = paste0("path/to/width/data/","Gauge__", gages)

##Check to see if they all exist. 
t = list.files("path/to/width/data/", full.names= TRUE)
tab = list()
for(i in 1:length(xsection_files)){
  print(i)
  testing = xsection_files[i]%in%t
  if(testing==FALSE){
    out = gsub("grdc", "GRDC", xsection_files[i])
    tab[[i]] = out
  }else{
    tab[[i]] = xsection_files[i]
  }}
xsection_files = unlist(tab)
grdc_files = gsub("path/to/width/data/","path/to/gauge/data/", xsection_files)
grdc_files = gsub("Gauge__", "", grdc_files)

actualRecords = list.files("path/to/gauge/data/")
actualRecords = gsub(".csv", "", actualRecords)














outer = function(xsec, gage){
  #paired_df = fread(xsection_files[i])
  paired_df = fread(xsec)
  paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
  paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
  ##Remove after August 27, 2021 so that everything lines up. 
  paired_df = paired_df[paired_df$Date<as.Date('2021-08-27')]
  paired_df$width = paired_df$width*paired_df$length
  paired_df = paired_df[paired_df$width>=30,]
  nodes = unique(paired_df$node_id)
  paired_df = as.data.table(paired_df)
  setkey(paired_df, node_id)
  pd1 = paired_df
  usgs_q = try(fread(gage))
  if(!is.error(usgs_q)){
    usgs_q$q = as.numeric(usgs_q$Q)
    all = usgs_q
    usgs_q = usgs_q[usgs_q$q>0&usgs_q$Date>as.Date("1983-12-31", format ="%Y-%m-%d"),]
    usgs_q$Date = as.Date(usgs_q$Date)
    paired_df = inner_join(paired_df, usgs_q)
    paired_df$Q = paired_df$q
    paired_df = paired_df[!is.na(paired_df$Q),]
    nodes = unique(paired_df$node_id)
    return(list(paired_df, nodes))
  }}

inner = function(paired_df,nodes){      
  #paired_df_1 = paired_df[.(nodes)]
  
  ##Split randomly. 
  set.seed(1)
  trainIndex = createDataPartition(paired_df_1$Q, p = training,
                                   list = FALSE)
  Train = paired_df_1[ trainIndex,]
  Valid = paired_df_1[-trainIndex,]
  

}  



##Subset to ones that haven't been ran yet and have records.  
'%!in%' <- function(x,y)!('%in%'(x,y))
alreadyDone = list.files("path/out/")
alreadyDone = gsub("Gauge__", "", alreadyDone)
gageNames = gsub("E:/research/GlobalGaugeData/combined/","",grdc_files)
loc = gageNames%!in%alreadyDone
Site_number_xsections = Site_number_xsections[loc]
grdc_files = grdc_files[loc]
xsection_files = xsection_files[loc]

actualRecords = list.files("path/to/gauge/data/")
gageNames = gsub("path/to/gauge/data/","",grdc_files)
gageNames = gsub("grdc", "GRDC", gageNames)
loc = gageNames%in%actualRecords
Site_number_xsections = Site_number_xsections[loc]
grdc_files = grdc_files[loc]
xsection_files = xsection_files[loc]

##Filter out blank width extractions. 
filteringBlanks = function(f){
  a = try(fread(f, nrow=0))
  if(is.error(a)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

loc2 = lapply(xsection_files, filteringBlanks)
loc2 = unlist(loc2)
grdc_files = grdc_files[loc2]
xsection_files = xsection_files[loc2]

##########################################################################################
##Parallelized. 
##########################################################################################
library(doParallel)
doParallel::registerDoParallel(6)
outDf =foreach(i = 1:length(Site_number_xsections), .combine='rbind')%dopar%{
  ##Libraries:
  library(caret)
  library(segmented)
  library(GA)
  library(mgcv)
  library(BBmisc)
  library(sf)
  library(geosphere)
  library(tidyverse)
  library(sf)
  library(sp)
  library(ggplot2)
  library(rgeos)
  library(dplyr)
  library(gmt)
  library(vroom)
  library(BAMMtools)
  library(hydroGOF)
  library(data.table)
  library(parallel)
  library(foreach)
  library(reshape)
  library(ggplot2)
  require(gridExtra)
  library(cowplot)
  library(randomForest)
  
  if (!"foreign" %in% rownames(installed.packages())){
    install.packages("foreign")}; require(foreign)
  if (!"rgdal" %in% rownames(installed.packages())){
    install.packages("rgdal")}; require(rgdal)
  if (!"shapefiles" %in% rownames(installed.packages())){
    install.packages("shapefiles")}; require(shapefiles)
  if (!"RColorBrewer" %in% rownames(installed.packages())){
    install.packages("RColorBrewer")}; require(RColorBrewer)
  if (!"zyp" %in% rownames(installed.packages())){
    install.packages("zyp")}; require(zyp)
  
  ##Error functions. 
  source("path/to/Error_stats_functions.R")
  
  
  print(paste0("Iteration:", i))
  first = lapply(xsection_files[i], FUN=outer, gage=grdc_files[i])
  df = first[[1]][[1]]
  nds = first[[1]][[2]]
  tab=list()
  for(j in 1:length(nds)){
    paired_df_1 = df[.(nds[j])]
    if(all(is.na(paired_df_1$width))){next}
    agg = aggregate(width ~ Date, paired_df_1, max)
    agg$Q = paired_df_1$Q[match(agg$Date, paired_df_1$Date)]
    paired_df_1 = agg
    if(nrow(paired_df_1)<3|length(unique(paired_df_1$Q))<3){next}
    ##Split randomly. 
    set.seed(1)
    paired_df_1 = paired_df_1[order(paired_df_1$Date),]
    trainIndex = round(nrow(paired_df_1)*.7)
    trainIndex = 1:trainIndex
    
    # trainIndex = createDataPartition(paired_df_1$Q, p = training,
    #                                  list = FALSE)
    Train = paired_df_1[ trainIndex,]
    Valid = paired_df_1[-trainIndex,]
    if(nrow(Valid)<3|length(unique(Train$width))<4){next}
    output = try(rc_function(Train, Valid))
    if(is.error(output)){next}
    else{
      agency = gsub("path/to/widths/","",xsection_files[i])
      agency = gsub(".csv", "", agency)
      output$Site_number = Site_number_xsections[i]
      output$node_id = as.numeric(nds[j])
      output$agency = agency
      output$obs = nrow(paired_df_1)
      output$cal = nrow(Train)
      output$val = nrow(Valid)
      tab[[j]]=output
    }
  }
  #outDf[[i]] = rbindlist(tab)
  out = rbindlist(tab)
  fwrite(out, paste0("path\\out\\",out$agency[1], ".csv"))
}

stopImplicitCluster()
