##Author: Ryan Riggs
##Date: 9/12/2022
##########################################################################################################
##Libraries
##########################################################################################################
library(sf)
library(raster)
library(spatial)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(caret)
library(segmented)
library(ga)
library(mgcv)
library(BBmisc)
library(hydroGOF)
library(hydroGOF)
library(randomForest)
library(GA)
library(dplyr)
#########################################################################################################
##Read in files and filter to max kge value. 
#########################################################################################################
files = list.files("path\\to\\RatingCurveResultsV6wDistance", full.names=TRUE)
outputList = list()
for(i in 1:length(files)){
  print(i)
  df = fread(files[i])
  df = df[,Site_number:=NULL]
  outputList[[i]] = df
}

processing = function(i, n, distance, wd){
  x = outputList[[i]]
  x = x[obs>=n&abs(flow)<distance&node_width>=wd]
  x$node_id_end = stringr::str_sub(x$node_id, -1)
  x = x[node_id_end=="1"]
  nd = x[x[,.I[which.max(kge)],]]
  return(nd)
}

outList = list()
for(j in 1:length(outputList)){
  print(j)
  df = processing(j, 33, 10000, 120)
  outList[[j]] = df
}
out = rbindlist(outList)

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
out$rrmse = as.numeric(out$rrmse)
out$kge = as.numeric(out$kge)
#######################################################################################
##Functions to actually fill in the gauge record. 
#######################################################################################
names = c("rrmse", "nse", "kge", "nrmse", "rbias", "bias", "rmse", "stde")
pal = rainbow(8)

sd.p=function(x){sd(na.omit(x))*sqrt((length(na.omit(x))-1)/length(na.omit(x)))}

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
  stde = sd.p(error)
  df = data.frame(rrmse=rrmse, nse=nse, kge=kge, nrmse=nrmse, rbias = rbias, bias=bias, rmse=rmse, stde=stde)
  return(df)
}

#####################################################################################
##Rating curve functions. 
#####################################################################################
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


rc_function = function(Train, Valid, alg, total){
  
  ##Random Forest
  if(alg=="RF"){
    rf = randomForest(Q ~width,data = Train, ntree= nrow(Train),maxnodes = round(nrow(Train)*.05), mtry = 1)
    out = predict(rf, Valid)
    output = out
    entire = predict(rf, total)
  }
  
  ##Spline. 
  if(alg=="Spl"){
    #spline_test = try(smooth.spline(Train$width, Train$Q, spar = 0.9))
    #spl_preds = predict(spline_test, Valid$width)
    kn = round(nrow(Train)*.05)
    kn = ifelse(kn>3, kn, 4)
    fv = mspline(Train$width, Train$Q,k=kn)
    spl_preds = predict.mspline(fv, Valid$width)
    output = as.vector(spl_preds)
    entire = predict.mspline(fv, total$width)
  }
  
  ##piecewise linear regression.
  if(alg=="Pcw"){
    splitFun = function(a){
      reg = lm(Q~width+I((width-a)*(width>=a)),
               data=Train)
      
      firstDf = predict(reg, Train[Train$width<a,])
      secondDf = predict(reg, Train[Train$width>=a,])
      
      if(sign(reg$coefficients[2])==-1){
        #output[Train$width<a] = min(output[Train$width<a],na.rm=TRUE)
        firstDf = rep(min(firstDf, na.rm=TRUE), length(firstDf))
      }else{
        #output[Train$width<a] = output[Train$width<a]
        firstDf = firstDf
      }
      if(sign(reg$coefficients[3])==-1){
        #output[Train$width>=a] = max(output[Train$width<a],na.rm=TRUE)
        secondDf = rep(max(firstDf, na.rm=TRUE), length(secondDf))
      }else{
        #output[Train$width>=a] = output[Train$width>=a]
        secondDf = secondDf
      }
      #return(output)
      return(KGE(c(firstDf, secondDf), c(Train$Q[Train$width<a],
                                         Train$Q[Train$width>=a])))
    }
    quants = quantile(Train$width, seq(.1,.9,.01))
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
      
      if(sign(reg$coefficients[2])==-1){
        #output[Train$width<a] = min(output[Train$width<a],na.rm=TRUE)
        firstDf = rep(min(firstDf, na.rm=TRUE), length(firstDf))
      }else{
        #output[Train$width<a] = output[Train$width<a]
        firstDf = firstDf
      }
      if(sign(reg$coefficients[3])==-1){
        #output[Train$width>=a] = max(output[Train$width<a],na.rm=TRUE)
        secondDf = rep(max(firstDf, na.rm=TRUE), length(secondDf))
      }else{
        #output[Train$width>=a] = output[Train$width>=a]
        secondDf = secondDf
      }
      #return(output)
      return(c(firstDf, secondDf))
    }
    #app = approxfun(Train$width, applyFun(outputs[,.SD[which.max(nse)]]$width))
    app = approxfun(c(Train$width[Train$width<outputs[,.SD[which.max(kge)]]$width], 
                      Train$width[Train$width>=outputs[,.SD[which.max(kge)]]$width]),
                    applyFun(outputs[,.SD[which.max(kge)]]$width))
    seg_preds = app(Valid$width)
    output = seg_preds
    entire = app(total$width)
  }
  
  ##Hydraulic Geometry. 
  if(alg=="hg"){
    pwrFun = function(a, b){
      kge = KGE(((Train$width/a)^(1/b)), Train$Q)
      return(kge)
    }
    
    ga = ga(type="real-valued",fitness=function(x) pwrFun(x[1], x[2]),lower = c(0, 0),  upper = c(max(Train$Q, na.rm=TRUE),1), 
            popSize = 1000, maxiter = 50, run = 10, monitor = FALSE,seed=1)
    df1 = summary(ga)$solution
    gaOut = (Valid$width/df1[[1]])^(1/df1[[2]])
    output = gaOut
    entire = (total$width/df1[[1]])^(1/df1[[2]])
  }
  
  output =ifelse(output<0, NA, output)
  entire =ifelse(entire<0, NA, entire)
  return(list(output, entire))
}
###########################################################################################################################
##Developing rating curves. 
###########################################################################################################################
outer = function(xsec, gage){
  #paired_df = fread(xsection_files[i])
  paired_df = fread(xsec)
  paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
  paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
  ##Remove after August 27, 2021 so that everything lines up. 
  paired_df = paired_df[paired_df$Date<as.Date('2021-08-27')]
  paired_df$width = paired_df$width*paired_df$length
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
  paired_df_1 = paired_df[.(nodes)]
  agg = aggregate(width ~ Date, paired_df_1, max)
  agg$Q = paired_df_1$Q[match(agg$Date, paired_df_1$Date)]
  paired_df_1 = agg
  ##Split randomly. 
  set.seed(1)
  paired_df_1 = paired_df_1[order(paired_df_1$Date),]
  trainIndex = round(nrow(paired_df_1)*.7)
  trainIndex = 1:trainIndex
  
  # set.seed(1)
  # trainIndex = createDataPartition(paired_df_1$Q, p = training,
  #                                  list = FALSE)
  Train = paired_df_1[ trainIndex,]
  Valid = paired_df_1[-trainIndex,]
  ##Split by timeline. 
  # paired_df_1 = paired_df_1[order(paired_df_1$Date),]
  # index = round(nrow(paired_df_1)*.7)
  # Train = paired_df_1[1:index,]
  # Valid = paired_df_1[(index+1):nrow(paired_df_1),]
  
  return(list(Train, Valid))
  # output = try(rc_function(Train, Valid))
  # output$Site_number = Site_number_xsections[i]
  # output$node_id = as.numeric(nodes)
  # finalOutput = output
  # return(finalOutput)
}  

###################################################################################################################
##Determine various statistics. 
###################################################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))
rm = fread("path\\to\\redundant\\removeTheseV5.csv")
out = out[out$Sttn_Nm%!in%rm$Sttn_Nm,]
path = "path/to/crossection/widths/"
files = paste0(path, "Gauge__", out$Sttn_Nm, ".csv")
gagePath = "path\\to\\gauge\\data\\"
gages  = gsub("grdc", "GRDC", out$Sttn_Nm)
gageFiles = paste0(gagePath, gages, ".csv")
outpath = paste0("path\\out\\allobs_wFlagsV6\\", 
                 gages, ".csv")
outpathVal = paste0("path\\out\\PerformanceCheckV6\\", 
                    gages, ".csv")

tab = list()
for(i in 1:nrow(out)){
  sub = out[i,]
  node = sub$node_id
  print(i)
  output = outer(files[i], gageFiles[i])
  nds = output[[2]]
  index = which(nds==node)
  inside = inner(output[[1]], output[[2]][index])
  cal = inside[[1]]
  val = inside[[2]]
  mdl = sub$model
  paired_df = fread(files[i])
  paired_df = paired_df[paired_df$node_id==output[[2]][index],]
  xsec = paired_df
  paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
  paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
  ##Remove after August 27, 2021 so that everything lines up. 
  paired_df = paired_df[paired_df$Date<as.Date('2021-08-27')]
  paired_df$width = paired_df$width*paired_df$length
  combined = rc_function(cal, val, mdl, paired_df)
  v = validation(combined[[1]], val$Q)
  total = combined[[2]]
  paired_df$RC = total
  gage = fread(gageFiles[i])
  gage$Date = as.Date(gage$Date)
  xsec = xsec[xsec$`system:index`%!in%paired_df$`system:index`,]
  xsec$Date = as.Date(as.POSIXct(xsec$`system:time_start`/1000, origin = "1970-01-01"))
  xsec$width = xsec$width*xsec$length
  xsec$RC = -9999
  xsec = xsec[,max(RC), by = Date]
  colnames(xsec)= c("Date", 'RC')
  paired_df = bind_rows(paired_df, xsec)
  joined = full_join(paired_df, gage)
  fwrite(joined, outpath[i])
  fwrite(v, outpathVal[i])
}





tab = list()
for(i in 1:nrow(out)){
  sub = out[i,]
  node = sub$node_id
  print(i)
  output = outer(files[i], gageFiles[i])
  nds = output[[2]]
  index = which(nds==node)
  #####Double check inner function to ensure that no duplicates are made and that performance is the same. 
  ###############
  inside = inner(output[[1]], output[[2]][index])
  cal = inside[[1]]
  val = inside[[2]]
  mdl = sub$model
  paired_df = fread(files[i])
  paired_df = paired_df[paired_df$node_id==output[[2]][index],]
  paired_df = paired_df[paired_df$width>0&paired_df$cloud<.1&paired_df$count==2&paired_df$max==0,]
  paired_df$Date = as.Date(as.POSIXct(paired_df$`system:time_start`/1000, origin = "1970-01-01"))
  paired_df$width = paired_df$width*paired_df$length
  combined = rc_function(cal, val, mdl, paired_df)
  v = validation(combined[[1]], val$Q)
  if(all.equal(v$kge,sub$kge)==TRUE){
    tab[[i]] = TRUE
  }else{
    tab[[i]] = FALSE
  }

}
ry = unlist(tab)
t = out[1:length(ry),]
##Are any different than ratingcurveparallel6? Should be zero. 
t = t[-which(ry),]
