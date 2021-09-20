# prepare the evironment####
source('r/functions_gcc.R')
library('imager')

# 
ROIdata <- read.csv("download/PACE_ROIdata.csv")

ROInames <- names(ROIdata)[5:length(names(ROIdata))]
ROInames <- gsub('X','',ROInames)
ROIDates <- strptime(ROInames,'%Y.%m.%d')
ROIdata$subp <- substring(ROIdata$SubPlotID,5,5)

# read data####
pace.df <- readRDS('d:/repo/PhenoMods/cache/gcc.met.pace.df.rds')
pace.df = pace.df[pace.df$Date >= as.Date('2018-10-1')&
                    pace.df$Date <= as.Date('2019-12-1'), ]
pace.df <- pace.df[pace.df$Species %in% c('Kan','Fes','Pha','Rho'),]

gcc.pace.df.daily <- pace.df

# plot####
gcc.pace.df.daily$days <- as.numeric(gcc.pace.df.daily$Date - min(gcc.pace.df.daily$Date))
# 
# date.range <- range(gcc.pace.df.daily$Date,na.rm=T)
date.vec <- as.Date(c('2018-11-1','2019-1-1','2019-3-1','2019-5-1','2019-7-1'))#seq(date.range[1],date.range[2],by='quarter')

subplot.vec <- unique(pace.df$SubplotID)

# loop through all subplots
for (spc.i in seq_along(subplot.vec)) {
  # subset data
  s1p3c.df <- gcc.pace.df.daily[gcc.pace.df.daily$SubplotID == subplot.vec[spc.i],]
  s1p3c.df$days <- as.numeric(s1p3c.df$Date - min(s1p3c.df$Date)) + 1
  s1p3c.df <- s1p3c.df[order(s1p3c.df$days),]
  s1p3c.df <- s1p3c.df[!is.na(s1p3c.df$GCC),]
  
  # gam fit
  library(mgcv)
  fit.gam <- gam(GCC~s(days,k=ceiling(length(unique(s1p3c.df$Date))/3)),
                 data = s1p3c.df)
  
  # set up plot environment
  jpeg(filename = paste0('figures/',
                         subplot.vec[spc.i],'_',unique(s1p3c.df$Species),
                         '_',unique(s1p3c.df$Precipitation),
                         '.jpg'),
       width = 1200, height = 1200*0.618, units = "px", pointsize = 12,
       quality = 75,
       bg = "white")
  
  # plot gcc
  par(mar=rep(0,4))
  par(fig = c(0,1,0,1))
  par(oma = c(5,5,1,5))
  
  plot(GCC~Date,data = s1p3c.df,pch=16,col='darkseagreen',
       xaxt='n',xlab='',ylim=c(0.3,0.5),ylab='',yaxt='n')
  points(fit.gam$fitted.values~s1p3c.df$Date,type='l',col='grey80',lwd=3)
  mtext('GCC',side = 2,line = 3,adj=0.2)
  axis(2,at = seq(0.3,0.4,by=0.02),labels =   seq(0.3,0.4,by=0.02))
  # add vertical lines
  abline(v = date.vec,lty='dashed',col='grey')
  
  # add month
  date.range = range(s1p3c.df$Date,na.rm=T)
  mons.vec =  seq(date.range[1],date.range[2],by='mon')
  mon.c <- format(mons.vec,'%m')
  axis(1,at = mons.vec,labels = mon.c)
  # add yeat
  library(lubridate)
  yr.vec <- unique(year(s1p3c.df$Date))
  where.c <-which(mon.c =='01') / length(mon.c)
  num.yr <- length(where.c)
  mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  
  # add irrigation
  par(new=T)
  plot(irrig.tot~Date,data = s1p3c.df,type='s',col='blue',ann=F,axes=F,xlab='',ylab='',ylim=c(0,200))
  axis(4,at=seq(0,30,by=10),labels =seq(0,30,by=10))
  mtext('Irrigation (mm/d)',side = 4,line=2,col='blue')
  
  # add photos
# make sure og cam num
  cam.nm <- unique(s1p3c.df$Cam)
  roi.letter <- unique(s1p3c.df$Subplot)
  
  all.pic.vec <- list.files('temp/imgfiles/',sprintf('PACECAM%02d',cam.nm),full.names = T)
  
  # select by dates
  days.vec <- format(date.vec,'%Y%m%d')
  
  index.pic.ls <- lapply(days.vec,grep,all.pic.vec)
  index.pic <- do.call(rbind,index.pic.ls) 
  index.pic <- index.pic[,1]
  
  pic.chosen.vec <- all.pic.vec[unlist(index.pic)]
  
  # loop through all dates
  for(date.i in seq_along(pic.chosen.vec)){
    # file name
    fn <- pic.chosen.vec[date.i]
    # load file
    t1 <- load.image(fn)
    imgdate <- as.Date(substr(fn,30,37),'%Y%m%d')
    # find roi
    rois <- unlist(get.rois.func(cam.nm,roi.letter,ROI.Date.func(imgdate)))
    # subset
    roi.1 <- imsub(t1,x %inr% c(rois[[1]],rois[[2]]),y %inr% c(rois[[3]],rois[[4]]))
    # find reletive position of the date
    x.position <- which(s1p3c.df$Date == imgdate) / length(unique(s1p3c.df$Date)) 
    # 
    par(fig = c(x.position-0.05,x.position+0.05, 0.5, 1), new = T)  
    par(mar=rep(0,4))
    # par(mar=c(5,5,1,1))
    plot(roi.1,ann=F,axes=F)
    
    
  }
  dev.off()


}









