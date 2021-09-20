plot.info.df <- read.csv('download/SubPlotTreatmentsMaster 2019-02-13.csv')
plot.info.df$Cam <- plot.info.df$Camera

fn.out <- sprintf('S%s_%s_%s.rds',
                  fn.search,format(as.Date(start.day),'%Y%m%d'),
                  format(as.Date(end.day),'%Y%m%d'))
pace.gcc.raw.ls <- list()
for (i in 1:6){
  
  fn <- sprintf('SPACE_AUTO_S%s_PHENOCAM_R__20190101_20191231.rds',i)
  pace.gcc.raw.ls[[i]] <- readRDS(fn)
}

pace.gcc.raw.df <- do.call(rbind,pace.gcc.raw.ls)
# 


pace.gcc.raw.df.plot <- merge(pace.gcc.raw.df,plot.info.df,by=c('Cam','Subplot')) 
pace.gcc.raw.df.plot$DateTime <- as.Date(pace.gcc.raw.df.plot$DateTime)

saveRDS(pace.gcc.raw.df.plot,'pace.gcc.2019.rds')

plot.sub.df <- pace.gcc.raw.df.plot[pace.gcc.raw.df.plot$Species == 'Luc' &
                                      pace.gcc.raw.df.plot$Precipitation == 'Control' &
                                      pace.gcc.raw.df.plot$Temperature == 'Ambient',]
plot.sub.df$GCC <- plot.sub.df$G / (plot.sub.df$R + plot.sub.df$G + plot.sub.df$B)
plot.sub.df$SubplotID <- as.character(plot.sub.df$SubplotID)
plot.sub.df$GCC.norm <- NA
plots.vec <- unique(plot.sub.df$SubplotID)

for (i in seq_along(plots.vec)) {
  plot.max <- max(plot.sub.df$GCC[plot.sub.df$SubplotID == plots.vec[i]],na.rm=T)
  plot.min <- min(plot.sub.df$GCC[plot.sub.df$SubplotID == plots.vec[i]],na.rm=T)
  
  plot.sub.df$GCC.norm[plot.sub.df$SubplotID == plots.vec[i]] <-
    (plot.sub.df$GCC[plot.sub.df$SubplotID == plots.vec[i]] - plot.min) /(plot.max - plot.min)
}

library(viridisLite)
tran.func <- function(cols,alpha=0.1){
  targe.vec <- col2rgb(cols)
  out.col <- apply(targe.vec,2,FUN = function(x){
    rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=alpha)})
  return(out.col)
}
palette(tran.func(viridis(6),alpha=0.3))
plot(GCC.norm~DateTime,data = plot.sub.df,pch=15,col=Shelter)
legend('topleft',legend = 1:6,col=palette(),pch=16,bty='n',title = 'Shelter')

library(mgcv)
