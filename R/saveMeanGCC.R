# get.gcc.mean.func <- function(Cam,dat){
#   
#   test.ls <- list()
#   for (i in seq_along(LETTERS[1:4])) {
#     test.df <- gcc.df.s1[dat$Cam == Cam & dat$Subplot == LETTERS[i],]
#     test.df <- test.df[order(test.df$DateTime),]
#     library(doBy)
#     test.ls[[i]] <- summaryBy(GCC + R + G + B ~ DateTime, data  = test.df,
#                              FUN=median,na.rm=T,keep.names = T,id=~c(Subplot))
#     
#     
#   }
#   test.sum.df <- do.call(rbind,test.ls)
#   test.sum.df$Cam <- Cam
#   return(test.sum.df)
# }

read.gcc.func <- function(shelter,date.in = '20180401_20181231'){
# shelter <- 1
  fn <- sprintf('SPACE_AUTO_S%d_PHENOCAM_R__%s.rds',shelter,date.in)
  
  gcc.df.s1 <- readRDS(fn)
  gcc.df.s1$DateTime <- as.Date(gcc.df.s1$DateTime)
  gcc.df.s1$GCC <- with(gcc.df.s1,G / (G+B+R))
  
  # cam.vec <- unique(gcc.df.s1$Cam)
  # # 
  # s1.gcc.ls <- sapply(cam.vec,get.gcc.mean.func,dat = gcc.df.s1,simplify = F)
  # s1.gcc.df <- do.call(rbind,s1.gcc.ls)
  library(doBy)
  s1.gcc.df <- summaryBy(GCC + R + G + B ~ DateTime + Cam + Subplot,
                       data = gcc.df.s1,FUN=median,keep.names = T,na.rm=T)
  # 
  s1.gcc.df$Shelter <- shelter
  s1.gcc.df$Plot <- s1.gcc.df$Cam %% 8
  s1.gcc.df$Plot[s1.gcc.df$Plot == 0] <- 8
  s1.gcc.df$SubplotID <- paste0('S',s1.gcc.df$Shelter,'P',s1.gcc.df$Plot,s1.gcc.df$Subplot)
  return(s1.gcc.df)
}

# pace.gcc.ls <- sapply(1:6,read.gcc.func,simplify = F)
# pace.gcc.df <- do.call(rbind,pace.gcc.ls)

temp=list()
for(i in 1:6){
  temp[[i]]=read.gcc.func(i,'20190101_20191231')
}
tm.df.19= do.call(rbind,temp)


# 
header.df <- read.csv('cache/SubPlotTreatmentsMaster 2019-02-13.csv')
header.df <- subset(header.df,select = -c(SubPlotID,x1,x2,y1,y2))

pace.gcc.header.df <- merge(tm.df.19,header.df[,!(names(header.df)%in%c('CameraSubplot','PlotID'))],all=T)

saveRDS(pace.gcc.header.df,'cache/pace.gcc.2019.rds')
# test= readRDS(cache/pace.gcc.2018.rds)
df=readRDS('cache/pace.gcc.2018.rds')
# library(lubridate)
# df$Date=as.Date(df$DateTime)
# with(subset(df,SubplotID == "S1P4C"),table(year(Date),month(Date)))

# 2018########
# # 
# pace.gcc.ls.18 <- sapply(1:6, read.gcc.func,simplify = F)
# pace.gcc.df <- do.call(rbind,pace.gcc.ls.18)

temp=list()
for(i in 1:6){
  temp[[i]]=read.gcc.func(i,date.in = '20180401_20181231')
}
tm.df.18= do.call(rbind,temp)

header.df <- read.csv('cache/SubPlotTreatmentsMaster 2019-02-13.csv')
header.df <- subset(header.df,select = -c(SubPlotID,x1,x2,y1,y2))

pace.gcc.header.df <- merge(tm.df.18,header.df[,!(names(header.df)%in%c('CameraSubplot','PlotID'))],all=T)

saveRDS(pace.gcc.header.df,'cache/pace.gcc.2018.rds')

# df$Date=as.Date(df$DateTime)
with(subset(pace.gcc.header.df,SubplotID == "S1P4C"),table(year(DateTime),month(DateTime)))
