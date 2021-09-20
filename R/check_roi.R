imagefiles <- imagefiles[grep("^PACE.*\\.jpg$",imagefiles)]


t1 <- load.image('temp/imgfiles/PACECAM19_0220_20190405_110109.jpg')
imgdate <- as.Date('2019-4-5')
cam <- 19
#select subset regions of interest
#rois<-ROIfile[which(ROIfile$Camera==cam & ROIfile$Subplot=="A"),c(3:6)]
rois <- unlist(get.rois.func(19,"B",ROI.Date.func(imgdate)))
# print(cat(paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), "now performing get.rois.func"));
roi.1 <- imsub(t1,x %inr% c(rois[[1]],rois[[2]]),y %inr% c(rois[[3]],rois[[4]]))
rt<-data.frame(DateTime=imgdate,Cam=cam,Subplot="B",R=mean(R(roi.1)),G=mean(G(roi.1)),B=mean(B(roi.1)))
results<-rbind(results,rt)

plot(t1)
points(x = c(rois[[1]],rois[[2]]),y = c(rois[[3]],rois[[4]]),pch=16,col='red')
polygon(x = c(rois[[1]],rois[[1]],rois[[2]],rois[[2]]),
        y = c(rois[[3]],rois[[4]],rois[[3]],rois[[4]]),border ='red')
