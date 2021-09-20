# functions####
#function that returns the date in the ROIDates list that preceeds a given date
ROI.Date.func<-function(dd,n=length(ROIDates))
{
  if (n==1) return(1) #if the date falls before the first ROI.Date.func return that.
  if (dd<ROIDates[n]) {ROI.Date.func(dd,n-1)}
  else {return(n)}
}
#functions to extract coordinates from ROIdata table where n is ordered date column
get.rois.func<-function(Cam,Subp,n){
  
  x1<-ROIdata[which(ROIdata$Camera==Cam & ROIdata$subp==Subp & ROIdata$Coordinates=="X1"),n+4]
  x2<-ROIdata[which(ROIdata$Camera==Cam & ROIdata$subp==Subp & ROIdata$Coordinates=="X2"),n+4]
  y1<-ROIdata[which(ROIdata$Camera==Cam & ROIdata$subp==Subp & ROIdata$Coordinates=="Y1"),n+4]
  y2<-ROIdata[which(ROIdata$Camera==Cam & ROIdata$subp==Subp & ROIdata$Coordinates=="Y2"),n+4]
  newlist<-list("x1"=x1,"x2"=x2,"y1"=y1,"y2"=y2)
  return(newlist)
}

#read the datesfile if it has been passed as an argument and exists to get dates to work on.
readControlfile <- function(filename) {
  out <- tryCatch(
    {
      ddates<-read.csv(filename, as.is=T)
      ddates
    },
    error=function(cond) {
      message(paste("file does not seem to exist:", filename))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(paste("File caused a warning:", filename))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally={
      
      message(paste("Processed datefile:",filename))
      
    }
  )    
  return(out)
}
# 
draw_ROI<-function(im,x0,y0,x1,y1,lw=10)
{
  im<-implot(im,lines(c(x0,y0),c(x0,y1),col="red",lwd=lw))
  im<-implot(im,lines(c(x0,y1),c(x1,y1),col="blue",lwd=lw))
  im<-implot(im,lines(c(x1,y1),c(x1,y0),col="green",lwd=lw))
  im<-implot(im,lines(c(x1,y0),c(x0,y0),col="cyan",lwd=lw))
  return(im)
}

# get GCC
gcc.func <- function(filename){
  # empty output file
  results <- data.frame(DateTime=as.POSIXct("2000-01-01 12:00:00",tz="UTC"),
                        Cam=0,Subplot="x",R=0.0,G=0.0,B=0.0)
  # get basic info
  cam<-as.numeric(substring(filename,8,9))
  imgdatetime<-as.POSIXct(substring(filename,16,31),"%Y%m%d_%H%M%OS",tz="UTC")
  imgdate<-as.Date(imgdatetime)
  # hour<-as.numeric(substring(filename,25,26))
  
  # check if file is corrupted
  fn.target <- paste0("temp/imgfiles/",filename)
  test <- file.info(fn.target)
  
  # if file corrupted then not reading
  if(test$size >200){
    # 
    t1 <- load.image(fn.target)
    
    #select subset regions of interest
    #rois<-ROIfile[which(ROIfile$Camera==cam & ROIfile$Subplot=="A"),c(3:6)]
    rois<-unlist(get.rois.func(cam,"A",ROI.Date.func(imgdate)))
    # print(cat(paste(Sys.info()[['nodename']], Sys.getpid(), sep='-'), "now performing get.rois.func"));
    roi.1<-imsub(t1,x %inr% c(rois[[1]],rois[[2]]),y %inr% c(rois[[3]],rois[[4]]))
    rt<-data.frame(DateTime=imgdate,Cam=cam,Subplot="A",R=mean(R(roi.1)),G=mean(G(roi.1)),B=mean(B(roi.1)))
    results<-rbind(results,rt)
    
    #rois<-ROIfile[which(ROIfile$Camera==cam & ROIfile$Subplot=="B"),c(3:6)]
    rois<-unlist(get.rois.func(cam,"B",ROI.Date.func(imgdate)))        
    roi.2<-imsub(t1,x %inr% c(rois[[1]],rois[[2]]),y %inr% c(rois[[3]],rois[[4]]))
    rt<-data.frame(DateTime=imgdate,Cam=cam,Subplot="B",R=mean(R(roi.2)),G=mean(G(roi.2)),B=mean(B(roi.2)))
    results<-rbind(results,rt) 
    
    #rois<-ROIfile[which(ROIfile$Camera==cam & ROIfile$Subplot=="C"),c(3:6)]
    rois<-unlist(get.rois.func(cam,"C",ROI.Date.func(imgdate)))        
    roi.3<-imsub(t1,x %inr% c(rois[[1]],rois[[2]]),y %inr% c(rois[[3]],rois[[4]]))
    rt<-data.frame(DateTime=imgdate,Cam=cam,Subplot="C",R=mean(R(roi.3)),G=mean(G(roi.3)),B=mean(B(roi.3)))
    results<-rbind(results,rt) 
    
    
    #rois<-ROIfile[which(ROIfile$Camera==cam & ROIfile$Subplot=="D"),c(3:6)]
    rois<-unlist(get.rois.func(cam,"D",ROI.Date.func(imgdate)))        
    roi.4<-imsub(t1,x %inr% c(rois[[1]],rois[[2]]),y %inr% c(rois[[3]],rois[[4]])) 
    rt<-data.frame(DateTime=imgdate,Cam=cam,Subplot="D",R=mean(R(roi.4)),G=mean(G(roi.4)),B=mean(B(roi.4)))
    results<-rbind(results,rt) 
    
    results<-results[2:nrow(results),]
    results$Subplot<-factor(results$Subplot)
    results$DateTime<-as.character(results$DateTime)
    
    # 
    # results$gcc <- results$G / (results$R + results$G + results$B)
  }
  
  return(results)
}


# 
get.pace.pheno.func <- function(start.day,end.day,shelter.nm){
  #enter default values for date and ROIfile
  # setwd("/home/30016674/PACE/input/")
  # Ddates<-data.frame(date=as.character(Sys.Date()-1),stringsAsFactors=FALSE)
  Ddates <- data.frame(date=as.character(seq(as.Date(start.day),as.Date(end.day),by='day')))
  
  #loop over all days in dates file
  gcc.ls <- list()
  for (i in 1:nrow(Ddates)){
    start<-Sys.time()
    #  writeLines(paste0("Starting Processing Day: ", Ddates[i,1], " at ",start),logfile)
    # s <- searchHIEv("PACE_AUTO_S[1-6]{1}_PHENOCAM_R_", startDate=as.Date(Ddates$date[i]), endDate=as.Date(Ddates$date[i]))
    
    fn.search <- sprintf("PACE_AUTO_S%s_PHENOCAM_R_",shelter.nm)
    s <- searchHIEv(fn.search, 
                    startDate=as.Date(Ddates$date[i]), 
                    endDate=as.Date(Ddates$date[i]))
    
    if (!is.null(s)){ 
      #account for days when no zip files were uploaded
      d <- downloadHIEv(s)
      #3. To get the full file path of the downloaded files,
      fnzips <- file.path(getToPath(), d)
      
      #loop over each zip file and process images
      n<-0
      tmp.ls <- list()
      
      for (j in 1:length(fnzips)) {
        #get list of file names in current zip file
        imagefiles <- unzip(fnzips[j], list=TRUE)$Name
        imagefiles <- imagefiles[grep("^PACE.*\\.jpg$",imagefiles)] #make sure no dodgy files got into the zip
        #unzip the images to a temp directory. 
        # Would like to be able to work directly on file without unzipping but can't make it do it.
        
        imgfiles <- list.files()
        # do.call(file.remove, list(imgfiles))
        unzip(fnzips[j],exdir="temp/imgfiles")
        # setwd("/home/30016674/PACE/temp/imgfiles")
        #can make the individual file processing parallel
        
        out.ls <- lapply(imagefiles,gcc.func)
        
        tmp.ls[[j]] <- do.call(rbind,out.ls)
        
        saveRDS(tmp.ls[[j]],paste0('cache/',fnzips[j],'.rds'))
        
        # # remove the files
        # file.remove(imagefiles)
        
        print(fnzips[j])
      } 
    }
    gcc.ls[[i]] <- do.call(rbind,tmp.ls)
  }
  gcc.df <- do.call(rbind,gcc.ls)
  gcc.df <- gcc.df[gcc.df$Cam != 0,]
  
  fn.out <- sprintf('S%s_%s_%s.rds',
                    fn.search,format(as.Date(start.day),'%Y%m%d'),
                    format(as.Date(end.day),'%Y%m%d'))
  saveRDS(gcc.df,fn.out)
  
}