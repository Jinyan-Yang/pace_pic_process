source('r/functions_gcc.R')

# ####
library(imager)
library(HIEv)
download.path <- file.path("download/")
setToPath(download.path)
setToken()
#ROIdata<-read.csv("//ad.uws.edu.au/dfshare/HomesK-W$/30016674/My Documents/facilities/PACE/PACEsecurecam/VMstuff/ROIdata.csv")
ROIdata <- downloadCSV(filename="PACE_ROIdata.csv")
ROInames <- names(ROIdata)[7:length(names(ROIdata))-2]
ROIDates <- strptime(ROInames,'%Y.%m.%d')
ROIdata$subp <- substring(ROIdata$SubPlotID,5,5)

get.pace.pheno.func('2018-4-1','2018-12-31',1)
get.pace.pheno.func('2018-4-1','2018-12-31',2)
get.pace.pheno.func('2018-4-1','2018-12-31',3)

get.pace.pheno.func('2018-4-1','2018-12-31',4)
get.pace.pheno.func('2018-4-1','2018-12-31',5)
get.pace.pheno.func('2018-4-1','2018-12-31',6)
# get.pace.pheno.func('2019-1-1','2019-12-31',2)
# get.pace.pheno.func('2019-1-1','2019-12-31',3)
# get.pace.pheno.func('2019-1-1','2019-12-31',4)
# 
# get.pace.pheno.func('2019-1-1','2019-12-31',5)
# get.pace.pheno.func('2019-1-1','2019-12-31',6)
