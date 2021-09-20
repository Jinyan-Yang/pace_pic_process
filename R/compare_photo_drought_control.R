pace.df <- readRDS('d:/repo/PhenoMods/cache/gcc.met.pace.df.rds')
pace.df = pace.df[pace.df$Date >= as.Date('2018-10-1')&
                    pace.df$Date <= as.Date('2019-12-1'), ]
pace.df <- pace.df[pace.df$Species %in% c('Kan','Fes','Pha','Rho'),]
# with(gcc.met.pace.df[gcc.met.pace.df$SubplotID == 'S1P1A',],plot(GCC~Date))

pace.df <- pace.df[pace.df$Temperature == 'Ambient',]
# # fes.df <- harve.df[harve.df$Cam==44,]
subplot.vec <- unique(pace.df$SubplotID)

# make a date vector to get photos
date.range <- range(pace.df$Date,na.rm=T)
date.vec <- seq(date.range[1],date.range[2],by='month')

for (spc.i in seq_along(subplot.vec)) {
  # PACECAM01_0123_20180401_140106.jpg
  sub.df <- pace.df[pace.df$SubplotID == subplot.vec[spc.i],]
  # sub.df$Date <- as.Date(sub.df$Date)
  cam.nm <- unique(sub.df$Cam)
  all.pic.vec <- list.files('temp/imgfiles/',sprintf('PACECAM%02d',cam.nm),full.names = T)
 
  days.vec <- format(date.vec,'%Y%m%d')
  
  index.pic <- sapply(days.vec,grep,all.pic.vec)
  
  # more files into a speprate folder
  if(!dir.exists('compare_pic_4spc')){dir.create('compare_pic_4spc')}
  # 
  out.path <- paste0('compare_pic_4spc/',subplot.vec[spc.i],'_',
                     unique(sub.df$Species),'_',
                     unique(sub.df$Precipitation))
  if(!dir.exists(out.path)){dir.create(out.path)}
  
  # move file.func
  move.func <- function(fn){
    out.nm <- gsub('temp/imgfiles/','',fn)
    
    file.copy(fn,
              paste0(out.path,'/',out.nm))
    
  }
  pic.chosen.vec <- all.pic.vec[unlist(index.pic)]
  # 
  sapply(pic.chosen.vec,move.func)
}



