harve.df <- read.csv('s:/storage/repo/PhenoMods/cache/pace_gcc_harvest.csv')
harve.df <- harve.df[harve.df$Temperature == 'Ambient',]
fes.df <- harve.df[harve.df$Cam==44,]
subplot.vec <- unique(harve.df$SubplotID)
for (spc.i in seq_along(subplot.vec)) {
  # PACECAM01_0123_20180401_140106.jpg
  sub.df <- harve.df[harve.df$SubplotID == subplot.vec[spc.i],]
  sub.df$Date <- as.Date(sub.df$Date)
  cam.nm <- unique(sub.df$Cam)
  all.pic.vec <- list.files('temp/imgfiles/',sprintf('PACECAM%02d',cam.nm),full.names = T)
  days.vec <- c(unique(sub.df$Date), 
                unique(sub.df$Date)+2,
                unique(sub.df$Date)-2)
  days.vec <- format(days.vec,'%Y%m%d')
  
  index.pic <- sapply(days.vec,grep,all.pic.vec)
  
  # more files into a speprate folder
  if(!dir.exists('compare_pic')){dir.create('compare_pic')}
  # 
  out.path <- paste0('compare_pic/',subplot.vec[spc.i],'_',
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
