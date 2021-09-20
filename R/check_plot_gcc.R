# read the processed data
# 
# gcc.df.s1 <- readRDS('S12019.rds')
gcc.df.s1 <- readRDS('SPACE_AUTO_S3_PHENOCAM_R__20190101_20191231.rds')
gcc.df.s1$DateTime <- as.Date(gcc.df.s1$DateTime)
gcc.df.s1$GCC <- gcc.df.s1$G / (gcc.df.s1$R + gcc.df.s1$G + gcc.df.s1$B)

# get a camera to check
test.df <- gcc.df.s1[gcc.df.s1$Cam == 21 & gcc.df.s1$Subplot == 'A',]
test.df <- test.df[order(test.df$DateTime),]
# method 1#############################################################
# this method simply calculates the difference between points
test.df$days.com <- c(rep(NA,4),abs(diff(test.df$GCC, 4)))
test.df$hours.com <- c(NA,diff(test.df$GCC))
# now flag the points with large changes
test.df$flag <- 0
test.df$flag[test.df$days.com > 0.02 | test.df$hours.com > 0.02] <- 1
# see which days have odd GCC change
test.df$DateTime[test.df$flag == 1]

# method 2#############################################################
# this is based on the smoothing method I mentioned before
library(mgcv)
library(lubridate)
gam.frdm = round(nrow(test.df)/5)
gam.in.df = data.frame(x = as.numeric(test.df$DateTime),
                       y = test.df$GCC)
fit.gam <- gam(y~s(x,k = gam.frdm),data = gam.in.df)
predict(fit.gam,gam.in.df)
wrong.index <- which(abs(fit.gam$residuals) > 0.007)

test.df$DateTime[wrong.index]

# make plots to check
with(test.df,plot(GCC~DateTime,pch=16,col='lightseagreen',
                  xlab='2019 S1'))
plot.df <- data.frame(y = fit.gam$fitted.values,
                      x = test.df$DateTime)
points(y~x,data = plot.df ,type='l',col='darkgreen',lwd=3)






# # things to be explored
# library(anomalize)
# test.df %>% 
#   time_decompose(GCC, method = "twitter", frequency = "auto", trend = "auto") %>%
#   anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
#   plot_anomaly_decomposition()


