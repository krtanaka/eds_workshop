library(ggplot2)
library(dplyr)
library(ggdark)

# you need "dat", "hd", and "catch_dat" data files

ymd <- data.frame(do.call('rbind', strsplit(as.character(dat$dt),'/',fixed=TRUE)))
ymd$X3 = as.character(ymd$X3)
ymd$year = ifelse(ymd$X3 %in% c(80:99), paste0("19", ymd$X3), paste0("20", ymd$X3))
ymd$month = as.character(ymd$X1)
dat = cbind(dat, ymd[,c("year", "month")])

percentiles = dat %>% 
  group_by(month, year) %>% 
  summarise(q10 = quantile(y, prob= 0.1),
         q90 = quantile(y, prob= 0.9))

catch_dat$year = as.character(catch_dat$V2)
catch_dat$month = as.character(catch_dat$V3)
catch_dat_percentiles = merge(catch_dat, percentiles)
catch_dat_percentiles$quant  <- ifelse(catch_dat_percentiles$V8 < catch_dat_percentiles$q10 | catch_dat_percentiles$V8 > catch_dat_percentiles$q90, 1, 0)

maya = ggplot() +
  geom_hex(aes(dat$x, dat$y), bins = 100, alpha = 0.4, show.legend = F) +
  geom_line(aes(x = hd$x, y = hd$y), color = "red", lwd = 1) +
  geom_point(aes(catch_dat_percentiles$V9, catch_dat_percentiles$V8, color = factor(catch_dat_percentiles$quant)), size = 4, show.legend = F, alpha = 0.8) + 
  labs(x= "Julian Day", y = "SST degrees C", title = "Main Hawaiian Islands Climatological SST KDE plot")+
  scale_fill_gradientn(colors = plasma(100)) +
  # scale_fill_discrete("") + 
  xlim(-25, 390) +
  ylim(23, 28.25)+
  dark_theme_classic()

print(maya)

pdf("/Users/Kisei/Desktop/catch_sst.pdf", height = 5, width = 6)
print(maya)
dev.off()
