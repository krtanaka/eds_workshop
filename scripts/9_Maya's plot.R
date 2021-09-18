library(ggplot2)
library(dplyr)
library(ggdark)
library(readr)
library(viridis)
library(TDPanalysis)

catch <- read_csv("outputs//df_env_MHI.csv")

catch$quant = ifelse(catch$abnormal == "Yes", "Abnormal", "Normal")
catch$date = paste0(catch$day, "/", catch$month, "/", catch$year)
catch$doy = date.to.DOY(catch$date, format = "dd/mm/yyyy")
catch$sst_f = (catch$sst * 1.8) + 32 # SST values from catch date & location
catch$mean_f = (catch$mean * 1.8) + 32 # Monthly mean SST from corresponding island & year

maya = ggplot() +
  geom_point(aes(catch$doy, catch$mean_f, color = "Mean"), size = 2, show.legend = T, alpha = 0.3) +
  # geom_smooth(aes(catch$doy, catch$mean_f, color = "Mean"), show.legend = F, alpha = 0.3, size = 0.5, span = 0.1) +
  labs(x= "Day of the year", y = "SST degrees F", title = "Unusual catches with normal or abnormal sea surface temperature conditions ")+
  geom_point(aes(catch$doy, catch$sst_f, color = factor(catch$quant)), size = 4, show.legend = T, alpha = 0.8) +
  scale_color_discrete("") +
  # xlim(25, 390) +
  # ylim(23, 28.25)+
  dark_theme_classic() +
  theme(legend.position = c(0.1, 0.9))

print(maya)

pdf("/Users/Kisei.Tanaka/Desktop/catch_sst.pdf", height = 7, width = 7)
print(maya)
dev.off()
