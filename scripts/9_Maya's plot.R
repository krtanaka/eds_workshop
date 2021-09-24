library(ggplot2)
library(dplyr)
library(ggdark)
library(readr)
library(viridis)
library(TDPanalysis)

catch <- read_csv("outputs/df_env_MHI.csv")

catch$quant = ifelse(catch$abnormal == "Yes", "Abnormal", "Normal")
catch$date = paste0(catch$day, "/", catch$month, "/", catch$year)
catch$doy = date.to.DOY(catch$date, format = "dd/mm/yyyy")
catch$sst_f = (catch$sst * 1.8) + 32 # SST values from catch date & location
catch$mean_f = (catch$mean * 1.8) + 32 # Monthly mean SST from corresponding island & year

(maya = ggplot() +
  geom_point(aes(catch$doy, catch$mean_f, color = "Mean"), size = 2, show.legend = T, alpha = 0.3) +
  # geom_smooth(aes(catch$doy, catch$mean_f, color = "Mean"), show.legend = F, alpha = 0.3, size = 0.5, span = 0.1) +
  labs(x= "Day of the year", y = "SST degrees F", title = "Unusual catches with normal or abnormal sea surface temperature conditions ")+
  geom_point(aes(catch$doy, catch$sst_f, color = factor(catch$quant)), size = 4, show.legend = T, alpha = 0.8) +
  scale_color_discrete("") +
  # xlim(25, 390) +
  # ylim(23, 28.25)+
  dark_theme_classic() +
  theme(legend.position = c(0.1, 0.9)))

pdf("/Users/Kisei.Tanaka/Desktop/catch_sst.pdf", height = 7, width = 7)
print(maya)
dev.off()

catch <- read_csv("outputs/df_env_MHI.csv")
catch$quant = ifelse(catch$abnormal == "Yes", "Abnormal", "Normal")
catch$date = paste0(catch$day, "/", catch$month, "/", catch$year)
catch$doy = date.to.DOY(catch$date, format = "dd/mm/yyyy")

islands = c("Hawaii", "Kauai", "Kaula", "Lanai", "Maui", "Molokai", "Niihau", "Oahu")

df_all = NULL

for (i in 1:length(islands)) {

  # i = 1

  load(paste0("outputs/", islands[i], "_raw_SST_CRW_Monthly.RData"))

  df = df_i[,c(7:441)]
  df = as.data.frame(t(df))
  df$mean = rowMeans(df, na.rm = T)
  df <- tibble::rownames_to_column(df, "date")
  df = df[,c("date", "mean")]
  df$date = gsub("01-31", "02-01", df$date)
  df$date = gsub("02-31", "03-01", df$date)
  df$date = gsub("03-31", "04-01", df$date)
  df$date = gsub("04-31", "05-01", df$date)
  df$date = gsub("05-31", "06-01", df$date)
  df$date = gsub("06-31", "07-01", df$date)
  df$date = gsub("07-31", "08-01", df$date)
  df$date = gsub("08-31", "09-01", df$date)
  df$date = gsub("09-31", "10-01", df$date)
  df$date = gsub("10-31", "11-01", df$date)
  df$date = gsub("11-31", "12-01", df$date)
  df$date = gsub("12-31", "01-01", df$date)
  df$month = substr(df$date, 6, 7)
  df$year = substr(df$date, 1, 4)
  df$island = islands[i]

  df_all = rbind(df_all, df)

}

df_all = df_all %>%
  subset(year %in% c(1991:2020))

mean = df %>%
  group_by(month) %>%
  summarise(mean = mean(mean))

catch = catch %>%
  subset(island == "Hawaii" & quant == "Abnormal") %>%
  group_by(month) %>%
  summarise(sst = mean(sst),
            catch = n())

(maya = ggplot() +
    geom_jitter(data = df_all, aes(month, mean, color = "Monthly SST (1985-2019)"), alpha = 0.1, size = 2) +
    geom_point(data = mean, aes(month, mean, color = "Mean SST"), size = 3) +
    geom_point(data = catch, aes(month, sst, color = "SST associated with Abnormal Catches", size = catch)) +
    # scale_color_viridis_d("") +
    ggdark::dark_theme_classic() +
    coord_fixed(ratio = 2) +
    ylab("SST") +
    xlab("Month") )

pdf("/Users/Kisei/Desktop/catch_sst.pdf", height = 5, width = 10)
print(maya)
dev.off()
