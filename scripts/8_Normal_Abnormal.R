library(colorRamps)
library(ggplot2)
library(dplyr)
library(patchwork)

rm(list = ls())

# choose variable...
variable = c("SST_CRW_Monthly", "Chlorophyll_A_ESAOCCCI_Monthly")[1]

# choose island...
island = c("Hawaii", "Kauai", "Kaula", "Lanai", "Maui", "Molokai", "Niihau", "Oahu")[1]

load(paste0('outputs/', island, '_raw_', variable, '.RData'))

species = unique(df_i$Sp_Common_Name)
island_sectors = unique(df_i$Island_Sector)

df_all = NULL

for (is in 1:length(island_sectors)) {

  # is = 1

  df = df_i %>% subset(Island_Sector == island_sectors[is])

  df = as.data.frame(t(df))

  df = df[-(1:6), , drop = FALSE] #remove first 6 rows

  df[colnames(df)] <- sapply(df[colnames(df)],as.character)
  df[colnames(df)] <- sapply(df[colnames(df)],as.numeric)

  sapply(df, class)

  df$n = rowMeans(df[,c(1:dim(df)[2])], na.rm = T)

  library(data.table)
  df = setDT(df, keep.rownames = TRUE)[]

  df = data.frame(date = df$rn, mean = df$n)

  if (variable == "SST_CRW_Monthly") {

    df$correct_time_step = seq(as.Date("1985/1/1"), by = "month", length.out = dim(df)[1])

  } else {

    df$correct_time_step = seq(as.Date("1997/9/1"), by = "month", length.out = dim(df)[1])

  }


  df$year = substr(df$correct_time_step, 1, 4)
  df$month = substr(df$correct_time_step, 6, 7)

  df$sector = island_sectors[is]

  df = df %>% select(year, month, mean, sector)

  df_all = rbind(df_all, df)

}

env_q = df_all %>%
  group_by(year, month) %>%
  mutate(q10 = quantile(mean, probs = 0.1),
         q90 = quantile(mean, probs = 0.9))

isl = island

load("outputs/Timeseries_2021-07-29.Rdata"); SM[SM == -9991] <- NA

# colnames(catch_grid) = c("island", "sector", "species", "date", "lon", "lat")

setDT(SM)[, paste0("DATE_R", 1:3) := tstrsplit(DATE_R, "-")]
colnames(SM)[36:38] = c("year", "month", "day")

df = SM %>%
  group_by(year, month, day, SP, ISLAND, SECTOR) %>%
  summarise(sst = mean(mean_SST_CRW_Daily_MO03, na.rm = T))
  # distinct(year, month, day, species, island, sector, .keep_all = TRUE)

df$catch = 1
df = df %>% na.omit()

colnames(df)[4:6] = c("species", "island", "sector")

df_env = merge(df, env_q)

df_env$abnormal = ifelse(df_env$sst > df_env$q10 & df_env$sst < df_env$q90, "No", "Yes")
