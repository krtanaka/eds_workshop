library(colorRamps)
library(ggplot2)
library(dplyr)
library(patchwork)

rm(list = ls())

# import catch-sst-chl_a-data
load("outputs/Timeseries_2021-07-27.Rdata"); SM[SM == -9991] <- NA

# pick species
species = unique(SM$SP)[1]; species

# choose variable...
variable = c("SST_CRW_Monthly", "Chlorophyll_A_ESAOCCCI_Monthly")[2]

# subset catch data
if (variable == "SST_CRW_Monthly") {

  sm = SM %>%
    subset(SP == species) %>%
    group_by(ISLAND,
             DATE_,
             SECTOR, SP) %>%
    summarise(sst = mean(mean_SST_CRW_Daily_MO01, na.rm = T)) %>%
    na.omit()
}

if (variable == "Chlorophyll_A_ESAOCCCI_Monthly") {

  sm = SM %>%
    subset(SP == species) %>%
    group_by(ISLAND,
             DATE_,
             SECTOR, SP) %>%
    summarise(sst = mean(mean_Chlorophyll_A_ESAOCCCI_8Day_MO01, na.rm = T)) %>%
    na.omit()
}

data.table::setDT(sm)[, paste0("DATE_", 1:3) := tstrsplit(DATE_, "/")]
colnames(sm) = c("island", "date", "sector", "species", "mean", "month", "day", "year")
sm$x = nchar(sm$month)
sm$month = ifelse(sm$x == "1", paste0("0", sm$month), sm$month)

# choose island...
# island = c("Hawaii", "Kauai", "Kaula", "Lanai", "Maui", "Molokai", "Niihau", "Oahu")[1]
island = unique(sm$island)[1]

load(paste0('outputs/', island, '_raw_', variable, '.RData'))

# species = unique(df_i$Sp_Common_Name)
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

df_all %>%
  group_by(month, sector) %>%
  summarise(mean = mean(mean, na.rm = T)) %>%
  ggplot(aes(month, mean, color = sector, group = sector)) +
  geom_point() +
  geom_line() +
  geom_point(data = sm, aes(month, mean), size = 5) +
  # facet_wrap(~sector) +
  # scale_color_manual(values = matlab.like(length(unique(df_all$sector))), "") +
  # ggdark::dark_theme_minimal() +
  theme_minimal() +
  facet_wrap(.~sector) +
  ggtitle(paste0(island, "_", species, "_", variable))

df_all %>%
  group_by(year, sector) %>%
  summarise(mean = mean(mean)) %>%
  ggplot(aes(year, mean, color = sector, group = sector)) +
  geom_point() +
  geom_line() +
  geom_point(data = sm, aes(year, mean), size = 3) +
  scale_x_discrete(breaks = round(seq(min(df_all$year), max(df_all$year), by = 10), 1)) +
  # scale_color_manual(values = matlab.like(length(unique(df_all$sector))), "") +
  # ggdark::dark_theme_minimal() +
  theme_minimal() +
  # facet_wrap(.~sector) +
  ggtitle(paste0(island, "_", species, "_", variable))


