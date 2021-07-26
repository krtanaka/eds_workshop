library(colorRamps)
library(ggplot2)
library(dplyr)
library(patchwork)

rm(list = ls())

# choose variable...
variable = c("SST_CRW_Monthly", "Chlorophyll_A_ESAOCCCI_Monthly")[2]

# choose island...
island = c("Hawaii", "Kauai", "Kaula", "Lanai", "Maui", "Molokai", "Niihau", "Oahu")[8]

load(paste0('outputs/', island, '_raw_', variable, '.RData'))
df_i = df_i %>% subset(Island_Sector != "W")

species = unique(df_i$Sp_Common_Name)
island_sectors = unique(df_i$Island_Sector)

df_all = NULL

for (is in 1:length(island_sectors)) {

  # is = 1

  df = df_i %>% subset(Island_Sector == island_sectors[is])

  df = as.data.frame(t(df))

  df = df[-(1:6), , drop = FALSE] #remove first 6 rows

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

# plot by month
p1 = df_all %>%
  ggplot(aes(month, mean, color = year, group = year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~sector) +
  scale_color_manual(values = matlab.like(length(unique(df_all$year))), "") +
  ggdark::dark_theme_minimal() +
  ggtitle(island)

# plot by year
p2 = df_all %>%
  group_by(year, sector) %>%
  summarise(mean = mean(mean)) %>%
  ggplot(aes(year, mean, color = sector, group = sector)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(breaks = round(seq(min(df_all$year), max(df_all$year), by = 4), 1)) +
  scale_color_manual(values = matlab.like(length(unique(df_all$sector))), "") +
  ggdark::dark_theme_minimal() +
  ggtitle(island)

p1
p2

p2 + p1
