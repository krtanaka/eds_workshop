library(colorRamps)
library(ggplot2)
library(dplyr)

rm(list = ls())

island = "Lanai"

load(paste0('outputs/', island, '_raw_SST_CRW_Monthly.RData'))
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

  df = data.frame(date = df$rn, mean_SST = df$n)

  df$correct_time_step = seq(as.Date("1985/1/1"), by = "month", length.out = 435)

  df$year = substr(df$correct_time_step, 1, 4)
  df$month = substr(df$correct_time_step, 6, 7)

  df$sector = island_sectors[is]

  df = df %>% select(year, month, mean_SST, sector)

  df_all = rbind(df_all, df)

}

df_all %>%
  ggplot(aes(month, mean_SST, color = year, group = year)) +
  geom_point() +
  geom_line() +
  facet_wrap(~sector) +
  scale_color_manual(values = matlab.like(length(unique(df_all$year))), "") +
  ggdark::dark_theme_minimal() +
  ggtitle(island)
