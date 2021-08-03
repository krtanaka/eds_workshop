library(colorRamps)
library(ggplot2)
library(dplyr)
library(patchwork)
library(data.table)
library(ggpubr)

rm(list = ls())

# import catch-sst-chl_a-data
load("outputs/Timeseries_2021-07-29.Rdata"); SM[SM == -9991] <- NA

# pick species
# species = unique(SM$SP)[1]; species

species_list = unique(SM$SP)

for (species in 1:length(species_list)) {

  # species = 21

  print(paste0("Working on ", species_list[species], "..."))

  # choose variable
  variable = c("SST_CRW_Monthly", "Chlorophyll_A_ESAOCCCI_Monthly")[1]

  # subset catch data
  if (variable == "SST_CRW_Monthly") {

    sm = SM %>%
      subset(SP == species_list[species]) %>%
      group_by(ISLAND,
               DATE_,
               SECTOR, SP) %>%
      summarise(sst = mean(mean_SST_CRW_Daily_MO01, na.rm = T)) %>%
      na.omit()

    label = "SST"

  }

  if (variable == "Chlorophyll_A_ESAOCCCI_Monthly") {

    sm = SM %>%
      subset(SP == species_list[species]) %>%
      group_by(ISLAND,
               DATE_,
               SECTOR, SP) %>%
      summarise(sst = mean(mean_Chlorophyll_A_ESAOCCCI_8Day_MO01, na.rm = T)) %>%
      na.omit()

    label = "chl_a"

  }

  setDT(sm)[, paste0("DATE_", 1:3) := tstrsplit(DATE_, "/")]
  colnames(sm) = c("island", "date", "sector", "species", "mean", "month", "day", "year")
  sm$x = nchar(sm$month)
  sm$month = ifelse(sm$x == "1", paste0("0", sm$month), sm$month)

  species_island = NULL

  for (i in 1:length(unique(sm$island))) {

    # i = 1

    island = unique(sm$island)[i]
    sectors = sm %>% subset(island == island)
    sectors = unique(sectors$sector)

    # choose island...
    # island = c("Hawaii", "Kauai", "Kaula", "Lanai", "Maui", "Molokai", "Niihau", "Oahu")[1]
    # island = unique(sm$island)[1]

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

    df_all = df_all %>% subset(sector %in% sectors)

    df_all$island = island

    species_island = rbind(species_island, df_all)

  }

  p1 = species_island %>%
    group_by(month, sector, island) %>%
    summarise(mean = mean(mean, na.rm = T)) %>%
    ggplot(aes(month, mean, color = sector, group = sector)) +
    geom_point() +
    geom_line() +
    geom_point(data = sm, aes(month, mean), size = 2, color = "red") +
    scale_x_discrete(breaks = c("03", "06", "09")) +
    ylab(label) +
    # scale_color_manual(values = matlab.like(length(unique(df_all$sector))), "") +
    # ggdark::dark_theme_minimal() +
    theme_pubr() +
    facet_grid(island ~ sector) +
    ggtitle(species_list[species])

  p2 = species_island %>%
    group_by(year, sector, island) %>%
    summarise(mean = mean(mean)) %>%
    ggplot(aes(year, mean, color = sector, group = sector)) +
    geom_point() +
    geom_line() +
    geom_point(data = sm, aes(year, mean), size = 2, color = "red") +
    scale_x_discrete(breaks = round(seq(min(df_all$year), max(df_all$year), by = 20), 1)) +
    ylab(label) +

    # scale_color_manual(values = matlab.like(length(unique(df_all$sector))), "") +
    # ggdark::dark_theme_minimal() +
    theme_pubr() +
    facet_grid(island ~ sector) +
    ggtitle(species_list[species])

  pdf(paste0('outputs/', species_list[species], '_', variable, '.pdf'), height = 5, width = 10)
  print(p1 + p2)
  dev.off()


}
