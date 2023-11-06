rm(list = ls())

library(raster)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(colorRamps)

source("scripts/eds_functions.R")

dir = paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/EDS/")

#################################
### list ocean color datasets ###
#################################
oc = read_csv("data/EDS_parameters.csv")

oc = oc %>% filter(Mask == T & Download == "YES")

var_list <- basename(list.dirs(dir, recursive = F))

oc <- var_list[var_list %in% oc$Dataset]

oc

compare_masking(oc_data = oc[2], unit = "Oahu")
compare_masking(oc_data = oc[2], unit = "Hawaii")
compare_masking(oc_data = oc[2], unit = "Maui")
