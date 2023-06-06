#################################################################### 
# All about analyses using MS global ML building footprints: allometry
# Made: 8th. June 2022
# Revised: 5th. June 2023
# Yuzuru Utsunomiya, Ph. D.
# Source: 
# https://github.com/microsoft/GlobalMLBuildingFootprints#will-there-be-more-data-coming-for-other-geographies
# Projection: WGS84
# ETSG: 4326
# license: https://opendatacommons.org/licenses/odbl/
#################################################################### 
# ---- read.library ----
library(tidyverse)
library(sf)
library(furrr)
library(rsample)
library(khroma)
# set concurrent computing plan
# multisession: use CPUs as many as possible
plan(multisession)
#
# ----- read.data ----- 
# read object footprint data
# NOTE
# To save computation period, we omit geometry.
# When we draw a map with the target object, 
# make another object.
object_Thailand_lat_lon <-
  readRDS("object_Thailand_lat_lon.rds") %>%
  dplyr::mutate(
    id = c(1:nrow(.)))%>%
  dplyr::select(-true_false) %>% 
  sf::st_drop_geometry()
# 
# read shapefiles
shp_Thailand_province <-
  sf::st_read(
    "./shapefiles/THA_adm1.shp",
    options = "ENCODING=UTF-8",
    stringsAsFactors=FALSE
  ) %>%
  dplyr::mutate_if(
    is.character,
    enc2utf8
  )
# 
# make a list of generated csv files
# Reference:
# https://qiita.com/Ringa_hyj/items/434e3a6794bb7ed8ee14
target_file_list <-
  dir(
    path = "../../southeastasiastudy/dataset/target_address_Thailand",
    pattern = "*.csv"
  )
# save the generated csv files
# NOTE
# There exist huge number of csv files.
# This procedure needs looooong computation period.
# Comment out when not in use.
# target_address_Thailand_combined <-
#   furrr::future_map_dfr(
#     paste0(
#       "../../southeastasiastudy/dataset/target_address_Thailand/",
#       target_file_list,
#       sep = ""
#     ),
#     data.table::fread
#   )
# # save the results
# readr::write_rds(
#   target_address_Thailand_combined, 
# "target_address_Thailand_combined.rds"
# )
# target_address_Thailand_combined <- 
#   readr::read_rds(
#     "target_address_Thailand_combined.rds"
#     )
# # combine necessary data
# object_Thailand_all <- 
#   target_address_Thailand_combined %>% 
#   dplyr::left_join(
#     object_Thailand_lat_lon,
#     by = "id"
#   ) %>% 
#   na.omit()
# # save the results
# readr::write_rds(
#   object_Thailand_all, 
#   "object_Thailand_all.rds"
#   )
# 
# ----- drad.maps -----
# read the data
object_Thailand_all <- 
  readr::read_rds(
    "object_Thailand_all.rds"
    )
# 
# make a summary table
object_Thailand_all_summary <- 
  object_Thailand_all %>% 
  dplyr::mutate(area = as.numeric(area)) %>% 
  group_by(province_name) %>% 
  dplyr::summarise(
    N = n(),
    SUM = sum(area),
    Min = min(area),
    Mean = mean(area),
    Median = median(area),
    Max = max(area),
    SD = sd(area),
    SE = sd(area)/sqrt(n())
  ) %>% 
  ungroup() %>% 
  dplyr::left_join(
    shp_Thailand_province,
    by = c("province_name" = "NAME_1")
  ) %>% 
  dplyr::select(province_name, N, SUM, Min, Mean, Median, Max, SD, SE, geometry)
# make a dataset for the map
object_Thailand_all_summary_df <- 
  object_Thailand_all %>% 
  dplyr::mutate(area = as.numeric(area)) %>% 
  group_by(province_name) %>% 
  dplyr::summarise(
    N = n(),
    SUM = sum(area),
    Min = min(area),
    Mean = mean(area),
    Median = median(area),
    Max = max(area),
    SD = sd(area),
    SE = sd(area)/sqrt(n())
  ) %>% 
  pivot_longer(
    cols = -province_name,
    names_to = "statistics",
    values_to = "number"
  ) %>% 
  dplyr::left_join(
    shp_Thailand_province,
    by = c("province_name" = "NAME_1")
  ) %>% 
  dplyr::select(province_name, statistics, number, geometry)
# draw maps by statistics
object_Thailand_all_summary_map <- 
  object_Thailand_all_summary_df %>% 
  group_by(statistics) %>% 
  nest() %>% 
  dplyr::mutate(
    map = purrr::map(
      data,
      ~
        ggplot2::ggplot(
          data = .,
          aes(fill = number, geometry = geometry)
        ) +
        geom_sf() +
        scale_fill_smoothrainbow() +
        labs(x = "Longitude", y = "Latitude", fill = "Statistics", title = statistics) +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(10, "mm")
        )
    )
  )
# save the maps
pdf(
  "object_Thailand_all_summary_map.pdf",
  width = 10,
  height = 10
  )
object_Thailand_all_summary_map$map
dev.off()

# ToDo
# 1. Draw maps by statistics
# 2. Choose 100 largest buildings by province and analyse
# 3. Simulate "optimum" combination of LF and area
