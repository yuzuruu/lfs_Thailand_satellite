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
# set concurrent computing plan
# multisession: use CPUs as many as possible
plan(multisession)
#
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
# vroom::vroom() might not be able to use.
# Then, use furrr::future_map_dfr() and data.table::fread() instead.
# vroom::vroom()
# target_address_mekong_combined <-
#   vroom::vroom(
#     paste0(
#       "../../southeastasiastudy/dataset/target_address_Thailand/",
#       target_file_list,
#       sep = ""
#     )
#   ) 
# furrr::future_map_dfr() and data.table::fread()
target_address_Thailand_combined <-
  furrr::future_map_dfr(
    paste0(
      "../../southeastasiastudy/dataset/target_address_Thailand/",
      target_file_list,
      sep = ""
    ),
    data.table::fread
  )
readr::write_rds(
  target_address_Thailand_combined, "target_address_Thailand_combined.rds")

readr::write_excel_csv(
  target_address_Thailand_combined, "target_address_Thailand_combined.csv")


# END


target_address_Thailand_combined <- 
  readr::read_rds(
    "target_address_Thailand_combined.rds"
    )

object_Thailand_lat_lon <-
  readRDS("object_Thailand_lat_lon.rds") %>%
  dplyr::mutate(
    id = c(1:nrow(.)))%>%
  dplyr::select(-true_false) %>% 
  sf::st_drop_geometry()


object_Thailand_all <- 
  target_address_Thailand_combined %>% 
  dplyr::left_join(
    object_Thailand_lat_lon,
    by = "id"
  ) %>% 
  na.omit()

readr::write_rds(object_Thailand_all, "object_Thailand_all.rds")


object_Thailand_all <- 
  readr::read_rds(
    "object_Thailand_all.rds"
    )

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



hoge <- 
  object_Thailand_all_summary %>% 
  ggplot2::ggplot(
    aes(fill = log(SUM), geometry = geometry)
  ) +
  geom_sf()




