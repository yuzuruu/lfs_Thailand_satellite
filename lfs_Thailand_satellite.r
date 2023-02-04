#################################################################### 
# All about analyses using MS global ML building footprints
# 8th. June 2022
# Yuzuru Utsunomiya
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
##
### END ---
##
#
# ---- read.data ----
# read object data
# object_Thailand_data <- sf::st_read("./object/Thailand.geojsonl")
# For google data
# 21,109,178 (= 2*41*127*2027) objects
# object_Thailand_data <-
#   sf::st_read("./object/Thailand_google.csv") %>%
#   st_as_sf(wkt = "geometry", crs = 4326)
# devtools::install_github("wpgp/foot", build_vignettes = TRUE)
# object_Vietnam_data <- sf::st_read("./object/Vietnam.geojsonl")
# object_Cambodia_data <- sf::st_read("./object/Cambodia.geojsonl")
# Remove objects when not in use
# rm(object_Thailand)
# rm(object_Vietnam)
# rm(object_Cambodia)

# read shapefiles by country
shp_Thailand <- 
  sf::st_read(
    # "./shapefiles/THA_adm3.shp", 
    "./shapefiles/THA_adm1.shp", 
    options = "ENCODING=UTF-8", 
    stringsAsFactors=FALSE
  ) %>% 
  dplyr::mutate_if(
    is.character, 
    enc2utf8
  )
# ---- read.function ----
# a function find address from lat / lon
# We thank following links.
# https://qiita.com/nozma/items/808bce2f496eabd50ff1
# https://qiita.com/uri/items/69b2c05f7b3a21d3aad3
find_city <- function(sp_polygon = df, lon = lon, lat = lat){
  # find a polygon containing a certain pair of lon / lat
  which.row <- 
    sf::st_contains(
      sp_polygon, 
      sf::st_point(
        c(
          lon, 
          lat
        )
      ), 
      sparse = FALSE
    ) %>%  
    grep(TRUE, .)
  # If not, leave a warning message
  if (identical(which.row, integer(0)) == TRUE) {
    # message("指定した座標がポリゴンに含まれません")
    return(NA)
    # If exist, obtain information of coordinates
  } else {
    geos <- 
      sp_polygon[which.row, ] %>%
      # transform from factor to character
      dplyr::mutate_if(
        is.factor, 
        as.character
      ) %>% 
      # obtain necessary part of the shapefile
      dplyr::mutate_at(
        # dplyr::vars(NAME_1, NAME_2, NAME_3), 
        dplyr::vars(NAME_1), 
        dplyr::funs(
          dplyr::if_else(
            # Is it NA?
            condition = is.na(.),
            # if NA, return blank
            true = "", 
            # if not, use it
            false = .
          )
        )
      )
    # make a dataset of administrative boundaries
    # Names and IDs are obtained from shapefiles
    res <- tibble::data_frame(
      province_code = geos$ID_1,
      # district_code = geos$ID_2,
      # town_code = geos$ID_3,
      province_name = geos$NAME_1,
      # district_name = geos$NAME_2,
      # town_name = geos$NAME_3
    )
    # for inspecting function movement
    print(res)
    # return results
    return(res)
  }
}
#
#
##
### END ---
##
#
# 
# ---- make.area.data.google ----
# WARNING
# This process needs long computation periods.
# object_Thailand_lat_lon <-
#   # provide data
#   object_Thailand_data%>%
#   # evaluate the geometries whether they are validated
#   # If not, functions below will not work.
#   # In detail, refer to the following page.
#   # https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using
#   dplyr::mutate(
#     true_false = sf::st_is_valid(.)
#     ) %>%
#   # select vali data
#   dplyr::filter(true_false == "TRUE") %>%
#   # add longitude and latitude
#   dplyr::mutate(
#     area = sf::st_area(.),
#     lon = st_coordinates(sf::st_centroid(.))[,1],
#     lat = st_coordinates(sf::st_centroid(.))[,2]
#     )
# 
# saveRDS(object_Thailand_lat_lon, "object_Thailand_lat_lon_google.rds")
# rm(object_Thailand_lat_lon_google)
# gc()
# gc()
# ---- make.area.data ----
# # WARNING
# # This process needs long computation periods.
# object_Thailand_lat_lon <- 
#   # provide data
#   object_Thailand_data%>%
#   # evaluate the geometries whether they are validated 
#   # If not, functions below will not work.
#   # In detail, refer to the following page.
#   # https://gis.stackexchange.com/questions/404385/r-sf-some-edges-are-crossing-in-a-multipolygon-how-to-make-it-valid-when-using
#   dplyr::mutate(
#     true_false = sf::st_is_valid(.)
#     ) %>%
#   # select vali data
#   dplyr::filter(true_false == "TRUE") %>%
#   # add longitude and latitude
#   dplyr::mutate(
#     area = sf::st_area(.),
#     lon = st_coordinates(sf::st_centroid(.))[,1],
#     lat = st_coordinates(sf::st_centroid(.))[,2]
#     )
# saveRDS(object_Thailand_lat_lon, "object_Thailand_lat_lon.rds")
# rm(object_Thailand_lat_lon)
# gc()
# gc()
# read
# object_Thailand_sample <- 
#   readRDS("object_Thailand_lat_lon.rds") %>% 
#   dplyr::mutate(
#     id = c(1:nrow(.))) %>%
#   dplyr::select(-true_false, -area) %>% 
#   sf::st_drop_geometry() 

object_Thailand_sample_google <-
  readRDS("object_Thailand_lat_lon_google.rds") %>%
  dplyr::mutate(
    id = c(1:nrow(.))) %>%
  dplyr::select(-true_false, -area) %>%
  sf::st_drop_geometry()
# %>% 
#   dplyr::sample_frac(size = 0.05)

# primitive but prudent way
set.seed(123)
# obtain group id
# NOTE
# We need to separate the data without surplus
# This time, our data can be divided by an appropriate number (16).
# idx <- sample(1:nrow(object_Thailand_sample)/16)
# cv <- 
#   split(
#     idx, 
#     ceiling(
#       seq_along(idx) / floor(length(idx) / 16
#       )
#     )
#   ) %>% 
#   bind_rows() %>% 
#   as_tibble() %>% 
#   data.table::setnames(
#     c(
#       "group01","group02","group03","group04","group05","group06","group07","group08","group09","group10",
#       "group11","group12","group13","group14","group15","group16"
#     )
#   ) %>% 
#   tidyr::pivot_longer(
#     cols = everything(),
#     names_to = "group",
#     values_to = "id"
#   )
# # combine the original data and randomly-allocated group
# object_Thailand_sample_group <- 
#   object_Thailand_sample%>% 
#   left_join(
#     cv, 
#     by = "id"
#   )
# google (41)
# https://katate.hateblo.jp/entry/2021/02/13/203139

idx <- sample(1:(nrow(object_Thailand_sample_google)))
cv <- 
  split(
    idx, 
    ceiling(
      seq_along(idx) / floor(length(idx) / 41)
    )
  ) %>% 
  bind_rows() %>% 
  as_tibble() %>% 
  data.table::setnames(
    paste0(
      "group",
      c(1:41)
    )
  ) %>% 
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "id"
  )
# combine the original data and randomly-allocated group
object_Thailand_sample_group_google <- 
  object_Thailand_sample_google %>% 
  left_join(
    cv, 
    by = "id"
  )

# ---- obtain.province.name.ms ----
# obtain provinces' name by point
# microsoft
# # 01
# object_Thailand_address_01 <- object_Thailand_sample_group %>%filter(group == "group01") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_01,"object_Thailand_address_hoge_01.rds")
# # 02
# object_Thailand_address_02 <- object_Thailand_sample_group %>%filter(group == "group02") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_02,"object_Thailand_address_hoge_02.rds")
# # 03
# object_Thailand_address_03 <- object_Thailand_sample_group %>%filter(group == "group03") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_03,"object_Thailand_address_hoge_03.rds")
# # 04
# object_Thailand_address_04 <- object_Thailand_sample_group %>%filter(group == "group04") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_04,"object_Thailand_address_hoge_04.rds")
# # 05
# object_Thailand_address_05 <- object_Thailand_sample_group %>%filter(group == "group05") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_05,"object_Thailand_address_hoge_05.rds")
# # 06
# object_Thailand_address_06 <- object_Thailand_sample_group %>%filter(group == "group06") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_06,"object_Thailand_address_hoge_06.rds")
# # 07
# object_Thailand_address_07 <- object_Thailand_sample_group %>%filter(group == "group07") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_07,"object_Thailand_address_hoge_07.rds")
# # 08
# object_Thailand_address_08 <- object_Thailand_sample_group %>%filter(group == "group08") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_08,"object_Thailand_address_hoge_08.rds")
# # 09
# object_Thailand_address_09 <- object_Thailand_sample_group %>%filter(group == "group09") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_09,"object_Thailand_address_hoge_09.rds")
# # 10
# object_Thailand_address_10 <- object_Thailand_sample_group %>%filter(group == "group10") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_10,"object_Thailand_address_hoge_10.rds")
# # 11
# object_Thailand_address_11 <- object_Thailand_sample_group %>%filter(group == "group11") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_11,"object_Thailand_address_hoge_11.rds")
# 12
# object_Thailand_address_12 <- object_Thailand_sample_group %>%filter(group == "group12") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_12,"object_Thailand_address_hoge_12.rds")
# # 13
# object_Thailand_address_13 <- object_Thailand_sample_group %>%filter(group == "group13") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_13,"object_Thailand_address_hoge_13.rds")
# # 14
# object_Thailand_address_14 <- object_Thailand_sample_group %>%filter(group == "group14") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_14,"object_Thailand_address_hoge_14.rds")
# # 15
# object_Thailand_address_15 <- object_Thailand_sample_group %>%filter(group == "group15") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_15,"object_Thailand_address_hoge_15.rds")
# # 16
# object_Thailand_address_16 <- object_Thailand_sample_group %>%filter(group == "group16") %>% dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%tibble()
# saveRDS(object_Thailand_address_16,"object_Thailand_address_hoge_16.rds")
# 

# ---- obtain.province.name.google ----

# object_Thailand_address_google_01 <-
#   object_Thailand_sample_group_google %>%
#   dplyr::select(area_in_meters, lon, lat, id, group) %>% 
#   filter(group == "group01") %>%
#   head() %>% 
#   dplyr::mutate(area_info = furrr::future_map2(.x = lon,.y = lat, ~ find_city(sp_polygon = shp_Thailand, lon = .x, lat = .y))) %>%
#   tibble()
# saveRDS(object_Thailand_address_google_01,"object_Thailand_address_google_01.rds")
# rm(object_Thailand_address_google_01)
# gc()
# gc()
#
# google
# # 01
source("object_Thailand_address_google_01.r")


#
#
##
### END ---
##
#

# EXPERIMENTAL
# NO USE
# This procedure needs a lot of memory. Sometimes it does not finish correctly.
# # set concurrent computing plan
# # multisession: use CPUs as many as possible
# plan(multisession)
# # obtain address from shapefiles
# object_Thailand_address <-
#   object_Thailand_sample %>%
#   # obtain area information from the shapefile
#   dplyr::mutate(
#     # using furrr() package enables us to concurrent computing!!
#     # It raises up computation period dramatically!!
#     # In detail, refer to the following page.
#     # https://blog.atusy.net/2018/12/06/furrr/
#     # Previously, we used to use the following code with purrr::map2_dfr()
#     # area_info = purrr::map2_dfr(
#     area_info = furrr::future_map2(
#       .x = lon,
#       .y = lat,
#       ~
#         find_city(
#           sp_polygon = shp_Thailand,
#           lon = .x,
#           lat = .y
#           )
#       )
#     ) %>%
#   tibble()
# object_Thailand_address
# object_Thailand_address$area_info
# 
# 
# saveRDS(object_Thailand_address, "object_Thailand_address.rds")
# # rm(object_Thailand_address)
# gc()
# gc()
# 
# object_Thailand_address <- readRDS("object_Thailand_address.rds")
# 
# 
# 
# # competed!!
# object_Thailand <- 
#   bind_cols(
#     object_Thailand_address$area_info, 
#     object_Thailand_data
#     ) %>% 
#   select(-area_info, -true_false)
# # save the completed data
# saveRDS(object_Thailand, "object_Thailand.rds")


