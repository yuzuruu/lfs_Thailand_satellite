#################################################################### 
# All about analyses using MS global ML building footprints
# Est. 8th. June 2022
# Revised 2nd. August 2022
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
library(units)
library(khroma)
library(viridis)
library(ggmap)
library(ggsn)
# set concurrent computing plan
# multisession: use CPUs as many as possible
plan(multisession)
#
#
##
### END ---
##
#
# # ---- read.data ----
# # read object data
# object_Thailand_data <- sf::st_read("./object/Thailand.geojsonl")
# For google data
# object_Thailand_data <- sf::st_read("./object/Thailand_google.csv")
# object_Vietnam_data <- sf::st_read("./object/Vietnam.geojsonl")
# object_Cambodia_data <- sf::st_read("./object/Cambodia.geojsonl")
# # Remove objects when not in use
# rm(object_Thailand)
# rm(object_Vietnam)
# rm(object_Cambodia)
# # read shapefiles by country
# shp_Thailand <-
#   sf::st_read(
#     # "./shapefiles/THA_adm3.shp",
#     "./shapefiles/THA_adm1.shp",
#     options = "ENCODING=UTF-8",
#     stringsAsFactors=FALSE
#   ) %>%
#   dplyr::mutate_if(
#     is.character,
#     enc2utf8
#   )
# # 
# # ---- read.function ----
# # a function find address from lat / lon
# # We thank following links.
# # https://qiita.com/nozma/items/808bce2f496eabd50ff1
# # https://qiita.com/uri/items/69b2c05f7b3a21d3aad3
# find_city <- function(sp_polygon = df, lon = lon, lat = lat){
#   # find a polygon containing a certain pair of lon / lat
#   which.row <- 
#     sf::st_contains(
#       sp_polygon, 
#       sf::st_point(
#         c(
#           lon, 
#           lat
#         )
#       ), 
#       sparse = FALSE
#     ) %>%  
#     grep(TRUE, .)
#   # If not, leave a warning message
#   if (identical(which.row, integer(0)) == TRUE) {
#     # message("指定した座標がポリゴンに含まれません")
#     return(NA)
#     # If exist, obtain information of coordinates
#   } else {
#     geos <- 
#       sp_polygon[which.row, ] %>%
#       # transform from factor to character
#       dplyr::mutate_if(
#         is.factor, 
#         as.character
#       ) %>% 
#       # obtain necessary part of the shapefile
#       dplyr::mutate_at(
#         # dplyr::vars(NAME_1, NAME_2, NAME_3), 
#         dplyr::vars(NAME_1), 
#         dplyr::funs(
#           dplyr::if_else(
#             # Is it NA?
#             condition = is.na(.),
#             # if NA, return blank
#             true = "", 
#             # if not, use it
#             false = .
#           )
#         )
#       )
#     # make a dataset of administrative boundaries
#     # Names and IDs are obtained from shapefiles
#     res <- tibble::data_frame(
#       province_code = geos$ID_1,
#       # district_code = geos$ID_2,
#       # town_code = geos$ID_3,
#       province_name = geos$NAME_1,
#       # district_name = geos$NAME_2,
#       # town_name = geos$NAME_3
#     )
#     # for inspecting function movement
#     print(res)
#     # return results
#     return(res)
#   }
# }
#
# # ---- make.area.data ----
# # WARNING
# # This process needs long computation periods.
# object_Thailand_lat_lon <-
#   # provide data
#   object_Thailand_data %>%
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
# # ---- obtain.address.from.shapefiles ----
# # read the lon-lat data
# object_Thailand_sample <- 
#   readRDS("object_Thailand_lat_lon.rds") %>% 
#   dplyr::mutate(
#     id = c(1:nrow(.))) %>%
#   dplyr::select(-true_false, -area) %>% 
#   sf::st_drop_geometry() 
# # 
# # primitive but prudent way
# set.seed(123)
# # obtain group id
# # NOTE
# # We need to separate the data without surplus
# # This time, our data can be divided by an appropriate number (16).
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
# # obtain provinces' name by point
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
# # 12
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
# object_Thailand_address <-
#   # obtain path list of the separated address file
#   list.files(path = "./address", pattern = "*.rds") %>% 
#   # add strings to make complete paths
#   paste0("./address/",.) %>% 
#   # read the target files listed above
#   purrr::map_df(
#     ., 
#     readRDS
#     ) %>% 
#   # rows containing NA (lgl[1]) list
#   # if the variable named "area_info" contains NA, it will not be "tibble" but "lgl".
#   # Using the characteristics, we remove the list sorely with NA.
#   dplyr::filter(
#     map_lgl(
#       area_info, 
#       is_tibble
#       )
#     ) %>% 
#   # bind the lists containing address
#   mutate(
#     areainfo = dplyr::bind_rows(.$area_info)
#     ) %>% 
#   # pick up necessary info
#   mutate(
#     province_code = areainfo$province_code,
#     province_name = areainfo$province_name
#     ) %>% 
#   # select necessary variables
#   select(lon, lat, id, group, province_code, province_name)
# # save the results
# saveRDS(object_Thailand_address, "object_Thailand_address.rds")
# # save the results as a .csv file
# # NOTE
# # The csv file is huge. No apps can open that.
# readr::write_excel_csv(
#   object_Thailand_address, 
#   "object_Thailand_address.csv"
#   )
# 
# # Final session
# # read the lat-lon data with geometry
# object_Thailand_lat_lon <- 
#   readRDS("object_Thailand_lat_lon.rds") %>% 
#   dplyr::mutate(
#     id = c(1:nrow(.))) %>%
#   dplyr::select(-true_false, -lat, -lon) 
# object_Thailand_address <- 
#   readRDS("object_Thailand_address.rds")
# # read the address data
# object_Thailand <- 
#   object_Thailand_lat_lon %>% 
#   inner_join(object_Thailand_address, by = "id")
# # save
# saveRDS(object_Thailand, "object_Thailand.rds")
# 
# 
# ### ---- make.map.summary ----
# # draw a map
# # read the original files
# object_Thailand <- readRDS("object_Thailand.rds")
# map_Thailand <- 
#   sf::read_sf("./shapefiles/THA_adm1.shp") 
# #
# # Draw maps
# object_Thailand_map <-
#   object_Thailand %>%
#   ggplot()+
#   geom_sf(data = map_Thailand, fill = NA, inherit.aes = FALSE, size = 0.1) +
#   geom_sf(size = 0.005) +
#   labs(
#     title = "Map of building footprint in Thailand",
#     subtitle = "We obtained the data from the Microsoft Inc. \n (https://github.com/microsoft/GlobalMLBuildingFootprints#will-there-be-more-data-coming-for-other-geographies)",
#     x = "Longitude",
#     y = "Latitude"
#     ) +
#   theme_classic() +
#   theme(
#     axis.text = element_text(size = 20),
#     axis.title = element_text(size = 30)
#   ) +
#   scalebar(
#     data = object_Thailand,
#     dist = 100,
#     dist_unit = "km",
#     transform = TRUE,
#     location = "bottomleft"
#     ) +
#   north(
#     data = object_Thailand,
#     symbol = 16,
#     location = "bottomright"
#   )
# #
# # save the map with .pdf format
# # NOTE
# # Size of the map is huge. It takes long time to display.
# # When you would like to look at a building, please enlarge.
# # For size of the buildings, the map is huge. Enlarging the
# # map, you will be able to find the shape of the building.
# # At a sight, the buildings looks like mass of dots.
# ggsave(
#   "object_Thailand_map.pdf",
#   plot = object_Thailand_map,
#   width = 500,
#   height = 500,
#   units = "mm",
#   limitsize = FALSE
#   )
# 
# make a summary table

object_Thailand <- readRDS("object_Thailand.rds")

object_Thailand_summary <- 
  object_Thailand %>% 
  as_tibble() %>% 
  dplyr::group_by(province_name) %>% 
  tidyr::nest() %>% 
  mutate(
    N = purrr::map_dbl(data, ~ length(.$area)),
    Min = purrr::map_dbl(data, ~ min(.$area)),
    Mean = purrr::map_dbl(data, ~ mean(.$area)),
    Median = purrr::map_dbl(data, ~ median(.$area)),
    Max = purrr::map_dbl(data, ~ max(.$area)),
    Sum = purrr::map_dbl(data, ~ sum(.$area)),
    SD = purrr::map_dbl(data, ~ sd(.$area)),
  ) %>% 
  ungroup() %>% 
  dplyr::select(-data)
# write the summary table for inspection
readr::write_excel_csv(object_Thailand_summary, "object_Thailand_summary.csv")




# 建物面積合計で労働力人口をうらなう
# データ読み込み
object_Thailand_summary <- readr::read_csv("object_Thailand_summary.csv")
current_lfs <- readRDS("../lfs_Thailand_province/fit_clf_01_summary_x_yhat_pd.rds")
# データ結合
current_lfs_yhat <- 
  current_lfs %>% 
  dplyr::filter(stringr::str_detect(variable, "(^yhat)")) %>% 
  dplyr::filter(year_month == "2020-01-01") %>%
  dplyr::select(mean, median, province) %>% 
  dplyr::mutate(
    province = stringr::str_replace(province, "_", " ")
  ) %>% 
  dplyr::left_join(object_Thailand_summary, by = c("province" = "province_name")) %>% 
  



# 























# check whether a building is a factory or not
# Yes: 1
# No: 0
# 1. set API key for google
# The google service requests us to obtain and input the key.
# API key: For maps
source("key.r")
has_google_key()
# 2. pick up 20 widest buildings by province
object_Thailand_top20 <- 
  object_Thailand %>% 
  dplyr::group_by(province_name) %>% 
  dplyr::slice_max(area, n = 20) %>% 
  dplyr::ungroup()
# 3. obtain satellite image from google
object_Thailand_satellite_image <-
  # th_sf_sample$sample_grid %>%
  # # bind the grids' coordinates together
  # base::do.call(dplyr::bind_rows,.) %>%
  # obtain the satellite imagery
  object_Thailand_top20 %>%
  dplyr::mutate(
    satellite = purrr::map2(
      lon,
      lat,
      function(lon, lat)
      {
        ggmap::get_map(
          location = c(
            lon = lon,
            lat = lat
          ),
          maptype = "satellite",
          size = c(320, 320),
          scale = 1,
          format = "png",
          zoom = 18 # <- BEWARE!!
        )
      }
    )
  ) 
# 4. save the results
# NOTE to save free API limit, we need to reuse the results.
# For the reuse, we need to save the results as a .rds file.
saveRDS(object_Thailand_satellite_image, "object_Thailand_satellite_image.rds")
# 5. print the imagery for inspection
pdf("object_Thailand_satellite_image_check.pdf")
# purrr::pmap() function prefers list for input data
  list(
    object_Thailand_satellite_image$satellite,
    object_Thailand_satellite_image$lon,
    object_Thailand_satellite_image$lat,
    object_Thailand_satellite_image$province_name,
    object_Thailand_satellite_image$id
    ) %>% 
    purrr::pmap(
      function(satellite, lon, lat, province_name, id){
        # plot the image
        ggmap::ggmap(ggmap = satellite) +
        # add province's name
        annotate(
          "text", 
          x = lon-0.001, 
          y = lat-0.0015, 
          size = 5, 
          colour = "white", 
          label = province_name
        ) +
          # add id
        annotate(
          "text", 
          x = lon, 
          y = lat-0.0015, 
          size = 5, 
          colour = "white", 
          label = id
        )
      }
    )
dev.off()
#
#
##
### END ---
##
#
