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
# shp_Thailand <- 
#   sf::st_read(
#     "./shapefiles/THA_adm3.shp",
#     # "./shapefiles/THA_adm1.shp", 
#     options = "ENCODING=UTF-8", 
#     stringsAsFactors=FALSE
#   ) %>% 
#   dplyr::mutate_if(
#     is.character, 
#     enc2utf8
#   )
# ---- read.function ----
# a function find address from lat / lon
# We thank following links.
# https://qiita.com/nozma/items/808bce2f496eabd50ff1
# https://qiita.com/uri/items/69b2c05f7b3a21d3aad3
# find_city <- 
#   function(sp_polygon = df, lon = lon, lat = lat){
#     # find a polygon containing a certain pair of lon / lat
#     which.row <- 
#       sf::st_contains(
#         sp_polygon, 
#         sf::st_point(
#           c(
#             lon, 
#             lat
#           )
#         ), 
#         sparse = FALSE
#       ) %>%  
#       grep(TRUE, .)
#     # If not, leave a warning message
#     # -> deliver values ("hoge") to avoid malfunction.
#     # We will remove NA by the values later. 
#     if (identical(which.row, integer(0)) == TRUE) {
#       # Original code provides the following message.
#       # message("指定した座標がポリゴンに含まれません")
#       geos <- 
#         data.frame(
#           ID_1 = "hoge", 
#           NAME_1 = "hoge"
#         )
#     }
#     # If exist, obtain information of coordinates
#     else {
#       geos <- 
#         sp_polygon[which.row, ] %>%
#         # transform from factor to character
#         dplyr::mutate_if(
#           is.factor, 
#           as.character
#         ) %>% 
#         # obtain necessary part of the shapefile
#         dplyr::mutate_at(
#           # dplyr::vars(NAME_1, NAME_2, NAME_3), 
#           dplyr::vars(NAME_1), 
#           dplyr::funs(
#             dplyr::if_else(
#               # Is it NA?
#               condition = is.na(.),
#               # if NA, return blank
#               true = "", 
#               # if not, use it
#               false = .
#             )
#           )
#         )
#       # make a dataset of administrative boundaries
#       # Names and IDs are obtained from shapefiles
#       res <- 
#         tibble::data_frame(
#           province_code = geos$ID_1,
#           # district_code = geos$ID_2,
#           # town_code = geos$ID_3,
#           province_name = geos$NAME_1,
#           # district_name = geos$NAME_2,
#           # town_name = geos$NAME_3
#         )
#       # for inspecting function movement
#       print(res)
#       # return results
#       return(res)
#     }
#   }
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
# 
# object_Thailand_sample_google <-
#   readRDS("object_Thailand_lat_lon_google.rds") %>%
#   dplyr::mutate(
#     id = c(1:nrow(.))) %>%
#   dplyr::select(-true_false, -area) %>%
#   sf::st_drop_geometry()
# # save the geometry-omitted object to save computation period
# saveRDS(
#   object_Thailand_sample_google, 
#   "object_Thailand_sample_google.rds"
#   )
# 
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
# 
# 
# object_Thailand_sample_google <- 
#   readRDS(
#     "object_Thailand_sample_google.rds"
#     )
# 
# idx <- sample(1:(nrow(object_Thailand_sample_google)))
# cv <- 
#   split(
#     idx, 
#     ceiling(
#       seq_along(idx) / floor(length(idx) / 2027)
#     )
#   ) %>% 
#   bind_rows() %>% 
#   as_tibble() %>% 
#   data.table::setnames(
#     paste0(
#       "group",
#       c(1:2027)
#       )
#   ) %>% 
#   tidyr::pivot_longer(
#     cols = everything(),
#     names_to = "group",
#     values_to = "id"
#   )
# # combine the original data and randomly-allocated group
# object_Thailand_sample_group_google <- 
#   object_Thailand_sample_google %>% 
#   left_join(
#     cv, 
#     by = "id"
#   )
# saveRDS(
#   object_Thailand_sample_group_google,
#   "object_Thailand_sample_group_google.rds"
# )
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
# set a random seed to prove repricability
set.seed(123)
# read the data including target objects
object_Thailand_sample_group_google_100 <- 
  readRDS(
    "object_Thailand_sample_group_google.rds"
  ) %>% 
  # separate by the designated group
  dplyr::group_by(
    group
  ) %>% 
  # sample by group
  # We sampled 106 samples per group.
  dplyr::sample_frac(
    size = 0.01
  ) %>% 
  # relase the group
  dplyr::ungroup() 
# provide the N. of group (2027)
group_2027 <- 
  levels(
    factor(
      object_Thailand_sample_group_google_100$group
    )
  )
# end

# obtain province name by building
# NOTICE
# This process consumes Approx. 1 Min./ sheet (106 observations),
# equivalent to 45 hours in total.
# object_Thailand_sample_group_google_100_address <-
#   for(i in 1:length(group_2027)){
#     target <- filter(
#       object_Thailand_sample_group_google_100, 
#       group == group_2027[i]
#     )
#     target_address <-
#       target %>%
#       dplyr::mutate(
#         area_info = furrr::future_map2_dfr(
#           .x = lon, 
#           .y = lat, 
#           ~ try(
#             find_city(
#               sp_polygon = shp_Thailand, 
#               lon = .x, 
#               lat = .y
#             )
#           )
#         )
#       ) %>%
#       tibble() 
#     # save the computation results
#     write_excel_csv(
#       # fix target column
#       bind_cols(
#         id = target_address$id, 
#         province_code = target_address$area_info$province_code,
#         province_name = target_address$area_info$province_name
#       ) %>% 
#         # remove rows containing NA
#         na.omit(), 
#       file = paste0("target_address/",target_address$group[1], ".csv")
#     )
#     # for larger data, enable the gc() function
#     gc()
#     gc()
#   }
# # 
# ---- top100 ----
# read the data
object_Thailand <- 
  readRDS("object_Thailand.rds")
# sample the largest 100 objects by province
object_Thailand_top100 <- 
  object_Thailand %>% 
  st_drop_geometry() %>% 
  group_by(province_name) %>% 
  dplyr::arrange(desc(area)) %>% 
  dplyr::slice(1:100)
write_excel_csv(
  object_Thailand_top100,
  "object_Thailand_top100.csv"
  )
# summary statistics
object_Thailand_summary <- 
  object_Thailand %>%
  dplyr::mutate(
    area = as.numeric(area)
  ) %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(area, id, province_name) %>% 
  dplyr::group_by(province_name) %>% 
  dplyr::summarise(
    N = n(),
    Min. = min(area),
    Mean = mean(area),
    Median = median(area),
    Max. = max(area),
    SD = sd(area),
    SE = sd(area)/sqrt(n()),
    area_sum = sum(area)
  )
readr::write_excel_csv(
  object_Thailand_summary,
  "object_Thailand_entire_summary.csv"
)
# ---- read.lfs.data ----
# read the N. of current labor force
# variable entitled "year" should be adjusted
lfs_province <- 
  readr::read_csv("../lfs_Thailand_province/fit_clf_01_summary_x_yhat_pd.csv") %>% 
  dplyr::mutate(
    # replace all the underscore ("_") into brank (" ").
    province = stringr::str_replace_all(
      province,
      "_", # 空白を
      " "  # アンダースコアにすべて置き換える
    )
  ) %>% 
  # choose requisite data
  dplyr::filter(
    parameter == "yhat" & year_month == "2017-10-01"
  )
# join the area data and LFS data
lfs_area_province <- 
  lfs_province %>% 
  dplyr::left_join(
    object_Thailand_summary,
    # largest_buildings_province_wide, 
    by = c("province" = "province_name"),
    multiple = "all"
  )
# 
# ---- analysis.allometry ----
# logarithmic transformation
lfs_area_province_log <- 
  lfs_area_province %>% 
  dplyr::select(province, mean, area_sum) %>% 
  # dplyr::select(mean, business, culture, industry)
  dplyr::mutate(
    LF = log(mean),
    Area = log(area_sum)
  ) 
# analysis
# linear regression
summary(lm(LF ~ Area, data = lfs_area_province_log))
# plot 
# Bayesian regression
allometry_bayes <- 
  brms::brm(formula = LF ~ Area,
            family = student(link = "identity"),
            cores = 4,
            backend = "cmdstanr",
            data = lfs_area_province_log
  )
plot(allometry_bayes)
WAIC(allometry_bayes)



plot(
  conditional_effects(allometry_bayes), 
  points =TRUE, 
  ask = FALSE,
  theme = theme_classic()
  )
ggsave(
  "allometry_all.pdf",
  width = 150, 
  height = 150,
  units = "mm"
  )


















# ---- provide.address.all ----
# read data
object_Thailand_sample <-
  readRDS("object_Thailand_lat_lon.rds") %>%
  dplyr::mutate(
    id = c(1:nrow(.))) %>%
  dplyr::select(-true_false, -area) %>%
  sf::st_drop_geometry()
# read shapefiles
shp_Thailand <- 
  sf::st_read(
    "./shapefiles/THA_adm3.shp",
    # "./shapefiles/THA_adm1.shp", 
    options = "ENCODING=UTF-8", 
    stringsAsFactors=FALSE
  ) %>% 
  dplyr::mutate_if(
    is.character, 
    enc2utf8
  )
# a function to find province, district, and town
find_city <- 
  function(sp_polygon = df, lon = lon, lat = lat){
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
    # -> deliver values ("hoge") to avoid malfunction.
    # We will remove NA by the values later. 
    if (identical(which.row, integer(0)) == TRUE) {
      # Original code provides the following message.
      # message("指定した座標がポリゴンに含まれません")
      geos <- 
        data.frame(
          ID_1 = "hoge", 
          NAME_1 = "hoge",
          ID_2 = "hoge", 
          NAME_2 = "hoge",
          ID_3 = "hoge",
          NAME_3 = "hoge"
        )
    }
    # If exist, obtain information of coordinates
    else {
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
      res <- 
        tibble::data_frame(
          province_code = geos$ID_1,
          district_code = geos$ID_2,
          province_name = geos$NAME_1,
          district_name = geos$NAME_2,
        )
      # for inspecting function movement
      # print(res)
      # return results
      return(res)
    }
  }
# allocate group randomly
# to ******, we set the random seed.
sample(123)
object_Thailand_sample_group <- 
  object_Thailand_sample %>% 
  dplyr::mutate(
    group = rep(
      paste0(
        "group",
        sample(
          c(1:125000),
          replace = FALSE
          )
        ),
      each = 800
      )[c(1:nrow(.))]
  )
# obtain the group level to use the following for loop
group_level <-
  levels(
    factor(
      object_Thailand_sample_group$group
    )
  )
# obtain address by group and object individually
object_Thailand_sample_group_address <-
  for(i in 1:length(group_level)){
      target <- filter(
      object_Thailand_sample_group,
      group == group_level[i]
    )
    target_address <-
      target %>%
      dplyr::mutate(
        area_info = furrr::future_map2_dfr(
          .x = lon,
          .y = lat,
          ~ try(
            find_city(
              sp_polygon = shp_Thailand,
              lon = .x,
              lat = .y
            )
          )
        )
      ) %>%
      tibble()
    # save the computation results
    write_excel_csv(
      # fix target column
      bind_cols(
        id = target_address$id,
        province_code = target_address$area_info$province_code,
        province_name = target_address$area_info$province_name,
        district_code = target_address$area_info$district_code,
        district_name = target_address$area_info$district_name
      ) %>%
        # remove rows containing NA
        na.omit(),
      file = paste0("target_address_Thailand/",target_address$group[1], ".csv")
    )
    # for larger data, enable the gc() function
    # gc()
    # gc()
  }
# make a list of generated csv files
# Reference:
# https://qiita.com/Ringa_hyj/items/434e3a6794bb7ed8ee14
target_file_list <-
  dir(
    path = "target_address_mekong",
    pattern = "*.csv"
    )
# save the generated csv files
# NOTE
# There exist huge number of csv files.
# vroom::vroom() might not be able to use.
# Then, use furrr::future_map_dfr() and data.table::fread() instead.
# vroom::vroom()
target_address_mekong_combined <-
  vroom::vroom(
    paste0(
      "target_address_mekong/",
      target_file_list,
      sep = ""
      )
    ) 
# furrr::future_map_dfr() and data.table::fread()
target_address_mekong_combined <-
  furrr::future_map_dfr(
    paste0(
      "target_address_mekong/",
      target_file_list,
      sep = ""
    ),
    data.table::fread
  )
readr::write_rds(
  target_address_mekong_combined, "target_address_mekong_combined.rds")

# END