#################################################################### 
# Allometry using MS global ML building footprints
# Made: 8th. June 2022
# Revised: 5th. June 2023
# Yuzuru Utsunomiya, Ph. D.
# Source: 
# https://github.com/microsoft/GlobalMLBuildingFootprints#will-there-be-more-data-coming-for-other-geographies
# Projection: WGS84
# ETSG: 4326
# license: https://opendatacommons.org/licenses/odbl/
#################################################################### 
# ---- read.library.allometry ----
library(tidyverse)
library(sf)
library(spdep)
library(sfdep)
library(furrr)
library(rsample)
library(khroma)
library(patchwork)
library(ggrepel)
library(qqplotr)
# set concurrent computing plan
# multisession: use CPUs as many as possible
plan(multisession)
#
# ---- top100.sample ----
# read the data
object_Thailand <-
  readr::read_rds("object_Thailand.rds")
# sample the largest 100 objects by province
# When the 100-largest sample would be necessary,
# run the code below.
# object_Thailand_top100 <-
#   object_Thailand %>%
#   st_drop_geometry() %>%
#   group_by(province_name) %>%
#   dplyr::arrange(desc(area)) %>%
#   dplyr::slice(1:100)
# write_excel_csv(
#   object_Thailand_top100,
#   "object_Thailand_top100.csv"
# )
# 
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
# for inspection
readr::write_excel_csv(
  object_Thailand_summary,
  "object_Thailand_entire_summary.csv"
)
# ---- read.lfs.data.allometry ----
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
# # join the area data and LFS data
lfs_area_province <-
  lfs_province %>%
  dplyr::left_join(
    object_Thailand_summary,
    # largest_buildings_province_wide,
    by = c("province" = "province_name"),
    multiple = "all"
  )
# For smooth computation, remove the temporary data.
# Otherwise, the R works very slowly.
rm(object_Thailand)
# 
# ---- analysis.regression.allometry ----
# logarithmic transformation
# PRECURE TRANSFORMATION!!
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
allometry_lm <- 
  summary(
    lm(
      LF ~ Area, 
      data = lfs_area_province_log
      )
    )
# plot fitting curve
allometry_lm_plot <- 
  lfs_area_province_log %>% 
  ggplot(
    aes(
      x = Area, y = LF
      )
    ) +
  geom_point() +
  # For the regression, we need not to prepare predicted numbers.
  # Instead, we apply the geom_smooth() function.
  geom_smooth(method = "lm") +
  labs(
    x = "建物面積総和（単位：m^2、対数変換値）", 
    y = "労働力人口（単位：人、対数変換値）"
    ) +
  geom_text_repel(
    aes(
      label = province
      )
    ) +
  theme_classic()
# regression diagnosis
# https://cran.r-project.org/web/packages/qqplotr/vignettes/introduction.html
# https://www.jstor.org/stable/pdf/24591489.pdf?refreqid=excelsior%3Af6822f4619bc12382faf279533ac6c7b&ab_segments=&origin=&initiator=&acceptTC=1
allometry_lm_diag <- 
  lfs_area_province_log %>% 
  ggplot(aes(sample = LF)) +
  geom_qq_band(bandType = "ks", mapping = aes(fill = "KS"), alpha = 0.25) +
  geom_qq_band(bandType = "ts", mapping = aes(fill = "TS"), alpha = 0.25) +
  geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.25) +
  geom_qq_band(bandType = "boot", mapping = aes(fill = "Bootstrap"), alpha = 0.25) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "正規分位数", y = "標本分位数", fill = "バンドタイプ") +
  scale_fill_discrete("Bandtype") +
  theme_classic()
# 
# ----- read.data.allometry -----
# # read objects' adress of footprint data
# # NOTE
# # 1. This code is for entire data. When we obtain 
# # 2. To save computation period, we omit geometry.
# # When we draw a map with the target object,
# # make another object.
object_Thailand_lat_lon <-
  readRDS("object_Thailand_lat_lon.rds") %>%
  dplyr::mutate(
    id = c(1:nrow(.)))%>%
  dplyr::select(-true_false) %>%
  sf::st_drop_geometry()
# #
# # read shapefiles
# # using information in the shapefiles, we allocate
# # adress including province and district.
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

shp_Thailand_district <-
  sf::st_read(
    "./shapefiles/THA_adm2.shp",
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
target_address_Thailand_combined <-
  furrr::future_map_dfr(
    paste0(
      "../../southeastasiastudy/dataset/target_address_Thailand/",
      target_file_list,
      sep = ""
    ),
    data.table::fread
  )
# save the results
readr::write_rds(
  target_address_Thailand_combined,
"target_address_Thailand_combined.rds"
)
# save the combined data
target_address_Thailand_combined <-
  readr::read_rds(
    "target_address_Thailand_combined.rds"
    )
# combine necessary data
object_Thailand_all <-
  target_address_Thailand_combined %>%
  dplyr::left_join(
    object_Thailand_lat_lon,
    by = "id"
  ) %>%
  na.omit()
# save the results
# From now, we 
readr::write_rds(
  object_Thailand_all,
  "object_Thailand_all.rds"
  )

# ----- draw.moran.maps -----
# To detect agglomerated province / district, we need to
# calculate Moran's I statistics and its results on maps.
# 
# read the data
object_Thailand_all <-
  readr::read_rds(
    "object_Thailand_all.rds"
    )
# make a dataset for the map
# Using dplyr::summarise(), we need to sum up and compute
# descriptive statistics of area by province / district.
# 1. province
object_Thailand_all_summary_province_df <-
  object_Thailand_all %>%
  # To deal with the variable including area info., 
  # we need to transform the area form into numeric.
  dplyr::mutate(area = as.numeric(area)) %>%
  # grouping by province
  dplyr::group_by(province_name) %>%
  # compute descriptive statistics
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
  # handle the data above 
  # transform the data into tidy
  ?tidyr::pivot_longer(
    cols = -province_name,
    names_to = "statistics",
    values_to = "number"
  ) %>%
  # join the summary table with province-level shapefiles
  dplyr::left_join(
    shp_Thailand_province,
    # by province
    by = c(
      "province_name" = "NAME_1"
      )
  ) %>%
  # select necessary variables
  dplyr::select(
    province_name, 
    statistics, 
    number, 
    geometry
    ) %>%
  # transform the data available to sf() package
  sf::st_as_sf() %>%
  # provide projection and datum to the data
  sf::st_transform(
    crs = "+proj=longlat +datum=WGS84"
    )
# 2. district
# We omit comment whereever the same as above.
object_Thailand_all_summary_district_df <-
  object_Thailand_all %>%
  dplyr::mutate(area = as.numeric(area)) %>%
  group_by(province_name, district_name) %>%
  dplyr::summarise(
    SUM = sum(area),
    Mean = mean(area),
    Median = median(area)
  ) %>%
  ungroup() %>% 
  pivot_longer(
    cols = c(-province_name, -district_name),
    names_to = "statistics",
    values_to = "number"
  ) %>%
  dplyr::left_join(
    shp_Thailand_district,
    by = c(
      "province_name" = "NAME_1", 
      "district_name" = "NAME_2"
      )
  ) %>%
  dplyr::select(province_name, district_name, statistics, number, geometry) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84")
# 
# calculate local Moran's statistics and draw map
# NOTE
# Refer to the following website.
# https://github.com/JosiahParry/sfdep
# 1. province
object_Thailand_all_summary_province_moran <-
  object_Thailand_all_summary_province_df %>% 
  # select either Mean or SUM depending on circumstances
  dplyr::filter(statistics == "Mean") %>%
  dplyr::mutate(
    # area scaled by province's area
    area_mean = as.numeric(number / (sf::st_area(geometry) %>% units::set_units(km^2)))
  ) %>% 
  dplyr::mutate(
    # K-nearest neighbours to avoid influence of islands such as Phuket
    # We applied so-called QWEEN allocation (k = 4).
    nb = sfdep::st_knn(geometry,  k = 4),
    # When no island might not be, instead of sf::st_knn(), apply the function
    # as follows:
    # nb = sfdep::st_contiguity(geometry),
    wt = sfdep::st_weights(nb),
    # As we will apply allometric function, we need to transform the number.
    # with logarithmic transformation.
    moran_mean = sfdep::local_moran(
      log(number), 
      nb, 
      wt
      )
  ) %>% 
  tidyr::unnest(moran_mean) %>% 
  dplyr::mutate(
    pysal = ifelse(
      p_folded_sim <= 0.05, 
      as.character(pysal), 
      NA
      )
    )
# draw a map
object_Thailand_all_summary_province_moran_plot <- 
  object_Thailand_all_summary_province_moran %>% 
  ggplot2::ggplot(
    aes(
      geometry = geometry,
      fill = pysal
    )
  ) +
  geom_sf() +
  geom_sf(lwd = 0.2, color = "black") +
  labs(
    title = "Province-level moran map of building footprints (Unit: km^2, mean, log Trans.)", 
    subtitle = "Thailand",
    fill = "Folded range ranked p"
  ) +
  theme_void() +
  khroma::scale_fill_okabeito(reverse = TRUE)
print(object_Thailand_all_summary_province_moran_plot)
# save the map
ggsave(
  "object_Thailand_all_summary_province_moran_plot_mean.pdf",
  plot = object_Thailand_all_summary_province_moran_plot,
  width = 300,
  height = 300,
  units = "mm"
)
# 
# 2. district
object_Thailand_all_summary_district_moran <-
  object_Thailand_all_summary_district_df %>% 
  dplyr::filter(statistics == "Mean") %>%
  dplyr::mutate(
    # area_mean = as.numeric(number / (sf::st_area(geometry) %>% units::set_units(km^2)))
    area_mean = number
  ) %>% 
  dplyr::mutate(
    # K-nearest neighbours to avoid influence of islands such as Phuket
    nb = sfdep::st_knn(geometry,  k = 4),
    # nb = sfdep::st_contiguity(geometry),
    wt = sfdep::st_weights(nb),
    moran_mean = sfdep::local_moran(log(number), nb, wt)
  ) %>% 
  tidyr::unnest(moran_mean) %>% 
  dplyr::mutate(pysal = ifelse(p_folded_sim <= 0.05, as.character(pysal), NA))
object_Thailand_all_summary_district_moran_plot <- 
  object_Thailand_all_summary_district_moran %>% 
  ggplot2::ggplot(
    aes(
      geometry = geometry,
      fill = pysal
    )
  ) +
  geom_sf() +
  geom_sf(lwd = 0.2, color = "grey") +
  labs(
    title = "District-level moran map of building footprints (Unit: km^2, mean, log Trans.)", 
    subtitle = "Thailand",
    fill = "Folded range ranked p"
  ) +
  # overwright provinces' boundaries
  geom_sf(
    data = shp_Thailand_province, 
    aes(fill = NA), 
    lwd = 0.5, 
    color = "black"
    ) + 
  theme_void() +
  khroma::scale_fill_okabeito(reverse = TRUE)
print(object_Thailand_all_summary_district_moran_plot)
# save the figure
ggsave(
  "object_Thailand_all_summary_district_moran_plot_mean.pdf",
  plot = object_Thailand_all_summary_district_moran_plot,
  width = 300,
  height = 300,
  units = "mm"
)
# 
























# 
# draw maps by statistics
# 1. province
# object_Thailand_all_summary_map <-
#   object_Thailand_all_summary_df %>%
#   group_by(statistics) %>%
#   nest() %>%
#   dplyr::mutate(
#     map = purrr::map(
#       data,
#       ~
#         ggplot2::ggplot(
#           data = .,
#           aes(fill = number, geometry = geometry)
#         ) +
#         geom_sf() +
#         scale_fill_smoothrainbow() +
#         labs(
#           x = "Longitude",
#           y = "Latitude",
#           fill = "Statistics",
#           title = statistics
#           # subtitle = "We used entire data by province generated from Microsoft(c) building footprints.",
#           # caption = "Yuzuru Utsunomiya, Ph. D."
#           ) +
#         theme_void() +
#         theme(
#           legend.position = "none",
#           legend.key.width = unit(10, "mm")
#         )
#     )
#   )
# object_Thailand_all_summary_map_patchwork <-
#   (object_Thailand_all_summary_map$map[[1]] | object_Thailand_all_summary_map$map[[2]] | object_Thailand_all_summary_map$map[[3]] | object_Thailand_all_summary_map$map[[4]] ) /
#   (object_Thailand_all_summary_map$map[[5]] | object_Thailand_all_summary_map$map[[6]] | object_Thailand_all_summary_map$map[[7]] | object_Thailand_all_summary_map$map[[8]] )
# ggsave(
#   "object_Thailand_all_summary_map_patchwork.pdf",
#   plot = object_Thailand_all_summary_map_patchwork,
#   width = 300,
#   height = 300,
#   units = "mm"
# )
# 
# # 100 largest samples
# object_Thailand_all_summary_df_100 <-
#   object_Thailand_all %>%
#   dplyr::mutate(area = as.numeric(area)) %>%
#   group_by(province_name) %>%
#   dplyr::arrange(desc(area)) %>%
#   dplyr::slice(1:100) %>%
#   dplyr::summarise(
#     N = n(),
#     SUM = sum(area),
#     Min = min(area),
#     Mean = mean(area),
#     Median = median(area),
#     Max = max(area),
#     SD = sd(area),
#     SE = sd(area)/sqrt(n())
#   ) %>%
#   pivot_longer(
#     cols = -province_name,
#     names_to = "statistics",
#     values_to = "number"
#   ) %>%
#   dplyr::left_join(
#     shp_Thailand_province,
#     by = c("province_name" = "NAME_1")
#   ) %>%
#   dplyr::select(province_name, statistics, number, geometry)
# #
# object_Thailand_all_summary_map_100 <-
#   object_Thailand_all_summary_df_100 %>%
#   group_by(statistics) %>%
#   nest() %>%
#   dplyr::mutate(
#     map = purrr::map(
#       data,
#       ~
#         ggplot2::ggplot(
#           data = .,
#           aes(fill = number, geometry = geometry)
#         ) +
#         geom_sf() +
#         scale_fill_smoothrainbow() +
#         labs(
#           x = "Longitude",
#           y = "Latitude",
#           fill = "Statistics",
#           title = statistics,
#           subtitle = "We used picked-up data (N = 100 by province) generated from Microsoft(c) building footprints.",
#           caption = "Yuzuru Utsunomiya, Ph. D."
#         ) +
#         theme_classic() +
#         theme(
#           legend.position = "bottom",
#           legend.key.width = unit(10, "mm")
#         )
#     )
#   )
# # save the maps
# pdf(
#   "object_Thailand_all_summary_map_100.pdf",
#   width = 10,
#   height = 10
# )
# object_Thailand_all_summary_map_100$map
# dev.off()
# 

