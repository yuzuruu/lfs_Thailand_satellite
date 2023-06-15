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
# object_Thailand_lat_lon <-
#   readRDS("object_Thailand_lat_lon.rds") %>%
#   dplyr::mutate(
#     id = c(1:nrow(.)))%>%
#   dplyr::select(-true_false) %>%
#   sf::st_drop_geometry()
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
# #
# # make a list of generated csv files
# # Reference:
# # https://qiita.com/Ringa_hyj/items/434e3a6794bb7ed8ee14
# target_file_list <-
#   dir(
#     path = "../../southeastasiastudy/dataset/target_address_Thailand",
#     pattern = "*.csv"
#   )
# # save the generated csv files
# # NOTE
# # There exist huge number of csv files.
# # This procedure needs looooong computation period.
# # Comment out when not in use.
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
# #
# target_address_Thailand_combined <-
#   readr::read_rds(
#     "target_address_Thailand_combined.rds"
#     )
# # # combine necessary data
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
# ----- draw.maps -----
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
  dplyr::select(province_name, N, SUM, Min, Mean, Median, Max, SD, SE, geometry) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs =  "+proj=longlat +datum=WGS84")

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
  dplyr::select(province_name, statistics, number, geometry) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs =  "+proj=longlat +datum=WGS84")

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
        labs(
          x = "Longitude",
          y = "Latitude",
          fill = "Statistics",
          title = statistics
          # subtitle = "We used entire data by province generated from Microsoft(c) building footprints.",
          # caption = "Yuzuru Utsunomiya, Ph. D."
          ) +
        theme_void() +
        theme(
          legend.position = "none",
          legend.key.width = unit(10, "mm")
        )
    )
  )

object_Thailand_all_summary_map_patchwork <-
  (object_Thailand_all_summary_map$map[[1]] | object_Thailand_all_summary_map$map[[2]] | object_Thailand_all_summary_map$map[[3]] | object_Thailand_all_summary_map$map[[4]] ) /
  (object_Thailand_all_summary_map$map[[5]] | object_Thailand_all_summary_map$map[[6]] | object_Thailand_all_summary_map$map[[7]] | object_Thailand_all_summary_map$map[[8]] )
ggsave(
  "object_Thailand_all_summary_map_patchwork.pdf",
  plot = object_Thailand_all_summary_map_patchwork,
  width = 300,
  height = 300,
  units = "mm"
)
# 
# save the maps
pdf(
  "object_Thailand_all_summary_map.pdf",
  width = 10,
  height = 10
  )
object_Thailand_all_summary_map$map
dev.off()
# 
# 100 largest samples
object_Thailand_all_summary_df_100 <-
  object_Thailand_all %>%
  dplyr::mutate(area = as.numeric(area)) %>%
  group_by(province_name) %>%
  dplyr::arrange(desc(area)) %>%
  dplyr::slice(1:100) %>%
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
#
object_Thailand_all_summary_map_100 <-
  object_Thailand_all_summary_df_100 %>%
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
        labs(
          x = "Longitude",
          y = "Latitude",
          fill = "Statistics",
          title = statistics,
          subtitle = "We used picked-up data (N = 100 by province) generated from Microsoft(c) building footprints.",
          caption = "Yuzuru Utsunomiya, Ph. D."
        ) +
        theme_classic() +
        theme(
          legend.position = "bottom",
          legend.key.width = unit(10, "mm")
        )
    )
  )
# save the maps
pdf(
  "object_Thailand_all_summary_map_100.pdf",
  width = 10,
  height = 10
)
object_Thailand_all_summary_map_100$map
dev.off()
# 

# ToDo
# 1. Draw maps by statistics
# 2. Choose 100 largest buildings by province and analyse
# 3. Simulate "optimum" combination of LF and area


# calculate local Moran's statistics
# NOTE
# Refer to the following website.
# https://github.com/JosiahParry/sfdep
statistics <- 
  levels(
    factor(
      object_Thailand_all_summary_df$statistics
      )
    )
# 
pdf("object_Thailand_all_summary_moran_map.pdf",
    width = 10,
    height = 10
    )
for(i in 1:length(statistics)){
  object_Thailand_all_summary_moran <-
  object_Thailand_all_summary_df %>%
  st_transform(crs = 6677) %>%
  dplyr::filter(
    statistics == statistics[i]
  ) %>%
  # for precise calculation, we need to transform the crs
  dplyr::mutate(
    # nearest neighbours
    # K-nearest neighbours to avoid influence of islands such as Phuket
    nb = sfdep::st_knn(geometry,  k = 4),
    # spatial weights
    # style: row standardized, popular in Spatial Econometrics
    wt = sfdep::st_weights(nb, style = "W"),
    number_lag = sfdep::st_lag(number, nb, wt),
    # calculate Moran's statistics
    moran = sfdep::local_moran(number, nb = nb, wt = wt, nsim = 1000)
  ) %>%
  # reform the crs as before
  sf::st_transform(crs = 4326)
  object_Thailand_all_summary_moran_map <-
    object_Thailand_all_summary_moran %>%
  ggplot2::ggplot(
    aes(
      fill = moran$ii
    )
  ) +
  geom_sf(
    aes(
      geometry = geometry
    )
  ) +
  scale_fill_YlOrBr() +
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Moran's I statistics",
    title = statistics[i],
    caption = "by Yuzuru Utsunomiya, Ph. D."
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(10, "mm")
  )
  print(object_Thailand_all_summary_moran_map)
}
dev.off()
# 
pdf("object_Thailand_all_summary_moran_map_p.pdf",
    width = 10,
    height = 10
)
for(i in 1:length(statistics)){
  object_Thailand_all_summary_moran <-
    object_Thailand_all_summary_df %>%
    st_transform(crs = 6677) %>%
    dplyr::filter(
      statistics == statistics[i]
    ) %>%
    # for precise calculation, we need to transform the crs
    dplyr::mutate(
      # nearest neighbours
      # K-nearest neighbours to avoid influence of islands such as Phuket
      nb = sfdep::st_knn(geometry,  k = 4),
      # spatial weights
      # style: row standardized, popular in Spatial Econometrics
      wt = sfdep::st_weights(nb, style = "W"),
      number_lag = sfdep::st_lag(number, nb, wt),
      # calculate Moran's statistics
      moran = sfdep::local_moran(number, nb = nb, wt = wt, nsim = 1000)
    ) %>%
    # reform the crs as before
    sf::st_transform(crs = 4326)
  object_Thailand_all_summary_moran_map_p <-
    object_Thailand_all_summary_moran %>%
    ggplot2::ggplot(
      aes(
        fill = moran$p_ii
      )
    ) +
    geom_sf(
      aes(
        geometry = geometry
      )
    ) +
    scale_fill_YlOrBr(reverse = TRUE) +
    labs(
      x = "Longitude",
      y = "Latitude",
      fill = "p value",
      title = statistics[i],
      caption = "by Yuzuru Utsunomiya, Ph. D."
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(10, "mm")
    )
  print(object_Thailand_all_summary_moran_map_p)
}
dev.off()
# 
# lisa_color_palette = c(
#   `High-High` = "red", `High-Low` = "salmon",
#   `Low-High` = "lightblue", `Low-Low` = "blue",
#   `Not Significant` = "white"
# )
# 
# pdf("object_Thailand_all_summary_moran_map_pysal.pdf",
#     width = 10,
#     height = 10
# )
# for(i in 1:length(statistics)){
#   object_Thailand_all_summary_moran <-
#     object_Thailand_all_summary_df %>%
#     st_transform(crs = 6677) %>%
#     dplyr::filter(
#       statistics == statistics[1]
#     ) %>%
#     # for precise calculation, we need to transform the crs
#     dplyr::mutate(
#       # nearest neighbours
#       # K-nearest neighbours to avoid influence of islands such as Phuket
#       nb = sfdep::st_knn(geometry,  k = 4),
#       # spatial weights
#       # style: row standardized, popular in Spatial Econometrics
#       wt = sfdep::st_weights(nb, style = "W"),
#       number_lag = sfdep::st_lag(number, nb, wt),
#       # calculate Moran's statistics
#       moran = sfdep::local_moran(number, nb = nb, wt = wt, nsim = 1000)
#     ) %>%
#     # reform the crs as before
#     sf::st_transform(crs = 4326)
#   object_Thailand_all_summary_moran_map_pysal <-
#     object_Thailand_all_summary_moran %>%
#     dplyr::mutate(pysal = moran$pysal) %>%
#     ggplot2::ggplot(
#       aes(
#         fill = pysal
#       )
#     ) +
#     geom_sf(
#       aes(
#         geometry = geometry
#       )
#     ) +
#     scale_fill_manual(name = moran$pysal, values = lisa_color_palette) +
#     labs(
#       x = "Longitude",
#       y = "Latitude",
#       fill = "p value",
#       title = statistics[i],
#       caption = "by Yuzuru Utsunomiya, Ph. D."
#     ) +
#     theme_classic() +
#     theme(
#       legend.position = "bottom",
#       legend.key.width = unit(10, "mm")
#     )
#   print(object_Thailand_all_summary_moran_map_pysal)
# }
# dev.off()
# 
