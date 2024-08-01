####################################################################
# Social survey in Thailand
# Original 25th. June 2024
# Revised 
# by Yuzuru Utsunomiya, Ph. D.
# 
####################################################################
# 
# ----- read.library -----
library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)
library(ggsci)
# library(cmdstanr)
# library(furrr)
# library(future)
# future::plan(multisession, workers = 16)
# 
# ----- read.data -----
# shapefiles (province)
Thailand_map_01 <- 
  sf::read_sf("./shapefiles/THA_adm1.shp") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) 
# shapefiles (district)
Thailand_map_02 <- 
  sf::read_sf("./shapefiles/THA_adm2.shp") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) 
# shapefiles (subdistrict)
Thailand_map_03 <- 
  sf::read_sf("./shapefiles/THA_adm3.shp") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) 

# make a boundary box for 
Thailand_bbox <- 
  sf::read_sf("./shapefiles/THA_adm1.shp") %>% 
  dplyr::mutate(
    # First, we obtain the gravity
    centroid = sf::st_centroid(geometry),
    # Second, we compute the coordinates of the centroid into two parts; x (longitude) and y (latitude)
    # x
    center_x = st_coordinates(centroid)[,1],
    # y
    center_y = st_coordinates(centroid)[,2]
  ) %>% 
  sf::st_transform(4326) %>% 
  sf::st_bbox()
# 
wgseqproj <- "EPSG:4087"
wgs84 <- "EPSG:4326"
# object_Thailand_all <- readr::read_rds("object_Thailand_all.rds") 

# object_Thailand_all <-
#   # object_Thailand_all %>%
#   readr::read_rds("object_Thailand_all.rds") %>%
#   dplyr::mutate(
#     # First, we obtain the gravity
#     centroid = sf::st_centroid(geometry),
#     lon = st_coordinates(centroid)[,1],
#     # y
#     lat = st_coordinates(centroid)[,2],
#     id_thailand = c(1:nrow(.))
#   ) %>%
#   dplyr::select(-geometry) %>%
#   st_as_sf() %>%
#   sf::st_transform(wgseqproj)
# # shapefiles
# Thailand_map <-
#   sf::read_sf("THA_adm1.shp") %>%
#   sf::st_as_sf() %>%
#   sf::st_transform(wgseqproj)
# Thailand_point <-
#   object_Thailand_all %>%
#   sf::st_as_sf() %>%
#   sf::st_transform(wgseqproj) 
# 
# rm(object_Thailand_all)
# gc(reset = TRUE)
# gc(reset = TRUE)

# readr::write_rds(Thailand_map, "Thailand_map.rds")
# readr::write_rds(Thailand_point, "Thailand_point.rds")
Thailand_map <- readr::read_rds("Thailand_map.rds")
Thailand_point <- readr::read_rds("Thailand_point.rds")

# Thailand_point_sub <- Thailand_point %>% dplyr::sample_n(100)

# grid data settings
# quadrat size
cellsize <- 
  data.frame(
    cellsize = c(500, 1000, 2500)
  ) %>% 
  dplyr::mutate(
    index = factor(order(cellsize))
  ) %>% 
  dplyr::tibble()
# N. of sample
n <- 100
set.seed(123)
# make grid data
# NOTE
# This process needs long computation period.
# Use saved data.
# Comment out when not in use.
target_location <- 
  cellsize %>% 
  group_by(index) %>% 
  nest() %>% 
  # make grid data
  dplyr::mutate(
    grid = purrr::map(
      data,
      ~
        st_make_grid(
          Thailand_map,
          cellsize = .$cellsize[1],
          square = TRUE,
          what = "polygons",
          crs = wgseqproj
        )  %>%
        st_intersection(Thailand_map) %>%
        sf::st_as_sf() %>%
        sf::st_transform(4326) %>% 
        dplyr::mutate(
          # First, we obtain the gravity
          area = sf::st_area(.),
          centroid = sf::st_centroid(x),
          lon = st_coordinates(centroid)[,1],
          lat = st_coordinates(centroid)[,2],
          id_grid = c(1:nrow(.))
        ) %>% 
        dplyr::select(-centroid) 
    )
  ) %>% 
  dplyr::mutate(
    obtain_location = purrr::map(
      grid,
      ~
        sf::st_intersects(
          # object_Thailand_all %>% 
          Thailand_point %>% 
            sf::st_transform(4326),
          .
        ) %>% 
        as.numeric() %>% 
        dplyr::tibble(location = .) 
    )
  ) %>% 
  dplyr::mutate(
    obtain_location_complete = purrr::map(
      obtain_location,
      ~
        dplyr::left_join(
          .,
          grid,
          by = c("location" = "id_grid"),
          copy = TRUE,
          keep = TRUE
        ) %>% 
        tidyr::complete(
          id_grid = tidyr::full_seq(id_grid, period = 1)
        ) %>% 
        dplyr::mutate(
          id_grid = factor(id_grid),
          area = as.numeric(area))
    ) 
  ) %>% 
  dplyr::mutate(
    summary = purrr::map(
      obtain_location_complete,
      ~
        tibble(.) %>% 
        dplyr::group_by(.$id_grid) %>% 
        dplyr::summarise(
          n_building = sum(!is.na(area)),
          mean_area_building  = mean(area, na.rm = TRUE),
          median_area_building  = median(area, na.rm = TRUE)
        ) %>%
        dplyr::mutate(
          p = n_building/sum(n_building)
        ) %>% 
        dplyr::mutate(
          across(contains("area"), \(x)replace_na(x,0))
        ) 
    )
  )
# save the results
readr::write_rds(
  target_location, 
  "target_location.rds"
)

target_location$grid[[1]]
target_location$obtain_location[[1]]
target_location$obtain_location[[2]]
target_location$obtain_location[[3]]
target_location$obtain_location_complete[[1]]
target_location$summary[[1]]

# 
# ----- obtain.100.targets -----
for(i in 1:1000){
  target_location_sampled <- 
    target_location %>% 
    dplyr::mutate(
      sample = purrr::map(
        summary,
        ~
          dplyr::tibble(target_id = sample(nrow(.), size = n, replace = TRUE, prob = .$p)) %>% 
          dplyr::left_join(
            .,
            grid,
            by = c("target_id" = "id_grid"),
            copy = TRUE
          ) %>%
          sf::st_as_sf() %>%
          sf::st_centroid() %>% 
          sf::st_as_sf()
      )
    ) %>%
    dplyr::mutate(
      disance_duration = purrr::map(
        sample,
        ~
          # obtain shortes route, distance, and duration via OSRM
          # https://project-osrm.org/
          osrm::osrmTrip(
            loc = data.frame(lon = .$lon, lat = .$lat),
            # by car
            osrm.profile = "car"
          ) %>% 
          # pick the minimum moving distance and duration
          dplyr::tibble(
            distance = .[[1]]$summary$distance,
            duration = .[[1]]$summary$duration
          )
      )
    )
  distance_duration <- 
    dplyr::bind_rows(target_location_sampled$disance_duration) %>%
    dplyr::select(2:3) %>%
    dplyr::mutate(
      index = rep(c(1:length(cellsize$index)), times = 1)
    )
  readr::write_excel_csv(
    distance_duration, 
    paste0("./distance_duration/distance_duation_",i,".csv")
  )
}

# st_geometry(hoge$sample[[1]]) <- "geometry"
# st_geometry(hoge$sample[[2]]) <- "geometry"
# st_geometry(hoge$sample[[3]]) <- "geometry"

hoge <- 
  fs::dir_ls("distance_duration", glob = "*.csv") %>%
  vroom::vroom() %>% 
  dplyr::mutate(
    quadrat_size = dplyr::case_when(
      index == 1 ~ "50m", 
      index == 2 ~ "100m", 
      index == 3 ~ "200m", 
      index == 4 ~ "500m", 
      index == 5 ~ "1,000m", 
      index == 6 ~ "2,000m", 
      index == 7 ~ "5,000m", 
      .default = "hoge", 
    ) %>% 
      factor(.,levels = c("50m","100m","200m","500m","1,000m","2,000m","5,000m"))
  )
hoge %>% ggplot(aes(x = duration)) + geom_boxplot(aes(color = quadrat_size))
hoge %>% ggplot(aes(x = duration)) + geom_density(aes(color = quadrat_size))
hoge %>% ggplot(aes(x = distance, color = quadrat_size)) + geom_density()
hoge %>% ggplot(aes(x = distance, y = duration, color = quadrat_size)) + geom_point()

summary(aov(duration ~ quadrat_size + distance, data = hoge))
summary(aov(distance ~ quadrat_size, data = hoge))
pairwise.wilcox.test(hoge$duration, hoge$quadrat_size, p.adjust.method = "bonf")
pairwise.wilcox.test(hoge$distance, hoge$quadrat_size, p.adjust.method = "bonf")

library(brms)
hogehoge <- 
  brms::brm(duration ~ distance + quadrat_size + (distance|quadrat_size),
            family = gaussian(),
            chains = 4,
            cores = 4,
            data = hoge
  )
summary(hogehoge)