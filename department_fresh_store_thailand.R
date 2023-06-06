#################################################################### 
# タイ市場・デパート店舗数分析
# 30th. May 2023
# Yuzuru Utsunomiya
#################################################################### 
# ---- read.library ----
# ライブラリを読み込む
library(tidyverse)
library(sf)
library(furrr)
library(rsample)
library(ggmosaic)
library(khroma)
library(brms)
# set concurrent computing plan
# multisession: use CPUs as many as possible
plan(multisession)
#
# ---- read.data ----
# データ読み込む
department_fresh_store_thailand <- 
  readxl::read_excel(
    "department_fresh_store_thailand.xlsx",
    sheet = "shop_list"
    ) %>% 
  dplyr::select(-shop_name) %>% 
  dplyr::mutate(dplyr::across(everything(),  factor))
# 
# ---- mosaic.major ----
# モザイクプロットを描く
# 百貨店
mosaic_department <- 
  department_fresh_store_thailand %>% 
  dplyr::filter(department_market == "D") %>% 
  ggplot() +
  geom_mosaic(
    aes(
      x = product(JSIC_major_group, region),
      fill = JSIC_major_group
    )
  ) +
  scale_fill_smoothrainbow(discrete = T) +
  labs(x = "地域", y = "産業大分類") +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
# 生鮮市場
mosaic_fresh <- 
  department_fresh_store_thailand %>% 
  dplyr::filter(department_market == "F") %>% 
  ggplot() +
  geom_mosaic(
    aes(
      x = product(JSIC_major_group, region),
      fill = JSIC_major_group
    )
  ) +
  scale_fill_smoothrainbow(discrete = T) +
  labs(x = "地域", y = "産業大分類") +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )
# 
# ----- analysis.major -----
# どんな要因が店舗数に作用するか検討するよ（抜粋）
# 産業大分類編
# データ作成
major_group <- 
  department_fresh_store_thailand %>% 
  group_by(region, department_market, JSIC_major_group) %>% 
  dplyr::summarise(
    n = n()
  ) %>% 
  ungroup()
# 分析その1
major_group_anal_01 <- 
  brms::brm(
    n ~ region + department_market + JSIC_major_group  + (JSIC_major_group|department_market) + (JSIC_major_group|region),
    data = major_group,
    cores = 4,
    family = gaussian(),
    backend = "cmdstanr"
    )
major_group_anal_01
loo(major_group_anal_01)
# 分析その2
major_group_anal_02 <- 
  brms::brm(
    n ~ department_market + JSIC_major_group,
    data = major_group,
    cores = 4,
    family = gaussian(),
    backend = "cmdstanr"
  )
major_group_anal_02
loo(major_group_anal_02)
# 
# ----- analysis.minor -----
# 分析　中分類編
minor_group <- 
  department_fresh_store_thailand %>% 
  group_by(region, department_market, JSIC_group) %>% 
  dplyr::summarise(
    n = n()
  ) %>% 
  ungroup()
minor_group_anal_01 <- 
  brms::brm(
    n ~ department_market,
    data = minor_group,
    cores = 4,
    family = negbinomial(),
    backend = "cmdstanr"
  )
minor_group_anal_01
loo(minor_group_anal_01)


