library(tidyverse)
library(sf)

object_Thailand <- sf::st_read("./object/Thailand.geojsonl")


hoge <- 
  object_Thailand[c(1:10),]

hogehoge <- 
  hoge %>% 
  mutate(
    area = sf::st_area(.),
    lon = st_coordinates(sf::st_centroid(.))[,1],
    lat = st_coordinates(sf::st_centroid(.))[,2]
  )





# 位置情報から住所を取得する関数
# 下記に書いてあったコードをほぼコピペ
# https://qiita.com/nozma/items/808bce2f496eabd50ff1
# https://qiita.com/uri/items/69b2c05f7b3a21d3aad3
find_city <- function(sp_polygon = df, lon = lon, lat = lat) {
  # ある座標が含まれるポリゴンを見つける
  which.row <- 
    sf::st_contains(sp_polygon, sf::st_point(c(lon, lat)), sparse = FALSE) %>%  
    grep(TRUE, .)
  # ある座標が含まれるポリゴンがなかった場合にメッセージを残す
  if (identical(which.row, integer(0)) == TRUE) {
    message("指定した座標がポリゴンに含まれません")
    # 座標があれば、座標に付帯する情報を引き出す
  } else {
    geos <- 
      sp_polygon[which.row, ] %>%
      # factor型なら文字列character型に変換してね。
      # 後でいろいろ面倒だから。
      dplyr::mutate_if(
        is.factor, 
        as.character
      ) %>% 
      # 1行目から必要な箇所だけ使うよ
      dplyr::mutate_at(
        dplyr::vars(N03_001:N03_004), 
        dplyr::funs(
          dplyr::if_else(
            # 値があるかな？
            condition = is.na(.),
            # NAだったら空白を
            true = "", 
            # NAじゃなかったら使いましょ
            false = .
          )
        )
      )
    # データフレームをつくる
    # この市区町村も、ESRIが提供するシェープファイルがスマート
    # あとでJCODEを使って結合したほうがよさげ
    res <- tibble::data_frame(
      # 市区町村コードを取得する
      city_code = geos$N03_007,
      # 市区町村名を結合・取得する
      # 001：都道府県名
      prefecture_name = geos$N03_001,
      # 002：所管する振興局名（北海道のみ）
      # 003：郡（政令指定都市は市）名
      # 004：市区町村名
      city_name = paste0(geos$N03_002, geos$N03_003, geos$N03_004)
    )
    # 返された結果を眺める。動作確認用なので、なければないでもいい。
    print(res)
    # 結果を返す
    return(res)
  }
}
