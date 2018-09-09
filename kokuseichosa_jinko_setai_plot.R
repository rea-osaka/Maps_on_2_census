#################################################
##  国勢調査(H27とH22の比較)　人口・世帯数　町丁目の変動　地図
#################################################

#######  準備
# パッケージをインストール未了の場合は
# install.packages("tidyverse") 等によりインストール
# tidyverse で ggplot2 がversion 3.0.0以上でることを確認。 <- geom_sfが動かないので
# Rもver3.5.0以上でないと多分動かない。

# ---Rがver3.4以下なら install.packages("devtools") して、
#    devtools::install_github("tidyverse/ggplot2")で開発版ggplot2をインストール

library(tidyverse)
library(sf)
library(mapview)
library(ggrepel)

# 作業フォルダを指定、適宜修正。
# この下にe-statから入手した下記の国勢調査の境界データを置く
# 「地図で見る」から「境界データダウンロード」「小地域」「国勢調査」「2015年」「小地域（町丁・字等別）」
#     「世界測地系平面直角座標系・Shape形式」「大阪府」「大阪府全域」をダウンロードして、「2015」フォルダに保存。
#  同様に2010年データも2010フォルダに保存しておく。  
setwd("E:/Jinko_Setai/")  

# Jinko_Setaiフォルダ内のファイル名を把握(子フォルダを含,recursive=TRUEオプション)
f_list <- list.files(recursive = TRUE)
# ファイル名の末尾が"27.shp"のみに絞りこみ、読み込み
f_list <- f_list[str_detect(f_list, "27.shp$")]
shp_list <- map(f_list,read_sf)

for ( i in seq_along(shp_list) ) {
  shp_list[[i]]$CityName <- paste0(as.character(shp_list[[i]]$PREF),as.character(shp_list[[i]]$CITY),
                                   as.character(shp_list[[i]]$GST_NAME),
                                   ifelse(is.na(shp_list[[i]]$CSS_NAME),"",as.character(shp_list[[i]]$CSS_NAME))) 
  
  shp_list[[i]]$AreaName <- paste0(as.character(shp_list[[i]]$GST_NAME),
                                   ifelse(is.na(shp_list[[i]]$CSS_NAME),"",as.character(shp_list[[i]]$CSS_NAME)),
                                   as.character( shp_list[[i]]$MOJI) ) 
}

h22xy <- shp_list[[1]]
h27xy <- shp_list[[2]]
# h22xyのうち必要なデータのみに(left_joinする際に項目がであまり多いと、うっとうしいので)
h22 <- data.frame(KEY_CODE = h22xy$KEY_CODE ,KIGO_E = h22xy$KIGO_E, h22AREA = h22xy$AREA,
                  h22JINKO = h22xy$JINKO, h22SETAI = h22xy$SETAI, h22AreaName = h22xy$AreaName)
h22$KEY_CODE <- as.character(h22$KEY_CODE)
h22$KIGO_E <- as.character(h22$KIGO_E)

# h22とh27のファイルを結合(left_join)。　地区のKey_CodeとKIGO_Eのみを比較して結合
# ２時点間で住居表示の実施等により、比較困難となる場合がある
h27 <- left_join(h27xy, h22, by=c("KEY_CODE","KIGO_E") )
h27anti <- anti_join(h27xy,h22,by=c("KEY_CODE","KIGO_E") ) # 不一致の一覧
   # h27anti をみると、表示できない町丁目がわかる

### 人口・世帯の増加率 ###

h27$JINKO_ratio <- ( h27$JINKO / h27$h22JINKO - 1)*100

# ２時点の面積が±10％をこえると比較困難としてNAとする。いずれかの人口が0の時もNAとする。、
h27$JINKO_ratio <- ifelse( abs( 1 - h27$AREA / h27$h22AREA ) > 0.1, # | h27$h22JINKO == 0 | h27$JINKO == 0, 
                          NA, (h27$JINKO / h27$h22JINKO - 1)*100 )
h27$SETAI_ratio <- ifelse( abs( 1 - h27$AREA / h27$h22AREA ) > 0.1 | h27$h22SETAI == 0 | h27$JINKO <= 10, 
                           NA, (h27$SETAI / h27$h22SETAI - 1)*100 )

# 人口・世帯の増減数 #
h27$JINKO_num <- ifelse(  !is.na(h27$KIGO_E) & h27$KIGO_E != "E1",
                          NA, h27$JINKO - h27$h22JINKO )
 
min_max <-100  # 地図での色付けの最大・最小の設定

h27$JINKO_zogen <- h27$JINKO_num
h27$JINKO_zogen <- ifelse(h27$JINKO_zogen < -min_max, -min_max, h27$JINKO_zogen)
h27$JINKO_zogen <- ifelse(h27$JINKO_zogen > min_max, min_max, h27$JINKO_zogen)

h27$SETAI_num <- ifelse( !is.na(h27$KIGO_E) & h27$KIGO_E != "E1",
                         NA, h27$SETAI - h27$h22SETAI )
h27$SETAI_zogen <- h27$SETAI_num
h27$SETAI_zogen <- ifelse(h27$SETAI_zogen < -min_max, -min_max, h27$SETAI_zogen)
h27$SETAI_zogen <- ifelse(h27$SETAI_zogen > min_max, min_max, h27$SETAI_zogen)

############  大阪府の各市区町村についてPDFに###################################################
# st_centroid 重心から　st_point_on_surface に
# geom_text() を geom_text_repel に変更すると線を結んでかわすが、見やすいいとは・・・
# 市毎の緯度・経度の最大・最小　： 表示・印刷の縦横の判断
h27city <- h27 %>% group_by(CityName) %>% summarise("")
l_max_min <- map(h27city$geometry, st_bbox)  # 各市町村の緯度経度の最大最小値を求める
city_ratio <- seq_along(l_max_min) 
for (i in seq_along(l_max_min)) {
  city_ratio[[i]] <- ( l_max_min[[i]][[3]] - l_max_min[[i]][[1]] ) /    # 縦横比を求める
    ( l_max_min[[i]][[4]] - l_max_min[[i]][[2]] )
}
h27city$city_ratio <- city_ratio

if(file.exists("output")){
  print("output already exits")
}else{
  print("make output folder")
  dir.create("output")#ファイルを出力するためのフォルダを作成。一度作ると2度目以降からはエラーが出る。
}


for (i in seq_along(city_ratio)) {
  # 人口
  h27[h27$CityName == h27city$CityName[i],] %>% mutate(Centroid = st_point_on_surface(geometry),
                                                       x = st_coordinates(Centroid)[, 1],
                                                       y = st_coordinates(Centroid)[, 2] ) %>% 
    ggplot(aes(fill=JINKO_zogen)) + geom_sf() + 
    scale_fill_distiller(palette="RdYlBu") + theme_bw() +
    geom_text(aes(x=x, y=y, label=as.character(round(JINKO_num,1))),size=2)
  outfilename <- paste0("./output/",h27city$CityName[i],"_jinko.pdf")
  ggsave(outfilename, units = "mm",width = ifelse(h27city$city_ratio > 1,210, 297),
         height = ifelse(h27city$city_ratio > 1, 297, 210))
  # 世帯数
  h27[h27$CityName == h27city$CityName[i],] %>% mutate(Centroid = st_point_on_surface(geometry),
                                                       x = st_coordinates(Centroid)[, 1],
                                                       y = st_coordinates(Centroid)[, 2] ) %>% 
    ggplot(aes(fill=SETAI_zogen)) + geom_sf() + 
    scale_fill_distiller(palette="RdYlBu") + theme_bw() +
    geom_text(aes(x=x, y=y, label=as.character(round(SETAI_num,1))),size=2)
  outfilename <- paste0("./output/",h27city$CityName[i],"_setai.pdf")
  ggsave(outfilename, units = "mm",width = ifelse(h27city$city_ratio > 1, 210, 297),
         height = ifelse(h27city$city_ratio > 1, 297, 210))
}


# 人口・世帯数と変動率
for (i in seq_along(city_ratio) ) {
  # 人口
  h27[h27$CityName == h27city$CityName[i],] %>% mutate(Centroid = st_point_on_surface(geometry),
                                                       x = st_coordinates(Centroid)[, 1],
                                                       y = st_coordinates(Centroid)[, 2] ) %>% 
    ggplot(aes(fill=JINKO_zogen)) + geom_sf() + 
    scale_fill_distiller(palette="RdYlBu") + theme_bw() +
    geom_text(aes(x=x, y=y, label=paste0(ifelse(is.na(JINKO_num),"",JINKO_num), 
                  ifelse( (is.na(JINKO_ratio) | is.infinite(JINKO_ratio) ), "", paste0("\n", round(JINKO_ratio,1), "%")))),size=1.5)
  outfilename <- paste0("./output/",h27city$CityName[i],"_jinko_ratio.pdf")
  ggsave(outfilename, units = "mm",width = ifelse(h27city$city_ratio > 1,210, 297),
         height = ifelse(h27city$city_ratio > 1, 297, 210))
  # 世帯数
  h27[h27$CityName == h27city$CityName[i],] %>% mutate(Centroid = st_point_on_surface(geometry),
                                                       x = st_coordinates(Centroid)[, 1],
                                                       y = st_coordinates(Centroid)[, 2] ) %>% 
    ggplot(aes(fill=SETAI_zogen)) + geom_sf() + 
    scale_fill_distiller(palette="RdYlBu") + theme_bw() +
    geom_text(aes(x=x, y=y, label=paste0(ifelse(is.na(SETAI_num),"",SETAI_num), 
                ifelse( (is.na(SETAI_ratio) | is.infinite(SETAI_ratio) ), "", paste0("\n", round(SETAI_ratio,1), "%")))),size=1.5)
  outfilename <- paste0("./output/",h27city$CityName[i],"_setai_ratio.pdf")
  ggsave(outfilename, units = "mm",width = ifelse(h27city$city_ratio > 1, 210, 297),
         height = ifelse(h27city$city_ratio > 1, 297, 210))
}

#######   東大阪市を例に個別に　地図を表示　####################################### 

h27[h27$CITY_NAME == "東大阪市", ] %>% 
  mutate(Centroid = st_centroid(geometry),
         x = st_coordinates(Centroid)[, 1],
         y = st_coordinates(Centroid)[, 2]
  ) %>% ggplot(aes(fill=SETAI_zogen)) + geom_sf() + 
  scale_fill_distiller(palette="RdYlBu") + theme_bw() +
  geom_text(aes(x=x, y=y, label=as.character(round(SETAI_num,1))),size=2) 

h27[h27$CITY_NAME == "東大阪市", ] %>% ggplot(aes(fill=JINKO_zogen), na.rm=TRUE) + 
  geom_sf() + scale_fill_distiller(palette="RdYlBu") + theme_bw()

h27[h27$CITY_NAME == "東大阪市", ] %>% ggplot(aes(fill=SETAI_zogen), na.rm=TRUE) + 
  geom_sf() + scale_fill_distiller(palette="RdYlBu") + theme_bw()

h27[h27$CITY_NAME == "東大阪市", ] %>% 
  mutate(Centroid = st_centroid(geometry),
         x = st_coordinates(Centroid)[, 1],
         y = st_coordinates(Centroid)[, 2]
  ) %>% ggplot(aes(fill=JINKO_zogen)) + geom_sf() + 
  scale_fill_distiller(palette="RdYlBu") + theme_bw() +
  geom_text(aes(x=x, y=y, label=as.character(round(JINKO_num,1))),size=3) 

h27[h27$CITY_NAME == "東大阪市", ] %>% 
  mutate(Centroid = st_point_on_surface(geometry),
         x = st_coordinates(Centroid)[, 1],
         y = st_coordinates(Centroid)[, 2]
  ) %>% ggplot(aes(fill=SETAI_zogen)) + geom_sf() + 
  scale_fill_distiller(palette="RdYlBu") + theme_bw() +
  geom_text(aes(x=x, y=y, label=paste0(ifelse(is.na(SETAI_num),"",SETAI_num), 
                                       ifelse( (is.na(SETAI_ratio) | is.infinite(SETAI_ratio) ), "", paste0("\n", round(SETAI_ratio,1), "%")))),size=2)
  
h27[h27$CITY_NAME == "東大阪市", ] %>% mapview(zcol="SETAI_zogen")


