#################################################
##  ��������(H27��H22�̔�r)�@�l���E���ѐ��@�����ڂ̕ϓ��@�n�}
#################################################

#######  ����
# �p�b�P�[�W���C���X�g�[�������̏ꍇ��
# install.packages("tidyverse") ���ɂ��C���X�g�[��
# tidyverse �� ggplot2 ��version 3.0.0�ȏ�ł邱�Ƃ��m�F�B <- geom_sf�������Ȃ��̂�
# R��ver3.5.0�ȏ�łȂ��Ƒ��������Ȃ��B

# ---R��ver3.4�ȉ��Ȃ� install.packages("devtools") ���āA
#    devtools::install_github("tidyverse/ggplot2")�ŊJ����ggplot2���C���X�g�[��

library(tidyverse)
library(sf)
library(mapview)
library(ggrepel)

# ��ƃt�H���_���w��A�K�X�C���B
# ���̉���e-stat������肵�����L�̍��������̋��E�f�[�^��u��
# �u�n�}�Ō���v����u���E�f�[�^�_�E�����[�h�v�u���n��v�u���������v�u2015�N�v�u���n��i�����E�����ʁj�v
#     �u���E���n�n���ʒ��p���W�n�EShape�`���v�u���{�v�u���{�S��v���_�E�����[�h���āA�u2015�v�t�H���_�ɕۑ��B
#  ���l��2010�N�f�[�^��2010�t�H���_�ɕۑ����Ă����B  
setwd("E:/Jinko_Setai/")  

# Jinko_Setai�t�H���_���̃t�@�C������c��(�q�t�H���_����,recursive=TRUE�I�v�V����)
f_list <- list.files(recursive = TRUE)
# �t�@�C�����̖�����"27.shp"�݂̂ɍi�肱�݁A�ǂݍ���
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
# h22xy�̂����K�v�ȃf�[�^�݂̂�(left_join����ۂɍ��ڂ��ł��܂葽���ƁA�����Ƃ������̂�)
h22 <- data.frame(KEY_CODE = h22xy$KEY_CODE ,KIGO_E = h22xy$KIGO_E, h22AREA = h22xy$AREA,
                  h22JINKO = h22xy$JINKO, h22SETAI = h22xy$SETAI, h22AreaName = h22xy$AreaName)
h22$KEY_CODE <- as.character(h22$KEY_CODE)
h22$KIGO_E <- as.character(h22$KIGO_E)

# h22��h27�̃t�@�C��������(left_join)�B�@�n���Key_Code��KIGO_E�݂̂��r���Č���
# �Q���_�ԂŏZ���\���̎��{���ɂ��A��r����ƂȂ�ꍇ������
h27 <- left_join(h27xy, h22, by=c("KEY_CODE","KIGO_E") )
h27anti <- anti_join(h27xy,h22,by=c("KEY_CODE","KIGO_E") ) # �s��v�̈ꗗ
   # h27anti ���݂�ƁA�\���ł��Ȃ������ڂ��킩��

### �l���E���т̑����� ###

h27$JINKO_ratio <- ( h27$JINKO / h27$h22JINKO - 1)*100

# �Q���_�̖ʐς��}10����������Ɣ�r����Ƃ���NA�Ƃ���B�����ꂩ�̐l����0�̎���NA�Ƃ���B�A
h27$JINKO_ratio <- ifelse( abs( 1 - h27$AREA / h27$h22AREA ) > 0.1, # | h27$h22JINKO == 0 | h27$JINKO == 0, 
                          NA, (h27$JINKO / h27$h22JINKO - 1)*100 )
h27$SETAI_ratio <- ifelse( abs( 1 - h27$AREA / h27$h22AREA ) > 0.1 | h27$h22SETAI == 0 | h27$JINKO <= 10, 
                           NA, (h27$SETAI / h27$h22SETAI - 1)*100 )

# �l���E���т̑����� #
h27$JINKO_num <- ifelse(  !is.na(h27$KIGO_E) & h27$KIGO_E != "E1",
                          NA, h27$JINKO - h27$h22JINKO )
 
min_max <-100  # �n�}�ł̐F�t���̍ő�E�ŏ��̐ݒ�

h27$JINKO_zogen <- h27$JINKO_num
h27$JINKO_zogen <- ifelse(h27$JINKO_zogen < -min_max, -min_max, h27$JINKO_zogen)
h27$JINKO_zogen <- ifelse(h27$JINKO_zogen > min_max, min_max, h27$JINKO_zogen)

h27$SETAI_num <- ifelse( !is.na(h27$KIGO_E) & h27$KIGO_E != "E1",
                         NA, h27$SETAI - h27$h22SETAI )
h27$SETAI_zogen <- h27$SETAI_num
h27$SETAI_zogen <- ifelse(h27$SETAI_zogen < -min_max, -min_max, h27$SETAI_zogen)
h27$SETAI_zogen <- ifelse(h27$SETAI_zogen > min_max, min_max, h27$SETAI_zogen)

############  ���{�̊e�s�撬���ɂ���PDF��###################################################
# st_centroid �d�S����@st_point_on_surface ��
# geom_text() �� geom_text_repel �ɕύX����Ɛ�������ł��킷���A���₷�����Ƃ́E�E�E
# �s���̈ܓx�E�o�x�̍ő�E�ŏ��@�F �\���E����̏c���̔��f
h27city <- h27 %>% group_by(CityName) %>% summarise("")
l_max_min <- map(h27city$geometry, st_bbox)  # �e�s�����̈ܓx�o�x�̍ő�ŏ��l�����߂�
city_ratio <- seq_along(l_max_min) 
for (i in seq_along(l_max_min)) {
  city_ratio[[i]] <- ( l_max_min[[i]][[3]] - l_max_min[[i]][[1]] ) /    # �c��������߂�
    ( l_max_min[[i]][[4]] - l_max_min[[i]][[2]] )
}
h27city$city_ratio <- city_ratio

if(file.exists("output")){
  print("output already exits")
}else{
  print("make output folder")
  dir.create("output")#�t�@�C�����o�͂��邽�߂̃t�H���_���쐬�B��x����2�x�ڈȍ~����̓G���[���o��B
}


for (i in seq_along(city_ratio)) {
  # �l��
  h27[h27$CityName == h27city$CityName[i],] %>% mutate(Centroid = st_point_on_surface(geometry),
                                                       x = st_coordinates(Centroid)[, 1],
                                                       y = st_coordinates(Centroid)[, 2] ) %>% 
    ggplot(aes(fill=JINKO_zogen)) + geom_sf() + 
    scale_fill_distiller(palette="RdYlBu") + theme_bw() +
    geom_text(aes(x=x, y=y, label=as.character(round(JINKO_num,1))),size=2)
  outfilename <- paste0("./output/",h27city$CityName[i],"_jinko.pdf")
  ggsave(outfilename, units = "mm",width = ifelse(h27city$city_ratio > 1,210, 297),
         height = ifelse(h27city$city_ratio > 1, 297, 210))
  # ���ѐ�
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


# �l���E���ѐ��ƕϓ���
for (i in seq_along(city_ratio) ) {
  # �l��
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
  # ���ѐ�
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

#######   �����s���ɌʂɁ@�n�}��\���@####################################### 

h27[h27$CITY_NAME == "�����s", ] %>% 
  mutate(Centroid = st_centroid(geometry),
         x = st_coordinates(Centroid)[, 1],
         y = st_coordinates(Centroid)[, 2]
  ) %>% ggplot(aes(fill=SETAI_zogen)) + geom_sf() + 
  scale_fill_distiller(palette="RdYlBu") + theme_bw() +
  geom_text(aes(x=x, y=y, label=as.character(round(SETAI_num,1))),size=2) 

h27[h27$CITY_NAME == "�����s", ] %>% ggplot(aes(fill=JINKO_zogen), na.rm=TRUE) + 
  geom_sf() + scale_fill_distiller(palette="RdYlBu") + theme_bw()

h27[h27$CITY_NAME == "�����s", ] %>% ggplot(aes(fill=SETAI_zogen), na.rm=TRUE) + 
  geom_sf() + scale_fill_distiller(palette="RdYlBu") + theme_bw()

h27[h27$CITY_NAME == "�����s", ] %>% 
  mutate(Centroid = st_centroid(geometry),
         x = st_coordinates(Centroid)[, 1],
         y = st_coordinates(Centroid)[, 2]
  ) %>% ggplot(aes(fill=JINKO_zogen)) + geom_sf() + 
  scale_fill_distiller(palette="RdYlBu") + theme_bw() +
  geom_text(aes(x=x, y=y, label=as.character(round(JINKO_num,1))),size=3) 

h27[h27$CITY_NAME == "�����s", ] %>% 
  mutate(Centroid = st_point_on_surface(geometry),
         x = st_coordinates(Centroid)[, 1],
         y = st_coordinates(Centroid)[, 2]
  ) %>% ggplot(aes(fill=SETAI_zogen)) + geom_sf() + 
  scale_fill_distiller(palette="RdYlBu") + theme_bw() +
  geom_text(aes(x=x, y=y, label=paste0(ifelse(is.na(SETAI_num),"",SETAI_num), 
                                       ifelse( (is.na(SETAI_ratio) | is.infinite(SETAI_ratio) ), "", paste0("\n", round(SETAI_ratio,1), "%")))),size=2)
  
h27[h27$CITY_NAME == "�����s", ] %>% mapview(zcol="SETAI_zogen")

