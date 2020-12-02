#---------------------------------------------------#
######## Project: pmdata_match.Rproj ########
# !/usr/bin/env Rscript
# -*- coding: utf-8 -*-
#
# File:        pmdata_match_beijing
# Author:      Baha
# Createdate:  Tue Dec 01 14:39:02 2020
# Software:    R version 4.0.2 (2020-06-22)
# Filepath:    E:/Projects/graduate_projects/xt_pmdata_beijing/pmdata_match
# Desc: 
#---------------------------------------------------#

# load_data -----------------------------------------------------------------------------------

pacman::p_load(tidyverse,tidyfst,fs,geosphere,sf)
pm_location <- readr::read_csv("dataset/Beijing_PM_loc.csv") %>% 
               select(OBJECTID,Longitude,Latitude)
object_location <- readxl::read_excel("dataset/object_location.xlsx") %>% 
                   select(1,5:6) %>% 
                   rename_with(function(x){x <- c("id","lon","lat")}) %>% 
                   mutate(lon=as.numeric(lon),lat=as.numeric(lat))


# distance_calculate --------------------------------------------------------------------------

pm_location %>% 
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326) -> pm_sf

object_location %>% 
  st_as_sf(coords=c("lon","lat"),crs=4326) -> object_sf
## 转为sf 对象，可以加快计算距离速度，底层是C语言，向量化操作，省区很多麻烦 
tictoc::tic()
st_distance(pm_sf,object_sf) -> distance_matirx## 60秒出结果
tictoc::toc()
apply(distance_matirx,2,which.min)  -> condition

pm_location[condition,] %>% 
  bind_cols(object_location) %>% 
  readr::write_csv("location_match.csv")
  
load("dataset/pm1kclean.RData")
pm_location[condition,] %>% 
  bind_cols(object_location) %>% 
  left_join(pm1kclean[,155:226] %>% 
              as.data.frame() %>% 
              rownames_to_column("OBJECTID") %>% 
              mutate(OBJECTID=as.numeric(OBJECTID)) %>% 
              as_tibble()) %>% 
  readr::write_csv("pm1kmclean_matched.csv")


##### 换一种方式计算一下距离，用于check你做的对不对 
tictoc::tic()
distance_matrix_two <- distm(pm_location[, c("Longitude", "Latitude")], 
                         object_location[, c("lon", "lat")],
                         fun = distVincentyEllipsoid)
tictoc::toc()
apply(distance_matrix, 2, which.min) -> condition_two

####

