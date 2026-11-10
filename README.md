# pmdata_match

把对象坐标匹配到最近的空气质量监测站点，再把该站点的污染物浓度时间序列拼回对象。

做暴露评估时的典型问题：手上只有对象的经纬度，但污染物数据是按监测站点组织的。这个脚本解决坐标与站点之间的对齐。

## 流程

1. 读站点坐标和对象坐标，转成 `sf` 空间对象（WGS84 / EPSG:4326）
2. 计算全量球面距离矩阵
3. 对每个对象取最近站点索引
4. 按索引把站点的浓度时间序列拼回对象
5. 用第二种算法交叉校验匹配结果

## 数据

脚本依赖以下文件（**未包含在本仓库中，需自备**）：

| 文件 | 内容 |
|---|---|
| `dataset/Beijing_PM_loc.csv` | 站点坐标，字段 `OBJECTID` `Longitude` `Latitude` |
| `dataset/object_location.xlsx` | 待匹配对象，第 1 列为 id，第 5-6 列为 lon / lat |
| `dataset/pm1kclean.RData` | 站点浓度矩阵，行 = 站点，列 = 时间点 |

## 运行

```r
# R 4.0+
install.packages(c("tidyverse", "tidyfst", "geosphere",
                   "sf", "readxl", "tictoc"))

source("math_pm_data.R")
```

## 实现要点

**用 `sf::st_distance()` 而不是循环**。底层是 C 实现且向量化，北京全域站点 × 对象的全量距离矩阵约 60 秒出结果：

```r
pm_location %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) -> pm_sf

st_distance(pm_sf, object_sf) -> distance_matrix
apply(distance_matrix, 2, which.min) -> condition
```

**保留第二种算法做校验**。脚本末尾用 `geosphere::distm(fun = distVincentyEllipsoid)` 重算一遍，两套结果对不上就说明坐标系或数据有问题：

```r
distm(pm_location[, c("Longitude", "Latitude")],
      object_location[, c("lon", "lat")],
      fun = distVincentyEllipsoid) -> distance_matrix_two
```

`tictoc::tic() / toc()` 保留着，方便对比两种实现的实际耗时。

## 输出

| 文件 | 内容 |
|---|---|
| `location_match.csv` | 对象 ↔ 站点匹配表 |
| `pm1kmclean_matched.csv` | 匹配后的污染物浓度时间序列 |

## License

MIT
