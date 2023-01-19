---
title: "메이저리그 구장의 변화 시각화"
subtitle: "수용좌석, 건설비용, 도심접근성을 중심으로"
author: "윤준식"
date: "2023-01-19" 
tags: [MLB, ballpark, USsports]
output:
  html_document:
    fig_height: 6
    fig_width: 10
    highlight: tango
    theme: cosmo
    toc: yes
    toc_depth: 3
    toc_float: yes
    self_contained: false
header-includes:
  - \usepackage{fontspec}
  - \setmainfont{Times New Roman}
---

# 라이브러리 설정

# 무엇이 궁금한가

<div align="left">
  <iframe id="introduction" 
          title="건축가가 설명하는 메이저리그 구장의 역사"
          width="560" height="315"
          src="https://www.youtube.com/embed/3OkSW5qV7jQ"
          frameborder="0" allowfullscreen>
   </iframe>
</div> 




# 데이터 & 전처리

```r
mlbpark <- read_sheet('https://docs.google.com/spreadsheets/d/1WLSVxzZw6Y9koMwyBn8PJiQXFER2mfI2B7jlICjlwz0/edit#gid=0')
```

```
## v Reading from "ballparklist_mlb".
```

```
## v Range '시트1'.
```

```r
as_tibble(mlbpark)
```

```
## # A tibble: 30 x 11
##    Name      Capac~1 Locat~2 Surface Team 
##    <chr>     <list>  <chr>   <chr>   <chr>
##  1 American~ <chr>   Milwau~ Grass   Milw~
##  2 Angel St~ <chr>   Anahei~ Grass   Los ~
##  3 Busch St~ <chr>   St. Lo~ Grass   St. ~
##  4 Chase Fi~ <chr>   Phoeni~ Artifi~ Ariz~
##  5 Citi Fie~ <chr>   Queens~ Grass   New ~
##  6 Citizens~ <chr>   Philad~ Grass   Phil~
##  7 Comerica~ <chr>   Detroi~ Grass   Detr~
##  8 Coors Fi~ <chr>   Denver~ Grass   Colo~
##  9 Dodger S~ <chr>   Los An~ Grass   Los ~
## 10 Fenway P~ <chr>   Boston~ Grass   Bost~
## # ... with 20 more rows, 6 more
## #   variables: Opened <list>,
## #   `Distance to center field` <chr>,
## #   Type <chr>, Rooftype <chr>,
## #   Cost <dbl>, url <chr>, and
## #   abbreviated variable names
## #   1: Capacity, 2: Location
```


```r
mlbpark$Capacity <- mlbpark$Capacity %>% 
  str_replace("\\[.*", "") %>% 
  str_replace(",", "") %>% 
  as.integer()

mlbpark$Team <- mlbpark$Team %>% 
  str_replace("\\[.*", "") 
mlbpark$Type <- mlbpark$Type %>% 
  str_replace("\\[.*", "") 
mlbpark$Opened <- mlbpark$Opened %>% 
  str_replace("\\[.*", "")
mlbpark[, 6] <- lapply(mlbpark[, 6], 
                         function(x) as.double(as.character(x)))
```

# 팀 고유색 더하기

```r
mlb_color <- read_sheet('https://docs.google.com/spreadsheets/d/1BNosPtWPI9jFvc5Mro2mAWaKlV8R754w4uyfgOnyAuY/edit#gid=0')
```

```
## v Reading from "mlb_team_color".
```

```
## v Range '시트1'.
```

```r
skim(mlb_color$`Color 1`)
```


Table: Data summary

|                         |                  |
|:------------------------|:-----------------|
|Name                     |mlb_color$Color 1 |
|Number of rows           |30                |
|Number of columns        |1                 |
|_______________________  |                  |
|Column type frequency:   |                  |
|character                |1                 |
|________________________ |                  |
|Group variables          |None              |


**Variable type: character**

|skim_variable | n_missing| complete_rate| min| max| empty| n_unique| whitespace|
|:-------------|---------:|-------------:|---:|---:|-----:|--------:|----------:|
|data          |         0|             1|  11|  23|     0|       29|          0|

```r
n_distinct(mlb_color$`Color 1`)
```

```
## [1] 29
```

```r
mlb_color%>% group_by(`Color 1`) %>%
  summarize(n = n())
```

```
## # A tibble: 29 x 2
##    `Color 1`               n
##    <chr>               <int>
##  1 Black #27251F           2
##  2 Blue #002D72            1
##  3 Blue #003087            1
##  4 Blue #003263            1
##  5 Blue #003278            1
##  6 Blue #0E3386            1
##  7 Blue #134A8E            1
##  8 Brown #2F241D           1
##  9 Dodger Blue #005A9C     1
## 10 Green #003831           1
## # ... with 19 more rows
```
시카고 화이트삭스와 피츠버그 파이러츠의 팀 컬러가 검정색으로 겹친다는 것을 알 수 있다. /n 이를 해결하기 위하여 피츠버그를 두번째 팀컬러인 노란색으로 표현하기로 한다.


```r
#피츠버그의 색을 노란색으로
mlb_color[mlb_color$`MLB Team Name`=="Pittsburgh Pirates", "Color 1"] <-  "Yellow #FDB827"
mlbcolor <- mlb_color %>% select(c(1:2)) %>%
  rename("Team" = 1, "colorcode" = 2)
```



```r
mlbcolor$colorcode <- gsub(".*#", "#", mlbcolor$colorcode)
mlb_1 <- mlbpark %>% left_join(mlbcolor, by="Team")
mlbcolor$colorcode
```

```
##  [1] "#A71930" "#CE1141" "#DF4601"
##  [4] "#BD3039" "#0E3386" "#27251F"
##  [7] "#C6011F" "#00385D" "#333366"
## [10] "#0C2340" "#002D62" "#004687"
## [13] "#003263" "#005A9C" "#00A3E0"
## [16] "#12284B" "#002B5C" "#002D72"
## [19] "#003087" "#003831" "#E81828"
## [22] "#FDB827" "#2F241D" "#FD5A1E"
## [25] "#0C2C56" "#C41E3A" "#092C5C"
## [28] "#003278" "#134A8E" "#AB0003"
```

```r
mlbcol <- unlist(mlbcolor$colorcode, use.names = mlbcolor$Team)
mlbc <- setNames(as.character(mlbcolor$colorcode), 
                   as.character(mlbcolor$Team))
```

```r
mlbc
```

```
##  Arizona Diamondbacks 
##             "#A71930" 
##        Atlanta Braves 
##             "#CE1141" 
##     Baltimore Orioles 
##             "#DF4601" 
##        Boston Red Sox 
##             "#BD3039" 
##          Chicago Cubs 
##             "#0E3386" 
##     Chicago White Sox 
##             "#27251F" 
##       Cincinnati Reds 
##             "#C6011F" 
##   Cleveland Guardians 
##             "#00385D" 
##      Colorado Rockies 
##             "#333366" 
##        Detroit Tigers 
##             "#0C2340" 
##        Houston Astros 
##             "#002D62" 
##    Kansas City Royals 
##             "#004687" 
##    Los Angeles Angels 
##             "#003263" 
##   Los Angeles Dodgers 
##             "#005A9C" 
##         Miami Marlins 
##             "#00A3E0" 
##     Milwaukee Brewers 
##             "#12284B" 
##       Minnesota Twins 
##             "#002B5C" 
##         New York Mets 
##             "#002D72" 
##      New York Yankees 
##             "#003087" 
##     Oakland Athletics 
##             "#003831" 
## Philadelphia Phillies 
##             "#E81828" 
##    Pittsburgh Pirates 
##             "#FDB827" 
##      San Diego Padres 
##             "#2F241D" 
##  San Francisco Giants 
##             "#FD5A1E" 
##      Seattle Mariners 
##             "#0C2C56" 
##   St. Louis Cardinals 
##             "#C41E3A" 
##        Tampa Bay Rays 
##             "#092C5C" 
##         Texas Rangers 
##             "#003278" 
##     Toronto Blue Jays 
##             "#134A8E" 
##  Washington Nationals 
##             "#AB0003"
```
  scale_fill_manual(values = as.character(df3$cat_cols))

# 시각화 작업
## 구장 수용인원 분석

```r
g1 <- mlb_1 %>% ggplot() +
  geom_point(aes(x=Opened, y=Capacity, colour = factor(Team))) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1900, 2022),
                     breaks = seq(1900, 2022, by = 10)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(20000, 60000),
                     breaks = seq(20000, 60000, by = 5000)) +
  labs(title="현재 쓰이는 MLB 구장의 건설연도와 수용인원") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_colour_manual(values=c(mlbc))

g1
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
#colour = thematic::okabe_ito(8)[6],
```

```r
mlbc
```

```
##  Arizona Diamondbacks 
##             "#A71930" 
##        Atlanta Braves 
##             "#CE1141" 
##     Baltimore Orioles 
##             "#DF4601" 
##        Boston Red Sox 
##             "#BD3039" 
##          Chicago Cubs 
##             "#0E3386" 
##     Chicago White Sox 
##             "#27251F" 
##       Cincinnati Reds 
##             "#C6011F" 
##   Cleveland Guardians 
##             "#00385D" 
##      Colorado Rockies 
##             "#333366" 
##        Detroit Tigers 
##             "#0C2340" 
##        Houston Astros 
##             "#002D62" 
##    Kansas City Royals 
##             "#004687" 
##    Los Angeles Angels 
##             "#003263" 
##   Los Angeles Dodgers 
##             "#005A9C" 
##         Miami Marlins 
##             "#00A3E0" 
##     Milwaukee Brewers 
##             "#12284B" 
##       Minnesota Twins 
##             "#002B5C" 
##         New York Mets 
##             "#002D72" 
##      New York Yankees 
##             "#003087" 
##     Oakland Athletics 
##             "#003831" 
## Philadelphia Phillies 
##             "#E81828" 
##    Pittsburgh Pirates 
##             "#FDB827" 
##      San Diego Padres 
##             "#2F241D" 
##  San Francisco Giants 
##             "#FD5A1E" 
##      Seattle Mariners 
##             "#0C2C56" 
##   St. Louis Cardinals 
##             "#C41E3A" 
##        Tampa Bay Rays 
##             "#092C5C" 
##         Texas Rangers 
##             "#003278" 
##     Toronto Blue Jays 
##             "#134A8E" 
##  Washington Nationals 
##             "#AB0003"
```



## 구장 건설비용 분석

```r
g2 <- mlbpark %>% ggplot() +
  geom_point(aes(x=Opened, y=Cost, color=`Rooftype`)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1900, 2022),
                     breaks = seq(1900, 2022, by = 10)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 2500),
                     breaks = seq(0, 2500, by = 500),
                     labels = c("0", "5000억", "1조", "1조5천억", "2조", "2조5천억\n(원)")) +
  labs(title="현재 쓰이는 MLB 구장의 건설연도와 건설비용") +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(size=0.5, 
                                         linetype="solid", 
                                         colour ="darkblue"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
# guides(fill = guide_legend(nrow = 4, byrow = T))

g2
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)


## 구장 위치 분석
### 구장 위치 좌표 크롤링


```r
# 함수 생성 전 연습
url1 <- "https://en.wikipedia.org/wiki/American_Family_Field"
webc <- rvest::read_html(url1)
webc %>% 
  rvest::html_nodes("td") %>%
  rvest::html_text() -> webtd
```



```r
coordinatetd <- webtd[grepl("Coordinates", webtd)]
coord <- gsub("^.*\\Coor","Coor",coordinatetd)
coor <- gsub("^.*\\°W / ","",coord)
coor
```

```
## [1] "Coordinates: 43°1′42″N 87°58′16″W<U+FEFF> / <U+FEFF>43.02833°N 87.97111°W<U+FEFF> / 43.02833; -87.97111"
```


```r
# 함수 생성
scrape_coordinate_wiki <- function(url) {
  webc <- rvest::read_html(url)
  webtd <- webc %>% 
  rvest::html_nodes("td") %>%
  rvest::html_text()
  coordinatetd <- webtd[grepl("Coordinates", webtd)]
  coord <- gsub("^.*\\Coor","Coor", coordinatetd)
  coor <- gsub("^.*\\°W﻿ / ","", coord)
  coordinate <- data.frame(
    url = url,
    coor = coor
  )
}
```



```r
mlb_2 <- mlb_1
length(mlb_2$url)
```

```
## [1] 30
```

```r
scrape_coordinate_wiki(mlb_2$url[2])
urllist <-mlb_2$url
length(urllist)
```

```
## [1] 30
```


```r
all_coord <- data.frame() 

for (i in 1:length(urllist)) {
  coordinate <- scrape_coordinate_wiki(urllist[i])
  all_coord <- rbind(all_coord, coordinate)
}

all_coord
```

```
##                                                          url
## 1        https://en.wikipedia.org/wiki/American_Family_Field
## 2                https://en.wikipedia.org/wiki/Angel_Stadium
## 3                https://en.wikipedia.org/wiki/Busch_Stadium
## 4                  https://en.wikipedia.org/wiki/Chase_Field
## 5                   https://en.wikipedia.org/wiki/Citi_Field
## 6           https://en.wikipedia.org/wiki/Citizens_Bank_Park
## 7                https://en.wikipedia.org/wiki/Comerica_Park
## 8                  https://en.wikipedia.org/wiki/Coors_Field
## 9               https://en.wikipedia.org/wiki/Dodger_Stadium
## 10                 https://en.wikipedia.org/wiki/Fenway_Park
## 11            https://en.wikipedia.org/wiki/Globe_Life_Field
## 12    https://en.wikipedia.org/wiki/Great_American_Ball_Park
## 13       https://en.wikipedia.org/wiki/Guaranteed_Rate_Field
## 14            https://en.wikipedia.org/wiki/Kauffman_Stadium
## 15              https://en.wikipedia.org/wiki/LoanDepot_Park
## 16            https://en.wikipedia.org/wiki/Minute_Maid_Park
## 17              https://en.wikipedia.org/wiki/Nationals_Park
## 18                 https://en.wikipedia.org/wiki/Oracle_Park
## 19 https://en.wikipedia.org/wiki/Oriole_Park_at_Camden_Yards
## 20                  https://en.wikipedia.org/wiki/Petco_Park
## 21                    https://en.wikipedia.org/wiki/PNC_Park
## 22           https://en.wikipedia.org/wiki/Progressive_Field
## 23            https://en.wikipedia.org/wiki/Oakland_Coliseum
## 24               https://en.wikipedia.org/wiki/Rogers_Centre
## 25               https://en.wikipedia.org/wiki/T-Mobile_Park
## 26                https://en.wikipedia.org/wiki/Target_Field
## 27             https://en.wikipedia.org/wiki/Tropicana_Field
## 28                 https://en.wikipedia.org/wiki/Truist_Park
## 29               https://en.wikipedia.org/wiki/Wrigley_Field
## 30              https://en.wikipedia.org/wiki/Yankee_Stadium
##                                                                                                                       coor
## 1          Coordinates: 43°1′42″N 87°58′16″W<U+FEFF> / <U+FEFF>43.02833°N 87.97111°W<U+FEFF> / 43.02833; -87.97111
## 2       Coordinates: 33°48′1″N 117°52′58″W<U+FEFF> / <U+FEFF>33.80028°N 117.88278°W<U+FEFF> / 33.80028; -117.88278
## 3         Coordinates: 38°37′21″N 90°11′35″W<U+FEFF> / <U+FEFF>38.62250°N 90.19306°W<U+FEFF> / 38.62250; -90.19306
## 4        Coordinates: 33°26′43″N 112°4′1″W<U+FEFF> / <U+FEFF>33.44528°N 112.06694°W<U+FEFF> / 33.44528; -112.06694
## 5         Coordinates: 40°45′25″N 73°50′45″W<U+FEFF> / <U+FEFF>40.75694°N 73.84583°W<U+FEFF> / 40.75694; -73.84583
## 6          Coordinates: 39°54′21″N 75°9′59″W<U+FEFF> / <U+FEFF>39.90583°N 75.16639°W<U+FEFF> / 39.90583; -75.16639
## 7          Coordinates: 42°20′21″N 83°2′55″W<U+FEFF> / <U+FEFF>42.33917°N 83.04861°W<U+FEFF> / 42.33917; -83.04861
## 8      Coordinates: 39°45′22″N 104°59′39″W<U+FEFF> / <U+FEFF>39.75611°N 104.99417°W<U+FEFF> / 39.75611; -104.99417
## 9       Coordinates: 34°4′25″N 118°14′24″W<U+FEFF> / <U+FEFF>34.07361°N 118.24000°W<U+FEFF> / 34.07361; -118.24000
## 10 Coordinates: 42°20′46.5″N 71°5′51.9″W<U+FEFF> / <U+FEFF>42.346250°N 71.097750°W<U+FEFF> / 42.346250; -71.097750
## 11      Coordinates: 32°44′50.5″N 97°5′3″W<U+FEFF> / <U+FEFF>32.747361°N 97.08417°W<U+FEFF> / 32.747361; -97.08417
## 12         Coordinates: 39°5′51″N 84°30′24″W<U+FEFF> / <U+FEFF>39.09750°N 84.50667°W<U+FEFF> / 39.09750; -84.50667
## 13         Coordinates: 41°49′48″N 87°38′2″W<U+FEFF> / <U+FEFF>41.83000°N 87.63389°W<U+FEFF> / 41.83000; -87.63389
## 14                  Coordinates: 39°03′04″N 94°28′48″W<U+FEFF> / <U+FEFF>39.051°N 94.48°W<U+FEFF> / 39.051; -94.48
## 15        Coordinates: 25°46′41″N 80°13′11″W<U+FEFF> / <U+FEFF>25.77806°N 80.21972°W<U+FEFF> / 25.77806; -80.21972
## 16        Coordinates: 29°45′25″N 95°21′20″W<U+FEFF> / <U+FEFF>29.75694°N 95.35556°W<U+FEFF> / 29.75694; -95.35556
## 17         Coordinates: 38°52′22″N 77°0′27″W<U+FEFF> / <U+FEFF>38.87278°N 77.00750°W<U+FEFF> / 38.87278; -77.00750
## 18     Coordinates: 37°46′43″N 122°23′21″W<U+FEFF> / <U+FEFF>37.77861°N 122.38917°W<U+FEFF> / 37.77861; -122.38917
## 19         Coordinates: 39°17′2″N 76°37′18″W<U+FEFF> / <U+FEFF>39.28389°N 76.62167°W<U+FEFF> / 39.28389; -76.62167
## 20         Coordinates: 32°42′26″N 117°09′24″W<U+FEFF> / <U+FEFF>32.7073°N 117.1566°W<U+FEFF> / 32.7073; -117.1566
## 21         Coordinates: 40°26′49″N 80°0′21″W<U+FEFF> / <U+FEFF>40.44694°N 80.00583°W<U+FEFF> / 40.44694; -80.00583
## 22         Coordinates: 41°29′45″N 81°41′7″W<U+FEFF> / <U+FEFF>41.49583°N 81.68528°W<U+FEFF> / 41.49583; -81.68528
## 23       Coordinates: 37°45′6″N 122°12′2″W<U+FEFF> / <U+FEFF>37.75167°N 122.20056°W<U+FEFF> / 37.75167; -122.20056
## 24        Coordinates: 43°38′29″N 79°23′21″W<U+FEFF> / <U+FEFF>43.64139°N 79.38917°W<U+FEFF> / 43.64139; -79.38917
## 25             Coordinates: 47°35′28″N 122°19′59″W<U+FEFF> / <U+FEFF>47.591°N 122.333°W<U+FEFF> / 47.591; -122.333
## 26        Coordinates: 44°58′54″N 93°16′42″W<U+FEFF> / <U+FEFF>44.98167°N 93.27833°W<U+FEFF> / 44.98167; -93.27833
## 27         Coordinates: 27°46′6″N 82°39′12″W<U+FEFF> / <U+FEFF>27.76833°N 82.65333°W<U+FEFF> / 27.76833; -82.65333
## 28                  Coordinates: 33°53′24″N 84°28′05″W<U+FEFF> / <U+FEFF>33.89°N 84.468°W<U+FEFF> / 33.89; -84.468
## 29        Coordinates: 41°56′53″N 87°39′20″W<U+FEFF> / <U+FEFF>41.94806°N 87.65556°W<U+FEFF> / 41.94806; -87.65556
## 30        Coordinates: 40°49′45″N 73°55′35″W<U+FEFF> / <U+FEFF>40.82917°N 73.92639°W<U+FEFF> / 40.82917; -73.92639
```



```r
mlb_2.5 <- left_join(mlb_2, all_coord, by="url")
```



```r
mlb_3 <- mlb_2.5 %>% separate(coor, c('coordY', 'coordX'), sep = ";")
```

### 개념 정의하기 : 연고지와 도심
<p> 연고지를 어떻게 결정할 것인가? 가령 텍사스 레인저스의 홈구장은 Arlington에 위치해있지만, 구장을 방문하는 팬이 가장 많은 도시는 Dallas이다. 반대로 Los Angeles Angels는 Los Angeles를 팀명에 달고 있지만, 정작 Los Angeles에서는 큰 인기를 얻지 못하고 근교인 Orange County에 팬 베이스가 형성되어 있다. 이를 판단하기 위한 자료로서 <a href="https://www.nytimes.com/interactive/2014/04/24/upshot/facebook-baseball-map.html">뉴욕 타임즈에서 페이스북 검색 기록을 기반으로 분석한 MLB Fan Map (2014)</a>을 적극 참조했다. 이 지도를 기준으로 보았을 때, Angels는 Anaheim을 중심으로 팬층이 형성되어 있다는 것을 확인하였고, 연고지를 Anaheim으로 판단하였다.</p> 

<p> 도심은 어떤 기준을 통해서 결정해야 할까? 시청이 위치한 곳? 가장 활발한 경제활동이 벌어지는 곳? 도시 원점? 부도심이 위치한 곳은? 광역 경제권이 형성되어 있는 곳은? 도시 중심에 대한 기준에 있어 다른 기준에 비해 가장 편차가 적은 시청사 소재 좌표를 도심의 기준으로 판단하되, 무시할 수 없는 오차를 고려해 범주형 데이터로서 참고하도록 한다.
</p>



```r
cityhall <- read_excel("./geocode_comparison_v5.xlsx", sheet=2) %>% select(c(3,4,6,7))
```

```
## New names:
## * `` -> `...5`
## * `` -> `...7`
## * `` -> `...8`
## * `` -> `...9`
```

```r
colnames(cityhall) <- c("GeographicArea", "Population2010", "hallcoordY", "hallcoordX")
```



```r
# 연고지 정보 정리
mlb_3$Basecity <-  mlb_3$Location %>% str_replace("\\,[^.]*$", "")
```



```r
mlb_3$Basecity[mlb_3$Team=="New York Yankees"] <- "New York"
mlb_3$Basecity[mlb_3$Team=="New York Mets"] <-  "New York"
mlb_3$Basecity[mlb_3$Team=="Texas Rangers"] <-  "Dallas"
mlb_3$Basecity[mlb_3$Team=="Atlanta Braves"] <-  "Atlanta"
mlb_3$Basecity[mlb_3$Team=="Washington Nationals"] <-  "Washington"
mlb_3$Basecity[mlb_3$Team=="Tampa Bay Rays"] <-  "Tampa"
```



```r
mlb_3$GeographicArea <- paste(mlb_3$Basecity, "city")
mlb_3$GeographicArea[mlb_3$Team=="Washington Nationals"] <- "Washington city, DC"
mlb_3$GeographicArea[mlb_3$Team=="St. Louis Cardinals"] <-  "St. Louis city, MO"
mlb_3$GeographicArea[mlb_3$Basecity=="New York"] <- "New York city, NY"
mlb_3$GeographicArea[mlb_3$Basecity=="Chicago"] <- "Chicago city, IL"
mlb_3$GeographicArea[mlb_3$Basecity=="Cincinnati"] <-  "Cincinnati city, OH"
mlb_3$GeographicArea[mlb_3$Basecity=="Philadelphia"] <- "Philadelphia city, PA"
mlb_3$GeographicArea[mlb_3$Basecity=="Boston"] <- "Boston city, MA"
mlb_3$GeographicArea[mlb_3$Basecity=="Kansas City"] <- "Kansas City city, MO"
mlb_3$GeographicArea[mlb_3$Basecity=="Minneapolis"] <- "Minneapolis city, MN"
mlb_3$GeographicArea[mlb_3$Basecity=="Cleveland"] <- "Cleveland city, OH"
```



```r
mlb_base <- mlb_3 %>% left_join(cityhall, by="GeographicArea")
```



```r
mlb_base[,c(13, 14, 17:19)] <- lapply(mlb_base[,c(13, 14, 17:19)], 
                         function(x) as.double(as.character(x)))
```

```
## Warning in FUN(X[[i]], ...): NAs
## introduced by coercion
```
### 지오코딩으로 나타내기

```r
#접속 가능 ip를 설정하여 무단 도용을 막자
ggmap::register_google(key = "AIzaSyCQnWe8Pzy7Erl9t6Qcq5kqgri9tgrrrt4", write = TRUE)
```

```
## i Replacing old key (AIzaSyCQnWe8Pzy7Erl9t6Qcq5kqgri9tgrrrt4) with new key in C:/Users/KRX/Documents/.Renviron
```




```r
uscenter <- c(-98.5795, 39.828175)
us1 <- leaflet(data=mlb_base) |> setView(lng = uscenter[1], 
                           lat = uscenter[2], 
                           zoom = 3) |> addProviderTiles("Stamen.Toner")
us1
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)



```r
park_info <- 
  paste(mlb_base$Name, 
        "<br>팀:", mlb_base$Team,
        "<br>개장연도", mlb_base$Opened,
        "<br>수용인원:", mlb_base$Capacity, "명",
        "<br>위키페이지:", 
        paste('<a href="', mlb_base$url,'" target="_blank">', mlb_base$Name, '</a>'))

city_info <- 
  paste(mlb_base$Basecity, "시청사",
        "<br>2010년 인구:", mlb_base$Population2010, "명")
```



```r
mlbmap <- us1 |> addCircleMarkers(~coordX, ~coordY,
                          color=mlb_base$colorcode,
                          radius = 4,
                          fillOpacity = 1,
                          stroke = F,
                          popup=~park_info
                          )
```

```
## Warning in validateCoords(lng, lat,
## funcName): Data contains 30 rows with
## either missing or invalid lat/lon values
## and will be ignored
```

```r
mlbmap
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)



```r
awesome <- makeAwesomeIcon(
  icon = "cityhall",
  iconColor = "black",
  markerColor = "blue",
  library = "fa"
)

mlbmap |> addAwesomeMarkers(~hallcoordX, ~hallcoordY,
                        popup=~city_info,
                        icon = cityhall
                        )
```

```
## Warning: Unknown or uninitialised column: `library`.
## Unknown or uninitialised column: `library`.
## Unknown or uninitialised column: `library`.
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

### 거리 구하기

```r
mlb_dist <- mlb_base %>% 
  transmute(Team, 
            "park_coord" = paste0(mlb_base$coordY, ", ", mlb_base$coordX),
            "hall_coord" = paste0(mlb_base$hallcoordY, ", ",  mlb_base$hallcoordX))

dist <- mapdist(from = mlb_dist$park_coord, to = mlb_dist$hall_coord)
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-104.99417&destinations=39.739242,+-104.990967&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-104.99417&destinations=39.739242,+-104.990967&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-112.06694&destinations=33.448782,+-112.077088&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-112.06694&destinations=33.448782,+-112.077088&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-117.1566&destinations=32.716788,+-117.162931&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-117.1566&destinations=32.716788,+-117.162931&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-117.88278&destinations=33.834521,+-117.91277&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-117.88278&destinations=33.834521,+-117.91277&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-118.24&destinations=34.053989,+-118.2434&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-118.24&destinations=34.053989,+-118.2434&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-122.20056&destinations=37.8053,+-122.2725&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-122.20056&destinations=37.8053,+-122.2725&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-122.333&destinations=47.603257,+-122.330211&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-122.333&destinations=47.603257,+-122.330211&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-122.38917&destinations=37.778503,+-122.418307&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-122.38917&destinations=37.778503,+-122.418307&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-71.09775&destinations=42.359184,+-71.057761&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-71.09775&destinations=42.359184,+-71.057761&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-73.84583&destinations=40.713415,+-74.004809&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-73.84583&destinations=40.713415,+-74.004809&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-73.92639&destinations=40.713415,+-74.004809&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-73.92639&destinations=40.713415,+-74.004809&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-75.16639&destinations=39.952218,+-75.162596&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-75.16639&destinations=39.952218,+-75.162596&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-76.62167&destinations=39.290864,+-76.610524&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-76.62167&destinations=39.290864,+-76.610524&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-77.0075&destinations=38.895374,+-77.030827&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-77.0075&destinations=38.895374,+-77.030827&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-79.38917&destinations=43.653908,+-79.384293&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-79.38917&destinations=43.653908,+-79.384293&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-80.00583&destinations=40.438413,+-79.997182&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-80.00583&destinations=40.438413,+-79.997182&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-80.21972&destinations=25.728905,+-80.233904&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-80.21972&destinations=25.728905,+-80.233904&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-81.68528&destinations=41.504679,+-81.694363&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-81.68528&destinations=41.504679,+-81.694363&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-82.65333&destinations=27.947457,+-82.457345&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-82.65333&destinations=27.947457,+-82.457345&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-83.04861&destinations=42.328913,+-83.04448&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-83.04861&destinations=42.328913,+-83.04448&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-84.468&destinations=33.748731,+-84.390436&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-84.468&destinations=33.748731,+-84.390436&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-84.50667&destinations=39.104257,+-84.519061&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-84.50667&destinations=39.104257,+-84.519061&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-87.63389&destinations=41.883484,+-87.632337&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-87.63389&destinations=41.883484,+-87.632337&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-87.65556&destinations=41.883484,+-87.632337&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-87.65556&destinations=41.883484,+-87.632337&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-87.97111&destinations=43.041766,+-87.909769&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-87.97111&destinations=43.041766,+-87.909769&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-90.19306&destinations=38.62653,+-90.199393&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-90.19306&destinations=38.62653,+-90.199393&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-93.27833&destinations=44.976847,+-93.265781&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-93.27833&destinations=44.976847,+-93.265781&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-94.48&destinations=39.09984,+-94.578363&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-94.48&destinations=39.09984,+-94.578363&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-95.35556&destinations=29.760205,+-95.369678&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-95.35556&destinations=29.760205,+-95.369678&key=xxx&mode=driving]8;;>
```

```
## i <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-97.08417&destinations=32.776901,+-96.795786&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=NA,+-97.08417&destinations=32.776901,+-96.795786&key=xxx&mode=driving]8;;>
```

```
## Error in `bind_cols()`:
## ! Can't recycle `..1` (size 30) to match `..2` (size 6).
```



```r
mlb_dist <- mlb_dist %>% left_join(dist ,by=c("park_coord"="from", "hall_coord" ="to")) %>% select(c(1,2,3,5,8))
mlb_stad <- mlb_base %>% left_join(mlb_dist, by="Team")
```

```r
addkm <- function(x, ...)
    format(paste0(x, "km"), ...)
addmin <- function(x, ...)
    format(paste0(x, "분"), ...)
addyear <- function(x, ...)
    format(paste0(x, "년"), ...)
```



```r
g3 <- mlb_stad %>% ggplot() +
  geom_point(aes(x=Opened, y=km, color=`Rooftype`)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1900, 2022),
                     breaks = seq(1900, 2022, by = 10),
                     labels = addyear) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,40),
                     breaks = seq(0, 40, by = 5),
                     labels = addkm) +
  labs(title="현재 쓰이는 MLB 구장의 도심으로부터의 거리(KM)") +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(size=0.5, 
                                         linetype="solid", 
                                         colour ="darkblue"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g3
```

```
## Warning: Removed 30 rows containing missing values
## (`geom_point()`).
```

![plot of chunk unnamed-chunk-32](figure/unnamed-chunk-32-1.png)

1990년대 지어진 구장들이 도심으로부터 15km 내에 주로 위치한 반면에,2000년대 수반부터 다시 20km 이상 떨어진 구장들이 등장하는 것을 알 수 있다.


```r
g4 <- mlb_stad %>% ggplot() +
  geom_point(aes(x=Opened, y=minutes, color=`Rooftype`)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1900, 2022),
                     breaks = seq(1900, 2022, by = 10),
                     labels = addyear) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 35),
                     breaks = seq(0, 35, by = 5),
                     labels = addmin) +
  labs(title="현재 쓰이는 MLB 구장의 도심으로부터의 거리(운전시간)") +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(size=0.5, 
                                         linetype="solid", 
                                         colour ="darkblue"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g4
```

```
## Warning: Removed 30 rows containing missing values
## (`geom_point()`).
```

![plot of chunk unnamed-chunk-33](figure/unnamed-chunk-33-1.png)
도심으로부터 운전 시간 역시, 점차 증가하는 것을 알 수 있다.
# 결론 및 마무리
## 이미지 저장


```r
ggsave("./mlb_cap.svg", g1)
```

```
## Saving 7 x 7 in image
```

```r
ggsave("./mlb_cost.svg", g2)
```

```
## Saving 7 x 7 in image
```

## 파일 저장

```r
write.csv(mlb_stad, "./mlb_stadium.csv")
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


