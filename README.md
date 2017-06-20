================

題目
----

玩寶可夢真的那麼危險？

資料介紹與來源
--------------

●A2\_record是事故類別：A2類的交通資料紀錄，A2的意思是交通事故當事人一人以上受傷(不管事輕傷或重傷)或超過24小時後死亡。 ●pokemon2 pokemontype 是全台各縣市寶可夢巢穴名稱及地點和CP值。 ●內政部警政署 (車禍事故資料) ●<http://kikinote.net/article/115309.html> (寶可夢資料) ●[http://www.otaku-hk.com/news/Pokemon-Go-%E5%B0%8F%E7%B2%BE%E9%9D%88%E6%95%B8%E6%93%9A%E8%AA%BF%E6%95%B4%E7%B8%BD%E8%A1%A8(CP,-%E6%94%BB%E5%8F%8A%E9%98%B2%E8%AE%8A%E5%8C%96)/412-1(寶可夢資料)](http://www.otaku-hk.com/news/Pokemon-Go-%E5%B0%8F%E7%B2%BE%E9%9D%88%E6%95%B8%E6%93%9A%E8%AA%BF%E6%95%B4%E7%B8%BD%E8%A1%A8(CP,-%E6%94%BB%E5%8F%8A%E9%98%B2%E8%AE%8A%E5%8C%96)/412-1(寶可夢資料))

格式
----

Excel檔案及網頁內容

分析議題
--------

●寶可夢GO是一款基於位置服務的擴增實境類手機遊戲，由任天堂公司、精靈寶可夢公司授權，Niantic, Inc.負責開發和營運。於2016年7月起在iOS和Android平台上發布。該遊戲允許玩家以現實世界為平台，捕捉、戰鬥、訓練和交易虛擬怪獸「寶可夢」。 ●台灣去年也盛行一時，因而產生一些問題，像是有些虛擬怪物會出現在特別的地方(例如：海邊、山區等等偏僻處)，使玩家會讓自己深入危險處，也有一些玩家會一邊開(騎)車一邊抓寶，以上問題皆可能造成自身安全受害。

假設
----

●自始Pokemon Go盛行，政府一再呼籲民眾玩樂之餘也要注意交通安全，卻仍在新聞媒體層出不窮出現因為玩寶可夢而致使交通打結的景況，欲藉由此期末專題了解交通是否因為寶可夢的盛行而有所影響，受傷人數多的區域是否皆為相同的寶可夢。 ●寶可夢是從105/08/06開始開放台灣下載的，因此假設是從該日期開始事故率逐漸提升。

分析結果
--------

●資料匯入

``` r
library(readxl)
```

    ## Warning: package 'readxl' was built under R version 3.3.3

``` r
A2_record2 <- read_excel("C:/Users/user/Desktop/A2 record2.xlsx", 
    skip = 2)
```

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in E145012 / R145012C5: got '頝舫�餈�(��)'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in E177678 / R177678C5: got '��)(��)'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in E201565 / R201565C5: got '��)'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in E260891 / R260891C5: got '摰�銵�139撌�(��)'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in E267664 / R267664C5: got '�楝��(��)'

    ## Warning in read_fun(path = path, sheet = sheet, limits = limits, shim =
    ## shim, : Expecting numeric in E282330 / R282330C5: got '銝銵(��)'

``` r
#View(A2_record2)

library(readxl)
pokemon2 <- read_excel("C:/Users/user/Desktop/pokemon2.xlsx")
#View(pokemon2)

library(readxl)
pokemontype <- read_excel("C:/Users/user/Desktop/pokemontype.xlsx")
#View(pokemontype)
```

●抓出行政區資料

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.3.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
A2 <- A2_record2[grepl("區",A2_record2$發生地點),]
```

●統計車禍紀錄中前五多的受傷人數地區

``` r
library(dplyr)
SumofHurt <- group_by(A2, place=`發生地點`)%>%
              summarise(ninjuries=sum(`受傷人數`))%>%
      arrange(desc(ninjuries))

#install.packages("ggplot2")
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.3.3

``` r
rank<-head(SumofHurt,decreasing = TRUE,5)
#install.packages("tidyr")
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 3.3.3

``` r
A<-spread(rank,key=place,value=ninjuries)
barplot(as.matrix(A),width =1,space=0.2,main="前五名受傷人數",xlab="地區名稱",ylab="受傷人數")
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png) ●根據前五多地區將該地區巢穴資料合併

``` r
no1<-pokemon2[grepl("高雄市三民區",pokemon2$巢穴地址),]
no2<-pokemon2[grepl("臺中市西屯區",pokemon2$巢穴地址),]
no3<-pokemon2[grepl("高雄市鳳山區",pokemon2$巢穴地址),]
no4<-pokemon2[grepl("桃園市桃園區",pokemon2$巢穴地址),]
no5<-pokemon2[grepl("新北市板橋區",pokemon2$巢穴地址),]
topfive<-rbind(no1,no2,no3,no4,no5)
SumofType<- group_by(topfive, name=`寶可夢名稱`)%>%
              summarise(ntype=n())%>%
      arrange(name)
W<-spread(SumofType,key=name,value=ntype)
barplot(as.matrix(W),width =10,space=0.2,main="前五名地區寶可夢種類統計",xlab="名稱",ylab="次數")
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png) ●統計每日事故量(折線圖與散布圖) -互動式圖表用於觀察單一日期之事故數量。 -散步圖上畫上趨勢線。

``` r
SumofTime <- group_by(A2, day=`發生日期`)%>%
              summarise(ncount=n())%>%
      arrange(day)
B<-spread(SumofTime,key=day,value=ncount)

qplot(x=day,                               
      y=ncount,                              
      data=SumofTime,                      
      geom="point",                         # 圖形=scatter plot
      main = "每日統計",  
      xlab="Date",                          
      ylab="發生次數"                          # 以顏色標註月份，複合式的散布圖
      )
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
library(plotly)
```

    ## Warning: package 'plotly' was built under R version 3.3.3

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
ggplotly()
```

    ## We recommend that you use the dev version of ggplot2 with `ggplotly()`
    ## Install it with: `devtools::install_github('hadley/ggplot2')`

<!--html_preserve-->

<!--/html_preserve-->
``` r
SumofTime$day<-c(1:366)
#SumofTime$day<-as.Date(as.character(SumofTime$day), format="%Y%m%d")
plot(SumofTime,type="l",xlab="Date",ylab="發生次數",main="每日統計",xlim=c(1,366),ylim = c(300,900))
```

![](README_files/figure-markdown_github/unnamed-chunk-5-3.png)

``` r
qplot(day, ncount, 
       data = SumofTime,
       geom = c("point", "smooth"))
```

    ## `geom_smooth()` using method = 'loess'

![](README_files/figure-markdown_github/unnamed-chunk-5-4.png) \#\#分析結果可能解決的問題 根據前五多地區多方宣導寶可夢之相關安全事宜。

組員名單與分工
--------------

●李士閎：找資料(50%)、CODE(55%)、書面彙整(50%)、上台簡報製作(45%)。 ●林宇辰：找資料(50%)、CODE(45%)、書面彙整(50%)、上台簡報製作(55%)。
