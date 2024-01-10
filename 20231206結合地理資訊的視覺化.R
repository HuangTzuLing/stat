dat = read.csv("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/data10_1.csv", header = TRUE, fileEncoding = 'CP950')
head(dat)

library(RgoogleMaps)

## 畫出台南市在2015年大流行時每天的流行狀況 ##
#先獲得台南市的google地圖作為底圖
lat = c(22.88751, 23.41373)
lon = c(120.023, 120.6562)
center = c(mean(lat), mean(lon))
zoom = min(MaxZoom(range(lat), range(lon)))
#輸出「一般地圖」
MyMap = GetMap(center = center, zoom = zoom, API_console_key = 'AIzaSyA4DVFtF70aXE7RgrXViy2z5Ku2pMkVxFI')
PlotOnStaticMap(MyMap)
#輸出「衛星圖」
MyMap2 = GetMap(center = center, zoom = zoom, maptype = "satellite", API_console_key = 'AIzaSyA4DVFtF70aXE7RgrXViy2z5Ku2pMkVxFI')
PlotOnStaticMap(MyMap2)

#找出台南市在2015年9月份整個月份的登革熱病例
dat[,1] = as.Date(dat[,1])
subdat = dat[dat[,1] <= as.Date("2015-09-30") & dat[,1] >= as.Date("2015-09-01") & dat[,6] == "台南市",]
nrow(subdat)
#透過函數「PlotOnStaticMap()」把點放到MyMap上面
PlotOnStaticMap(MyMap, lat = subdat$最小統計區中心點Y, lon = subdat$最小統計區中心點X, pch = 19, col = "red", cex = 1)

##將散布圖的點做顏色密度的改:
x1 <- subdat$最小統計區中心點Y
x2 <- subdat$最小統計區中心點X
df <- data.frame(x1,x2)

## Use densCols() output to get density at each point
x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
df$dens <- col2rgb(x)[1,] + 1L

## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
df$col <- cols[df$dens]

PlotOnStaticMap(MyMap, lat = df$x1, lon = df$x2, pch = 19, col = df$col, cex = 1.5)
##END

## 畫行政區邊界的圖 ##
mapdat = read.table("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/鄉村資料.txt", fileEncoding = "UTF-8")
head(mapdat)

#找出與台南市有關的鄉里
Tainan.mapdat = mapdat[mapdat[,4] == "臺南市",]
nrow(Tainan.mapdat)

#利用迴圈把村里邊界標上去
MysubMap = GetMap(center = center, zoom = zoom, maptype = "satellite", API_console_key = 'AIzaSyA4DVFtF70aXE7RgrXViy2z5Ku2pMkVxFI')
PlotOnStaticMap(MysubMap)

for (i in 1:nrow(Tainan.mapdat)) {
  linedat = read.csv(paste0("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/編號", Tainan.mapdat[i,1], ".csv"), header = TRUE, fileEncoding = 'CP950')
  PlotOnStaticMap(MysubMap, lat = linedat[,2], lon = linedat[,1], FUN = lines, add = TRUE, col = "red")
}

#再把剛剛的點打上去
MysubMap = GetMap(center = center, zoom = zoom, maptype = "satellite", API_console_key = 'AIzaSyA4DVFtF70aXE7RgrXViy2z5Ku2pMkVxFI')
PlotOnStaticMap(MysubMap)

for (i in 1:nrow(Tainan.mapdat)) {
  linedat = read.csv(paste0("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/編號", Tainan.mapdat[i,1], ".csv"), header = TRUE, fileEncoding = 'CP950')
  PlotOnStaticMap(MysubMap, lat = linedat[,2], lon = linedat[,1], FUN = lines, add = TRUE, col = "red")
}
PlotOnStaticMap(MysubMap, lat = df$x1, lon = df$x2, pch = 19, col = df$col, add = TRUE)

#利用函數「polygon()」畫出地圖邊界
lat = c(22.88751, 23.41373)
lon = c(120.023, 120.6562)
plot.new()
plot.window(xlim = lon, ylim = lat)

for (i in 1:nrow(Tainan.mapdat)) {
  linedat = read.csv(paste0("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/編號", Tainan.mapdat[i,1], ".csv"), header = TRUE, fileEncoding = 'CP950')
  polygon(linedat[,1], linedat[,2], col = "white")
}

#結合以上兩個資訊，畫出各行政區的發病狀況
lat = c(22.88751, 23.41373)
lon = c(120.023, 120.6562)
plot.new()
plot.window(xlim = lon, ylim = lat)

for (i in 1:nrow(Tainan.mapdat)) {
  linedat = read.csv(paste0("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/編號", Tainan.mapdat[i,1], ".csv"), header = TRUE, fileEncoding = 'CP950')
  n.sample = sum(subdat[,16] == as.character(Tainan.mapdat[i,2]))
  if (n.sample == 0) {COL = "#FFFFFF"} 
  else if (n.sample <= 3) {COL = "#000099"} 
  else if (n.sample <= 10) {COL = "#00FEFF"} 
  else if (n.sample <= 30) {COL = "#45FE4F"} 
  else if (n.sample <= 50) {COL = "#FCFF00"} 
  else if (n.sample <= 100) {COL = "#FF9400"} 
  else {COL = "#FF3100"} 
  polygon(linedat[,1], linedat[,2], col = COL)
}

#（承上）使用8位色碼，讓顏色變成透明的
lat = c(22.88751, 23.41373)
lon = c(120.023, 120.6562)
plot.new()
plot.window(xlim = lon, ylim = lat)

for (i in 1:nrow(Tainan.mapdat)) {
  linedat = read.csv(paste0("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/編號", Tainan.mapdat[i,1], ".csv"), header = TRUE, fileEncoding = 'CP950')
  n.sample = sum(subdat[,16] == as.character(Tainan.mapdat[i,2]))
  if (n.sample == 0) {COL = "#FFFFFF80"} 
  else if (n.sample <= 3) {COL = "#00009980"} 
  else if (n.sample <= 10) {COL = "#00FEFF80"} 
  else if (n.sample <= 30) {COL = "#45FE4F80"} 
  else if (n.sample <= 50) {COL = "#FCFF0080"} 
  else if (n.sample <= 100) {COL = "#FF940080"} 
  else {COL = "#FF310080"} 
  polygon(linedat[,1], linedat[,2], col = COL)
}

#將圖片疊在google地圖上
MysubMap = GetMap(center = center, zoom = zoom, maptype = "satellite", API_console_key = 'AIzaSyA4DVFtF70aXE7RgrXViy2z5Ku2pMkVxFI')
PlotOnStaticMap(MysubMap)

for (i in 1:nrow(Tainan.mapdat)) {
  linedat = read.csv(paste0("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/編號", Tainan.mapdat[i,1], ".csv"), header = TRUE, fileEncoding = 'CP950')
  n.sample = sum(subdat[,16] == as.character(Tainan.mapdat[i,2]))
  if (n.sample == 0) {COL = "#FFFFFF80"} 
  else if (n.sample <= 3) {COL = "#00009980"} 
  else if (n.sample <= 10) {COL = "#00FEFF80"} 
  else if (n.sample <= 30) {COL = "#45FE4F80"} 
  else if (n.sample <= 50) {COL = "#FCFF0080"} 
  else if (n.sample <= 100) {COL = "#FF940080"} 
  else {COL = "#FF310080"} 
  PlotOnStaticMap(MysubMap, lat = linedat[,2], lon = linedat[,1], FUN = polygon, add = TRUE, col = COL)
}

'''
練習1：逐日變化圖
現在，你已經會結合地理資訊來進行視覺化了。
– 假定你希望呈現整個9月份『逐日』的『累積個案散布圖』，你有辦法熟練得操作你的畫圖技術了嗎?

如果你想把一系列的圖片轉換成GIF檔案，可以使用這個網站:http://gifmaker.me/
– 如果你想要多學學結合地理資訊系統的視覺化技術，可以參考Spatial data in R: Using R as a GIS：https://pakillo.github.io/R-GIS-tutorial/
'''
MysubMap = GetMap(center = center, zoom = zoom, maptype = "satellite", API_console_key = 'AIzaSyA4DVFtF70aXE7RgrXViy2z5Ku2pMkVxFI')

pdf('Tainan.pdf', width = 7, height = 7)

for (k in 1:30) {
  subdat = dat[dat[,1] <= as.Date(paste0("2015-09-", k)) & dat[,1] >= as.Date("2015-09-01") & dat[,6] == "台南市",]
  PlotOnStaticMap(MysubMap)
  for (i in 1:nrow(Tainan.mapdat)) {
    linedat = read.csv(paste0("/Users/ling/FJU/醫藥臨床統計分析與實作/Lesson10/TWmap/TWmap/編號", Tainan.mapdat[i,1], ".csv"), header = TRUE, fileEncoding = 'CP950')
    n.sample = sum(subdat[,16] == as.character(Tainan.mapdat[i,2]))
    if (n.sample == 0) {COL = "#FFFFFF80"} 
    else if (n.sample <= 3) {COL = "#00009980"} 
    else if (n.sample <= 10) {COL = "#00FEFF80"} 
    else if (n.sample <= 30) {COL = "#45FE4F80"} 
    else if (n.sample <= 50) {COL = "#FCFF0080"} 
    else if (n.sample <= 100) {COL = "#FF940080"} 
    else {COL = "#FF310080"} 
    PlotOnStaticMap(MysubMap, lat = linedat[,2], lon = linedat[,1], FUN = polygon, add = TRUE, col = COL)
  }
  text(-200, -250, as.Date(paste0("2015-09-", k)), cex = 2, col = "red")
}

dev.off()










