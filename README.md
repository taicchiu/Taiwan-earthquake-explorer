
# Taiwan Earthquake Event Explorer (2020~2022/01)

一個以 R Shiny 打造的互動式地震資料探索工具，視覺化 2020 年至 2022 年 1 月間台灣地區的地震事件，讓使用者能依深度、規模與時間篩選事件，並透過地圖與統計圖表觀察分布特徵。

## 功能特色

### Spatial Distribution
- 互動式地圖（Leaflet），以顏色標示地震深度（0–200 km），圓點大小對應地震規模（M4–M7+）
- 即時統計卡片：事件總數、最大規模、平均深度
- 可用側邊欄篩選器依「深度」「規模」「日期區間」動態更新地圖

### Statistical Analysis
- **Temporal Trends**：按月份呈現各年度（2020, 2021, 2022）地震事件數量趨勢
- **Magnitude vs Depth**：規模與深度的分布，依年度分色比較

## 資料篩選器

- **Depth (km)**：0–200 km，可調整範圍滑桿
- **Magnitude (M_L)**：4–8，可調整範圍滑桿
- **Select Date**：自訂查詢時間區間

## 資料來源

Autobats Earthquake Catalog（2020–2022/01）

## 使用技術

- R Shiny
- bslib（介面主題與版面）
- leaflet（互動地圖）
- plotly（統計圖表）

## 如何在本機執行

1. 複製這個 repository：
```bash
   git clone https://github.com/taicchiu/Taiwan-earthquake-explorer.git
```
2. 在 RStudio 開啟 `taiwan_eq.Rproj`
3. 安裝所需要的packages：
```r
   install.packages(c("shiny", "bslib", "leaflet", "plotly"))
```
4. 開啟 `app.R`，點擊 **Run App**

## 作者

Tai-Chia Chiu

