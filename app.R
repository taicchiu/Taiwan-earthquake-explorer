library(shiny)
library(leaflet)
library(plotly)
library(bslib)
library(bsicons)
library(tidyverse)
library(sf)
library(leafpop)
library(RFOC)

# --- 1. 資料預處理 ---
# 假設資料檔案在同目錄
dat1 <- read.csv("2020-2022_autobats.csv", skip=9)[-1,]
dat1 <- dat1[1:9]
dat1$Year <- substr(dat1$Date, 1, 4)
dat1$Month <- substr(dat1$Date, 6, 7)
dat1$DateTime <- paste(dat1$Date, dat1$Time)
dat1$lng_val <- as.numeric(dat1$Longitude)
dat1$lat_val <- as.numeric(dat1$Latitude) #複製一份經緯度欄位，以便後續使用

dat1_sf <- st_as_sf(dat1, coords=c('Longitude', 'Latitude'), crs=4326)
dat1_sf$diffDate <- as.numeric(as.Date(dat1_sf$Date))
dat1_sf$ML <- as.numeric(dat1_sf$ML)
dat1_sf$CWB.Depth <- as.numeric(dat1_sf$CWB.Depth)

# --- 2. UI part ---
ui <- page_navbar(
  title = "Taiwan Earthquake Event Explorer (2020~2022/01)",
  theme = bs_theme(bootswatch = "lux", primary = "#2c3e50"), #simple style
  fillable = TRUE,

  header = tags$head( #調整style
    tags$style(HTML(" 
      /* 1. 強制Leaflet Popup內的圖片不要超出框框 */
      .leaflet-popup-content img {
        max-width: 100% !important;
        height: auto !important;
        display: block;
        margin: 5px auto; /* 圖片置中並給一點間距 */
      }
      
      /* 2. 強制滑鼠點擊Popup內容置中 */
      .leaflet-popup-content {
        text-align: center !important;
        width: auto !important;
      }

      /* 3. 調整滑鼠放置Popup內容置中*/
      .leaflet-tooltip {
        text-align: center;
        white-space: nowrap !important; /* 確保提示框文字不隨意斷行 */
      }
      }
      /* 調整日期選擇視窗的z-index */
      .datepicker {
        z-index: 9999 !important;
      }
      
      #date input {
        font-size: 12px !important;  /* 調整字體大小 */
        padding: 0px 0px !important; /* 縮小內距 */
        height: 50px !important;    /* 調整框高度 */
      }
    "))
  ),
  
  # Sidebar
  sidebar = sidebar(
    title = "Data Filters",
    sliderInput("dep", "Depth (km)", min = 0, max = 200, value = c(0, 50)),
    sliderInput("ml", HTML("Magnitude (M<sub>L</sub>)"), min = 4, max = 8, value = c(4, 6), step = 0.1),
    dateRangeInput("date", "Select Date", 
                   start = "2020-01-01", end = "2022-01-05",
                   min = "2020-01-01", max = "2022-01-05"),
    hr(),
    helpText(HTML(
      "<div style='font-size: 12px; color: #7f8c8d;'>
      Data source: Autobats Earthquake Catalog<br/>Author: Tai
    </div>"
    ))
    #helpText("Author: Tai")
  ),
  
  # Main page 1: Map
  nav_panel(
    title = "Spatial Distribution",
    icon = bs_icon("map"),
    
      # 上方數值卡片
    layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        height = "120px",
        value_box(
          title = span("Total Events", style="font-size: 16px;"),
          value = span(textOutput("count_text"), style="font-size: 20px;"),
          showcase = bs_icon("activity", size="0.5em"),
          theme = "primary"
        ),
        value_box(
          title = span("Max Magnitude", style="font-size: 16px;"),
          value = span(textOutput("max_ml_text"), style="font-size: 20px;"),
          showcase = bs_icon("exclamation-triangle", size="0.5em"),
          theme = value_box_theme(bg = "#CD0000", fg = "#ffffff")
        ),
        value_box(
          title = span("Average Depth", style="font-size: 16px;"),
          value = span(textOutput("avg_dep_text"), style="font-size: 20px;"),
          showcase = bs_icon("chevron-double-down", size="0.5em"),
          theme = value_box_theme(bg = "#8B4510", fg = "#ffffff")
        )
      ),
      # 地圖本體
      card(
        full_screen = TRUE,
        card_header("Earthquake Map (Color by Depth)"),
        leafletOutput("eqmap", height="600px")
      )
    ),
  
  # Main page 2: Statistical charts
  nav_panel(
    title = "Statistical Analysis",
    icon = bs_icon("bar-chart"),
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Temporal Trends"),
        plotlyOutput("stat1")
      ),
      card(
        card_header("Magnitude vs Depth"),
        plotlyOutput("stat2")
      )
    )
  )
)

# --- 3. Server ---
server <- function(input, output, session) {
  
  # 使用 reactive 統一處理資料過濾，減少運算負擔
  filtered_data <- reactive({
    dat1_sf %>%
      filter(ML >= input$ml[1], ML <= input$ml[2],
             diffDate >= as.numeric(input$date[1]),
             diffDate <= as.numeric(input$date[2]),
             CWB.Depth >= input$dep[1], CWB.Depth <= input$dep[2])
  })
  
  # 數值卡片計算
  output$count_text <- renderText({ nrow(filtered_data()) })
  output$max_ml_text <- renderText({ 
    val <- max(filtered_data()$ML, na.rm = TRUE)
    if(is.infinite(val)) "0" else val 
  })
  output$avg_dep_text <- renderText({ 
    val <- round(mean(filtered_data()$CWB.Depth, na.rm = TRUE), 1)
    if(is.nan(val)) "0 km" else paste(val, "km")
  })
  
  # Earthquake map
  # Earthquake map
  output$eqmap <- renderLeaflet({
    res <- filtered_data()
    if(nrow(res) == 0) return(leaflet() %>% addTiles() %>% setView(120.95, 23.7, zoom = 7))
    
    # 1. 取得前端 Slider 區間的整數範圍
    # 使用 floor 確保如果使用者拉到 4.5，我們從 4 開始算；拉到 7.5，我們算到 7
    slider_min <- floor(input$ml[1])
    slider_max <- floor(input$ml[2])
    
    # 確保範圍在 4 ~ 8 之間
    slider_min <- max(4, min(8, slider_min))
    slider_max <- max(4, min(8, slider_max))
    
    # 2. 定義 4, 5, 6, 7, 8 各自固定的半徑大小 (px)
    # 這樣不論 Slider 怎麼拉，4 永遠是 5px，5 永遠是 8px... 依此類推
    radius_lookup <- c("4" = 8, "5" = 12, "6" = 16, "7" = 20, "8" = 24)
    
    # Focal mechanism
    fm <- list()
    display_res <- head(res, 100) 
    
    for (i in 1:nrow(display_res)){
      par(mar = c(2, 2, 2, 2))
      mc <- CONVERTSDR(display_res$strike1[i], display_res$dip1[i], display_res$slip1[i])
      plotMEC(mc, detail=0, up=F)
      fm[[i]] <- recordPlot()
    }
    
    depth_bins <- c(0, 10, 20, 30, 50, 100, 200)
    pal <- colorBin(palette = "inferno", domain = c(0, 200), bins = depth_bins, reverse = TRUE)
    
    # --- 關鍵修正 1：將地圖上的圓圈依照「整數級別」固定大小 ---
    # 使用 floor() 讓 4.1 ~ 4.9 變成整數 "4"，並對應到 radius_lookup
    display_res$ml_floor <- as.character(floor(display_res$ML))
    display_res$marker_radius <- radius_lookup[display_res$ml_floor]
    
    # 預防萬一有超出 4-8 範圍的極端值，給予預設大小
    display_res$marker_radius[is.na(display_res$marker_radius)] <- 5
    
    # --- 關鍵修正 2：動態生成分級 HTML Legend ---
    # 根據使用者當前拉的 Slider 範圍 (例如 4 到 8)，動態決定要顯示哪些級別的圖例
    active_levels <- as.character(slider_min:slider_max)
    
    max_rad <- max(radius_lookup) # 最大半徑，用來固定 SVG 寬度以利對齊
    svg_max_wh <- max_rad * 2 + 4
    
    # 利用寫好的對應表，動態拼出 HTML 內容
    legend_items <- lapply(active_levels, function(lvl) {
      rad <- radius_lookup[lvl]
      svg_h <- rad * 2 + 4
      
      paste0(
        '<div style="display:flex; align-items:center; gap:8px; margin-top:4px;">',
        '<svg width="', svg_max_wh, '" height="', svg_h, '">',
        '<circle cx="', max_rad + 2, '" cy="', rad + 2, '" r="', rad, '" fill="#888" opacity="0.6"/>',
        '</svg> ', lvl, ' - ', as.numeric(lvl) + 1,
        '</div>'
      )
    })
    
    dynamic_legend <- paste0(
      '<div style="background: white; padding: 10px; border-radius: 5px; ',
      'border: 1px solid #ccc; font-size: 13px;">',
      '<b>Magnitude (M<sub>L</sub>)</b><br/>',
      paste(legend_items, collapse = ""),
      '</div>'
    )
    
    # 3. 渲染地圖
    leaflet(display_res) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        radius = ~marker_radius,  # 這裡的半徑已經是固定的分級大小
        color = "white", weight = 1,
        fillColor = ~pal(CWB.Depth), fillOpacity = 0.7,
        popup = popupGraph(fm, width = 300, height = 200),
        
        label = ~lapply(paste0(
          "<div style='font-size: 12px; font-family: sxans-serif; text-align: left;'>",
          "<b>Time: </b>", DateTime, "<br/>",
          "<b>Magnitude (M<sub>L</sub>): </b>", ML, "<br/>",
          "<b>Depth: </b>", CWB.Depth, " km<br/>",
          "<b>Location: </b>", round(as.numeric(lng_val), 3), "°E, ", 
          round(as.numeric(lat_val), 3), "°N",
          "</div>"
        ), htmltools::HTML)
      ) %>%
      addLegend(pal = pal, values = ~CWB.Depth, title = "Depth (km)", position = "bottomright") %>%
      addControl(
        html = dynamic_legend, # 帶入動態分級的 HTML 圖例
        position = "bottomleft"
      )
  })
  
  # 圖表 1
  output$stat1 <- renderPlotly({
    dd <- data.frame(xtabs(~Year + Month, filtered_data()))
    dd$Year <- as.factor(dd$Year)
    plot_ly(dd, x = ~Month, y = ~Freq, color = ~Year, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        xaxis = list(title = 'Month'), 
        yaxis = list(title = 'Number of Events'),
        margin = list(l = 60, b = 60, r = 20, t = 40),
        showlegend = TRUE
        )
  })
  
  # 圖表 2
  output$stat2 <- renderPlotly({
    plot_ly(filtered_data() %>% mutate(Year = as.factor(Year)),
            x = ~ML, y = ~CWB.Depth, color = ~Year, mode = "markers",
            marker = list(opacity = 1)) %>%
      add_markers() %>%
      layout(
        xaxis = list(
          title = 'Magnitude (M<sub>L</sub>)',
          showticklabels = TRUE,
          range = c(input$ML[1], input$ML[2])
        ), 
        yaxis = list(
          title = 'Depth (km)',
          showticklabels = TRUE, 
          range = c(input$dep[2], input$dep[1])
        ),
        margin = list(l = 60, b = 60, r = 20, t = 40)
      )
  })
  
  
}

shinyApp(ui, server)