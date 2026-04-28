# ============================================================
# SISTEM REKOMENDASI LOKASI USAHA DI SULAWESI SELATAN
# Algoritma: K-Means Clustering + DBSCAN
# Data: Google Maps (Dummy)
# Framework: R Shiny + Leaflet
# Design: Professional Dark Minimal
# ============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(cluster)
library(dbscan)
library(DT)
library(plotly)
library(factoextra)
library(shinycssloaders)

# ============================================================
# DATA DUMMY
# ============================================================
set.seed(42)

generate_data <- function() {
  centers <- data.frame(
    kota = c("Makassar", "Makassar", "Makassar",
             "Gowa", "Maros", "Pangkep",
             "Bone", "Soppeng", "Wajo",
             "Bulukumba", "Sinjai", "Selayar",
             "Toraja", "Palopo", "Luwu"),
    lat_center = c(-5.1477, -5.1800, -5.1200,
                   -5.2900, -5.0050, -4.7700,
                   -4.5400, -4.3500, -3.9500,
                   -5.5400, -5.1100, -6.1300,
                   -3.0500, -3.0040, -3.5900),
    lng_center = c(119.4327, 119.4600, 119.4000,
                   119.5300, 119.5700, 119.6000,
                   120.3800, 119.8700, 120.0300,
                   120.1900, 120.2500, 120.4500,
                   119.8600, 120.1900, 120.4300)
  )
  n_per_center <- c(45, 40, 35, 25, 20, 15, 20, 15, 15, 20, 15, 10, 15, 20, 15)
  data_list <- list()
  kategori_list <- c("Kuliner", "Retail", "Jasa", "Fashion", "Elektronik", "Kesehatan", "Pendidikan", "Otomotif")
  for (i in 1:nrow(centers)) {
    n <- n_per_center[i]
    lat_spread <- ifelse(centers$kota[i] == "Makassar", 0.04, 0.06)
    lng_spread <- ifelse(centers$kota[i] == "Makassar", 0.04, 0.06)
    df <- data.frame(
      nama_usaha    = paste0("Usaha_", centers$kota[i], "_", 1:n),
      kota          = centers$kota[i],
      kategori      = sample(kategori_list, n, replace = TRUE,
                             prob = c(0.30, 0.20, 0.15, 0.10, 0.10, 0.07, 0.05, 0.03)),
      lat           = rnorm(n, centers$lat_center[i], lat_spread),
      lng           = rnorm(n, centers$lng_center[i], lng_spread),
      rating        = round(runif(n, 3.0, 5.0), 1),
      jumlah_review = sample(10:2000, n, replace = TRUE),
      harga_sewa    = sample(c("< 2 Juta", "2-5 Juta", "5-10 Juta", "> 10 Juta"), n, replace = TRUE,
                             prob = c(0.25, 0.35, 0.25, 0.15)),
      kepadatan     = round(runif(n, 10, 500), 0),
      skor_potensi  = round(runif(n, 40, 100), 1)
    )
    data_list[[i]] <- df
  }
  do.call(rbind, data_list)
}

data_usaha <- generate_data()

# ============================================================
# FUNGSI ANALISIS
# ============================================================
run_kmeans <- function(data, k = 4) {
  features <- data[, c("lat", "lng", "rating", "jumlah_review", "skor_potensi")]
  features_scaled <- scale(features)
  km <- kmeans(features_scaled, centers = k, nstart = 25, iter.max = 100)
  data$cluster_kmeans <- as.factor(km$cluster)
  list(data = data, model = km, scaled = features_scaled)
}

run_dbscan <- function(data, eps = 0.08, minPts = 5) {
  features <- data[, c("lat", "lng")]
  features_scaled <- scale(features)
  db <- dbscan::dbscan(features_scaled, eps = eps, minPts = minPts)
  data$cluster_dbscan <- as.factor(db$cluster)
  list(data = data, model = db)
}

cluster_colors <- c("#3B82F6", "#10B981", "#F59E0B", "#EF4444",
                    "#8B5CF6", "#06B6D4", "#EC4899", "#84CC16", "#F97316", "#6366F1")

# ============================================================
# CSS — Professional Dark Minimal
# ============================================================
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@300;400;500;600;700&family=DM+Mono:wght@400;500&display=swap');

* { box-sizing: border-box; }

body, .wrapper {
  font-family: 'DM Sans', sans-serif !important;
  background-color: #0C0E12 !important;
  color: #E2E8F0 !important;
}

/* ---- HEADER ---- */
.main-header .navbar {
  background-color: #0C0E12 !important;
  border-bottom: 1px solid #1E2530 !important;
  box-shadow: none !important;
  min-height: 56px !important;
}
.main-header .logo {
  background-color: #0C0E12 !important;
  border-bottom: 1px solid #1E2530 !important;
  border-right: 1px solid #1E2530 !important;
  height: 56px !important;
  line-height: 56px !important;
  font-size: 15px !important;
  font-weight: 600 !important;
  letter-spacing: -0.2px;
  color: #F8FAFC !important;
}
.main-header .navbar-nav > li > a,
.main-header .navbar .sidebar-toggle {
  color: #64748B !important;
  line-height: 56px !important;
  height: 56px !important;
}
.main-header .navbar .sidebar-toggle:hover {
  background: #1A1F2E !important;
  color: #94A3B8 !important;
}

/* ---- SIDEBAR ---- */
.main-sidebar {
  background-color: #0C0E12 !important;
  border-right: 1px solid #1E2530 !important;
  box-shadow: none !important;
  padding-top: 0 !important;
}
.sidebar { background-color: #0C0E12 !important; }

.sidebar-menu > li > a {
  color: #64748B !important;
  font-size: 13px !important;
  font-weight: 400 !important;
  padding: 10px 18px 10px 16px !important;
  border-left: 2px solid transparent !important;
  transition: all 0.15s ease !important;
  letter-spacing: 0.1px;
}
.sidebar-menu > li > a:hover {
  color: #CBD5E1 !important;
  background-color: #141820 !important;
  border-left-color: #334155 !important;
}
.sidebar-menu > li.active > a {
  color: #F8FAFC !important;
  background-color: #141820 !important;
  border-left-color: #3B82F6 !important;
  font-weight: 500 !important;
}
.sidebar-menu > li > a .fa {
  color: inherit !important;
  width: 18px !important;
  margin-right: 10px !important;
  font-size: 13px !important;
}

/* ---- CONTENT AREA ---- */
.content-wrapper {
  background-color: #0C0E12 !important;
  padding: 24px !important;
  min-height: calc(100vh - 56px) !important;
  margin-left: 230px !important;
}

/* ---- BOXES ---- */
.box {
  background: #111520 !important;
  border: 1px solid #1E2530 !important;
  border-radius: 8px !important;
  box-shadow: none !important;
  margin-bottom: 16px !important;
}
.box-header {
  padding: 16px 20px 12px !important;
  border-bottom: 1px solid #1E2530 !important;
  background: transparent !important;
}
.box-title {
  font-size: 13px !important;
  font-weight: 600 !important;
  color: #CBD5E1 !important;
  letter-spacing: 0.2px !important;
  text-transform: none !important;
}
.box-body {
  padding: 16px 20px !important;
}

/* ---- VALUE BOXES ---- */
.small-box {
  background: #111520 !important;
  border: 1px solid #1E2530 !important;
  border-radius: 8px !important;
  box-shadow: none !important;
  padding: 18px 20px !important;
}
.small-box h3 {
  font-size: 26px !important;
  font-weight: 700 !important;
  color: #F8FAFC !important;
  font-family: 'DM Mono', monospace !important;
  letter-spacing: -1px !important;
}
.small-box p {
  font-size: 12px !important;
  color: #64748B !important;
  font-weight: 500 !important;
  letter-spacing: 0.5px !important;
  text-transform: uppercase !important;
}
.small-box .icon { display: none !important; }
.small-box:hover { border-color: #2D3748 !important; }
.small-box.bg-blue, .small-box.bg-green,
.small-box.bg-yellow, .small-box.bg-red {
  background: #111520 !important;
}
.small-box-footer {
  background: transparent !important;
  color: #3B82F6 !important;
  font-size: 11px !important;
  padding: 4px 20px 12px !important;
  letter-spacing: 0.3px;
}

/* ---- SIDEBAR WIDTH ---- */
.main-sidebar, .left-side { width: 230px !important; }
.main-header .navbar { margin-left: 230px !important; }
.main-header .logo { width: 230px !important; }

/* ---- INPUTS ---- */
.form-control {
  background: #0C0E12 !important;
  border: 1px solid #1E2530 !important;
  border-radius: 5px !important;
  color: #CBD5E1 !important;
  font-size: 12px !important;
  height: 32px !important;
  font-family: 'DM Sans', sans-serif !important;
  transition: border-color 0.15s !important;
}
.form-control:focus {
  border-color: #3B82F6 !important;
  box-shadow: 0 0 0 2px rgba(59,130,246,0.12) !important;
  outline: none !important;
}
select.form-control { height: 32px !important; }

.selectize-control .selectize-input {
  background: #0C0E12 !important;
  border: 1px solid #1E2530 !important;
  border-radius: 5px !important;
  color: #CBD5E1 !important;
  font-size: 12px !important;
  min-height: 32px !important;
  box-shadow: none !important;
  font-family: 'DM Sans', sans-serif !important;
}
.selectize-control .selectize-input.focus {
  border-color: #3B82F6 !important;
  box-shadow: 0 0 0 2px rgba(59,130,246,0.12) !important;
}
.selectize-dropdown {
  background: #141820 !important;
  border: 1px solid #1E2530 !important;
  border-radius: 5px !important;
  font-size: 12px !important;
  color: #CBD5E1 !important;
}
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active {
  background: #1E2530 !important;
  color: #F8FAFC !important;
}

/* ---- SLIDER ---- */
.irs--shiny .irs-bar { background: #3B82F6 !important; border-top-color: #3B82F6 !important; border-bottom-color: #3B82F6 !important; }
.irs--shiny .irs-handle { border-color: #3B82F6 !important; background: #3B82F6 !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: #3B82F6 !important;
  font-size: 10px !important;
  font-family: 'DM Mono', monospace !important;
}
.irs--shiny .irs-line { background: #1E2530 !important; }
.irs--shiny .irs-min, .irs--shiny .irs-max { color: #475569 !important; font-size: 10px !important; }
.irs-grid-text { color: #475569 !important; font-size: 10px !important; }

/* ---- RADIO BUTTONS ---- */
.radio-inline { color: #94A3B8 !important; font-size: 12px !important; margin-right: 14px !important; }
.radio-inline input[type='radio'] { accent-color: #3B82F6; }

/* ---- LABELS ---- */
label, .control-label {
  font-size: 11px !important;
  color: #475569 !important;
  font-weight: 500 !important;
  letter-spacing: 0.4px !important;
  text-transform: uppercase !important;
  margin-bottom: 5px !important;
}

/* ---- ACTION BUTTON ---- */
#run_analysis {
  width: 100% !important;
  background: #3B82F6 !important;
  color: #fff !important;
  border: none !important;
  border-radius: 6px !important;
  font-size: 12px !important;
  font-weight: 600 !important;
  font-family: 'DM Sans', sans-serif !important;
  padding: 9px 14px !important;
  letter-spacing: 0.3px !important;
  transition: background 0.15s, transform 0.1s !important;
  cursor: pointer !important;
}
#run_analysis:hover { background: #2563EB !important; }
#run_analysis:active { transform: scale(0.98) !important; }

/* ---- DOWNLOAD BUTTON ---- */
#download_data {
  background: transparent !important;
  color: #10B981 !important;
  border: 1px solid #10B981 !important;
  border-radius: 5px !important;
  font-size: 11px !important;
  font-weight: 600 !important;
  font-family: 'DM Sans', sans-serif !important;
  padding: 5px 14px !important;
  letter-spacing: 0.3px !important;
  transition: all 0.15s !important;
}
#download_data:hover {
  background: #10B981 !important;
  color: #0C0E12 !important;
}

/* ---- DATA TABLE ---- */
.dataTables_wrapper {
  font-size: 12px !important;
  color: #94A3B8 !important;
  font-family: 'DM Sans', sans-serif !important;
}
table.dataTable thead th {
  background: #141820 !important;
  color: #475569 !important;
  border-bottom: 1px solid #1E2530 !important;
  font-weight: 600 !important;
  font-size: 11px !important;
  letter-spacing: 0.4px !important;
  text-transform: uppercase !important;
  padding: 10px 12px !important;
}
table.dataTable tbody td {
  background: #111520 !important;
  color: #94A3B8 !important;
  border-bottom: 1px solid #141820 !important;
  padding: 8px 12px !important;
}
table.dataTable tbody tr:hover td { background: #141820 !important; color: #CBD5E1 !important; }
.dataTables_filter input {
  background: #0C0E12 !important;
  border: 1px solid #1E2530 !important;
  color: #CBD5E1 !important;
  border-radius: 5px !important;
  font-size: 12px !important;
  padding: 4px 10px !important;
  height: 30px !important;
}
.dataTables_length select {
  background: #0C0E12 !important;
  border: 1px solid #1E2530 !important;
  color: #CBD5E1 !important;
  border-radius: 4px !important;
}
.dataTables_info, .dataTables_length, .dataTables_filter { color: #475569 !important; }
.paginate_button { color: #64748B !important; border-radius: 4px !important; }
.paginate_button.current { background: #1E2530 !important; color: #F8FAFC !important; border: 1px solid #334155 !important; }
.paginate_button:hover { background: #1E2530 !important; color: #CBD5E1 !important; border: 1px solid #1E2530 !important; }

/* ---- SIDEBAR DIVIDER ---- */
.sidebar-divider {
  border-color: #1E2530 !important;
  margin: 8px 0 !important;
}

/* ---- SECTION LABEL ---- */
.sidebar-section-label {
  color: #334155;
  font-size: 10px;
  font-weight: 700;
  letter-spacing: 1.2px;
  text-transform: uppercase;
  padding: 12px 18px 6px;
  display: block;
}

/* ---- SPINNER ---- */
.shiny-spinner-output-container .load-container { background: transparent !important; }
.sk-three-bounce .sk-child { background-color: #3B82F6 !important; }

/* ---- CLUSTER INTERPRETATION CARDS ---- */
.cluster-card {
  border-radius: 6px;
  padding: 12px 16px;
  margin-bottom: 8px;
  border-left: 3px solid;
}
.cluster-card-green  { background: rgba(16,185,129,0.06); border-left-color: #10B981; }
.cluster-card-yellow { background: rgba(245,158,11,0.06); border-left-color: #F59E0B; }
.cluster-card-orange { background: rgba(249,115,22,0.06); border-left-color: #F97316; }
.cluster-card-red    { background: rgba(239,68,68,0.06);  border-left-color: #EF4444; }
.cluster-card-title  { font-size: 13px; font-weight: 600; color: #CBD5E1; margin: 0 0 4px 0; }
.cluster-card-meta   { font-size: 11px; color: #64748B; margin: 0; font-family: 'DM Mono', monospace; }

/* ---- ABOUT PAGE ---- */
.about-section { padding: 4px 0 12px; }
.about-section h4 { font-size: 16px; font-weight: 700; color: #F8FAFC; margin-bottom: 4px; }
.about-section h5 { font-size: 12px; font-weight: 600; color: #64748B; letter-spacing: 0.6px; text-transform: uppercase; margin: 16px 0 6px; }
.about-section p, .about-section li { font-size: 13px; color: #94A3B8; line-height: 1.7; }
.about-section ul { padding-left: 16px; }
.about-section li { margin-bottom: 4px; }
.about-divider { border-color: #1E2530; margin: 14px 0; }
.tech-badge {
  display: inline-block;
  background: #141820;
  border: 1px solid #1E2530;
  border-radius: 4px;
  padding: 3px 10px;
  font-size: 11px;
  color: #64748B;
  margin: 3px 3px 3px 0;
  font-family: 'DM Mono', monospace;
}
.note-box {
  background: rgba(59,130,246,0.06);
  border: 1px solid rgba(59,130,246,0.2);
  border-radius: 6px;
  padding: 10px 14px;
  font-size: 12px;
  color: #64748B;
  margin-top: 14px;
}

/* ---- MAIN FOOTER ---- */
.main-footer {
  background: #0C0E12 !important;
  border-top: 1px solid #1E2530 !important;
  color: #334155 !important;
  font-size: 11px !important;
  padding: 10px 20px !important;
}

/* ---- SCROLLBAR ---- */
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: #0C0E12; }
::-webkit-scrollbar-thumb { background: #1E2530; border-radius: 3px; }
::-webkit-scrollbar-thumb:hover { background: #334155; }

/* ---- LEAFLET ---- */
.leaflet-container { border-radius: 6px; }
.leaflet-control-zoom a {
  background: #141820 !important;
  color: #94A3B8 !important;
  border-color: #1E2530 !important;
}

/* Remove AdminLTE blue borders */
.box.box-primary { border-top-color: #1E2530 !important; }
"

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span(
      tags$span(style = "color:#3B82F6; font-weight:700; font-size:15px; letter-spacing:-0.3px;", "Lokasi"),
      tags$span(style = "color:#F8FAFC; font-weight:300; font-size:15px; letter-spacing:-0.3px;", "Usaha"),
      tags$span(style = "color:#334155; font-weight:400; font-size:13px; margin-left:6px;", "/ Sulsel")
    ),
    titleWidth = 230
  ),
  
  dashboardSidebar(
    width = 230,
    
    tags$head(tags$style(HTML(custom_css))),
    
    sidebarMenu(
      id = "tabs",
      
      tags$span(class = "sidebar-section-label", "Navigation"),
      menuItem("Beranda",         tabName = "home",       icon = icon("chart-line")),
      menuItem("Peta Interaktif", tabName = "map",        icon = icon("map")),
      menuItem("Analisis K-Means",tabName = "kmeans",     icon = icon("circle-nodes")),
      menuItem("Analisis DBSCAN", tabName = "dbscan_tab", icon = icon("layer-group")),
      menuItem("Data & Tabel",    tabName = "data_tab",   icon = icon("table")),
      menuItem("Tentang",         tabName = "about",      icon = icon("info-circle")),
      
      tags$hr(class = "sidebar-divider"),
      tags$span(class = "sidebar-section-label", "Filter"),
      
      tags$div(style = "padding: 0 14px;",
               selectInput("filter_kota", "Kota / Kabupaten",
                           choices = c("Semua", sort(unique(data_usaha$kota))),
                           selected = "Semua"),
               
               selectInput("filter_kategori", "Kategori Usaha",
                           choices = c("Semua", sort(unique(data_usaha$kategori))),
                           selected = "Semua"),
               
               sliderInput("filter_rating", "Rating Minimum",
                           min = 1, max = 5, value = 3, step = 0.5)
      ),
      
      tags$hr(class = "sidebar-divider"),
      tags$span(class = "sidebar-section-label", "Parameter"),
      
      tags$div(style = "padding: 0 14px;",
               sliderInput("k_clusters", "Cluster K-Means",
                           min = 2, max = 8, value = 4, step = 1),
               
               sliderInput("eps_val", "Epsilon — DBSCAN",
                           min = 0.02, max = 0.30, value = 0.08, step = 0.01),
               
               sliderInput("minpts_val", "Min Points — DBSCAN",
                           min = 2, max = 15, value = 5, step = 1),
               
               tags$br(),
               actionButton("run_analysis", "Jalankan Analisis",
                            icon = icon("play"))
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ---- BERANDA ----
      tabItem(tabName = "home",
              
              fluidRow(
                valueBoxOutput("vbox_total",    width = 3),
                valueBoxOutput("vbox_kota",     width = 3),
                valueBoxOutput("vbox_kategori", width = 3),
                valueBoxOutput("vbox_rating",   width = 3)
              ),
              
              fluidRow(
                box(title = "Distribusi Kategori", width = 6, solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_kategori", height = "280px"), color = "#3B82F6", type = 4)
                ),
                box(title = "Distribusi Rating", width = 6, solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_rating", height = "280px"), color = "#3B82F6", type = 4)
                )
              ),
              
              fluidRow(
                box(title = "Sebaran per Kota", width = 8, solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_kota", height = "280px"), color = "#3B82F6", type = 4)
                ),
                box(title = "Ringkasan", width = 4, solidHeader = FALSE,
                    tableOutput("tbl_summary")
                )
              )
      ),
      
      # ---- PETA ----
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Peta Sebaran Lokasi Usaha — Sulawesi Selatan",
                  width = 12, solidHeader = FALSE,
                  tags$div(
                    style = "margin-bottom: 14px;",
                    radioButtons("map_mode", NULL,
                                 choices = c("Semua Titik" = "all",
                                             "Hasil K-Means" = "kmeans",
                                             "Hasil DBSCAN" = "dbscan"),
                                 selected = "all", inline = TRUE)
                  ),
                  withSpinner(leafletOutput("map_plot", height = "560px"), color = "#3B82F6", type = 4)
                )
              )
      ),
      
      # ---- K-MEANS ----
      tabItem(tabName = "kmeans",
              fluidRow(
                box(title = "Visualisasi Cluster — PCA", width = 7, solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_kmeans_pca", height = "400px"), color = "#3B82F6", type = 4)
                ),
                box(title = "Elbow Method", width = 5, solidHeader = FALSE,
                    withSpinner(plotOutput("plot_elbow", height = "400px"), color = "#3B82F6", type = 4)
                )
              ),
              fluidRow(
                box(title = "Profil Cluster", width = 12, solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_cluster_profile", height = "320px"), color = "#3B82F6", type = 4)
                )
              ),
              fluidRow(
                box(title = "Interpretasi & Rekomendasi", width = 12, solidHeader = FALSE,
                    uiOutput("kmeans_interpretation")
                )
              )
      ),
      
      # ---- DBSCAN ----
      tabItem(tabName = "dbscan_tab",
              fluidRow(
                box(title = "Visualisasi Cluster DBSCAN", width = 7, solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_dbscan", height = "400px"), color = "#3B82F6", type = 4)
                ),
                box(title = "k-NN Distance Plot", width = 5, solidHeader = FALSE,
                    withSpinner(plotOutput("plot_knn", height = "400px"), color = "#3B82F6", type = 4)
                )
              ),
              fluidRow(
                box(title = "Distribusi Cluster DBSCAN", width = 12, solidHeader = FALSE,
                    withSpinner(plotlyOutput("plot_dbscan_stats", height = "280px"), color = "#3B82F6", type = 4)
                )
              )
      ),
      
      # ---- DATA TABLE ----
      tabItem(tabName = "data_tab",
              fluidRow(
                box(title = "Tabel Data Lengkap", width = 12, solidHeader = FALSE,
                    tags$div(style = "margin-bottom: 14px;",
                             downloadButton("download_data", "Export CSV",
                                            icon = icon("download", class = "fa-sm"))
                    ),
                    withSpinner(DTOutput("data_table"), color = "#3B82F6", type = 4)
                )
              )
      ),
      
      # ---- TENTANG ----
      tabItem(tabName = "about",
              fluidRow(
                box(title = "Tentang Sistem", width = 8, solidHeader = FALSE,
                    tags$div(class = "about-section",
                             tags$h4("Sistem Rekomendasi Lokasi Usaha"),
                             tags$p("Dikembangkan sebagai bagian dari penelitian Program Studi Bisnis Digital, Universitas Negeri Makassar (UNM). Membantu pelaku usaha dan UMKM menentukan lokasi strategis di Sulawesi Selatan menggunakan pendekatan Machine Learning."),
                             tags$hr(class = "about-divider"),
                             tags$h5("Metode"),
                             tags$p(tags$b(style = "color:#CBD5E1;", "K-Means Clustering"), " — Mengelompokkan lokasi ke dalam K cluster berdasarkan kesamaan fitur: koordinat, rating, review, dan skor potensi."),
                             tags$p(tags$b(style = "color:#CBD5E1;", "DBSCAN"), " — Density-Based Spatial Clustering. Mendeteksi cluster berbasis kepadatan dan mengidentifikasi outlier sebagai noise."),
                             tags$hr(class = "about-divider"),
                             tags$h5("Fitur"),
                             tags$ul(
                               tags$li("Peta interaktif sebaran lokasi usaha"),
                               tags$li("Analisis K-Means dengan visualisasi PCA dan Elbow Method"),
                               tags$li("Analisis DBSCAN dengan k-NN Distance Plot"),
                               tags$li("Filter dinamis berdasarkan kota, kategori, dan rating"),
                               tags$li("Export data hasil analisis ke CSV")
                             ),
                             tags$div(class = "note-box",
                                      tags$b(style = "color:#3B82F6;", "Catatan: "),
                                      "Data yang digunakan adalah data dummy/simulasi. Pada penelitian sesungguhnya, data diambil langsung dari Google Maps API."
                             )
                    )
                ),
                box(title = "Stack Teknologi", width = 4, solidHeader = FALSE,
                    tags$div(class = "about-section",
                             tags$h5("Framework"),
                             tags$div(
                               tags$span(class = "tech-badge", "R 4.3+"),
                               tags$span(class = "tech-badge", "Shiny"),
                               tags$span(class = "tech-badge", "shinydashboard")
                             ),
                             tags$h5("Visualisasi"),
                             tags$div(
                               tags$span(class = "tech-badge", "leaflet"),
                               tags$span(class = "tech-badge", "plotly"),
                               tags$span(class = "tech-badge", "ggplot2")
                             ),
                             tags$h5("Machine Learning"),
                             tags$div(
                               tags$span(class = "tech-badge", "cluster"),
                               tags$span(class = "tech-badge", "dbscan"),
                               tags$span(class = "tech-badge", "factoextra")
                             ),
                             tags$hr(class = "about-divider"),
                             tags$h5("Variabel Input"),
                             tags$div(
                               tags$span(class = "tech-badge", "Latitude"),
                               tags$span(class = "tech-badge", "Longitude"),
                               tags$span(class = "tech-badge", "Rating"),
                               tags$span(class = "tech-badge", "Jumlah Review"),
                               tags$span(class = "tech-badge", "Skor Potensi"),
                               tags$span(class = "tech-badge", "Kepadatan")
                             )
                    )
                )
              )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # Reactive: filter data
  filtered_data <- reactive({
    d <- data_usaha
    if (input$filter_kota     != "Semua") d <- d[d$kota     == input$filter_kota, ]
    if (input$filter_kategori != "Semua") d <- d[d$kategori == input$filter_kategori, ]
    d <- d[d$rating >= input$filter_rating, ]
    d
  })
  
  # Reactive: run analysis
  analysis_result <- eventReactive(input$run_analysis, {
    d <- filtered_data()
    if (nrow(d) < input$k_clusters + 1) {
      showNotification("Data terlalu sedikit untuk jumlah cluster ini.", type = "error")
      return(NULL)
    }
    km_res <- run_kmeans(d, k = input$k_clusters)
    db_res <- run_dbscan(km_res$data, eps = input$eps_val, minPts = input$minpts_val)
    list(data = db_res$data, km_model = km_res$model, scaled = km_res$scaled)
  }, ignoreNULL = FALSE)
  
  result_data <- reactive({
    res <- analysis_result()
    if (is.null(res)) return(filtered_data())
    res$data
  })
  
  # Plotly layout base (dark theme)
  dark_layout <- list(
    plot_bgcolor  = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    font          = list(family = "DM Sans, sans-serif", color = "#64748B", size = 11),
    xaxis         = list(gridcolor = "#1E2530", zerolinecolor = "#1E2530", tickfont = list(color = "#475569")),
    yaxis         = list(gridcolor = "#1E2530", zerolinecolor = "#1E2530", tickfont = list(color = "#475569")),
    margin        = list(t = 10, b = 40, l = 50, r = 20),
    legend        = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#64748B", size = 11))
  )
  
  # ---- VALUE BOXES ----
  output$vbox_total <- renderValueBox({
    valueBox(
      tags$span(style = "font-family:'DM Mono',monospace; font-size:28px; font-weight:700; color:#F8FAFC;",
                nrow(filtered_data())),
      tags$span(style = "font-size:11px; color:#475569; letter-spacing:0.5px; text-transform:uppercase;",
                "Total Lokasi"),
      icon = NULL, color = "black"
    )
  })
  output$vbox_kota <- renderValueBox({
    valueBox(
      tags$span(style = "font-family:'DM Mono',monospace; font-size:28px; font-weight:700; color:#F8FAFC;",
                length(unique(filtered_data()$kota))),
      tags$span(style = "font-size:11px; color:#475569; letter-spacing:0.5px; text-transform:uppercase;",
                "Kota / Kabupaten"),
      icon = NULL, color = "black"
    )
  })
  output$vbox_kategori <- renderValueBox({
    valueBox(
      tags$span(style = "font-family:'DM Mono',monospace; font-size:28px; font-weight:700; color:#F8FAFC;",
                length(unique(filtered_data()$kategori))),
      tags$span(style = "font-size:11px; color:#475569; letter-spacing:0.5px; text-transform:uppercase;",
                "Kategori"),
      icon = NULL, color = "black"
    )
  })
  output$vbox_rating <- renderValueBox({
    valueBox(
      tags$span(style = "font-family:'DM Mono',monospace; font-size:28px; font-weight:700; color:#F8FAFC;",
                round(mean(filtered_data()$rating), 2)),
      tags$span(style = "font-size:11px; color:#475569; letter-spacing:0.5px; text-transform:uppercase;",
                "Rata-rata Rating"),
      icon = NULL, color = "black"
    )
  })
  
  # ---- HOME PLOTS ----
  output$plot_kategori <- renderPlotly({
    d <- filtered_data() %>% count(kategori) %>% arrange(n)
    p <- plot_ly(d, x = ~n, y = ~reorder(kategori, n), type = "bar",
                 orientation = "h",
                 marker = list(color = "#3B82F6", opacity = 0.8)) %>%
      layout(
        xaxis = modifyList(dark_layout$xaxis, list(title = "")),
        yaxis = modifyList(dark_layout$yaxis, list(title = "", tickfont = list(color = "#64748B", size = 11))),
        plot_bgcolor  = dark_layout$plot_bgcolor,
        paper_bgcolor = dark_layout$paper_bgcolor,
        font          = dark_layout$font,
        margin        = list(t = 10, b = 30, l = 80, r = 20)
      )
    p
  })
  
  output$plot_rating <- renderPlotly({
    d <- filtered_data()
    plot_ly(d, x = ~rating, type = "histogram", nbinsx = 15,
            marker = list(color = "#10B981", opacity = 0.75,
                          line = list(color = "#0C0E12", width = 1))) %>%
      layout(
        xaxis = modifyList(dark_layout$xaxis, list(title = "Rating")),
        yaxis = modifyList(dark_layout$yaxis, list(title = "Frekuensi")),
        plot_bgcolor  = dark_layout$plot_bgcolor,
        paper_bgcolor = dark_layout$paper_bgcolor,
        font          = dark_layout$font,
        margin        = dark_layout$margin
      )
  })
  
  output$plot_kota <- renderPlotly({
    d <- filtered_data() %>% count(kota) %>% arrange(n)
    plot_ly(d, x = ~n, y = ~reorder(kota, n), type = "bar",
            orientation = "h",
            marker = list(color = "#8B5CF6", opacity = 0.8)) %>%
      layout(
        xaxis = modifyList(dark_layout$xaxis, list(title = "Jumlah Usaha")),
        yaxis = modifyList(dark_layout$yaxis, list(title = "", tickfont = list(color = "#64748B", size = 11))),
        plot_bgcolor  = dark_layout$plot_bgcolor,
        paper_bgcolor = dark_layout$paper_bgcolor,
        font          = dark_layout$font,
        margin        = list(t = 10, b = 40, l = 80, r = 20)
      )
  })
  
  output$tbl_summary <- renderTable({
    d <- filtered_data()
    data.frame(
      Metrik = c("Total Data", "Kota Terbanyak", "Kategori Terbanyak",
                 "Rating Tertinggi", "Rating Terendah", "Rata-rata Review"),
      Nilai  = c(
        nrow(d),
        names(sort(table(d$kota),     decreasing = TRUE))[1],
        names(sort(table(d$kategori), decreasing = TRUE))[1],
        max(d$rating),
        min(d$rating),
        round(mean(d$jumlah_review))
      )
    )
  },
  striped = FALSE, bordered = FALSE, spacing = "s", width = "100%",
  align = "l",
  sanitize.text.function = identity)
  
  # ---- PETA ----
  output$map_plot <- renderLeaflet({
    d    <- result_data()
    mode <- input$map_mode
    
    pal_fn <- if (mode == "kmeans" && "cluster_kmeans" %in% names(d)) {
      colorFactor(cluster_colors, domain = d$cluster_kmeans)
    } else if (mode == "dbscan" && "cluster_dbscan" %in% names(d)) {
      colorFactor(c("#475569", cluster_colors), domain = d$cluster_dbscan)
    } else {
      colorFactor(cluster_colors, domain = d$kategori)
    }
    
    color_var <- if (mode == "kmeans" && "cluster_kmeans" %in% names(d)) {
      d$cluster_kmeans
    } else if (mode == "dbscan" && "cluster_dbscan" %in% names(d)) {
      d$cluster_dbscan
    } else {
      d$kategori
    }
    
    popup_text <- paste0(
      "<div style='font-family:DM Sans,sans-serif;padding:2px;'>",
      "<b style='font-size:13px;color:#1e293b;'>", d$nama_usaha, "</b><br>",
      "<span style='color:#64748b;font-size:11px;'>", d$kota, " · ", d$kategori, "</span><br><br>",
      "<span style='font-size:12px;'>⭐ ", d$rating, "</span>",
      "<span style='color:#94a3b8;font-size:11px;'> (", d$jumlah_review, " ulasan)</span><br>",
      "<span style='font-size:11px;color:#64748b;'>Sewa: ", d$harga_sewa,
      " · Potensi: ", d$skor_potensi, "</span>",
      "</div>"
    )
    
    leaflet(d) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 120.0, lat = -4.5, zoom = 7) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        color    = ~pal_fn(color_var),
        fillColor = ~pal_fn(color_var),
        fillOpacity = 0.85, opacity = 1,
        radius = 5, weight = 1,
        popup  = popup_text
      ) %>%
      addLegend("bottomright",
                pal    = pal_fn,
                values = color_var,
                title  = if (mode == "kmeans") "Cluster K-Means"
                else if (mode == "dbscan") "Cluster DBSCAN"
                else "Kategori",
                opacity = 0.9,
                labFormat = labelFormat())
  })
  
  # ---- K-MEANS PLOTS ----
  output$plot_kmeans_pca <- renderPlotly({
    res <- analysis_result()
    if (is.null(res)) return(NULL)
    d      <- res$data
    scaled <- res$scaled
    pca    <- prcomp(scaled)
    pca_df <- data.frame(
      PC1     = pca$x[,1],
      PC2     = pca$x[,2],
      Cluster = paste0("C", d$cluster_kmeans),
      Nama    = d$nama_usaha,
      Kota    = d$kota,
      Rating  = d$rating
    )
    plot_ly(pca_df, x = ~PC1, y = ~PC2, color = ~Cluster,
            colors = cluster_colors[1:input$k_clusters],
            type = "scatter", mode = "markers",
            marker = list(size = 7, opacity = 0.7),
            text  = ~paste0(Nama, "<br>", Kota, "<br>Rating: ", Rating),
            hoverinfo = "text") %>%
      layout(
        xaxis = modifyList(dark_layout$xaxis, list(title = "PC1")),
        yaxis = modifyList(dark_layout$yaxis, list(title = "PC2")),
        plot_bgcolor  = dark_layout$plot_bgcolor,
        paper_bgcolor = dark_layout$paper_bgcolor,
        font   = dark_layout$font,
        legend = dark_layout$legend,
        margin = dark_layout$margin
      )
  })
  
  output$plot_elbow <- renderPlot({
    d               <- filtered_data()
    features_scaled <- scale(d[, c("lat", "lng", "rating", "jumlah_review", "skor_potensi")])
    wss <- sapply(2:8, function(k) kmeans(features_scaled, centers = k, nstart = 10)$tot.withinss)
    df_elbow <- data.frame(K = 2:8, WSS = wss)
    
    ggplot(df_elbow, aes(x = K, y = WSS)) +
      geom_line(color = "#334155", linewidth = 0.8) +
      geom_point(color = "#3B82F6", size = 4, fill = "#3B82F6",
                 shape = 21, stroke = 0) +
      geom_vline(xintercept = input$k_clusters,
                 linetype = "dashed", color = "#EF4444", linewidth = 0.6, alpha = 0.7) +
      annotate("text", x = input$k_clusters + 0.2, y = max(wss) * 0.97,
               label = paste0("K = ", input$k_clusters),
               color = "#EF4444", size = 3.2, hjust = 0,
               family = "sans") +
      scale_x_continuous(breaks = 2:8) +
      labs(x = "K", y = "Within-Cluster SS", title = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background    = element_rect(fill = "#111520", color = NA),
        panel.background   = element_rect(fill = "#111520", color = NA),
        panel.grid.major   = element_line(color = "#1E2530", linewidth = 0.4),
        panel.grid.minor   = element_blank(),
        axis.text          = element_text(color = "#475569", size = 10),
        axis.title         = element_text(color = "#475569", size = 10),
        plot.margin        = margin(16, 16, 16, 16)
      )
  })
  
  output$plot_cluster_profile <- renderPlotly({
    res <- analysis_result()
    if (is.null(res)) return(NULL)
    d <- res$data
    profile <- d %>%
      group_by(cluster_kmeans) %>%
      summarise(
        Avg_Rating  = round(mean(rating), 2),
        Avg_Review  = round(mean(jumlah_review), 0),
        Avg_Potensi = round(mean(skor_potensi), 1),
        Jumlah      = n(), .groups = "drop"
      ) %>%
      mutate(Cluster = paste0("C", cluster_kmeans))
    
    plot_ly(profile, x = ~Cluster, y = ~Avg_Rating, type = "bar",
            name = "Avg Rating", marker = list(color = "#3B82F6", opacity = 0.85)) %>%
      add_trace(y = ~Avg_Potensi / 20, name = "Potensi ÷20",
                marker = list(color = "#10B981", opacity = 0.85)) %>%
      add_trace(y = ~Jumlah / 10, name = "Jumlah ÷10",
                marker = list(color = "#F59E0B", opacity = 0.85)) %>%
      layout(
        barmode = "group",
        xaxis = modifyList(dark_layout$xaxis, list(title = "")),
        yaxis = modifyList(dark_layout$yaxis, list(title = "Nilai")),
        plot_bgcolor  = dark_layout$plot_bgcolor,
        paper_bgcolor = dark_layout$paper_bgcolor,
        font   = dark_layout$font,
        legend = dark_layout$legend,
        margin = dark_layout$margin
      )
  })
  
  output$kmeans_interpretation <- renderUI({
    res <- analysis_result()
    if (is.null(res)) {
      return(tags$p(style = "color:#475569; font-size:13px;", "Jalankan analisis terlebih dahulu."))
    }
    d <- res$data
    profile <- d %>%
      group_by(cluster_kmeans) %>%
      summarise(
        rating_avg  = round(mean(rating), 2),
        review_avg  = round(mean(jumlah_review), 0),
        potensi_avg = round(mean(skor_potensi), 1),
        jumlah      = n(), .groups = "drop"
      ) %>%
      arrange(desc(potensi_avg))
    
    make_card <- function(row) {
      if (row$potensi_avg >= 75 && row$rating_avg >= 4.2) {
        cls <- "cluster-card-green";  label <- "Zona Sangat Strategis"
      } else if (row$potensi_avg >= 60) {
        cls <- "cluster-card-yellow"; label <- "Zona Strategis"
      } else if (row$potensi_avg >= 45) {
        cls <- "cluster-card-orange"; label <- "Zona Potensi Sedang"
      } else {
        cls <- "cluster-card-red";    label <- "Zona Kurang Strategis"
      }
      tags$div(
        class = paste("cluster-card", cls),
        tags$p(class = "cluster-card-title",
               paste0("Cluster ", row$cluster_kmeans, "  ·  ", label)),
        tags$p(class = "cluster-card-meta",
               paste0(row$jumlah, " lokasi  ·  Rating ", row$rating_avg,
                      "  ·  ", row$review_avg, " review  ·  Potensi ", row$potensi_avg))
      )
    }
    
    do.call(tagList, lapply(1:nrow(profile), function(i) make_card(profile[i, ])))
  })
  
  # ---- DBSCAN PLOTS ----
  output$plot_dbscan <- renderPlotly({
    res <- analysis_result()
    if (is.null(res)) return(NULL)
    d <- res$data
    d$cluster_label <- ifelse(d$cluster_dbscan == "0", "Noise",
                              paste0("Cluster ", d$cluster_dbscan))
    
    plot_ly(d, x = ~lng, y = ~lat, color = ~cluster_label,
            colors = c("#475569", cluster_colors),
            type = "scatter", mode = "markers",
            marker = list(size = 6, opacity = 0.7),
            text  = ~paste0(nama_usaha, "<br>", kota, "<br>Rating: ", rating),
            hoverinfo = "text") %>%
      layout(
        xaxis = modifyList(dark_layout$xaxis, list(title = "Longitude")),
        yaxis = modifyList(dark_layout$yaxis, list(title = "Latitude")),
        plot_bgcolor  = dark_layout$plot_bgcolor,
        paper_bgcolor = dark_layout$paper_bgcolor,
        font   = dark_layout$font,
        legend = dark_layout$legend,
        margin = dark_layout$margin
      )
  })
  
  output$plot_knn <- renderPlot({
    d             <- filtered_data()
    coords_scaled <- scale(d[, c("lat", "lng")])
    knn_dist      <- sort(kNNdist(coords_scaled, k = input$minpts_val))
    df_knn        <- data.frame(Index = seq_along(knn_dist), Dist = knn_dist)
    
    ggplot(df_knn, aes(x = Index, y = Dist)) +
      geom_line(color = "#8B5CF6", linewidth = 0.8, alpha = 0.9) +
      geom_hline(yintercept = input$eps_val,
                 linetype = "dashed", color = "#EF4444", linewidth = 0.6, alpha = 0.7) +
      annotate("text",
               x     = nrow(df_knn) * 0.05,
               y     = input$eps_val + diff(range(knn_dist)) * 0.04,
               label = paste0("ε = ", input$eps_val),
               color = "#EF4444", size = 3.2, hjust = 0, family = "sans") +
      labs(x = "Index", y = paste0(input$minpts_val, "-NN Distance"), title = NULL) +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#111520", color = NA),
        panel.background = element_rect(fill = "#111520", color = NA),
        panel.grid.major = element_line(color = "#1E2530", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(color = "#475569", size = 10),
        axis.title       = element_text(color = "#475569", size = 10),
        plot.margin      = margin(16, 16, 16, 16)
      )
  })
  
  output$plot_dbscan_stats <- renderPlotly({
    res <- analysis_result()
    if (is.null(res)) return(NULL)
    d <- res$data
    stats <- d %>%
      group_by(cluster_dbscan) %>%
      summarise(Jumlah = n(), Avg_Rating = round(mean(rating), 2), .groups = "drop") %>%
      mutate(
        Label    = ifelse(cluster_dbscan == "0", "Noise", paste0("C", cluster_dbscan)),
        is_noise = cluster_dbscan == "0"
      )
    
    plot_ly(stats, x = ~Label, y = ~Jumlah, type = "bar",
            marker = list(
              color   = ifelse(stats$is_noise, "#334155", "#3B82F6"),
              opacity = 0.85,
              line    = list(color = "#0C0E12", width = 1)
            ),
            text = ~Jumlah, textposition = "outside",
            textfont = list(color = "#64748B", size = 11)) %>%
      layout(
        xaxis = modifyList(dark_layout$xaxis, list(title = "")),
        yaxis = modifyList(dark_layout$yaxis, list(title = "Jumlah Lokasi")),
        plot_bgcolor  = dark_layout$plot_bgcolor,
        paper_bgcolor = dark_layout$paper_bgcolor,
        font          = dark_layout$font,
        margin        = dark_layout$margin
      )
  })
  
  # ---- DATA TABLE ----
  output$data_table <- renderDT({
    d    <- result_data()
    cols <- c("nama_usaha", "kota", "kategori", "lat", "lng",
              "rating", "jumlah_review", "harga_sewa", "skor_potensi")
    if ("cluster_kmeans" %in% names(d)) cols <- c(cols, "cluster_kmeans")
    if ("cluster_dbscan" %in% names(d)) cols <- c(cols, "cluster_dbscan")
    
    col_names <- c("Nama Usaha", "Kota", "Kategori", "Lat", "Lng",
                   "Rating", "Review", "Sewa", "Potensi")
    if ("cluster_kmeans" %in% names(d)) col_names <- c(col_names, "K-Means")
    if ("cluster_dbscan" %in% names(d)) col_names <- c(col_names, "DBSCAN")
    
    datatable(
      d[, cols],
      colnames = col_names,
      rownames = FALSE,
      options  = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "lfrtip",
        language   = list(search = "Cari:", lengthMenu = "Tampilkan _MENU_ baris"),
        initComplete = JS("
          function(settings, json) {
            $(this.api().table().header()).css({
              'background-color': '#141820',
              'color': '#475569'
            });
          }
        ")
      ),
      class = "display compact"
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() paste0("lokasi_usaha_sulsel_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(result_data(), file, row.names = FALSE)
  )
}

# ============================================================
shinyApp(ui = ui, server = server)