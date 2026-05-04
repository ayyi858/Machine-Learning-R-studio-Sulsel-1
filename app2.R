# ============================================================
# LOKASI USAHA MAKASSAR — ULTRA PREMIUM EDITION
# Fixed & Enhanced | Data: 1.590 Lokasi | Mamajang & Mariso
# ============================================================

library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(cluster)
library(dbscan)
library(DT)
library(plotly)
library(factoextra)
library(shinycssloaders)
library(shinyWidgets)
library(scales)
library(tidyr)

# ============================================================
# LOAD & PREP DATA
# ============================================================
data_usaha <- read.csv("data_usaha_makassar_clean.csv", stringsAsFactors = FALSE)
data_usaha$latitude      <- as.numeric(data_usaha$latitude)
data_usaha$longitude     <- as.numeric(data_usaha$longitude)
data_usaha$rating        <- as.numeric(data_usaha$rating)
data_usaha$jumlah_ulasan <- as.numeric(data_usaha$jumlah_ulasan)
data_usaha$skor_potensi  <- as.numeric(data_usaha$skor_potensi)
data_usaha$foto_url      <- ifelse(is.na(data_usaha$foto_url) | data_usaha$foto_url == "", NA, data_usaha$foto_url)
data_usaha$telepon       <- ifelse(is.na(data_usaha$telepon)  | data_usaha$telepon  == "", "-", data_usaha$telepon)

# Filter valid coords only for spatial ops (keep all for non-spatial)
data_usaha_spatial <- data_usaha %>%
  filter(!is.na(latitude), !is.na(longitude),
         latitude  >= -5.25, latitude  <= -5.05,
         longitude >= 119.35, longitude <= 119.55)

# Pre-compute unique values
KECS   <- sort(unique(data_usaha$kecamatan))
GRUPS  <- sort(unique(data_usaha$kategori_grup))
LAYAKS <- sort(unique(data_usaha$label_kelayakan))

# ============================================================
# CLUSTERING FUNCTIONS
# ============================================================
run_kmeans <- function(data, k = 4) {
  d_sp <- data %>% filter(!is.na(latitude), !is.na(longitude))
  if (nrow(d_sp) < k + 1) return(list(data = data, model = NULL, scaled = NULL))
  f  <- scale(d_sp[, c("latitude", "longitude", "rating", "jumlah_ulasan", "skor_potensi")])
  m  <- kmeans(f, centers = k, nstart = 25, iter.max = 100)
  d_sp$cluster_kmeans <- as.factor(m$cluster)
  # Merge back — non-spatial rows get NA cluster
  data <- data %>% left_join(d_sp[, c("nama", "alamat", "cluster_kmeans")],
                             by = c("nama", "alamat"))
  list(data = data, model = m, scaled = f, spatial_data = d_sp)
}

run_dbscan <- function(data, eps = 0.08, minPts = 5) {
  d_sp <- data %>% filter(!is.na(latitude), !is.na(longitude))
  f    <- scale(d_sp[, c("latitude", "longitude")])
  m    <- dbscan::dbscan(f, eps = eps, minPts = minPts)
  d_sp$cluster_dbscan <- as.factor(m$cluster)
  data <- data %>% left_join(d_sp[, c("nama", "alamat", "cluster_dbscan")],
                             by = c("nama", "alamat"))
  list(data = data, model = m, spatial_data = d_sp)
}

# ============================================================
# PALETTE & LAYOUT HELPERS
# ============================================================
PAL <- c("#3B82F6","#10B981","#F59E0B","#EF4444","#8B5CF6",
         "#06B6D4","#EC4899","#84CC16","#F97316","#6366F1",
         "#14B8A6","#F43F5E","#A78BFA","#34D399","#FCD34D")

mk_layout <- function(...) {
  base <- list(
    plot_bgcolor  = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)",
    font          = list(family = "Inter,sans-serif", color = "#64748B", size = 11),
    xaxis         = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", tickfont = list(color = "#475569")),
    yaxis         = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", tickfont = list(color = "#475569")),
    margin        = list(t = 10, b = 40, l = 50, r = 20),
    legend        = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#64748B", size = 11))
  )
  modifyList(base, list(...))
}

# ============================================================
# CSS
# ============================================================
CSS <- '
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500;600&display=swap");
*{box-sizing:border-box;margin:0;padding:0}
:root{
  --bg0:#060810;--bg1:#0A0D18;--bg2:#0D1120;--bg3:#111827;
  --border:#161D2E;--border2:#1E293B;
  --text1:#F1F5F9;--text2:#94A3B8;--text3:#475569;--text4:#2D3F55;
  --blue:#3B82F6;--green:#10B981;--yellow:#F59E0B;--red:#EF4444;
  --purple:#8B5CF6;--cyan:#06B6D4;--pink:#EC4899;
}
body,.wrapper{font-family:"Inter",sans-serif!important;background:var(--bg0)!important;color:var(--text1)!important}
body::before{content:"";position:fixed;top:0;left:0;width:100%;height:100%;
  background:radial-gradient(ellipse at 20% 50%,rgba(59,130,246,.03) 0%,transparent 50%),
             radial-gradient(ellipse at 80% 20%,rgba(139,92,246,.03) 0%,transparent 50%),
             radial-gradient(ellipse at 50% 80%,rgba(16,185,129,.02) 0%,transparent 50%);
  pointer-events:none;z-index:0}
.main-header .navbar{background:rgba(6,8,16,.95)!important;border-bottom:1px solid var(--border)!important;
  box-shadow:0 1px 0 rgba(255,255,255,.03)!important;min-height:58px!important;backdrop-filter:blur(20px)!important}
.main-header .logo{background:rgba(6,8,16,.95)!important;border-bottom:1px solid var(--border)!important;
  border-right:1px solid var(--border)!important;height:58px!important;line-height:58px!important;
  font-size:14px!important;font-weight:800!important;color:var(--text1)!important;
  letter-spacing:-.5px!important;backdrop-filter:blur(20px)!important}
.main-header .navbar-nav>li>a,.main-header .navbar .sidebar-toggle{
  color:var(--text3)!important;line-height:58px!important;height:58px!important;transition:color .2s!important}
.main-header .navbar .sidebar-toggle:hover{background:var(--bg2)!important;color:var(--text2)!important}
.main-sidebar,.left-side{width:248px!important;background:rgba(6,8,16,.98)!important;
  border-right:1px solid var(--border)!important;backdrop-filter:blur(20px)!important}
.sidebar{background:transparent!important}
.main-header .navbar{margin-left:248px!important}
.main-header .logo{width:248px!important}
.sidebar-menu>li>a{color:var(--text3)!important;font-size:12px!important;font-weight:500!important;
  padding:9px 16px!important;border-left:2px solid transparent!important;transition:all .15s ease!important;
  display:flex!important;align-items:center!important;gap:10px!important}
.sidebar-menu>li>a .fa{width:15px!important;font-size:12px!important;margin:0!important}
.sidebar-menu>li>a:hover{color:var(--text2)!important;background:rgba(255,255,255,.03)!important;border-left-color:var(--border2)!important}
.sidebar-menu>li.active>a{color:#fff!important;background:rgba(59,130,246,.08)!important;
  border-left-color:var(--blue)!important;font-weight:600!important}
.sidebar-section-label{color:var(--text4);font-size:9px;font-weight:700;letter-spacing:2px;
  text-transform:uppercase;padding:14px 16px 4px;display:block}
.sidebar-divider{border-color:var(--border)!important;margin:5px 0!important}
.content-wrapper{background:var(--bg0)!important;padding:18px 20px!important;
  margin-left:248px!important;min-height:calc(100vh - 58px)!important;position:relative;z-index:1}
.box{background:var(--bg1)!important;border:1px solid var(--border)!important;
  border-radius:12px!important;box-shadow:0 4px 24px rgba(0,0,0,.15)!important;
  margin-bottom:14px!important;overflow:hidden!important;transition:border-color .2s!important}
.box:hover{border-color:var(--border2)!important}
.box-header{padding:14px 16px 10px!important;border-bottom:1px solid var(--border)!important;
  background:rgba(255,255,255,.01)!important}
.box-title{font-size:12px!important;font-weight:600!important;color:var(--text2)!important;letter-spacing:.2px!important}
.box-body{padding:14px 16px!important}
.small-box{background:var(--bg1)!important;border:1px solid var(--border)!important;
  border-radius:12px!important;box-shadow:none!important;padding:16px 16px 10px!important;
  transition:all .2s!important;overflow:hidden!important;position:relative!important}
.small-box::after{content:"";position:absolute;top:0;left:0;right:0;height:2px;
  background:linear-gradient(90deg,var(--blue),var(--purple));opacity:0;transition:opacity .3s}
.small-box:hover{border-color:var(--border2)!important;transform:translateY(-2px)!important;
  box-shadow:0 8px 32px rgba(0,0,0,.2)!important}
.small-box:hover::after{opacity:1}
.small-box h3{font-size:28px!important;font-weight:700!important;color:var(--text1)!important;
  font-family:"JetBrains Mono",monospace!important;letter-spacing:-1.5px!important;line-height:1!important;margin-bottom:6px!important}
.small-box p{font-size:10px!important;color:var(--text4)!important;font-weight:600!important;
  text-transform:uppercase!important;letter-spacing:.8px!important}
.small-box .icon{display:none!important}
.small-box.bg-aqua,.small-box.bg-blue,.small-box.bg-green,.small-box.bg-yellow,
.small-box.bg-red,.small-box.bg-purple,.small-box.bg-black{background:var(--bg1)!important}
.small-box-footer{background:transparent!important;padding:4px 16px 10px!important;font-size:9.5px!important;color:var(--text4)!important}
.form-control{background:rgba(0,0,0,.3)!important;border:1px solid var(--border)!important;
  border-radius:7px!important;color:var(--text2)!important;font-size:11.5px!important;
  height:32px!important;font-family:"Inter",sans-serif!important;transition:all .15s!important}
.form-control:focus{border-color:var(--blue)!important;box-shadow:0 0 0 3px rgba(59,130,246,.1)!important;outline:none!important}
.selectize-control .selectize-input{background:rgba(0,0,0,.3)!important;border:1px solid var(--border)!important;
  border-radius:7px!important;color:var(--text2)!important;font-size:11.5px!important;
  min-height:32px!important;box-shadow:none!important;padding:5px 10px!important}
.selectize-control .selectize-input.focus{border-color:var(--blue)!important;box-shadow:0 0 0 3px rgba(59,130,246,.1)!important}
.selectize-dropdown{background:var(--bg2)!important;border:1px solid var(--border2)!important;
  border-radius:8px!important;font-size:11.5px!important;color:var(--text2)!important;
  box-shadow:0 16px 48px rgba(0,0,0,.5)!important}
.selectize-dropdown .option:hover,.selectize-dropdown .option.active{background:var(--bg3)!important;color:var(--text1)!important}
.irs--shiny .irs-bar{background:var(--blue)!important;border-top-color:var(--blue)!important;border-bottom-color:var(--blue)!important}
.irs--shiny .irs-handle{border-color:var(--blue)!important;background:var(--blue)!important;
  box-shadow:0 0 0 4px rgba(59,130,246,.15)!important}
.irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single{background:var(--blue)!important;
  font-size:9.5px!important;font-family:"JetBrains Mono",monospace!important;border-radius:4px!important;padding:2px 6px!important}
.irs--shiny .irs-line{background:var(--border2)!important;border-radius:2px!important}
.irs--shiny .irs-min,.irs--shiny .irs-max{color:var(--text4)!important;font-size:9px!important}
label,.control-label{font-size:10px!important;color:var(--text4)!important;font-weight:600!important;
  letter-spacing:.6px!important;text-transform:uppercase!important;margin-bottom:5px!important}
.btn-mega{width:100%!important;background:linear-gradient(135deg,#1D4ED8,#3B82F6,#6366F1)!important;
  background-size:200% 200%!important;animation:grad 3s ease infinite!important;
  color:#fff!important;border:none!important;border-radius:8px!important;
  font-size:12px!important;font-weight:700!important;font-family:"Inter",sans-serif!important;
  padding:11px!important;letter-spacing:.4px!important;cursor:pointer!important;
  transition:all .2s!important;box-shadow:0 4px 20px rgba(59,130,246,.35)!important}
.btn-mega:hover{transform:translateY(-2px)!important;box-shadow:0 8px 32px rgba(59,130,246,.5)!important}
.btn-mega:active{transform:scale(.97)!important}
@keyframes grad{0%{background-position:0% 50%}50%{background-position:100% 50%}100%{background-position:0% 50%}}
.btn-dl-green{background:transparent!important;color:var(--green)!important;border:1px solid var(--green)!important;
  border-radius:6px!important;font-size:11px!important;font-weight:600!important;padding:5px 12px!important;transition:all .15s!important}
.btn-dl-green:hover{background:var(--green)!important;color:var(--bg0)!important}
.btn-dl-blue{background:transparent!important;color:var(--blue)!important;border:1px solid var(--blue)!important;
  border-radius:6px!important;font-size:11px!important;font-weight:600!important;padding:5px 12px!important;transition:all .15s!important}
.btn-dl-blue:hover{background:var(--blue)!important;color:#fff!important}
.dataTables_wrapper{font-size:11.5px!important;color:var(--text3)!important;font-family:"Inter",sans-serif!important}
table.dataTable thead th{background:rgba(0,0,0,.3)!important;color:var(--text4)!important;
  border-bottom:1px solid var(--border)!important;font-size:10px!important;font-weight:700!important;
  letter-spacing:.6px!important;text-transform:uppercase!important;padding:10px 12px!important}
table.dataTable tbody td{background:var(--bg1)!important;color:var(--text2)!important;
  border-bottom:1px solid var(--border)!important;padding:8px 12px!important;transition:background .1s!important}
table.dataTable tbody tr:hover td{background:var(--bg2)!important;color:var(--text1)!important}
.dataTables_filter input,.dataTables_length select{background:rgba(0,0,0,.3)!important;
  border:1px solid var(--border)!important;color:var(--text2)!important;border-radius:6px!important;font-size:11px!important}
.dataTables_info,.dataTables_length,.dataTables_filter{color:var(--text4)!important;font-size:10.5px!important}
.paginate_button{color:var(--text3)!important;border-radius:5px!important;font-size:11px!important}
.paginate_button.current,.paginate_button.current:hover{background:var(--bg2)!important;
  color:var(--text1)!important;border:1px solid var(--border2)!important}
.paginate_button:hover{background:var(--bg2)!important;color:var(--text2)!important;border:1px solid var(--border)!important}
.main-footer{background:rgba(6,8,16,.98)!important;border-top:1px solid var(--border)!important;color:var(--text4)!important;font-size:10.5px!important}
::-webkit-scrollbar{width:4px;height:4px}
::-webkit-scrollbar-track{background:var(--bg0)}
::-webkit-scrollbar-thumb{background:var(--border2);border-radius:2px}
::-webkit-scrollbar-thumb:hover{background:#2D3F55}
.leaflet-container{border-radius:10px}
.leaflet-control-zoom a{background:var(--bg1)!important;color:var(--text3)!important;
  border-color:var(--border)!important;transition:all .15s!important}
.leaflet-control-zoom a:hover{background:var(--bg2)!important;color:var(--text2)!important}
.leaflet-popup-content-wrapper{background:var(--bg2)!important;border:1px solid var(--border2)!important;
  border-radius:10px!important;box-shadow:0 16px 48px rgba(0,0,0,.5)!important;color:var(--text2)!important}
.leaflet-popup-tip{background:var(--bg2)!important}
.kpi-card{background:var(--bg1);border:1px solid var(--border);border-radius:10px;
  padding:14px 16px;margin-bottom:10px;transition:all .2s;position:relative;overflow:hidden}
.kpi-card::before{content:"";position:absolute;top:0;left:0;width:3px;height:100%;border-radius:0 2px 2px 0}
.kpi-card.blue::before{background:var(--blue)}
.kpi-card.green::before{background:var(--green)}
.kpi-card.yellow::before{background:var(--yellow)}
.kpi-card.purple::before{background:var(--purple)}
.kpi-card.red::before{background:var(--red)}
.kpi-card:hover{border-color:var(--border2);transform:translateX(2px)}
.kpi-label{font-size:9.5px;font-weight:700;color:var(--text4);letter-spacing:1px;text-transform:uppercase;margin-bottom:6px}
.kpi-val{font-size:24px;font-weight:700;color:var(--text1);font-family:"JetBrains Mono",monospace;letter-spacing:-1px;line-height:1.1}
.kpi-sub{font-size:10.5px;color:var(--text3);margin-top:4px;line-height:1.4}
.cl-card{border-radius:10px;padding:14px 16px;margin-bottom:8px;border-left:3px solid;transition:all .2s;cursor:default;position:relative;overflow:hidden}
.cl-card:hover{transform:translateX(4px)}
.cl-green{background:rgba(16,185,129,.04);border-left-color:var(--green)}
.cl-blue{background:rgba(59,130,246,.04);border-left-color:var(--blue)}
.cl-yellow{background:rgba(245,158,11,.04);border-left-color:var(--yellow)}
.cl-orange{background:rgba(249,115,22,.04);border-left-color:#F97316}
.cl-red{background:rgba(239,68,68,.04);border-left-color:var(--red)}
.cl-title{font-size:13px;font-weight:700;color:var(--text1);margin-bottom:6px;display:flex;align-items:center;gap:8px}
.cl-meta{font-size:11px;color:var(--text3);font-family:"JetBrains Mono",monospace;line-height:1.7}
.cl-badge{display:inline-flex;align-items:center;padding:2px 9px;border-radius:20px;font-size:9.5px;font-weight:700;letter-spacing:.4px}
.cl-rec{font-size:11.5px;color:var(--text2);margin-top:6px;line-height:1.5;padding-top:6px;border-top:1px solid var(--border)}
.nav-tabs{border-bottom:1px solid var(--border)!important}
.nav-tabs>li>a{color:var(--text3)!important;font-size:11.5px!important;border:none!important;
  border-bottom:2px solid transparent!important;padding:8px 14px!important;font-weight:500!important;transition:all .15s!important}
.nav-tabs>li>a:hover{color:var(--text2)!important;background:transparent!important;border-bottom-color:var(--border2)!important}
.nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{
  color:var(--text1)!important;background:transparent!important;
  border:none!important;border-bottom:2px solid var(--blue)!important;font-weight:600!important}
.tab-content{padding-top:14px!important}
.tech-badge{display:inline-block;background:var(--bg2);border:1px solid var(--border);
  border-radius:5px;padding:3px 10px;font-size:10.5px;color:var(--text3);margin:3px;
  font-family:"JetBrains Mono",monospace;transition:all .15s}
.tech-badge:hover{border-color:var(--border2);color:var(--text2)}
.insight-box{background:var(--bg2);border:1px solid var(--border);border-radius:8px;
  padding:12px 14px;margin-bottom:8px;transition:border-color .2s}
.insight-box:hover{border-color:var(--border2)}
.insight-title{font-size:9.5px;font-weight:700;color:var(--blue);letter-spacing:.8px;text-transform:uppercase;margin-bottom:5px}
.insight-text{font-size:12px;color:var(--text2);line-height:1.6}
@keyframes pulse{0%,100%{opacity:1}50%{opacity:.4}}
.pulse-dot{width:7px;height:7px;background:var(--green);border-radius:50%;display:inline-block;
  margin-right:6px;animation:pulse 2s ease-in-out infinite}
.radio-inline{color:var(--text2)!important;font-size:11px!important;margin-right:12px!important}
.radio-inline input[type=radio]{accent-color:var(--blue)}
.about-section h4{font-size:20px;font-weight:800;color:var(--text1);margin-bottom:6px;letter-spacing:-.5px}
.about-section h5{font-size:10px;font-weight:700;color:var(--text4);letter-spacing:1.2px;text-transform:uppercase;margin:18px 0 8px}
.about-section p,.about-section li{font-size:13px;color:var(--text3);line-height:1.8}
.about-divider{border-color:var(--border);margin:16px 0}
.note-box{background:rgba(59,130,246,.04);border:1px solid rgba(59,130,246,.15);border-radius:8px;padding:12px 16px;font-size:12px;color:var(--text3);margin-top:12px}
.sk-three-bounce .sk-child{background-color:var(--blue)!important}
.ticker{font-family:"JetBrains Mono",monospace;font-size:11px;color:var(--green);
  background:rgba(16,185,129,.08);border:1px solid rgba(16,185,129,.15);
  border-radius:5px;padding:2px 8px;display:inline-block;margin-top:4px}
.bootstrap-select .btn{background:rgba(0,0,0,.3)!important;border:1px solid var(--border)!important;
  color:var(--text2)!important;font-size:11.5px!important;border-radius:7px!important;height:32px!important}
.bootstrap-select .dropdown-menu{background:var(--bg2)!important;border:1px solid var(--border2)!important;
  border-radius:8px!important;box-shadow:0 16px 48px rgba(0,0,0,.5)!important}
.bootstrap-select .dropdown-item{color:var(--text2)!important;font-size:11.5px!important}
.bootstrap-select .dropdown-item:hover{background:var(--bg3)!important;color:var(--text1)!important}
'

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$div(
      style = "display:flex;align-items:center;gap:10px;",
      tags$div(
        style = "width:26px;height:26px;background:linear-gradient(135deg,#1D4ED8,#6366F1);
          border-radius:7px;display:flex;align-items:center;justify-content:center;
          box-shadow:0 4px 12px rgba(59,130,246,.4);flex-shrink:0;",
        tags$span(style = "color:#fff;font-size:11px;font-weight:800;", "MK")
      ),
      tags$span(style = "color:#F1F5F9;font-weight:700;font-size:13.5px;letter-spacing:-.4px;", "LOKASI"),
      tags$span(style = "color:#334155;font-weight:400;font-size:13.5px;", "USAHA")
    ), titleWidth = 248
  ),
  
  dashboardSidebar(
    width = 248,
    tags$head(tags$style(HTML(CSS))),
    sidebarMenu(
      id = "tabs",
      tags$div(
        style = "padding:12px 16px 6px;",
        tags$div(
          style = "background:rgba(16,185,129,.06);border:1px solid rgba(16,185,129,.12);
                   border-radius:8px;padding:8px 12px;display:flex;align-items:center;",
          tags$span(class = "pulse-dot"),
          tags$span(style = "font-size:10.5px;color:#10B981;font-weight:600;", "LIVE ANALYTICS")
        )
      ),
      tags$span(class = "sidebar-section-label", "Navigasi"),
      menuItem("Command Center",    tabName = "home",       icon = icon("gauge-high")),
      menuItem("Peta 3D Interaktif",tabName = "map",        icon = icon("map")),
      menuItem("K-Means Pro",       tabName = "kmeans",     icon = icon("circle-nodes")),
      menuItem("DBSCAN Spatial",    tabName = "dbscan_tab", icon = icon("layer-group")),
      menuItem("AI Insights",       tabName = "ai",         icon = icon("brain")),
      menuItem("Komparasi Algo",    tabName = "compare",    icon = icon("code-compare")),
      menuItem("Eksplorasi Multi",  tabName = "explore",    icon = icon("chart-scatter")),
      menuItem("Temporal Analysis", tabName = "temporal",   icon = icon("clock")),
      menuItem("Data Lab",          tabName = "data_tab",   icon = icon("flask")),
      menuItem("Tentang",           tabName = "about",      icon = icon("circle-info")),
      
      tags$hr(class = "sidebar-divider"),
      tags$span(class = "sidebar-section-label", "Filter Global"),
      tags$div(
        style = "padding:0 12px;",
        pickerInput("f_kec", "Kecamatan",
                    choices = c("Semua", KECS), selected = "Semua",
                    options = pickerOptions(liveSearch = TRUE, size = 8)),
        pickerInput("f_grup", "Kategori",
                    choices = c("Semua", GRUPS), selected = "Semua",
                    options = pickerOptions(liveSearch = TRUE, size = 8)),
        pickerInput("f_layak", "Kelayakan",
                    choices = c("Semua", LAYAKS), selected = "Semua",
                    options = pickerOptions(size = 6)),
        sliderInput("f_rating", "Rating Min",  min = 1, max = 5, value = 1, step = 0.5),
        sliderInput("f_ulasan", "Min Ulasan",  min = 0, max = 999, value = 0, step = 10),
        sliderInput("f_pot",    "Min Potensi", min = 0, max = 100, value = 0, step = 5)
      ),
      
      tags$hr(class = "sidebar-divider"),
      tags$span(class = "sidebar-section-label", "Parameter Algoritma"),
      tags$div(
        style = "padding:0 12px;",
        sliderInput("k_n",   "K — K-Means",    min = 2, max = 10, value = 4, step = 1),
        sliderInput("eps",   "Epsilon DBSCAN",  min = 0.01, max = 0.40, value = 0.08, step = 0.01),
        sliderInput("mpts",  "MinPts DBSCAN",   min = 2, max = 20, value = 5, step = 1),
        selectInput("dist_method", "Jarak K-Means",
                    choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan"), selected = "euclidean"),
        tags$br(),
        tags$button(id = "run_analysis", class = "btn btn-mega action-button",
                    icon("bolt"), " Analisis Sekarang")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ====================================================
      # COMMAND CENTER
      # ====================================================
      tabItem(tabName = "home",
              fluidRow(
                valueBoxOutput("vb1", 3), valueBoxOutput("vb2", 3),
                valueBoxOutput("vb3", 3), valueBoxOutput("vb4", 3)
              ),
              fluidRow(
                valueBoxOutput("vb5", 3), valueBoxOutput("vb6", 3),
                valueBoxOutput("vb7", 3), valueBoxOutput("vb8", 3)
              ),
              fluidRow(
                box(title = "Distribusi Kategori",  width = 3, withSpinner(plotlyOutput("ph_kat",   height = "220px"), color = "#3B82F6", type = 4)),
                box(title = "Rating Histogram",     width = 3, withSpinner(plotlyOutput("ph_rat",   height = "220px"), color = "#3B82F6", type = 4)),
                box(title = "Kelayakan Donut",      width = 3, withSpinner(plotlyOutput("ph_layak", height = "220px"), color = "#3B82F6", type = 4)),
                box(title = "Popularitas Donut",    width = 3, withSpinner(plotlyOutput("ph_pop",   height = "220px"), color = "#3B82F6", type = 4))
              ),
              fluidRow(
                box(title = "Scatter 3D — Rating × Ulasan × Potensi", width = 8,
                    withSpinner(plotlyOutput("ph_3d",  height = "340px"), color = "#3B82F6", type = 4)),
                box(title = "Kecamatan × Kategori Treemap",            width = 4,
                    withSpinner(plotlyOutput("ph_tree", height = "340px"), color = "#3B82F6", type = 4))
              ),
              fluidRow(
                box(title = "Top 15 Usaha Terbaik — Skor Potensi", width = 12,
                    withSpinner(plotlyOutput("ph_top", height = "220px"), color = "#3B82F6", type = 4))
              )
      ),
      
      # ====================================================
      # PETA INTERAKTIF
      # ====================================================
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, title = "Peta Interaktif Premium — Kota Makassar",
                    fluidRow(
                      column(2, radioGroupButtons("map_mode", "Tampilan",
                                                  choices = c("All" = "all", "K-Means" = "km", "DBSCAN" = "db", "Heatmap" = "heat"),
                                                  selected = "all", size = "xs", status = "primary")),
                      column(2, radioGroupButtons("map_tile", "Basemap",
                                                  choices = c("Dark" = "dk", "Topo" = "tp", "Sat" = "st", "Minimal" = "mn"),
                                                  selected = "dk", size = "xs", status = "primary")),
                      column(2, switchInput("sw_cluster", "Cluster Markers", FALSE, size = "small", onStatus = "primary")),
                      column(2, switchInput("sw_heat",    "Heat Overlay",    FALSE, size = "small", onStatus = "success")),
                      column(2, switchInput("sw_foto",    "Tampilkan Foto",  TRUE,  size = "small", onStatus = "warning")),
                      column(2, switchInput("sw_mini",    "MiniMap",         TRUE,  size = "small", onStatus = "info"))
                    ),
                    tags$div(style = "height:8px"),
                    withSpinner(leafletOutput("map_out", height = "520px"), color = "#3B82F6", type = 4),
                    fluidRow(
                      column(4, tags$div(style = "margin-top:10px;", verbatimTextOutput("map_click"))),
                      column(8, tags$div(style = "margin-top:10px;", uiOutput("map_summary_chips")))
                    )
                )
              )
      ),
      
      # ====================================================
      # K-MEANS PRO
      # ====================================================
      tabItem(tabName = "kmeans",
              fluidRow(
                box(title = "PCA Biplot Interaktif", width = 8,
                    fluidRow(
                      column(4, radioGroupButtons("pca_col", "Warna",
                                                  choices = c("Cluster" = "cl", "Kecamatan" = "kc", "Rating" = "rt", "Potensi" = "pt"),
                                                  selected = "cl", size = "xs", status = "primary")),
                      column(4, switchInput("pca_el", "Ellipse 95%", TRUE,  size = "small", onStatus = "primary")),
                      column(4, switchInput("pca_3d", "Mode 3D",     FALSE, size = "small", onStatus = "warning"))
                    ),
                    tags$div(style = "height:6px"),
                    withSpinner(uiOutput("pca_out"), color = "#3B82F6", type = 4)
                ),
                box(title = "Diagnostik Cluster", width = 4,
                    tabsetPanel(
                      tabPanel("Elbow",     withSpinner(plotOutput("p_elbow", height = "280px"), color = "#3B82F6", type = 4)),
                      tabPanel("Silhouette",withSpinner(plotOutput("p_sil",   height = "280px"), color = "#3B82F6", type = 4)),
                      tabPanel("Gap Stat",  withSpinner(plotOutput("p_gap",   height = "280px"), color = "#3B82F6", type = 4))
                    )
                )
              ),
              fluidRow(
                box(title = "Radar Chart — Profil Multidimensi", width = 5,
                    withSpinner(plotlyOutput("p_radar", height = "320px"), color = "#3B82F6", type = 4)),
                box(title = "Box Plot per Cluster", width = 4,
                    selectInput("box_v", "Variabel",
                                choices = c("Rating" = "rating", "Ulasan" = "jumlah_ulasan", "Potensi" = "skor_potensi"),
                                selected = "skor_potensi"),
                    withSpinner(plotlyOutput("p_box", height = "270px"), color = "#3B82F6", type = 4)),
                box(title = "Cluster Summary Stats", width = 3,
                    withSpinner(tableOutput("cl_stats"), color = "#3B82F6", type = 4))
              ),
              fluidRow(
                box(title = "Rekomendasi Strategis per Cluster", width = 12,
                    uiOutput("km_cards"))
              )
      ),
      
      # ====================================================
      # DBSCAN
      # ====================================================
      tabItem(tabName = "dbscan_tab",
              fluidRow(
                box(title = "Peta Spasial DBSCAN", width = 7,
                    withSpinner(plotlyOutput("db_scatter", height = "400px"), color = "#3B82F6", type = 4)),
                box(title = "k-NN Distance + Reachability", width = 5,
                    tabsetPanel(
                      tabPanel("k-NN Dist",   withSpinner(plotOutput("db_knn",   height = "330px"), color = "#3B82F6", type = 4)),
                      tabPanel("Reachability",withSpinner(plotOutput("db_reach", height = "330px"), color = "#3B82F6", type = 4))
                    )
                )
              ),
              fluidRow(
                box(title = "Distribusi Cluster", width = 4,
                    withSpinner(plotlyOutput("db_bar",   height = "240px"), color = "#3B82F6", type = 4)),
                box(title = "Density Heatmap Cluster × Kategori", width = 5,
                    withSpinner(plotlyOutput("db_heat2", height = "240px"), color = "#3B82F6", type = 4)),
                box(title = "Noise Analysis", width = 3,
                    withSpinner(uiOutput("db_noise_ui"), color = "#3B82F6", type = 4))
              )
      ),
      
      # ====================================================
      # AI INSIGHTS
      # ====================================================
      tabItem(tabName = "ai",
              fluidRow(
                box(title = "AI-Generated Business Insights", width = 8,
                    tags$div(
                      style = "margin-bottom:12px;",
                      actionButton("gen_insight", "Generate AI Insights", icon = icon("brain"),
                                   class = "btn",
                                   style = "background:linear-gradient(135deg,#7C3AED,#8B5CF6);color:#fff;
                                     border:none;border-radius:7px;padding:8px 16px;font-size:12px;font-weight:600;
                                     cursor:pointer;box-shadow:0 4px 16px rgba(139,92,246,.3)")
                    ),
                    withSpinner(uiOutput("ai_insights"), color = "#8B5CF6", type = 4)
                ),
                box(title = "Recommendation Engine", width = 4,
                    selectInput("rec_kec", "Pilih Kecamatan Target", choices = KECS),
                    selectInput("rec_kat", "Pilih Kategori Usaha",   choices = GRUPS),
                    actionButton("get_rec", "Dapatkan Rekomendasi", icon = icon("sparkles"),
                                 class = "btn",
                                 style = "width:100%;background:#10B981;color:#fff;border:none;
                                   border-radius:7px;padding:8px;font-size:12px;font-weight:600;
                                   cursor:pointer;margin-top:10px;"),
                    tags$div(style = "height:10px"),
                    withSpinner(uiOutput("rec_ui"), color = "#10B981", type = 4)
                )
              ),
              fluidRow(
                box(title = "Anomali & Outlier Detection (Mahalanobis)", width = 6,
                    withSpinner(plotlyOutput("p_outlier",  height = "300px"), color = "#3B82F6", type = 4)),
                box(title = "Market Gap Analysis — Kecamatan × Kategori", width = 6,
                    withSpinner(plotlyOutput("p_gap_mkt",  height = "300px"), color = "#3B82F6", type = 4))
              )
      ),
      
      # ====================================================
      # KOMPARASI
      # ====================================================
      tabItem(tabName = "compare",
              fluidRow(
                box(title = "Confusion Matrix — K-Means vs DBSCAN", width = 6,
                    withSpinner(plotlyOutput("p_conf",    height = "320px"), color = "#3B82F6", type = 4)),
                box(title = "Metrik Kualitas Clustering", width = 6,
                    withSpinner(uiOutput("p_metrics"), color = "#3B82F6", type = 4))
              ),
              fluidRow(
                box(title = "Parallel Coordinates — Profil Cluster", width = 12,
                    withSpinner(plotlyOutput("p_parallel", height = "300px"), color = "#3B82F6", type = 4))
              )
      ),
      
      # ====================================================
      # EKSPLORASI MULTI
      # ====================================================
      tabItem(tabName = "explore",
              fluidRow(
                box(title = "Multi-Variable Explorer", width = 8,
                    fluidRow(
                      column(3, selectInput("ex_x", "Sumbu X",
                                            choices = c("Rating" = "rating", "Ulasan" = "jumlah_ulasan",
                                                        "Potensi" = "skor_potensi", "Lat" = "latitude", "Lng" = "longitude"),
                                            selected = "rating")),
                      column(3, selectInput("ex_y", "Sumbu Y",
                                            choices = c("Potensi" = "skor_potensi", "Rating" = "rating",
                                                        "Ulasan" = "jumlah_ulasan", "Lat" = "latitude", "Lng" = "longitude"),
                                            selected = "skor_potensi")),
                      column(3, selectInput("ex_c", "Warna",
                                            choices = c("Kategori" = "kategori_grup", "Kecamatan" = "kecamatan",
                                                        "Kelayakan" = "label_kelayakan", "Popularitas" = "popularitas"),
                                            selected = "kategori_grup")),
                      column(3, selectInput("ex_s", "Ukuran Titik",
                                            choices = c("Ulasan" = "jumlah_ulasan", "Potensi" = "skor_potensi", "Rating" = "rating"),
                                            selected = "jumlah_ulasan"))
                    ),
                    withSpinner(plotlyOutput("p_ex", height = "370px"), color = "#3B82F6", type = 4)
                ),
                box(title = "Distribusi Variabel", width = 4,
                    selectInput("ex_hv", "Variabel",
                                choices = c("Rating" = "rating", "Ulasan" = "jumlah_ulasan", "Potensi" = "skor_potensi"),
                                selected = "skor_potensi"),
                    withSpinner(plotlyOutput("p_ex_hist", height = "155px"), color = "#3B82F6", type = 4),
                    tags$div(style = "height:6px"),
                    withSpinner(plotlyOutput("p_ex_box",  height = "155px"), color = "#3B82F6", type = 4)
                )
              ),
              fluidRow(
                box(title = "Correlation Matrix Heatmap", width = 5,
                    withSpinner(plotlyOutput("p_corr",   height = "300px"), color = "#3B82F6", type = 4)),
                box(title = "Bubble Chart — Kecamatan Analytics", width = 4,
                    withSpinner(plotlyOutput("p_bubble", height = "300px"), color = "#3B82F6", type = 4)),
                box(title = "Sunburst Kategori × Kecamatan", width = 3,
                    withSpinner(plotlyOutput("p_sun",    height = "300px"), color = "#3B82F6", type = 4))
              )
      ),
      
      # ====================================================
      # TEMPORAL
      # ====================================================
      tabItem(tabName = "temporal",
              fluidRow(
                box(title = "Distribusi Rating per Kecamatan (Violin)", width = 12,
                    withSpinner(plotlyOutput("p_temp1", height = "320px"), color = "#3B82F6", type = 4))
              ),
              fluidRow(
                box(title = "Cumulative Density per Kategori", width = 6,
                    withSpinner(plotlyOutput("p_ecdf", height = "280px"), color = "#3B82F6", type = 4)),
                box(title = "Ranking Kecamatan — Multivariate", width = 6,
                    withSpinner(plotlyOutput("p_rank", height = "280px"), color = "#3B82F6", type = 4))
              )
      ),
      
      # ====================================================
      # DATA LAB
      # ====================================================
      tabItem(tabName = "data_tab",
              fluidRow(
                box(title = tags$span(icon("flask"), " Data Laboratory"), width = 12,
                    fluidRow(
                      column(2, downloadButton("dl1", "Export Full CSV",
                                               class = "btn btn-dl-green", icon = icon("download", class = "fa-xs"))),
                      column(2, downloadButton("dl2", "Export Filtered",
                                               class = "btn btn-dl-blue",  icon = icon("filter",   class = "fa-xs"))),
                      column(3, selectInput("tbl_cols", "Tampilkan Kolom",
                                            choices = c("Semua" = "all", "Dasar" = "basic",
                                                        "Koordinat" = "coord", "Cluster" = "cluster"),
                                            selected = "all")),
                      column(5)
                    ),
                    tags$div(style = "height:10px"),
                    withSpinner(DTOutput("data_tbl"), color = "#3B82F6", type = 4)
                )
              )
      ),
      
      # ====================================================
      # TENTANG
      # ====================================================
      tabItem(tabName = "about",
              fluidRow(
                box(title = "Tentang Platform", width = 8,
                    tags$div(
                      class = "about-section",
                      tags$h4("LOKASI USAHA MAKASSAR"),
                      tags$p("Platform analitik bisnis generasi berikutnya — menggabungkan machine learning,
                              geospatial analytics, dan AI insights untuk membantu pelaku usaha menemukan
                              lokasi terbaik di Kota Makassar (Kecamatan Mamajang & Mariso)."),
                      tags$hr(class = "about-divider"),
                      tags$h5("Metodologi Pengumpulan Data"),
                      tags$p("Data diambil via Google Maps scraping menggunakan Playwright automation Python.
                              Koordinat diperoleh dari pin Google Maps pada zoom level 19 — presisi tingkat bangunan.
                              Foto usaha diambil langsung dari Google Maps API."),
                      tags$h5("Algoritma Machine Learning"),
                      tags$p(tags$b(style = "color:#F1F5F9", "K-Means Clustering"),
                             " — Spatial + feature clustering menggunakan PCA untuk reduksi dimensi.
                              Evaluasi dengan Elbow Method, Silhouette Score, dan Gap Statistic."),
                      tags$p(tags$b(style = "color:#F1F5F9", "DBSCAN"),
                             " — Density-Based Spatial Clustering dengan k-NN distance optimization.
                              Otomatis deteksi noise dan cluster tidak beraturan."),
                      tags$hr(class = "about-divider"),
                      tags$div(
                        class = "note-box",
                        tags$b(style = "color:#3B82F6", "Dataset: "),
                        "1.590 lokasi usaha | 2 kecamatan (Mamajang & Mariso) | 10 kategori | Kota Makassar | 2026"
                      )
                    )
                ),
                box(title = "Tech Stack", width = 4,
                    tags$div(
                      class = "about-section",
                      tags$h5("Core Platform"),
                      tags$div(lapply(c("R 4.3+", "Shiny", "shinydashboard", "shinyWidgets"),
                                      function(x) tags$span(class = "tech-badge", x))),
                      tags$h5("Visualization"),
                      tags$div(lapply(c("Leaflet", "Plotly", "ggplot2", "leaflet.extras"),
                                      function(x) tags$span(class = "tech-badge", x))),
                      tags$h5("Machine Learning"),
                      tags$div(lapply(c("K-Means", "DBSCAN", "PCA", "Silhouette", "Gap Stat"),
                                      function(x) tags$span(class = "tech-badge", x))),
                      tags$h5("Data Pipeline"),
                      tags$div(lapply(c("Python", "Playwright", "Google Maps API", "Gemini Vision"),
                                      function(x) tags$span(class = "tech-badge", x))),
                      tags$h5("Dataset"),
                      tags$div(lapply(c("1.590 lokasi", "2 kecamatan", "10 kategori", "2026"),
                                      function(x) tags$span(class = "tech-badge", x)))
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
  
  # ── FILTERED DATA ─────────────────────────────────────────
  fd <- reactive({
    d <- data_usaha
    if (!is.null(input$f_kec)   && input$f_kec   != "Semua") d <- d[d$kecamatan     == input$f_kec,   ]
    if (!is.null(input$f_grup)  && input$f_grup  != "Semua") d <- d[d$kategori_grup == input$f_grup,  ]
    if (!is.null(input$f_layak) && input$f_layak != "Semua") d <- d[d$label_kelayakan == input$f_layak,]
    d <- d[!is.na(d$rating) & d$rating >= input$f_rating, ]
    d <- d[!is.na(d$jumlah_ulasan) & d$jumlah_ulasan >= input$f_ulasan, ]
    d <- d[!is.na(d$skor_potensi) & d$skor_potensi >= input$f_pot, ]
    d
  })
  
  # Spatial subset of filtered data
  fd_sp <- reactive({
    fd() %>% filter(!is.na(latitude), !is.na(longitude))
  })
  
  # ── RUN ANALYSIS ──────────────────────────────────────────
  ar <- eventReactive(input$run_analysis, {
    d <- fd()
    d_sp <- fd_sp()
    if (nrow(d_sp) < input$k_n + 1) {
      showNotification("Data spasial terlalu sedikit untuk clustering.", type = "error", duration = 4)
      return(NULL)
    }
    withProgress(message = "Menjalankan analisis...", value = 0, {
      setProgress(0.3, detail = "K-Means clustering...")
      fs <- scale(d_sp[, c("latitude", "longitude", "rating", "jumlah_ulasan", "skor_potensi")])
      km <- kmeans(fs, centers = input$k_n, nstart = 25, iter.max = 100)
      d_sp$cluster_kmeans <- as.factor(km$cluster)
      
      setProgress(0.7, detail = "DBSCAN clustering...")
      f2 <- scale(d_sp[, c("latitude", "longitude")])
      db <- dbscan::dbscan(f2, eps = input$eps, minPts = input$mpts)
      d_sp$cluster_dbscan <- as.factor(db$cluster)
      
      setProgress(1, detail = "Selesai!")
      list(data = d_sp, km = km, scaled = fs)
    })
  }, ignoreNULL = FALSE)
  
  rd <- reactive({
    res <- ar()
    if (is.null(res)) fd_sp() else res$data
  })
  
  # ── VALUE BOXES ───────────────────────────────────────────
  mkVB <- function(v, l, col = "#F1F5F9") {
    valueBox(
      tags$span(style = paste0("font-family:'JetBrains Mono',monospace;font-size:26px;font-weight:700;color:", col, ";letter-spacing:-1.5px;"), v),
      tags$span(style = "font-size:9.5px;color:#2D3F55;text-transform:uppercase;letter-spacing:.8px;font-weight:700;", l),
      icon = NULL, color = "black"
    )
  }
  
  output$vb1 <- renderValueBox(mkVB(nrow(fd()), "Total Lokasi", "#F1F5F9"))
  output$vb2 <- renderValueBox(mkVB(length(unique(fd()$kecamatan)), "Kecamatan", "#3B82F6"))
  output$vb3 <- renderValueBox(mkVB(length(unique(fd()$kategori_grup)), "Kategori", "#10B981"))
  output$vb4 <- renderValueBox(mkVB(round(mean(fd()$rating, na.rm = TRUE), 2), "Avg Rating", "#F59E0B"))
  output$vb5 <- renderValueBox(mkVB(round(mean(fd()$skor_potensi, na.rm = TRUE), 1), "Avg Potensi", "#8B5CF6"))
  output$vb6 <- renderValueBox(mkVB(round(mean(fd()$jumlah_ulasan, na.rm = TRUE)), "Avg Ulasan", "#06B6D4"))
  output$vb7 <- renderValueBox(mkVB(sum(fd()$rating == 5, na.rm = TRUE), "Rating 5★", "#EC4899"))
  output$vb8 <- renderValueBox(mkVB(nrow(fd()[grepl("Sangat", fd()$label_kelayakan), ]), "Sangat Layak", "#84CC16"))
  
  # ── COMMAND CENTER PLOTS ──────────────────────────────────
  output$ph_kat <- renderPlotly({
    d <- fd() %>% count(kategori_grup) %>% arrange(n)
    plot_ly(d, x = ~n, y = ~reorder(kategori_grup, n), type = "bar", orientation = "h",
            marker = list(color = PAL[1:nrow(d)], opacity = .85,
                          line = list(color = "rgba(0,0,0,0)", width = 0))) %>%
      layout(mk_layout(margin = list(t = 5, b = 20, l = 140, r = 10),
                       xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = ""),
                       yaxis = list(gridcolor = "#1A2235", title = "", tickfont = list(color = "#475569", size = 9))))
  })
  
  output$ph_rat <- renderPlotly({
    plot_ly(fd(), x = ~rating, type = "histogram", nbinsx = 9,
            marker = list(color = "#10B981", opacity = .8, line = list(color = "#090B0F", width = .5))) %>%
      layout(mk_layout(margin = list(t = 5, b = 30, l = 40, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = "Rating", tickfont = list(color = "#475569")),
                       yaxis = list(gridcolor = "#1A2235", title = "",       tickfont = list(color = "#475569"))))
  })
  
  output$ph_layak <- renderPlotly({
    d <- fd() %>% count(label_kelayakan)
    plot_ly(d, labels = ~label_kelayakan, values = ~n, type = "pie", hole = .55,
            marker = list(colors = c("#10B981", "#3B82F6", "#F59E0B", "#EF4444"),
                          line = list(color = "#090B0F", width = 2)),
            textfont = list(size = 10, color = "#94A3B8"), hoverinfo = "label+percent") %>%
      layout(mk_layout(margin = list(t = 5, b = 5, l = 5, r = 5),
                       legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#475569", size = 9),
                                     orientation = "h", x = .05, y = -.05), showlegend = TRUE))
  })
  
  output$ph_pop <- renderPlotly({
    d <- fd() %>% count(popularitas) %>% arrange(desc(n))
    pop_colors <- c("Sangat Populer" = "#3B82F6", "Populer" = "#10B981",
                    "Cukup Dikenal" = "#F59E0B", "Baru" = "#8B5CF6", "Tidak Ada Ulasan" = "#334155")
    cols <- pop_colors[d$popularitas]
    plot_ly(d, labels = ~popularitas, values = ~n, type = "pie", hole = .55,
            marker = list(colors = cols, line = list(color = "#090B0F", width = 2)),
            textfont = list(size = 10, color = "#94A3B8"), hoverinfo = "label+percent") %>%
      layout(mk_layout(margin = list(t = 5, b = 5, l = 5, r = 5),
                       legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#475569", size = 8),
                                     orientation = "h", x = -.05, y = -.1), showlegend = TRUE))
  })
  
  output$ph_3d <- renderPlotly({
    d <- fd()
    plot_ly(d, x = ~rating, y = ~jumlah_ulasan, z = ~skor_potensi,
            color = ~kategori_grup, colors = PAL, type = "scatter3d", mode = "markers",
            marker = list(size = 4, opacity = .75, line = list(color = "rgba(0,0,0,.1)", width = .5)),
            text = ~paste0("<b>", nama, "</b><br>", kecamatan), hoverinfo = "text") %>%
      layout(scene = list(
        xaxis = list(title = "Rating",  gridcolor = "#1A2235", backgroundcolor = "rgba(0,0,0,0)", showbackground = TRUE, tickfont = list(color = "#475569", size = 9)),
        yaxis = list(title = "Ulasan",  gridcolor = "#1A2235", backgroundcolor = "rgba(0,0,0,0)", showbackground = TRUE, tickfont = list(color = "#475569", size = 9)),
        zaxis = list(title = "Potensi", gridcolor = "#1A2235", backgroundcolor = "rgba(0,0,0,0)", showbackground = TRUE, tickfont = list(color = "#475569", size = 9)),
        bgcolor = "rgba(0,0,0,0)", camera = list(eye = list(x = 1.5, y = 1.5, z = 1.2))),
        paper_bgcolor = "rgba(0,0,0,0)",
        legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#475569", size = 10)),
        margin = list(t = 0, b = 0, l = 0, r = 0))
  })
  
  output$ph_tree <- renderPlotly({
    d <- fd() %>% count(kecamatan, kategori_grup)
    plot_ly(d, type = "treemap", labels = ~paste(kecamatan, "-", kategori_grup),
            parents = ~kecamatan, values = ~n,
            marker = list(colorscale = list(c(0, "#0D1120"), c(.5, "#1D4ED8"), c(1, "#3B82F6")),
                          line = list(color = "#090B0F", width = 1)),
            textfont = list(color = "#F1F5F9", size = 9)) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", margin = list(t = 0, b = 0, l = 0, r = 0))
  })
  
  output$ph_top <- renderPlotly({
    d <- fd() %>% arrange(desc(skor_potensi)) %>% head(15)
    d$label_nm <- paste0(substr(d$nama, 1, 25), ifelse(nchar(d$nama) > 25, "…", ""))
    plot_ly(d, x = ~skor_potensi, y = ~reorder(label_nm, skor_potensi),
            type = "bar", orientation = "h",
            marker = list(color = ~skor_potensi,
                          colorscale = list(c(0, "#1E293B"), c(.5, "#2563EB"), c(1, "#06B6D4")),
                          showscale = FALSE, opacity = .9,
                          line = list(color = "rgba(0,0,0,0)", width = 0)),
            text = ~paste0(kecamatan, " | ", rating, "★"),
            textposition = "outside", textfont = list(color = "#475569", size = 9)) %>%
      layout(mk_layout(margin = list(t = 5, b = 20, l = 210, r = 60),
                       xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Skor Potensi"),
                       yaxis = list(gridcolor = "#1A2235", title = "", tickfont = list(color = "#64748B", size = 9.5))))
  })
  
  # ── PETA ──────────────────────────────────────────────────
  output$map_out <- renderLeaflet({
    d    <- rd()
    mode <- input$map_mode
    provider <- switch(input$map_tile,
                       "dk" = providers$CartoDB.DarkMatter,
                       "tp" = providers$OpenTopoMap,
                       "st" = providers$Esri.WorldImagery,
                       "mn" = providers$CartoDB.Positron)
    
    pal_fn <- if (mode == "km" && "cluster_kmeans" %in% names(d)) {
      colorFactor(PAL, domain = d$cluster_kmeans)
    } else if (mode == "db" && "cluster_dbscan" %in% names(d)) {
      colorFactor(c("#334155", PAL), domain = d$cluster_dbscan)
    } else if (mode == "heat") {
      colorNumeric(c("#0A0D18", "#1E3A5F", "#1D4ED8", "#06B6D4", "#10B981"), domain = d$skor_potensi)
    } else {
      colorFactor(PAL, domain = d$kategori_grup)
    }
    
    color_var <- if (mode == "km" && "cluster_kmeans" %in% names(d)) d$cluster_kmeans
    else if (mode == "db" && "cluster_dbscan" %in% names(d)) d$cluster_dbscan
    else if (mode == "heat") d$skor_potensi
    else d$kategori_grup
    
    rad <- if (mode == "heat") rescale(d$skor_potensi, to = c(3, 14)) else 5.5
    
    # Build popup with optional foto
    foto_html <- ifelse(
      isTRUE(input$sw_foto) & !is.na(d$foto_url),
      paste0('<img src="', d$foto_url, '" style="width:100%;border-radius:6px;margin-bottom:8px;max-height:120px;object-fit:cover;" onerror="this.style.display=\'none\'">'),
      ""
    )
    
    popup_html <- paste0(
      "<div style='font-family:Inter,sans-serif;min-width:220px;max-width:280px;'>",
      foto_html,
      "<div style='font-size:13px;font-weight:700;color:#F1F5F9;margin-bottom:2px'>", d$nama, "</div>",
      "<div style='font-size:10px;color:#64748B;margin-bottom:8px'>", d$kecamatan, " &bull; ", d$kategori_grup, "</div>",
      "<div style='display:grid;grid-template-columns:1fr 1fr 1fr;gap:6px;'>",
      "<div style='text-align:center;'><div style='font-size:15px;font-weight:700;color:#F59E0B;font-family:JetBrains Mono'>", d$rating, "</div><div style='font-size:9px;color:#475569'>Rating</div></div>",
      "<div style='text-align:center;'><div style='font-size:15px;font-weight:700;color:#3B82F6;font-family:JetBrains Mono'>", d$jumlah_ulasan, "</div><div style='font-size:9px;color:#475569'>Ulasan</div></div>",
      "<div style='text-align:center;'><div style='font-size:15px;font-weight:700;color:#10B981;font-family:JetBrains Mono'>", d$skor_potensi, "</div><div style='font-size:9px;color:#475569'>Potensi</div></div>",
      "</div>",
      "<div style='margin-top:8px;font-size:10px;color:#334155;'>", d$label_kelayakan, " &bull; ", d$telepon, "</div>",
      "</div>"
    )
    
    m <- leaflet(d) %>%
      addProviderTiles(provider) %>%
      setView(lng = 119.43, lat = -5.14, zoom = 13) %>%
      addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE))
    
    if (isTRUE(input$sw_heat)) {
      m <- m %>% addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~skor_potensi,
                            blur = 18, max = 0.06, radius = 14,
                            gradient = list("0" = "#090B0F", "0.5" = "#1D4ED8", "1" = "#06B6D4"))
    }
    
    if (isTRUE(input$sw_cluster)) {
      m <- m %>% addCircleMarkers(lng = ~longitude, lat = ~latitude,
                                  color = ~pal_fn(color_var), fillColor = ~pal_fn(color_var),
                                  fillOpacity = .85, opacity = 1, radius = rad, weight = 1.5,
                                  popup = popup_html,
                                  clusterOptions = markerClusterOptions(
                                    iconCreateFunction = JS("function(c){return L.divIcon({html:'<div style=\"background:linear-gradient(135deg,#1D4ED8,#6366F1);color:#fff;border-radius:50%;width:38px;height:38px;display:flex;align-items:center;justify-content:center;font-size:12px;font-weight:700;box-shadow:0 0 0 4px rgba(59,130,246,.25),0 4px 16px rgba(0,0,0,.4)\">'+c.getChildCount()+'</div>',className:''})}") ))
    } else {
      m <- m %>% addCircleMarkers(lng = ~longitude, lat = ~latitude,
                                  color = ~pal_fn(color_var), fillColor = ~pal_fn(color_var),
                                  fillOpacity = .85, opacity = 1, radius = rad, weight = 1.5,
                                  popup = popup_html)
    }
    
    if (mode != "heat") {
      m <- m %>% addLegend("bottomright", pal = pal_fn, values = color_var,
                           title = switch(mode, "all" = "Kategori", "km" = "K-Means", "db" = "DBSCAN", "Kategori"),
                           opacity = .9)
    }
    
    if (isTRUE(input$sw_mini)) {
      m <- m %>% addMiniMap(tiles = providers$CartoDB.DarkMatter,
                            toggleDisplay = TRUE, width = 130, height = 100,
                            minimized = FALSE, position = "bottomright")
    }
    m
  })
  
  output$map_summary_chips <- renderUI({
    d <- rd()
    tags$div(
      style = "display:flex;flex-wrap:wrap;gap:8px;",
      lapply(sort(unique(d$kategori_grup)), function(g) {
        n <- sum(d$kategori_grup == g, na.rm = TRUE)
        tags$div(
          style = "background:rgba(59,130,246,.06);border:1px solid rgba(59,130,246,.15);
                   border-radius:6px;padding:5px 10px;display:flex;align-items:center;gap:6px;",
          tags$span(style = "font-size:10px;color:#3B82F6;font-weight:700;", g),
          tags$span(style = "font-size:11px;color:#F1F5F9;font-family:'JetBrains Mono',monospace;font-weight:600;", n)
        )
      })
    )
  })
  
  # ── K-MEANS ───────────────────────────────────────────────
  output$pca_out <- renderUI({
    if (isTRUE(input$pca_3d)) plotlyOutput("p_pca_3d", height = "370px")
    else                       plotlyOutput("p_pca_2d", height = "370px")
  })
  
  pca_data <- reactive({
    res <- ar(); if (is.null(res)) return(NULL)
    d   <- res$data
    pca <- prcomp(res$scaled)
    var_exp <- summary(pca)$importance[2, ] * 100
    col_var <- switch(input$pca_col,
                      "cl" = paste0("C", d$cluster_kmeans),
                      "kc" = d$kecamatan,
                      "rt" = d$rating_kategori,
                      "pt" = d$label_kelayakan)
    list(pca = pca, d = d, col = col_var, var = var_exp)
  })
  
  output$p_pca_2d <- renderPlotly({
    pd <- pca_data(); if (is.null(pd)) return(NULL)
    plot_ly(x = pd$pca$x[, 1], y = pd$pca$x[, 2], color = pd$col, colors = PAL,
            type = "scatter", mode = "markers",
            marker = list(size = 7, opacity = .75, line = list(color = "rgba(0,0,0,.15)", width = .5)),
            text = paste0("<b>", pd$d$nama, "</b><br>", pd$d$kecamatan, "<br>Rating: ", pd$d$rating),
            hoverinfo = "text") %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = paste0("PC1 (", round(pd$var[1], 1), "%)"), tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = paste0("PC2 (", round(pd$var[2], 1), "%)"), tickfont = list(color = "#475569"))))
  })
  
  output$p_pca_3d <- renderPlotly({
    pd <- pca_data(); if (is.null(pd) || ncol(pd$pca$x) < 3) return(NULL)
    plot_ly(x = pd$pca$x[, 1], y = pd$pca$x[, 2], z = pd$pca$x[, 3],
            color = pd$col, colors = PAL, type = "scatter3d", mode = "markers",
            marker = list(size = 4, opacity = .75),
            text = paste0("<b>", pd$d$nama, "</b><br>", pd$d$kecamatan), hoverinfo = "text") %>%
      layout(scene = list(bgcolor = "rgba(0,0,0,0)",
                          xaxis = list(title = paste0("PC1(", round(pd$var[1], 1), "%)"), gridcolor = "#1A2235", backgroundcolor = "rgba(0,0,0,0)", showbackground = TRUE),
                          yaxis = list(title = paste0("PC2(", round(pd$var[2], 1), "%)"), gridcolor = "#1A2235", backgroundcolor = "rgba(0,0,0,0)", showbackground = TRUE),
                          zaxis = list(title = paste0("PC3(", round(pd$var[3], 1), "%)"), gridcolor = "#1A2235", backgroundcolor = "rgba(0,0,0,0)", showbackground = TRUE)),
             paper_bgcolor = "rgba(0,0,0,0)",
             legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#475569", size = 10)),
             margin = list(t = 0, b = 0, l = 0, r = 0))
  })
  
  output$p_elbow <- renderPlot({
    d <- fd_sp(); if (nrow(d) < 3) return(NULL)
    fs  <- scale(d[, c("latitude", "longitude", "rating", "jumlah_ulasan", "skor_potensi")])
    wss <- sapply(2:10, function(k) tryCatch(kmeans(fs, k, nstart = 10)$tot.withinss, error = function(e) NA))
    df_e <- data.frame(K = 2:10, WSS = wss) %>% filter(!is.na(WSS))
    ggplot(df_e, aes(K, WSS)) +
      geom_area(fill = "#3B82F6", alpha = .06) +
      geom_line(color = "#2D3F55", linewidth = .7) +
      geom_point(aes(color = K == input$k_n), size = 5, show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = "#3B82F6", "TRUE" = "#EF4444")) +
      geom_vline(xintercept = input$k_n, linetype = "dashed", color = "#EF4444", linewidth = .6, alpha = .7) +
      scale_x_continuous(breaks = 2:10) + labs(x = "K", y = "WSS") +
      theme_minimal(base_size = 10) +
      theme(plot.background  = element_rect(fill = "#0D1120", color = NA),
            panel.background = element_rect(fill = "#0D1120", color = NA),
            panel.grid.major = element_line(color = "#161D2E", linewidth = .4),
            panel.grid.minor = element_blank(),
            axis.text  = element_text(color = "#475569"),
            axis.title = element_text(color = "#475569"),
            plot.margin = margin(14, 14, 14, 14))
  })
  
  output$p_sil <- renderPlot({
    d <- fd_sp(); if (nrow(d) < input$k_n + 1) return(NULL)
    fs  <- scale(d[, c("latitude", "longitude", "rating", "jumlah_ulasan", "skor_potensi")])
    km  <- kmeans(fs, input$k_n, nstart = 25)
    sil <- silhouette(km$cluster, dist(fs))
    df_s <- data.frame(cl = factor(sil[, 1]), sw = sil[, 3]) %>%
      arrange(cl, desc(sw)) %>% mutate(i = row_number())
    ggplot(df_s, aes(i, sw, fill = cl)) +
      geom_col(width = 1, show.legend = FALSE) +
      scale_fill_manual(values = PAL[1:input$k_n]) +
      geom_hline(yintercept = mean(sil[, 3]), linetype = "dashed", color = "#F59E0B", linewidth = .7) +
      annotate("text", x = nrow(df_s) * .5, y = mean(sil[, 3]) + .04,
               label = paste0("Avg=", round(mean(sil[, 3]), 3)), color = "#F59E0B", size = 3) +
      labs(x = "", y = "Silhouette Width") +
      theme_minimal(base_size = 10) +
      theme(plot.background  = element_rect(fill = "#0D1120", color = NA),
            panel.background = element_rect(fill = "#0D1120", color = NA),
            panel.grid.major = element_line(color = "#161D2E", linewidth = .4),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text  = element_text(color = "#475569"),
            axis.title = element_text(color = "#475569"),
            plot.margin = margin(14, 14, 14, 14))
  })
  
  output$p_gap <- renderPlot({
    d <- fd_sp(); if (nrow(d) < 10) return(NULL)
    fs <- scale(d[, c("latitude", "longitude", "rating", "jumlah_ulasan", "skor_potensi")])
    set.seed(42)
    gs <- tryCatch(clusGap(fs, FUNcluster = kmeans, K.max = 8, B = 20,
                           FUN.args = list(nstart = 5, iter.max = 20)), error = function(e) NULL)
    if (is.null(gs)) return(NULL)
    df_g  <- data.frame(K = 1:8, Gap = gs$Tab[, 3], SE = gs$Tab[, 4])
    k_opt <- maxSE(gs$Tab[, 3], gs$Tab[, 4])
    ggplot(df_g, aes(K, Gap)) +
      geom_ribbon(aes(ymin = Gap - SE, ymax = Gap + SE), fill = "#8B5CF6", alpha = .1) +
      geom_line(color = "#8B5CF6", linewidth = .7) +
      geom_point(aes(color = K == k_opt), size = 5, show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = "#8B5CF6", "TRUE" = "#EF4444")) +
      geom_vline(xintercept = k_opt, linetype = "dashed", color = "#EF4444", linewidth = .6, alpha = .7) +
      annotate("text", x = k_opt + .2, y = min(df_g$Gap) + (max(df_g$Gap) - min(df_g$Gap)) * .9,
               label = paste0("K opt=", k_opt), color = "#EF4444", size = 3) +
      scale_x_continuous(breaks = 1:8) + labs(x = "K", y = "Gap Statistic") +
      theme_minimal(base_size = 10) +
      theme(plot.background  = element_rect(fill = "#0D1120", color = NA),
            panel.background = element_rect(fill = "#0D1120", color = NA),
            panel.grid.major = element_line(color = "#161D2E", linewidth = .4),
            panel.grid.minor = element_blank(),
            axis.text  = element_text(color = "#475569"),
            axis.title = element_text(color = "#475569"),
            plot.margin = margin(14, 14, 14, 14))
  })
  
  output$p_radar <- renderPlotly({
    res <- ar(); if (is.null(res)) return(NULL)
    d    <- res$data
    vars <- c("rating", "jumlah_ulasan", "skor_potensi", "latitude", "longitude")
    prof <- d %>% group_by(cluster_kmeans) %>%
      summarise(across(all_of(vars), ~ rescale(mean(.), to = c(0, 1)), .names = "{.col}"), .groups = "drop")
    cats <- c("Rating", "Ulasan", "Potensi", "Lat", "Lng", "Rating")
    p <- plot_ly(type = "scatterpolar", fill = "toself")
    for (i in 1:nrow(prof)) {
      v     <- as.numeric(prof[i, vars])
      rgb_v <- col2rgb(PAL[i])
      p <- add_trace(p, r = c(v, v[1]), theta = cats,
                     name = paste0("C", prof$cluster_kmeans[i]),
                     line = list(color = PAL[i], width = 2),
                     fillcolor = paste0("rgba(", rgb_v[1], ",", rgb_v[2], ",", rgb_v[3], ",.12)"))
    }
    p %>% layout(
      polar = list(
        radialaxis  = list(visible = TRUE, range = c(0, 1), gridcolor = "#1A2235",
                           linecolor = "#1A2235", tickfont = list(color = "#334155", size = 8)),
        angularaxis = list(color = "#475569", gridcolor = "#1A2235", linecolor = "#1A2235",
                           tickfont = list(size = 10))
      ),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font   = list(family = "Inter", color = "#64748B", size = 10),
      legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#475569", size = 10)),
      margin = list(t = 20, b = 20, l = 30, r = 30))
  })
  
  output$p_box <- renderPlotly({
    res <- ar(); if (is.null(res)) return(NULL)
    d   <- res$data; v <- input$box_v
    plot_ly(d, x = ~paste0("C", cluster_kmeans), y = ~get(v),
            type = "box", color = ~cluster_kmeans, colors = PAL, showlegend = FALSE,
            marker = list(size = 3, opacity = .5), line = list(width = 1.5)) %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Cluster",   tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = v,           tickfont = list(color = "#475569"))))
  })
  
  output$cl_stats <- renderTable({
    res <- ar(); if (is.null(res)) return(NULL)
    res$data %>% group_by(Cluster = paste0("C", cluster_kmeans)) %>%
      summarise(N      = n(),
                Rating  = round(mean(rating), 2),
                Ulasan  = round(mean(jumlah_ulasan)),
                Potensi = round(mean(skor_potensi), 1), .groups = "drop")
  }, striped = FALSE, bordered = FALSE, spacing = "xs", width = "100%", align = "l")
  
  output$km_cards <- renderUI({
    res <- ar()
    if (is.null(res)) {
      return(tags$div(class = "insight-box",
                      tags$div(class = "insight-title", "Menunggu Analisis"),
                      tags$div(class = "insight-text", "Klik 'Analisis Sekarang' untuk memulai clustering.")))
    }
    prof <- res$data %>% group_by(cluster_kmeans) %>%
      summarise(r = round(mean(rating), 2), u = round(mean(jumlah_ulasan)),
                p = round(mean(skor_potensi), 1), n = n(), .groups = "drop") %>%
      arrange(desc(p))
    
    make_card <- function(row) {
      if      (row$p >= 75 && row$r >= 4.2) { cls <- "cl-green";  lbl <- "Sangat Strategis"; bc <- "#10B981"; rec <- "Zona premium — sangat direkomendasikan untuk investasi usaha. Rating tinggi & skor potensi maksimal." }
      else if (row$p >= 65)                  { cls <- "cl-blue";   lbl <- "Strategis";         bc <- "#3B82F6"; rec <- "Zona potensial — cocok untuk usaha baru dengan diferensiasi yang tepat." }
      else if (row$p >= 55)                  { cls <- "cl-yellow"; lbl <- "Cukup Strategis";   bc <- "#F59E0B"; rec <- "Zona berkembang — butuh strategi pemasaran intensif untuk tumbuh." }
      else if (row$p >= 45)                  { cls <- "cl-orange"; lbl <- "Potensi Sedang";    bc <- "#F97316"; rec <- "Zona selektif — pertimbangkan niche market atau spesialisasi." }
      else                                   { cls <- "cl-red";    lbl <- "Kurang Strategis";  bc <- "#EF4444"; rec <- "Zona risiko tinggi — hindari untuk usaha baru. Pertimbangkan relokasi." }
      rgb_v <- col2rgb(bc)
      tags$div(class = paste("cl-card", cls),
               tags$div(class = "cl-title",
                        paste0("Cluster ", row$cluster_kmeans),
                        tags$span(class = "cl-badge",
                                  style = paste0("background:rgba(", rgb_v[1], ",", rgb_v[2], ",", rgb_v[3], ",.12);color:", bc, ";"), lbl)),
               tags$div(class = "cl-meta",
                        paste0(row$n, " lokasi  |  Rating: ", row$r, "★  |  Avg Ulasan: ", row$u, "  |  Skor Potensi: ", row$p)),
               tags$div(class = "cl-rec", rec))
    }
    do.call(tagList, lapply(1:nrow(prof), function(i) make_card(prof[i, ])))
  })
  
  # ── DBSCAN ────────────────────────────────────────────────
  output$db_scatter <- renderPlotly({
    res <- ar(); if (is.null(res)) return(NULL)
    d   <- res$data
    d$lbl <- ifelse(d$cluster_dbscan == "0", "⚫ Noise", paste0("Cluster ", d$cluster_dbscan))
    plot_ly(d, x = ~longitude, y = ~latitude, color = ~lbl, colors = c("#2D3F55", PAL),
            type = "scatter", mode = "markers",
            marker = list(size = 6.5, opacity = .78, line = list(color = "rgba(0,0,0,.1)", width = .5)),
            text = ~paste0("<b>", nama, "</b><br>", kecamatan, "<br>Rating:", rating, " | Potensi:", skor_potensi),
            hoverinfo = "text") %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Longitude", tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Latitude",  tickfont = list(color = "#475569"))))
  })
  
  output$db_knn <- renderPlot({
    d  <- fd_sp()
    cs <- scale(d[, c("latitude", "longitude")])
    kd <- sort(kNNdist(cs, k = input$mpts))
    df_k <- data.frame(I = seq_along(kd), D = kd)
    ggplot(df_k, aes(I, D)) +
      geom_area(fill = "#8B5CF6", alpha = .07) +
      geom_line(color = "#8B5CF6", linewidth = .7) +
      geom_hline(yintercept = input$eps, linetype = "dashed", color = "#EF4444", linewidth = .7, alpha = .85) +
      annotate("text", x = nrow(df_k) * .05, y = input$eps + diff(range(kd)) * .05,
               label = paste0("eps=", input$eps), color = "#EF4444", size = 3, hjust = 0) +
      labs(x = "Data Points (sorted)", y = paste0(input$mpts, "-NN Distance")) +
      theme_minimal(base_size = 10) +
      theme(plot.background  = element_rect(fill = "#0D1120", color = NA),
            panel.background = element_rect(fill = "#0D1120", color = NA),
            panel.grid.major = element_line(color = "#161D2E", linewidth = .4),
            panel.grid.minor = element_blank(),
            axis.text  = element_text(color = "#475569"),
            axis.title = element_text(color = "#475569"),
            plot.margin = margin(14, 14, 14, 14))
  })
  
  output$db_reach <- renderPlot({
    d  <- fd_sp()
    cs <- scale(d[, c("latitude", "longitude")])
    db    <- dbscan::dbscan(cs, eps = input$eps, minPts = input$mpts)
    reach <- dbscan::optics(cs, minPts = input$mpts)
    df_r  <- data.frame(I = seq_along(reach$reachdist), RD = reach$reachdist,
                        CL = as.factor(db$cluster[reach$order]))
    mx <- max(df_r$RD[!is.infinite(df_r$RD)], na.rm = TRUE)
    df_r$RD[is.infinite(df_r$RD)] <- mx * 1.1
    ggplot(df_r, aes(I, RD, fill = CL)) +
      geom_col(width = 1, show.legend = FALSE) +
      scale_fill_manual(values = c("#334155", PAL)) +
      geom_hline(yintercept = input$eps, linetype = "dashed", color = "#EF4444", linewidth = .7, alpha = .8) +
      labs(x = "Order", y = "Reachability Distance") +
      theme_minimal(base_size = 10) +
      theme(plot.background  = element_rect(fill = "#0D1120", color = NA),
            panel.background = element_rect(fill = "#0D1120", color = NA),
            panel.grid.major = element_line(color = "#161D2E", linewidth = .4),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text  = element_text(color = "#475569"),
            axis.title = element_text(color = "#475569"),
            plot.margin = margin(14, 14, 14, 14))
  })
  
  output$db_bar <- renderPlotly({
    res <- ar(); if (is.null(res)) return(NULL)
    d   <- res$data %>% group_by(cluster_dbscan) %>%
      summarise(N = n(), R = round(mean(rating), 2), .groups = "drop") %>%
      mutate(L  = ifelse(cluster_dbscan == "0", "Noise", paste0("C", cluster_dbscan)),
             no = cluster_dbscan == "0")
    plot_ly(d, x = ~L, y = ~N, type = "bar",
            marker = list(color = ifelse(d$no, "#1E293B", "#8B5CF6"), opacity = .85),
            text = ~N, textposition = "outside", textfont = list(color = "#64748B", size = 11)) %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "",        tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Jumlah", tickfont = list(color = "#475569"))))
  })
  
  output$db_heat2 <- renderPlotly({
    res <- ar(); if (is.null(res)) return(NULL)
    d   <- res$data %>% filter(cluster_dbscan != "0") %>%
      group_by(cluster_dbscan, kategori_grup) %>% summarise(N = n(), .groups = "drop")
    plot_ly(d, x = ~kategori_grup, y = ~paste0("C", cluster_dbscan), z = ~N, type = "heatmap",
            colorscale = list(c(0, "#090B0F"), c(.5, "#1D3461"), c(1, "#3B82F6")),
            text = ~N, texttemplate = "%{text}", textfont = list(color = "#F1F5F9", size = 10)) %>%
      layout(mk_layout(margin = list(t = 5, b = 85, l = 60, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = "", tickangle = -45, tickfont = list(color = "#475569", size = 9)),
                       yaxis = list(gridcolor = "#1A2235", title = "Cluster", tickfont = list(color = "#475569"))))
  })
  
  output$db_noise_ui <- renderUI({
    res <- ar(); if (is.null(res)) return(NULL)
    d       <- res$data
    n_noise <- sum(d$cluster_dbscan == "0")
    n_cl    <- length(unique(d$cluster_dbscan[d$cluster_dbscan != "0"]))
    pct     <- round(n_noise / nrow(d) * 100, 1)
    tags$div(
      tags$div(class = "kpi-card red",
               tags$div(class = "kpi-label", "Noise Points"),
               tags$div(class = "kpi-val", n_noise),
               tags$div(class = "kpi-sub", paste0(pct, "% dari total data"))),
      tags$div(class = "kpi-card blue",
               tags$div(class = "kpi-label", "Cluster Terdeteksi"),
               tags$div(class = "kpi-val", n_cl),
               tags$div(class = "kpi-sub", paste0("eps=", input$eps, " | minPts=", input$mpts))),
      tags$div(class = "kpi-card green",
               tags$div(class = "kpi-label", "Core Points"),
               tags$div(class = "kpi-val", nrow(d) - n_noise),
               tags$div(class = "kpi-sub", "Termasuk dalam cluster"))
    )
  })
  
  # ── AI INSIGHTS ───────────────────────────────────────────
  ai_data <- eventReactive(input$gen_insight, {
    d <- fd()
    list(
      top_kec     = names(sort(table(d$kecamatan),     decreasing = TRUE))[1],
      top_kat     = names(sort(table(d$kategori_grup), decreasing = TRUE))[1],
      top_pop     = names(sort(table(d$popularitas),   decreasing = TRUE))[1],
      avg_rating  = round(mean(d$rating,        na.rm = TRUE), 2),
      avg_pot     = round(mean(d$skor_potensi,  na.rm = TRUE), 1),
      n_sangat    = sum(grepl("Sangat", d$label_kelayakan), na.rm = TRUE),
      best_usaha  = d %>% arrange(desc(skor_potensi)) %>% head(1),
      pct_5star   = round(mean(d$rating == 5, na.rm = TRUE) * 100, 1),
      n_total     = nrow(d)
    )
  }, ignoreNULL = FALSE)
  
  output$ai_insights <- renderUI({
    ins <- ai_data()
    if (is.null(ins)) {
      return(tags$div(class = "insight-box",
                      tags$div(class = "insight-title", "Ready"),
                      tags$div(class = "insight-text", "Klik Generate untuk mendapatkan insights otomatis.")))
    }
    tags$div(
      tags$div(class = "insight-box",
               tags$div(class = "insight-title", "ZONA TERPANAS"),
               tags$div(class = "insight-text",
                        tags$span(style = "font-size:16px;font-weight:700;color:#F1F5F9;", ins$top_kec), tags$br(),
                        "Kecamatan dengan konsentrasi usaha tertinggi dalam filter aktif.")),
      tags$div(class = "insight-box",
               tags$div(class = "insight-title", "KATEGORI DOMINAN"),
               tags$div(class = "insight-text",
                        tags$span(style = "font-size:16px;font-weight:700;color:#F1F5F9;", ins$top_kat), tags$br(),
                        "Segmen usaha paling kompetitif. Pertimbangkan diferensiasi atau niche market.")),
      tags$div(class = "insight-box",
               tags$div(class = "insight-title", "KUALITAS PASAR"),
               tags$div(class = "insight-text",
                        tags$span(style = "font-size:15px;font-weight:700;color:#F59E0B;",
                                  paste0(ins$avg_rating, "★ avg | ", ins$pct_5star, "% perfect score")), tags$br(),
                        paste0("Popularitas dominan: ", ins$top_pop, ". Rating rata-rata tinggi = pasar mature."))),
      tags$div(class = "insight-box",
               tags$div(class = "insight-title", "BEST PERFORMER"),
               tags$div(class = "insight-text",
                        tags$span(style = "font-size:15px;font-weight:700;color:#10B981;", ins$best_usaha$nama[1]), tags$br(),
                        paste0("Skor Potensi: ", ins$best_usaha$skor_potensi[1],
                               " | ", ins$best_usaha$kecamatan[1],
                               " | ", ins$best_usaha$kategori_grup[1]))),
      tags$div(class = "insight-box",
               tags$div(class = "insight-title", "REKOMENDASI STRATEGIS"),
               tags$div(class = "insight-text",
                        paste0(ins$n_sangat, " dari ", ins$n_total, " lokasi (",
                               round(ins$n_sangat / ins$n_total * 100, 1), "%) masuk Sangat Layak. ",
                               "Fokus ekspansi ke ", ins$top_kec, " dengan kategori selain ", ins$top_kat,
                               " untuk menghindari saturasi.")))
    )
  })
  
  output$rec_ui <- eventReactive(input$get_rec, {
    d <- fd() %>%
      filter(kecamatan == input$rec_kec, kategori_grup == input$rec_kat) %>%
      arrange(desc(skor_potensi))
    if (nrow(d) == 0) {
      return(tags$div(class = "insight-box",
                      tags$div(class = "insight-text", "Tidak ada data untuk kombinasi ini.")))
    }
    top3 <- head(d, 3)
    tags$div(
      tags$div(class = "kpi-card blue",
               tags$div(class = "kpi-label", "Temuan"),
               tags$div(class = "kpi-val", nrow(d)),
               tags$div(class = "kpi-sub", paste0("usaha di ", input$rec_kec, " | ", input$rec_kat))),
      lapply(1:nrow(top3), function(i) {
        tags$div(class = "insight-box",
                 tags$div(class = "insight-title", paste0("TOP ", i)),
                 tags$div(class = "insight-text",
                          tags$b(style = "color:#F1F5F9;", top3$nama[i]), tags$br(),
                          paste0("Rating: ", top3$rating[i], "★ | Potensi: ", top3$skor_potensi[i],
                                 " | Ulasan: ", top3$jumlah_ulasan[i])))
      })
    )
  })
  
  output$p_outlier <- renderPlotly({
    d  <- fd_sp()
    fs <- scale(d[, c("rating", "jumlah_ulasan", "skor_potensi")])
    maha <- mahalanobis(fs, colMeans(fs), cov(fs))
    d$maha    <- maha
    d$outlier <- maha > quantile(maha, .95)
    plot_ly(d, x = ~rating, y = ~skor_potensi, color = ~outlier,
            colors = c("#3B82F6", "#EF4444"), type = "scatter", mode = "markers",
            marker = list(size = ~rescale(maha, to = c(4, 12)), opacity = .75),
            text = ~paste0("<b>", nama, "</b><br>Mahalanobis: ", round(maha, 2)),
            hoverinfo = "text") %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Rating",      tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Skor Potensi",tickfont = list(color = "#475569")),
        legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#475569", size = 10))))
  })
  
  output$p_gap_mkt <- renderPlotly({
    d <- fd() %>% group_by(kecamatan, kategori_grup) %>%
      summarise(n = n(), avg_pot = round(mean(skor_potensi), 1), .groups = "drop")
    plot_ly(d, x = ~kecamatan, y = ~kategori_grup, z = ~n, type = "heatmap",
            colorscale = list(c(0, "#090B0F"), c(.5, "#1E3A5F"), c(1, "#06B6D4")),
            text = ~paste0("N=", n, "<br>Avg Pot=", avg_pot), hoverinfo = "text+x+y") %>%
      layout(mk_layout(margin = list(t = 5, b = 50, l = 130, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = "", tickfont = list(color = "#475569")),
                       yaxis = list(gridcolor = "#1A2235", title = "", tickfont = list(color = "#475569", size = 9.5))))
  })
  
  # ── KOMPARASI ─────────────────────────────────────────────
  output$p_conf <- renderPlotly({
    res <- ar(); if (is.null(res)) return(NULL)
    d   <- res$data
    if (!"cluster_kmeans" %in% names(d) || !"cluster_dbscan" %in% names(d)) return(NULL)
    ct <- as.data.frame(table(KM = d$cluster_kmeans, DB = d$cluster_dbscan))
    plot_ly(ct, x = ~DB, y = ~KM, z = ~Freq, type = "heatmap",
            colorscale = list(c(0, "#090B0F"), c(.5, "#1D4ED8"), c(1, "#06B6D4")),
            text = ~Freq, texttemplate = "%{text}", textfont = list(color = "#F1F5F9", size = 12)) %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", title = "DBSCAN",  tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", title = "K-Means", tickfont = list(color = "#475569"))))
  })
  
  output$p_metrics <- renderUI({
    res <- ar(); if (is.null(res)) return(NULL)
    d   <- res$data; fs <- res$scaled; km <- res$km
    sil_km  <- round(mean(silhouette(km$cluster, dist(fs))[, 3]), 4)
    n_db    <- length(unique(d$cluster_dbscan[d$cluster_dbscan != "0"]))
    n_noise <- sum(d$cluster_dbscan == "0")
    tags$div(
      tags$div(class = "kpi-card blue",
               tags$div(class = "kpi-label", "K-Means Silhouette"),
               tags$div(class = "kpi-val", sil_km),
               tags$div(class = "kpi-sub",
                        if (sil_km >= .5) "Struktur kuat" else if (sil_km >= .25) "Struktur sedang" else "Struktur lemah")),
      tags$div(class = "kpi-card purple",
               tags$div(class = "kpi-label", "DBSCAN Clusters"),
               tags$div(class = "kpi-val", n_db),
               tags$div(class = "kpi-sub", paste0(n_noise, " noise | eps=", input$eps, " | minPts=", input$mpts))),
      tags$div(class = "kpi-card green",
               tags$div(class = "kpi-label", "Data Tercluster"),
               tags$div(class = "kpi-val", paste0(round((nrow(d) - n_noise) / nrow(d) * 100, 1), "%")),
               tags$div(class = "kpi-sub", paste0(nrow(d) - n_noise, " dari ", nrow(d), " total"))),
      tags$div(class = "kpi-card yellow",
               tags$div(class = "kpi-label", "K-Means Inertia"),
               tags$div(class = "kpi-val", round(km$tot.withinss, 0)),
               tags$div(class = "kpi-sub", "Within-cluster sum of squares"))
    )
  })
  
  output$p_parallel <- renderPlotly({
    res <- ar(); if (is.null(res)) return(NULL)
    d      <- res$data
    vars_p <- c("rating", "jumlah_ulasan", "skor_potensi", "latitude", "longitude")
    d_norm <- d
    for (v in vars_p) d_norm[[v]] <- rescale(d[[v]], to = c(0, 1))
    k <- input$k_n
    cs_list <- lapply(seq(0, 1, length.out = k), function(i) list(i, PAL[max(1, ceiling(i * k))]))
    plot_ly(type = "parcoords",
            line = list(color = as.numeric(d$cluster_kmeans), colorscale = cs_list, showscale = FALSE),
            dimensions = list(
              list(label = "Rating",    values = d_norm$rating,        range = c(0, 1)),
              list(label = "Ulasan",    values = d_norm$jumlah_ulasan, range = c(0, 1)),
              list(label = "Potensi",   values = d_norm$skor_potensi,  range = c(0, 1)),
              list(label = "Latitude",  values = d_norm$latitude,      range = c(0, 1)),
              list(label = "Longitude", values = d_norm$longitude,     range = c(0, 1))
            )) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)",
             font   = list(family = "Inter", color = "#64748B", size = 11),
             margin = list(t = 30, b = 30, l = 80, r = 80))
  })
  
  # ── EKSPLORASI ────────────────────────────────────────────
  output$p_ex <- renderPlotly({
    d <- fd()
    plot_ly(d, x = ~get(input$ex_x), y = ~get(input$ex_y),
            color = ~get(input$ex_c), size = ~get(input$ex_s),
            colors = PAL, type = "scatter", mode = "markers",
            marker = list(opacity = .72, sizemode = "diameter", sizeref = .6,
                          line = list(color = "rgba(0,0,0,.1)", width = .5)),
            text  = ~paste0("<b>", nama, "</b><br>", kecamatan, "<br>", kategori_grup),
            hoverinfo = "text") %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = input$ex_x, tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = input$ex_y, tickfont = list(color = "#475569"))))
  })
  
  output$p_ex_hist <- renderPlotly({
    plot_ly(fd(), x = ~get(input$ex_hv), type = "histogram", nbinsx = 12,
            marker = list(color = "#3B82F6", opacity = .8, line = list(color = "#090B0F", width = .5))) %>%
      layout(mk_layout(margin = list(t = 5, b = 25, l = 40, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = input$ex_hv, tickfont = list(color = "#475569")),
                       yaxis = list(gridcolor = "#1A2235", title = "",           tickfont = list(color = "#475569"))))
  })
  
  output$p_ex_box <- renderPlotly({
    d <- fd(); v <- input$ex_hv
    plot_ly(d, x = ~kategori_grup, y = ~get(v), type = "box",
            color = ~kategori_grup, colors = PAL, showlegend = FALSE,
            marker = list(size = 3, opacity = .5), line = list(width = 1.5)) %>%
      layout(mk_layout(margin = list(t = 5, b = 80, l = 40, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = "", tickangle = -40, tickfont = list(color = "#475569", size = 8)),
                       yaxis = list(gridcolor = "#1A2235", title = v, tickfont = list(color = "#475569"))))
  })
  
  output$p_corr <- renderPlotly({
    d  <- fd()[, c("rating", "jumlah_ulasan", "skor_potensi", "latitude", "longitude")]
    d  <- d[complete.cases(d), ]
    cm <- cor(d, use = "complete.obs")
    colnames(cm) <- rownames(cm) <- c("Rating", "Ulasan", "Potensi", "Lat", "Lng")
    plot_ly(z = cm, x = colnames(cm), y = colnames(cm), type = "heatmap",
            colorscale = list(c(0, "#EF4444"), c(.5, "#090B0F"), c(1, "#3B82F6")),
            zmin = -1, zmax = 1,
            text = round(cm, 2), texttemplate = "%{text}", textfont = list(color = "#F1F5F9", size = 11)) %>%
      layout(mk_layout(margin = list(t = 5, b = 60, l = 75, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = "", tickangle = -30, tickfont = list(color = "#475569")),
                       yaxis = list(gridcolor = "#1A2235", title = "",                  tickfont = list(color = "#475569"))))
  })
  
  output$p_bubble <- renderPlotly({
    d <- fd() %>% group_by(kecamatan) %>%
      summarise(ar = mean(rating, na.rm = TRUE), ap = mean(jumlah_ulasan, na.rm = TRUE),
                apt = mean(skor_potensi, na.rm = TRUE), n = n(), .groups = "drop")
    plot_ly(d, x = ~ar, y = ~ap, size = ~n, color = ~apt,
            text = ~paste0("<b>", kecamatan, "</b><br>N=", n),
            hoverinfo = "text+x+y", type = "scatter", mode = "markers+text",
            textposition = "top center", textfont = list(color = "#94A3B8", size = 9),
            marker = list(sizemode = "diameter", sizeref = .25, opacity = .8,
                          colorscale = list(c(0, "#1E293B"), c(.5, "#1D4ED8"), c(1, "#06B6D4")),
                          showscale = TRUE,
                          colorbar = list(bgcolor = "#0D1120", bordercolor = "#1A2235", thickness = 10,
                                          tickfont = list(color = "#475569", size = 9)))) %>%
      layout(mk_layout(
        xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Avg Rating", tickfont = list(color = "#475569")),
        yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Avg Ulasan", tickfont = list(color = "#475569"))))
  })
  
  output$p_sun <- renderPlotly({
    d <- fd() %>% count(kategori_grup, kecamatan)
    plot_ly(d, type = "sunburst",
            labels  = ~paste(kecamatan, "-", kategori_grup),
            parents = ~kategori_grup,
            values  = ~n,
            branchvalues = "total",
            marker = list(colorscale = "Blues", line = list(color = "#090B0F", width = 1)),
            textfont = list(color = "#F1F5F9", size = 9)) %>%
      layout(paper_bgcolor = "rgba(0,0,0,0)", margin = list(t = 0, b = 0, l = 0, r = 0))
  })
  
  # ── TEMPORAL ──────────────────────────────────────────────
  output$p_temp1 <- renderPlotly({
    d <- fd()
    plot_ly(d, x = ~kecamatan, y = ~rating, type = "violin",
            color = ~kecamatan, colors = PAL,
            box = list(visible = TRUE), meanline = list(visible = TRUE),
            showlegend = FALSE, line = list(width = 1.5),
            marker = list(size = 2, opacity = .4)) %>%
      layout(mk_layout(margin = list(t = 5, b = 60, l = 50, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = "",       tickangle = -25, tickfont = list(color = "#475569", size = 10)),
                       yaxis = list(gridcolor = "#1A2235", title = "Rating",                  tickfont = list(color = "#475569"))))
  })
  
  output$p_ecdf <- renderPlotly({
    d <- fd()
    p <- plot_ly()
    for (i in seq_along(GRUPS)) {
      g   <- GRUPS[i]
      sub <- d[d$kategori_grup == g, ]
      if (nrow(sub) < 2) next
      ecdf_fn <- ecdf(sub$skor_potensi)
      xs      <- sort(sub$skor_potensi)
      p <- add_trace(p, x = xs, y = ecdf_fn(xs), type = "scatter", mode = "lines",
                     name = g, line = list(color = PAL[i], width = 1.5))
    }
    p %>% layout(mk_layout(
      xaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "Skor Potensi", tickfont = list(color = "#475569")),
      yaxis = list(gridcolor = "#1A2235", zerolinecolor = "#1A2235", title = "CDF",         range = c(0, 1), tickfont = list(color = "#475569"))))
  })
  
  output$p_rank <- renderPlotly({
    d <- fd() %>% group_by(kecamatan) %>%
      summarise(rating   = mean(rating,        na.rm = TRUE),
                ulasan   = mean(jumlah_ulasan, na.rm = TRUE),
                potensi  = mean(skor_potensi,  na.rm = TRUE),
                n        = n(), .groups = "drop") %>%
      mutate(across(c(rating, ulasan, potensi), ~ rescale(.))) %>%
      pivot_longer(c(rating, ulasan, potensi), names_to = "metric", values_to = "val")
    plot_ly(d, x = ~val, y = ~kecamatan, color = ~metric,
            colors = c("#3B82F6", "#10B981", "#F59E0B"),
            type = "bar", orientation = "h", barmode = "group") %>%
      layout(mk_layout(margin = list(t = 5, b = 30, l = 80, r = 10),
                       xaxis = list(gridcolor = "#1A2235", title = "Normalized Score", tickfont = list(color = "#475569")),
                       yaxis = list(gridcolor = "#1A2235", title = "",                 tickfont = list(color = "#475569", size = 10))))
  })
  
  # ── DATA LAB ──────────────────────────────────────────────
  output$data_tbl <- renderDT({
    d         <- rd()
    base_cols <- c("nama", "kecamatan", "kategori_grup", "rating", "jumlah_ulasan",
                   "skor_potensi", "label_kelayakan", "popularitas", "telepon")
    coord_cols   <- c("latitude", "longitude", "alamat")
    cluster_cols <- c()
    if ("cluster_kmeans" %in% names(d)) cluster_cols <- c(cluster_cols, "cluster_kmeans")
    if ("cluster_dbscan" %in% names(d)) cluster_cols <- c(cluster_cols, "cluster_dbscan")
    
    cols <- switch(input$tbl_cols,
                   "all"     = c(base_cols, coord_cols, cluster_cols),
                   "basic"   = base_cols,
                   "coord"   = c(base_cols[1:2], coord_cols),
                   "cluster" = c(base_cols[1:2], cluster_cols))
    cols <- cols[cols %in% names(d)]
    
    cn_map <- c(nama = "Nama", kecamatan = "Kecamatan", kategori_grup = "Kategori",
                rating = "Rating", jumlah_ulasan = "Ulasan", skor_potensi = "Potensi",
                label_kelayakan = "Kelayakan", popularitas = "Popularitas", telepon = "Telepon",
                latitude = "Lat", longitude = "Lng", alamat = "Alamat",
                cluster_kmeans = "K-Means", cluster_dbscan = "DBSCAN")
    cn <- sapply(cols, function(c) ifelse(c %in% names(cn_map), cn_map[c], c))
    
    dt <- datatable(d[, cols], colnames = cn, rownames = FALSE, filter = "top",
                    options = list(pageLength = 15, scrollX = TRUE, dom = "Blfrtip",
                                   language = list(search = "Cari:", lengthMenu = "Tampilkan _MENU_ baris",
                                                   zeroRecords = "Tidak ada data")),
                    class = "display compact hover")
    
    if ("rating" %in% cols)
      dt <- dt %>% formatStyle("rating",
                               background = styleColorBar(range(d$rating, na.rm = TRUE), "rgba(245,158,11,.2)"),
                               backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    if ("skor_potensi" %in% cols)
      dt <- dt %>% formatStyle("skor_potensi",
                               background = styleColorBar(range(d$skor_potensi, na.rm = TRUE), "rgba(16,185,129,.2)"),
                               backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "center")
    if ("label_kelayakan" %in% cols)
      dt <- dt %>% formatStyle("label_kelayakan",
                               color = styleEqual(c("Sangat Layak", "Layak", "Cukup Layak", "Kurang Layak"),
                                                  c("#10B981",      "#3B82F6", "#F59E0B",     "#EF4444")),
                               fontWeight = "600")
    dt
  })
  
  output$dl1 <- downloadHandler(
    filename = function() paste0("makassar_full_",     Sys.Date(), ".csv"),
    content  = function(file) write.csv(data_usaha, file, row.names = FALSE))
  output$dl2 <- downloadHandler(
    filename = function() paste0("makassar_filtered_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(fd(),         file, row.names = FALSE))
}

shinyApp(ui = ui, server = server)