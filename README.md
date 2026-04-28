# 🗺️ Sistem Rekomendasi Lokasi Usaha — Sulawesi Selatan

<div align="center">

![R](https://img.shields.io/badge/R-4.3%2B-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-Framework-blue?style=for-the-badge&logo=rstudio&logoColor=white)
![Machine Learning](https://img.shields.io/badge/Machine%20Learning-K--Means%20%7C%20DBSCAN-10B981?style=for-the-badge)
![License](https://img.shields.io/badge/License-Academic-F59E0B?style=for-the-badge)

**Aplikasi berbasis R Shiny untuk mendeteksi dan merekomendasikan lokasi usaha strategis di Sulawesi Selatan menggunakan algoritma Unsupervised Machine Learning.**

</div>

---

## 📌 Deskripsi Proyek

Proyek ini merupakan tugas akhir mata kuliah **Machine Learning** pada Program Studi **Bisnis Digital, Universitas Negeri Makassar (UNM)**. Sistem ini membantu pelaku usaha dan UMKM dalam menentukan lokasi bisnis yang paling strategis di Sulawesi Selatan dengan memanfaatkan dua pendekatan *clustering*:

- **K-Means Clustering** — mengelompokkan lokasi berdasarkan kesamaan fitur numerik
- **DBSCAN** *(Density-Based Spatial Clustering of Applications with Noise)* — mendeteksi klaster berdasarkan kepadatan spasial dan mengidentifikasi *outlier/noise*

Data yang digunakan adalah data simulasi (*dummy*) yang merepresentasikan titik usaha dari **Google Maps** di berbagai kota/kabupaten di Sulawesi Selatan.

---

## ✨ Fitur Utama

| Fitur | Deskripsi |
|---|---|
| 🏠 **Beranda (Dashboard)** | Ringkasan statistik: total lokasi, distribusi kategori, rating, dan sebaran per kota |
| 🗺️ **Peta Interaktif** | Visualisasi sebaran lokasi dengan Leaflet (mode: semua titik, K-Means, DBSCAN) |
| 📊 **Analisis K-Means** | Visualisasi PCA, Elbow Method, dan profil tiap cluster |
| 🔍 **Analisis DBSCAN** | Scatter plot spasial, k-NN Distance Plot, dan distribusi cluster |
| 📋 **Data & Tabel** | Tabel data lengkap dengan pencarian dan export ke CSV |
| ℹ️ **Tentang** | Informasi sistem, metode, dan stack teknologi |
| ⚙️ **Filter Dinamis** | Filter berdasarkan kota, kategori usaha, dan rating minimum |
| 🎛️ **Parameter Fleksibel** | Slider untuk jumlah K, nilai epsilon ε, dan minPts DBSCAN |

---

## 🗃️ Struktur Data

Data simulasi mencakup **325 titik usaha** yang tersebar di 15 kota/kabupaten:

```
Makassar · Gowa · Maros · Pangkep · Bone · Soppeng · Wajo
Bulukumba · Sinjai · Selayar · Toraja · Palopo · Luwu
```

### Variabel Dataset

| Variabel | Tipe | Deskripsi |
|---|---|---|
| `nama_usaha` | Character | Nama titik usaha |
| `kota` | Factor | Kota / kabupaten |
| `kategori` | Factor | Jenis usaha (Kuliner, Retail, Jasa, dll.) |
| `lat` | Numeric | Koordinat lintang |
| `lng` | Numeric | Koordinat bujur |
| `rating` | Numeric | Rating usaha (1.0 – 5.0) |
| `jumlah_review` | Integer | Jumlah ulasan pelanggan |
| `harga_sewa` | Ordinal | Estimasi harga sewa lokasi |
| `kepadatan` | Numeric | Skor kepadatan area |
| `skor_potensi` | Numeric | Skor potensi bisnis (40 – 100) |

### Kategori Usaha

`Kuliner` · `Retail` · `Jasa` · `Fashion` · `Elektronik` · `Kesehatan` · `Pendidikan` · `Otomotif`

---

## 🧠 Metodologi Machine Learning

### 1. K-Means Clustering

Mengelompokkan lokasi usaha ke dalam **K cluster** berdasarkan kemiripan fitur:

```
Fitur Input: Latitude, Longitude, Rating, Jumlah Review, Skor Potensi
```

- Data dinormalisasi menggunakan `scale()` sebelum clustering
- Jumlah K dapat diatur dari **2 hingga 8** cluster
- Visualisasi dimensi dikurangi menggunakan **PCA (Principal Component Analysis)**
- Pemilihan K optimal menggunakan **Elbow Method** (Within-Cluster Sum of Squares)

**Interpretasi Cluster:**

| Label | Kriteria |
|---|---|
| 🟢 Zona Sangat Strategis | Potensi ≥ 75 **DAN** Rating ≥ 4.2 |
| 🟡 Zona Strategis | Potensi ≥ 60 |
| 🟠 Zona Potensi Sedang | Potensi ≥ 45 |
| 🔴 Zona Kurang Strategis | Potensi < 45 |

### 2. DBSCAN

Mendeteksi cluster berbasis **kepadatan spasial** menggunakan koordinat geografis:

```
Fitur Input: Latitude, Longitude (scaled)
```

- Parameter **ε (Epsilon)**: radius pencarian tetangga (0.02 – 0.30)
- Parameter **MinPts**: jumlah minimum titik dalam radius ε (2 – 15)
- Titik yang tidak masuk cluster manapun dikategorikan sebagai **Noise (Cluster 0)**
- Pemilihan ε optimal dibantu oleh **k-NN Distance Plot**

---

## 🛠️ Stack Teknologi

### Framework & Runtime
| Package | Versi | Fungsi |
|---|---|---|
| `R` | 4.3+ | Bahasa pemrograman utama |
| `shiny` | ≥1.7 | Web app framework |
| `shinydashboard` | ≥0.7 | Layout dashboard |
| `shinycssloaders` | ≥1.0 | Loading spinner animasi |

### Visualisasi
| Package | Fungsi |
|---|---|
| `leaflet` | Peta interaktif dengan marker dan legenda |
| `plotly` | Grafik interaktif (scatter, bar, histogram) |
| `ggplot2` | Grafik statis (Elbow plot, k-NN Distance plot) |
| `DT` | Tabel data interaktif |

### Machine Learning
| Package | Fungsi |
|---|---|
| `cluster` | Algoritma K-Means |
| `dbscan` | Algoritma DBSCAN & kNNdist |
| `factoextra` | Visualisasi dan analisis cluster |

### Data Wrangling
| Package | Fungsi |
|---|---|
| `dplyr` | Manipulasi dan transformasi data |

---

## 🚀 Cara Menjalankan

### Prasyarat

Pastikan R versi **4.3 atau lebih baru** telah terinstal. Unduh di [r-project.org](https://www.r-project.org/).

### 1. Install Package yang Dibutuhkan

Buka R atau RStudio, lalu jalankan:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "leaflet",
  "dplyr",
  "ggplot2",
  "cluster",
  "dbscan",
  "DT",
  "plotly",
  "factoextra",
  "shinycssloaders"
))
```

### 2. Clone / Download Repositori

```bash
git clone https://github.com/ayyi858/Machine-Learning-R-studio-Sulsel-1.git
cd Machine-Learning-R-studio-Sulsel-1
```

### 3. Jalankan Aplikasi

**Opsi A — dari RStudio:**

Buka file `app.R` di RStudio, lalu klik tombol **"Run App"** di pojok kanan atas editor.

**Opsi B — dari R Console:**

```r
shiny::runApp("app.R")
```

**Opsi C — dari Terminal:**

```bash
Rscript -e "shiny::runApp('app.R')"
```

Aplikasi akan terbuka otomatis di browser pada `http://127.0.0.1:PORT`.

---

## 📸 Tampilan Aplikasi

### Dashboard Beranda
Menampilkan 4 value box (total lokasi, kota, kategori, rata-rata rating) beserta grafik distribusi kategori, rating, dan sebaran per kota.

### Peta Interaktif
Peta gelap (*dark map*) berbasis CartoDB DarkMatter yang menampilkan marker lokasi usaha. Klik marker untuk melihat detail (nama, kategori, rating, harga sewa, skor potensi).

### Analisis K-Means
Scatter plot PCA 2D dengan warna berbeda per cluster, Elbow Method untuk pemilihan K optimal, dan profil perbandingan antar cluster.

### Analisis DBSCAN
Visualisasi spasial cluster berdasarkan koordinat, k-NN Distance Plot dengan garis ε, dan distribusi jumlah titik per cluster.

---

## 📂 Struktur Repositori

```
📦 Machine-Learning-R-studio-Sulsel-1/
├── 📄 app.R          # File utama aplikasi Shiny (UI + Server)
├── 📄 FINAL.Rproj    # RStudio Project file
└── 📄 README.md      # Dokumentasi proyek
```

---

## 👤 Informasi Mahasiswa

| | |
|---|---|
| **Mata Kuliah** | Machine Learning |
| **Program Studi** | Bisnis Digital |
| **Universitas** | Universitas Negeri Makassar (UNM) |
| **GitHub** | [@ayyi858](https://github.com/ayyi858) |

---

## 📝 Catatan Penting

> **⚠️ Data Simulasi:** Seluruh data yang digunakan dalam aplikasi ini adalah **data dummy/simulasi** yang dibangkitkan secara acak menggunakan fungsi `generate_data()`. Data ini tidak mencerminkan kondisi nyata lapangan. Pada penelitian sesungguhnya, data akan diambil langsung dari **Google Maps API** atau survei lapangan.

---

## 📄 Lisensi

Proyek ini dibuat untuk keperluan akademik. Seluruh hak cipta dimiliki oleh pembuat sesuai ketentuan institusi.

---

<div align="center">

Dibuat dengan ❤️ menggunakan **R Shiny** · Bisnis Digital UNM · 2026

</div>
