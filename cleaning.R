# ============================================================
# CLEANING DATA USAHA MAKASSAR
# ============================================================
library(dplyr); library(stringr); library(readxl)
library(writexl); library(scales)

cat("=== CLEANING DATA USAHA MAKASSAR ===\n\n")

df <- read_excel("data_usaha_makassar.xlsx")
cat(sprintf("Awal: %d baris x %d kolom\n", nrow(df), ncol(df)))

# 1. RENAME kolom Latitude/Longitude -> lowercase
if("Latitude"  %in% names(df)) { df$latitude  <- df$Latitude;  df$Latitude  <- NULL }
if("Longitude" %in% names(df)) { df$longitude <- df$Longitude; df$Longitude <- NULL }

# 2. FIX TIPE KOLOM
df$latitude      <- as.numeric(df$latitude)
df$longitude     <- as.numeric(df$longitude)
df$rating        <- as.numeric(str_replace(as.character(df$rating), ",", "."))
df$jumlah_ulasan <- as.numeric(df$jumlah_ulasan)
df$jumlah_ulasan[is.na(df$jumlah_ulasan)] <- 0

cat(sprintf("[1] Rating range: %.1f - %.1f\n", min(df$rating,na.rm=TRUE), max(df$rating,na.rm=TRUE)))

# 3. FIX KOORDINAT (titik desimal hilang)
fix_lat <- function(v) {
  if(is.na(v)) return(NA_real_)
  for(d in c(1,10,100,1000,10000,100000,1000000,10000000,1e15)) {
    r <- v/d
    if(!is.na(r) && r >= -5.30 && r <= -5.00) return(round(r,7))
  }; NA_real_
}
fix_lng <- function(v) {
  if(is.na(v)) return(NA_real_)
  for(d in c(1,10,100,1000,10000,100000,1000000,10000000,1e8)) {
    r <- v/d
    if(!is.na(r) && r >= 119.20 && r <= 119.60) return(round(r,7))
  }; NA_real_
}
df$latitude  <- sapply(df$latitude,  fix_lat)
df$longitude <- sapply(df$longitude, fix_lng)
cat(sprintf("[2] Koordinat valid: %d / %d\n",
            sum(!is.na(df$latitude)&!is.na(df$longitude)), nrow(df)))

# 4. FIX KECAMATAN
KECS <- c("Mariso","Mamajang","Tamalate","Rappocini","Makassar",
          "Ujung Pandang","Wajo","Bontoala","Ujung Tanah","Tallo",
          "Panakkukang","Manggala","Biringkanaya","Tamalanrea","Kepulauan Sangkarrang")
fix_kec <- function(kec, kw) {
  if(kec %in% KECS) return(kec)
  for(k in KECS) if(str_detect(kw, fixed(k))) return(k)
  return(kec)
}
df$kecamatan <- mapply(fix_kec, df$kecamatan, df$keyword)
n_invalid <- sum(!df$kecamatan %in% KECS)
if(n_invalid > 0) df <- df %>% filter(kecamatan %in% KECS)
cat(sprintf("[3] Kecamatan fix: %s\n", paste(sort(unique(df$kecamatan)), collapse=", ")))

# 5. BERSIHKAN STRING
bersih <- function(x) { if(is.character(x)) x[x %in% c("","NaN","N/A","nan","NULL","None","-")] <- NA; x }
df <- df %>% mutate(across(where(is.character), bersih))

# 6. IMPUTASI
df$kategori_usaha[is.na(df$kategori_usaha)] <- "Tidak Diketahui"
df$rating[is.na(df$rating)] <- median(df$rating, na.rm=TRUE)
cat(sprintf("[4] Rating NA diisi median: %.2f\n", median(df$rating, na.rm=TRUE)))

# 7. HAPUS DUPLIKAT (keep rating tertinggi)
n_bfr <- nrow(df)
df <- df %>% arrange(nama, desc(rating), desc(jumlah_ulasan)) %>%
  distinct(nama, alamat, .keep_all=TRUE)
cat(sprintf("[5] Duplikat dihapus: %d baris\n", n_bfr - nrow(df)))

# 8. HAPUS RATING OUTLIER
df <- df %>% filter(rating >= 1 & rating <= 5)

# 9. TAMBAH KOLOM TURUNAN
df <- df %>% mutate(
  kategori_grup = case_when(
    str_detect(tolower(kategori_usaha),
               "kafe|coffee|kopi|restoran|makan|seafood|bakso|mie|soto|sate|nasi|burger|pizza|korean|japanese|chinese|dessert|es |roti|donat|warung|kuliner|minuman|jus|boba|pisang|coto|konro|pallubasa|kapurung|jalangkote|martabak|geprek|ayam") ~ "Kuliner & Minuman",
    str_detect(tolower(kategori_usaha),
               "minimarket|supermarket|toko|retail|pakaian|sepatu|tas|buku|mainan|kosmetik|aksesoris|elektronik|komputer|furnitur|bangunan|kelontong|oleh") ~ "Retail & Toko",
    str_detect(tolower(kategori_usaha),
               "apotek|klinik|dokter|rumah sakit|puskesmas|optik|kesehatan|medis|farmasi") ~ "Kesehatan",
    str_detect(tolower(kategori_usaha),
               "salon|barbershop|cukur|kecantikan|spa|nail|perawatan|rambut") ~ "Kecantikan",
    str_detect(tolower(kategori_usaha),
               "bengkel|cuci motor|cuci mobil|tambal|sparepart|otomotif|ban|modifikasi") ~ "Otomotif",
    str_detect(tolower(kategori_usaha),
               "laundry|cuci|percetakan|foto studio|print|jasa|servis|jahit|ekspedisi|travel|notaris") ~ "Jasa Umum",
    str_detect(tolower(kategori_usaha),
               "bimbel|kursus|les|sekolah|pendidikan") ~ "Pendidikan",
    str_detect(tolower(kategori_usaha),
               "gym|fitness|futsal|kolam|olahraga|karaoke|hiburan|bioskop") ~ "Hiburan & Olahraga",
    str_detect(tolower(kategori_usaha),
               "hotel|penginapan|kost|guest house|villa|resort") ~ "Akomodasi",
    TRUE ~ "Lainnya"
  ),
  rating_kategori = case_when(
    rating>=4.5~"Sangat Baik", rating>=4.0~"Baik",
    rating>=3.5~"Cukup",       rating>=3.0~"Kurang",
    !is.na(rating)~"Buruk",    TRUE~NA_character_
  ),
  popularitas = case_when(
    jumlah_ulasan>=500~"Sangat Populer", jumlah_ulasan>=100~"Populer",
    jumlah_ulasan>=20~"Cukup Dikenal",   jumlah_ulasan>=1~"Baru",
    TRUE~"Tidak Ada Ulasan"
  ),
  skor_potensi = round(
    rescale(rating, to=c(0,100), from=c(1,5)) * 0.6 +
      rescale(log1p(jumlah_ulasan), to=c(0,100)) * 0.4, 1
  ),
  label_kelayakan = case_when(
    skor_potensi>=75~"Sangat Layak", skor_potensi>=55~"Layak",
    skor_potensi>=35~"Cukup Layak",  TRUE~"Kurang Layak"
  )
)
cat(sprintf("[6] Kolom turunan ditambahkan\n"))

# 10. RAPIKAN & SIMPAN
df_clean <- df %>%
  select(nama, kategori_usaha, kategori_grup, kecamatan, alamat,
         rating, rating_kategori, jumlah_ulasan, popularitas,
         skor_potensi, label_kelayakan, latitude, longitude,
         telepon, website, foto_url, keyword, tanggal) %>%
  arrange(kecamatan, kategori_grup, desc(skor_potensi))

write_xlsx(df_clean, "data_usaha_makassar_clean.xlsx")
write.csv(df_clean, "data_usaha_makassar_clean.csv", row.names=FALSE, fileEncoding="UTF-8")

cat(sprintf("\n=== HASIL ===\n"))
cat(sprintf("Baris final    : %d\n", nrow(df_clean)))
cat(sprintf("Koordinat valid: %d (%.1f%%)\n",
            sum(!is.na(df_clean$latitude)), sum(!is.na(df_clean$latitude))/nrow(df_clean)*100))
cat(sprintf("Avg rating     : %.2f\n", mean(df_clean$rating,na.rm=TRUE)))
cat(sprintf("Avg potensi    : %.1f\n", mean(df_clean$skor_potensi,na.rm=TRUE)))
cat(sprintf("\nKecamatan:\n"))
print(sort(table(df_clean$kecamatan),decreasing=TRUE))
cat(sprintf("\nKategori grup:\n"))
print(sort(table(df_clean$kategori_grup),decreasing=TRUE))
cat("\nFile: data_usaha_makassar_clean.xlsx & .csv\nSelesai!\n")