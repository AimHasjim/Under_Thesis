setwd('E:/Kuliah/Skripsi/Data Kos Python/Repository')
library(tidyverse)
library(sf)
library(tmap)
library(ggplot2)
library(stringr)
library(FactoMineR)
library(readxl)
library(haven)
library(stargazer)
library(broom)
library(huxtable)
library(car)
library(RColorBrewer)  # Install
library(vtable)
library(lmtest)
library(sandwich)


kos <- read.csv('data_kos[2].csv')
##### Outlier detection
kos_outlier <- kos %>% arrange(desc(price_monthly)) %>% select(X_id, name_slug, price_monthly,url)
kos_outlier <- kos_outlier %>% mutate(outlier = str_detect(name_slug,"Mamitest|Guest House| Homestay") | price_monthly > 5000000)
id_outlier <- kos_outlier$X_id[kos_outlier$outlier == F]
kos <- kos %>% filter(X_id %in% id_outlier)

##Map Making

sfpoint <- kos %>% select(latitude, longitude, X_id) %>% st_as_sf(
                          coords = c('longitude', 'latitude'),
                          crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

yogya_peta <-  st_read("Maps/gadm41_IDN_4.shp") %>% filter(NAME_1 == "Yogyakarta")
ind_dist <- tm_shape(yogya_peta)+ tm_borders()+ tm_shape(sfpoint) + tm_symbols(col = "red", scale = .1)


##### Contains

desa <- yogya_peta[st_within(sfpoint,yogya_peta) %>% as.numeric(),]
desa_kos <- bind_cols(sfpoint, desa %>% select(NAME_4, NAME_3, NAME_2, CC_4))
jml_kos <- desa_kos %>% group_by(NAME_2,NAME_4, CC_4) %>% mutate(ones = 1) %>% summarise(jml_kos = sum(ones)) %>% arrange(desc(jml_kos))
jml_kos_geom <- left_join(jml_kos, yogya_peta %>% select(geometry, "NAME_4","NAME_2"), by = c("NAME_4","NAME_2")) %>% filter(!is.na(NAME_4))
jml_kos %>% ungroup() %>% top_n( 10, jml_kos) %>%ggplot(aes(reorder(NAME_4,-jml_kos), jml_kos, fill = NAME_2)) + geom_bar(stat = 'identity') +
  labs(x = 'Village',
       y = 'Number of HMOs',
       fill = 'Kabupaten/Kota',
       title = 'Village With the Most HMOs')

############### Bathrooms


bathrooms <- kos %>% select(X_id,Kamar.Mandi.Luar...WC.Jongkok, Kamar.Mandi.Luar...WC.Duduk, kloset.duduk, Kloset.Duduk, Kloset.Jongkok, K..Mandi.Luar, K..Mandi.Dalam) 
bathrooms <- bathrooms %>% mutate(kloset.duduk = (kloset.duduk == "True" | Kloset.Duduk == "True" |Kamar.Mandi.Luar...WC.Duduk == "True"),
                     kmr_mandi_dalam = K..Mandi.Dalam == "True",
                     no_kmr_mandi = !(Kamar.Mandi.Luar...WC.Jongkok == "True"| Kamar.Mandi.Luar...WC.Duduk == "True"| kloset.duduk == "True"| Kloset.Duduk == "True"| Kloset.Jongkok == "True"| K..Mandi.Luar== "True"| K..Mandi.Dalam== "True"))
id_no_kmrmandi <- bathrooms$X_id[bathrooms$no_kmr_mandi == F]
kos <- kos %>% filter(X_id %in% id_no_kmrmandi)
bathrooms <- bathrooms %>% filter(X_id %in% id_no_kmrmandi)
bathrooms <- bathrooms %>% mutate(kloset_duduk = case_when(kloset.duduk == T~"Kloset Duduk",kloset.duduk == F~"Kloset Jongkok"),
                                  kmr_mandi_dalam= case_when(kmr_mandi_dalam == T~"kmr_mandi_dalam",kmr_mandi_dalam == F~"kmr_mandi_luar"))
############### MCA
kolom <- colnames(kos)
test_kolom <- kolom[c(38:39,41,42, 44:46,48:51,53,55, 57:59,61,66:67,71,79,90,109,155)]
kos_test <- kos %>% select(all_of(test_kolom), luas, price_monthly, X_id)
test_kolom
i <-  0
for (i in 1:length(test_kolom)){
  coll <- test_kolom[i]
  kos_test[i] <- dplyr::recode(kos_test[,i], "True" = paste(coll), "False" = paste("no", coll))
}
kos_test <- cbind(kos_test, bathrooms %>% select(kloset_duduk, kmr_mandi_dalam, X_id))
kos_test <- kos_test %>% select(!(X_id))
eks <- MCA(kos_test,quanti.sup = 25:26)
writexl::write_xlsx(eks$eig %>% as.data.frame(), "MCA Eigenvalues.xlsx")
index_eks <- eks[['ind']]$coord[]
eksklusif_data <- data.frame(eksklusif_index = index_eks, harga = kos$price_monthly, luas = kos$luas, build = kos$building_year)
for (i in 1:5){
  colnames(eksklusif_data)[i] <- paste0("eks_", i)
}


####### Minimal Distance Calculation
univ_loc <- read_xlsx("univ_loc.xlsx", sheet = "used")

univ_loc_sf <- st_as_sf(x = univ_loc, 
                                  coords = c('longitude', 'latitude'),
                                  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% mutate(`Nama PT` = Singkatan)

##### Distance Calculation -> minimum_distance_data
dist_mat_kos <- st_distance(sfpoint$geometry, univ_loc_sf$geometry)
rownames(dist_mat_kos) <- sfpoint$X_id
colnames(dist_mat_kos) <- univ_loc_sf$Singkatan
nama_univ <- univ_loc_sf$Singkatan
min_univ_dist<- c()
min_univ <- c()
for (ind in 1:4212) {
  min_dist <- min(dist_mat_kos[ind,])
  minimal_condition <- dist_mat_kos[ind,] == min(dist_mat_kos[ind,])
  univ_min <- nama_univ[minimal_condition]
  min_univ_dist <- append(min_univ_dist, min_dist)
  min_univ <- append(min_univ, as.vector(univ_min)[1])
}

minimum_distance_data <- data.frame(min_dist_univ = min_univ_dist,
                                    nearest_univ = min_univ)
minimum_distance_data %>% mutate(ones = 1) %>%group_by(nearest_univ) %>% summarise(jml = sum(ones))

##### enchanced maps
desa_kos <- desa_kos %>% filter(X_id %in% id_no_kmrmandi)
sfpoint <- sfpoint %>% filter(X_id %in% id_no_kmrmandi)
desa_kos_nonspatial <- desa_kos  %>% tibble() %>% select(!c(geometry...2, geometry...7))
kos_2 <- cbind(kos, minimum_distance_data, eksklusif = eksklusif_data[1:5], desa_kos %>% select(!X_id))
cords_2 <- cbind(sfpoint, minimum_distance_data)
pal = c(brewer.pal(12, "Set3"), '#129890','#23774a') %>% sample(14)
tm_shape(yogya_peta)+ tm_borders()+ 
  tm_shape(cords_2) + tm_dots(col = 'nearest_univ', legend.show = F, palette = pal) + 
  tm_shape(univ_loc_sf) + tm_symbols(size = 0.3, col = "Nama PT", palette = pal) + tm_layout( legend.outside = TRUE,
                                                                                                legend.text.size = 2,
                                                                                              legend.title.size = 2)

###################### PODES
podes <- read_dta("podes2020_b8b13.dta")
podes_olahan <- podes %>% filter(R101 == 34) %>% select(provinsi = R101, kabupaten = R102, kecamatan = R103, 
                                                        desa_nama = R104N, desa_kode = R104, freq_shops = R906AK2,
                                                        freq_permanentmarkets = R906BK2, freq_semipermanent = R906CK2, freq_unbuilt = R906DK2 ,freq_supermarkets = R906EK2,
                                                        freq_restaurants = R906FK2, freq_stalls = R906GK2, freq_hotel = R906HK2,freq_inn = R906HK2,
                                                        freq_shopstall = R906JK2)


podes_olahan <- podes_olahan %>% mutate(kode_desa = paste0(provinsi, kabupaten,kecamatan, desa_kode))

podespca <- PCA(podes_olahan[,6:15])
writexl::write_xlsx(podespca$eig %>% as.data.frame(), "PCA eigenvalues.xlsx")
summary(podespca)
desa_pusat <- c('TRIDADI', 'SURYATMAJAN','SOSROMENDURAN','MUJA MUJU','BANTUL','WONOSARI','WATES')
podes_olahan <- podes_olahan %>% mutate(amenitas_1 =podespca$ind$coord[,1], amenitas_2 = podespca$ind$coord[,2],amenitas_3 = podespca$ind$coord[,3],amenitas_4 = podespca$ind$coord[,4],
                                        pusat = case_when(desa_nama %in% desa_pusat~1, T~0) ) 

kos_podes <- left_join(desa_kos_nonspatial %>% select(!c(X_id, NAME_2, NAME_3, NAME_4)), podes_olahan, by = c("CC_4" = "kode_desa"))
##### Merge Data For Analysis

kos_3 <- cbind(kos_2 %>% select(!c("Guling":"Parkir.Motor", CC_4)), kos_podes) 
kos_3[5] <- dplyr::recode(kos_3[,5], "0" = "Campur", "1" = "Putra", "2" = "Putri")
kos_3 <- kos_3 %>% mutate(umur = 2023 - building_year)
kos_3 <- kos_3 %>% mutate(amenitas_3 = kos_podes$amenitas_3)
kos_3 <- cbind(kos_3, kos_test %>% select(!c(luas, price_monthly)))
writexl::write_xlsx(kos_3, "data_kos_reg.xlsx")

desa_univ <- st_within(univ_loc_sf,st_sf(jml_kos_geom)) %>% as.numeric()
jml_kos <- jml_kos%>% ungroup() %>% mutate(no = 1:156) %>% mutate(univ_di_desa = no %in% desa_univ)
jml_kos <- left_join(jml_kos, podes_olahan, by = c("CC_4" = "kode_desa"))
writexl::write_xlsx(jml_kos, "data_h3.xlsx")



