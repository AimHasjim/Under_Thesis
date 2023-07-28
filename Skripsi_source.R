setwd('E:/Kuliah/Skripsi/Data Kos Python')
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

yogya_peta <-  st_read("E:/Kuliah/SMT 7/Regional/map project/gadm41_IDN_shp/gadm41_IDN_4.shp") %>% filter(NAME_1 == "Yogyakarta")
tm_shape(yogya_peta)+ tm_borders()+ tm_shape(sfpoint) + tm_symbols(col = "red", scale = .1)


##### Contains

desa <- yogya_peta[st_within(sfpoint,yogya_peta) %>% as.numeric(),]
desa_kos <- bind_cols(sfpoint, desa %>% select(NAME_4, NAME_3, NAME_2, CC_4))
jml_kos <- desa_kos %>% group_by(NAME_2,NAME_4, CC_4) %>% mutate(ones = 1) %>% summarise(jml_kos = sum(ones)) %>% arrange(desc(jml_kos))
jml_kos_geom <- left_join(jml_kos, yogya_peta %>% select(geometry, "NAME_4","NAME_2"), by = c("NAME_4","NAME_2")) %>% filter(!is.na(NAME_4))
jml_kos %>% ungroup() %>% top_n( 10, jml_kos) %>%ggplot(aes(reorder(NAME_4,-jml_kos), jml_kos, fill = NAME_2)) + geom_bar(stat = 'identity') +
  labs(x = 'Desa',
       y = 'Jumlah Kos',
       fill = 'Kabupaten/Kota',
       title = 'Desa Dengan Indekos Terbesar')

jml_kos  %>%ggplot(aes(reorder(NAME_2,-jml_kos), jml_kos)) + geom_bar(stat = 'identity') +
  labs(x = 'Desa',
       y = 'Jumlah Kos',
       fill = 'Provinsi')
tm_shape(yogya_peta)+tm_borders() + tm_shape(st_sf(jml_kos_geom)) + tm_polygons(col = "jml_kos")

############### Bathrooms

summary(kos)
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
kolom
nomor_kolom <- str_detect(kolom,"X[:digit:]")
test_kolom <- kolom[c(38:39,41,42, 44:46,48:51,53,55, 57:59,61,66:67,71,79,90,109,155)]
kos_test <- kos %>% select(all_of(test_kolom), luas, price_monthly, X_id)
test_kolom
i= 0
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
dplyr::re
# Max and Min Ekslklusif
eksklusif_data[eksklusif_data$eksklusif_i == min(eksklusif_data$eksklusif_i),]
eksklusif_data[eksklusif_data$eksklusif_i == max(eksklusif_data$eksklusif_i),]

reg_eks <- lm(eksklusif_data, formula = harga~eks_1+ luas+build)
summary(reg_eks)

####### Univ -> univ_loc_sf
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

####
min_univ_dist<- c()
min_univ <- c()
dups_m <- 0
for (ind in 1:4247) {
  min_dist <- min(dist_mat_kos[ind,])
  minimal_condition <- dist_mat_kos[ind,] == min(dist_mat_kos[ind,])
  univ_min <- nama_univ[minimal_condition]
  if (sum(minimal_condition) > 1) {
    dups_m = dups_m + 1
  }
  min_univ_dist <- append(min_univ_dist, min_dist)
  min_univ <- append(min_univ, univ_min)
}
#### How Many Simiilarities

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
podes17 <- read_dta("podes2020_b1b7.dta")
podes_olahan <- podes %>% filter(R101 == 34) %>% select(provinsi = R101, kabupaten = R102, kecamatan = R103, 
                                                        desa_nama = R104N, desa_kode = R104, jml_pertokoan = R906AK2,
                                                        jml_pasarperm = R906BK2, jml_pasarsperm = R906CK2, jml_pasartb = R906DK2 ,jml_swalayan = R906EK2,
                                                        jml_resto = R906FK2, jml_warung = R906GK2, jml_hotel = R906HK2,jml_inap = R906HK2,
                                                        jml_kelontong = R906JK2)

podes_olahan <- podes_olahan %>% mutate(kode_desa = paste0(provinsi, kabupaten,kecamatan, desa_kode))

podespca <- PCA(podes_olahan[,6:15] %>% scale())
writexl::write_xlsx(podespca$eig %>% as.data.frame(), "PCA eigenvalues.xlsx")
summary(podespca)
podespca
desa_pusat <- c('TRIDADI', 'SURYATMAJAN','SOSROMENDURAN','MUJA MUJU','BANTUL','WONOSARI','WATES')
podes_olahan <- podes_olahan %>% mutate(amenitas_1 =podespca$ind$coord[,1], amenitas_2 = podespca$ind$coord[,2],amenitas_3 = podespca$ind$coord[,3],amenitas_4 = podespca$ind$coord[,4],
                                        pusat = case_when(desa_nama %in% desa_pusat~1, T~0) ) 

kos_podes <- left_join(desa_kos_nonspatial %>% select(!c(X_id, NAME_2, NAME_3, NAME_4)), podes_olahan, by = c("CC_4" = "kode_desa"))
kos_podes[c('amenitas_4','amenitas_3')]
podes_olahan[c('amenitas_4','amenitas_3')]
##### Regression tests
kos_3 %>% names()
kos_3 <- cbind(kos_2 %>% select(!c("Guling":"Parkir.Motor", CC_4)), kos_podes) 
kos_3[5] <- recode(kos_3[,5], "0" = "Campur", "1" = "Putra", "2" = "Putri")
kos_3 <- kos_3 %>% mutate(umur = 2023 - building_year)
kos_3 <- kos_3 %>% mutate(amenitas_3 = kos_podes$amenitas_3)
kos_3 <- cbind(kos_3, kos_test %>% select(!c(luas, price_monthly)))
writexl::write_xlsx(kos_3, "data_kos_reg_7.xlsx")
kos_3<- readxl::read_xlsx("data_kos_reg_6.xlsx")
## h1
vif(h1_1)
h1_1 <- data_reg %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = price_monthly~luas+as.factor(gender)+min_dist_univ+umur+
                                                                     eksklusif.eks_1+eksklusif.eks_2+eksklusif.eks_3+eksklusif.eks_4+eksklusif.eks_5+
                                                                     amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
summary(h1_1)
h1_2 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = price_monthly~luas+as.factor(gender)+min_dist_univ+
                                                                     eksklusif.eks_1+eksklusif.eks_2+eksklusif.eks_3+eksklusif.eks_4+eksklusif.eks_5+
                                                                     amenitas_1+amenitas_2+amenitas_3+amenitas_4) 

kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = price_monthly~luas+as.factor(gender)+min_dist_univ+umur) %>% summary()
summary(h1_2)
h1_1$AIC <- AIC(h1_1)
h1_2$AIC <- AIC(h1_2)
stargazer(h1_1, h1_2,title = "Hipotesis 1", type = "html", out = "Hipotesis 1.html",
          keep.stat = c("aic", "rsq","adj.rsq" ,"n","f"))

## h2
h2_1 <- data_reg %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_1~luas+as.factor(gender)+min_dist_univ+umur+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
h2_2 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_1~luas+as.factor(gender)+min_dist_univ+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
h2_2 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_2~luas+as.factor(gender)+min_dist_univ+umur+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
h2_3 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_3~luas+as.factor(gender)+min_dist_univ+umur+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
h2_4 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_4~luas+as.factor(gender)+min_dist_univ+umur+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
h2_5 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_5~luas+as.factor(gender)+min_dist_univ+umur+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
summary(h2_1)
AIC(h2_1)
summary(h2_2)
AIC(h2_2)
summary(h2_3)
BIC(h2_3)
summary(h2_4)
BIC(h2_4)
summary(h2_5)
BIC(h2_5)
h2_1$AIC <- AIC(h2_1)
h2_2$AIC <- AIC(h2_2)
AIC(h1_1)
summary(h2_2)
tidy(h2_1)
stargazer(h2_1, h2_2, title = "Hipotesis 2", type = "html", out = "Hipotesis 2_2.html",
          keep.stat = c("aic", "rsq","adj.rsq" ,"n","f"))
## h3
desa_univ <- st_within(univ_loc_sf,st_sf(jml_kos_geom)) %>% as.numeric()
jml_kos <- jml_kos%>% ungroup() %>% mutate(no = 1:156) %>% mutate(univ_di_desa = no %in% desa_univ)
jml_kos <- left_join(jml_kos, podes_olahan, by = c("CC_4" = "kode_desa"))
writexl::write_xlsx(jml_kos, "data_h3.xlsx")
h3_1 <- jml_kos %>% lm(formula = amenitas_1~pusat+as.factor(univ_di_desa))
h3_2 <- jml_kos %>% lm(formula = amenitas_1~pusat+jml_kos+as.factor(univ_di_desa))
summary(h3_1)
summary(h3_2)
h3_1$AIC <- AIC(h3_1)
h3_2$AIC <- AIC(h3_2)

stargazer(h3_1, h3_2, title = "Hipotesis 3", type = "html", out = "Hipotesis 3_2.html",
          keep.stat = c("aic", "rsq","adj.rsq" ,"n","f"))
?stargazer
###### Interpretation
kos_ekss <- kos_3[c(72:97,41:45)]
name_kos <- names(kos_3)[c(96:97, 41:45)]
name_kos


recode(kos_ekss[,i], parse(eval(coll))  = 1)
nomor_kolom <- str_detect(kolom,"X[:digit:]")
test_kolom <- kolom[c(38:39,41,42, 44:46,48:51,53,55, 57:59,61,66:67,71,79,90,109,155)]
kos_test <- kos %>% select(all_of(test_kolom), luas, price_monthly, X_id)
i = 0
for (i in 1:length(test_kolom)){
  coll <- test_kolom[i]
  kos_test[i] <- recode(kos_test[,i], True = 1, False = 0)
}

kos_7 <- cbind(kos_test, kos_3[c(96:97, 41:45)])
kos_7['kmr_mandi_dalam'] <- recode(kos_7[,'kmr_mandi_dalam'], "kmr_mandi_dalam" = 1, "kmr_mandi_luar" = 0)
kos_7['kloset_duduk'] <- recode(kos_7[,'kloset_duduk'], "Kloset Duduk" = 1, "Kloset Jongkok" = 0)
kos_8 <- kos_7 %>% select(!c(luas, X_id, price_monthly))

corr_mat <- cor(x= kos_8[1:26], y = kos_8[27:31])  %>% as.data.frame() 
names(corr_mat) == "eksklusif.eks_1"
for (i in 1:5){
  colnames(corr_mat)[colnames(corr_mat) == paste0("eksklusif.eks_", i)] <- paste0("eks",i)
}
corr_mat <- corr_mat %>% mutate(fasilitas = rownames(corr_mat))
corr_mat <- corr_mat %>% pivot_longer(names_to = "PC", values_to = "Korelasi", "eks1":"eks5")
corr_mat %>% ggplot(aes(PC, fasilitas, fill = Korelasi)) + geom_tile() + scale_fill_gradientn(colours = c("red", "white", "blue"), values = c(0,0.4,1))
pod_mat <- cor(podes_olahan[6:15], podes_olahan[17:20])
pod_mat <- pod_mat %>% as.data.frame() %>% mutate(fasilitas = rownames(pod_mat)) %>% pivot_longer(names_to = "PC", values_to = "Korelasi", "amenitas_1":"amenitas_4")
pod_mat[1] <- dplyr::recode(pod_mat$fasilitas, jml_warung = "Jumlah Warung",
                            jml_swalayan = "Jumlah Swalayan",
                            jml_resto = "Jumlah Restoran",
                            jml_pertokoan = "Jumlah Pertokoan",
                            jml_pasartb = "Jumlah Pasar Tanpa Bangunan",
                            jml_pasarsperm = "Jumlah pasar Semi Permanen",
                            jml_pasarperm = "Jumlah Pasar Permanen",
                            jml_kelontong = "Jumlah Toko Kelontong",
                            jml_inap = "Jumlah Penginapan",
                            jml_hotel = "Jumlah Hotel")
pod_mat %>% ggplot(aes(PC, fasilitas, fill = Korelasi)) + geom_tile() + scale_fill_gradientn(colours = c("red", "white", "blue"), values = c(0,0.4,1))+
  labs(y = "Amenitas")
vif(h2_1)
plot(fitted(h2_1), resid(h2_1))

######Desc Statistics
data_reg <- kos_3[c("price_monthly","gender", "luas", "min_dist_univ","umur", "eksklusif.eks_1",
        "eksklusif.eks_2", "eksklusif.eks_3", "eksklusif.eks_4"
        , "eksklusif.eks_5","amenitas_1","amenitas_2","amenitas_3","amenitas_4")]
summary.table(data_reg)
data_reg <- data_reg %>% mutate(umur = case_when(umur == 2023~0, T~umur))
summary_stats <- st(data_reg, digits = 3, out = "return")
writexl::write_xlsx(summary_stats, "s12.xlsx")
data_reg$umur %>% mean()


data_reg_3 <- jml_kos[c("jml_kos", "univ_di_desa", "pusat", "amenitas_1")]
summ_stats_3 <- st(data_reg_3, out = "return")

jml_kos_univ <- kos_3["nearest_univ"] %>% group_by(nearest_univ) %>%count()
writexl::write_xlsx(jml_kos_univ, "jku.xlsx")
sum(jml_kos$univ_di_desa)
podes_olahan[431,]

# LIST REG
reglist <- list( h1_1 = h1_1, h1_2 = h1_2, h2_1 = h2_1, h2_2 = h2_2, h3_1 = h3_1, h3_2 = h3_2)
reglist[1] %>% names()
vif_list <- list()
for (i in names(reglist)){
  vif_i <- vif(reglist[[i]])
  row_vif <- row.names(vif_i)
  vif_list <- append(vif_list, list( as.data.frame(vif_i) %>% mutate(var = row_vif)))
}
i
reglist[[i]] %>% vif()
names(vif_list) <- names(reglist)
k <- vif(h2_2) %>% as.data.frame(row.names = row.names(vif(h2_2)))
writexl::write_xlsx(vif_list, "vifs.xlsx")
?as.data.frame
?writexl::write_xlsx
h1_1$residuals %>% ks.test(y = "pnorm")
?ks.test
nnn <- bptest(h2_1)
coeftest(h1_1, vcov. = vcovHC(h1_1, type = "HC0"))
nnn$p.value

#### hetero
hetero_list <- list()
for (i in reglist){
  rob_reg <- coeftest(i, vcov. = vcovHC(i, type = "HC0"))
  hetero_list <- append(hetero_list, list(rob_reg))
}
rob_reg
write.csv(hetero_list[[3]], "rreg2.csv")

jjk <- hetero_list[[1]] 
jjk %>% as.tibble()
writexl::write_xlsx(rob_reg, "jjk.xlsx")

BPF <- c()
BPPV <- c()
for (i in reglist){
  het_test <- bptest(i)
  BPF <- append(BPF, het_test$statistic)
  BPPV <- append(BPPV, het_test$p.value)
}


hetero_tests <- data.frame(model = names(reglist),
                           F_score = BPF,
                           P_Value = BPPV)
writexl::write_xlsx(hetero_tests, "hetero_tests.xlsx")
###### Normality
kkj <- reglist[[1]]$residuals %>% shapiro.test()
kkj$sta
wscores <- c()
pscores <- c()

for ( i in reglist){
  sw_t <- shapiro.test(i$residuals)
  wscores <- append(wscores, sw_t$statistic)
  pscores <- append(pscores, sw_t$p.value)
}
shap_test <- data.frame(model = names(reglist),
                        W_Score = wscores,
                        p_Values = pscores)

writexl::write_xlsx(shap_test, "normal_tests.xlsx")

neuni <- kos_3 %>% mutate(ones = 1) %>%group_by(nearest_univ) %>% summarise(jml = sum(ones)) %>% arrange(desc(jml))
writexl::write_xlsx(neuni, "neuni.xlsx")
