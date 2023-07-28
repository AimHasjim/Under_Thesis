setwd('E:/Kuliah/Skripsi/Data Kos Python/Repository')
library(tidyverse)
library(readxl)
library(haven)
library(stargazer)
library(lmtest)

### Regression Analysis
kos_3<- readxl::read_xlsx("data_kos_reg.xlsx")
## h1
h1_1 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = price_monthly~luas+as.factor(gender)+min_dist_univ+umur+
                                                                        eksklusif.eks_1+eksklusif.eks_2+eksklusif.eks_3+eksklusif.eks_4+eksklusif.eks_5+
                                                                        amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
summary(h1_1)
h1_2 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = price_monthly~luas+as.factor(gender)+min_dist_univ+
                                                                     eksklusif.eks_1+eksklusif.eks_2+eksklusif.eks_3+eksklusif.eks_4+eksklusif.eks_5+
                                                                     amenitas_1+amenitas_2+amenitas_3+amenitas_4)
summary(h1_2)
h1_1$AIC <- AIC(h1_1)
h1_2$AIC <- AIC(h1_2)
stargazer(h1_1, h1_2,title = "Hipotesis 1", type = "html", out = "Hypothesis Tests/Hipotesis 1.html",
          keep.stat = c("aic", "rsq","adj.rsq" ,"n","f"))

## h2
h2_1 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_1~luas+as.factor(gender)+min_dist_univ+umur+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 
h2_2 <- kos_3 %>% filter(as.numeric(min_dist_univ) < 15000) %>% lm(formula = eksklusif.eks_1~luas+as.factor(gender)+min_dist_univ+amenitas_1+amenitas_2+amenitas_3+amenitas_4) 

summary(h2_1)
AIC(h2_1)
summary(h2_2)
AIC(h2_2)
h2_1$AIC <- AIC(h2_1)
h2_2$AIC <- AIC(h2_2)
summary(h2_2)
tidy(h2_1)
stargazer(h2_1, h2_2, title = "Hipotesis 2", type = "html", out = "Hypothesis Tests/Hipotesis 2.html",
          keep.stat = c("aic", "rsq","adj.rsq" ,"n","f"))
## h3
jml_kos <- read_xlsx("data_h3.xlsx")
h3_1 <- jml_kos %>% lm(formula = amenitas_1~pusat+as.factor(univ_di_desa))
h3_2 <- jml_kos %>% lm(formula = amenitas_1~pusat+jml_kos+as.factor(univ_di_desa))
summary(h3_1)
summary(h3_2)
h3_1$AIC <- AIC(h3_1)
h3_2$AIC <- AIC(h3_2)

stargazer(h3_1, h3_2, title = "Hipotesis 3", type = "html", out = "Hypothesis Tests/Hipotesis 3.html",
          keep.stat = c("aic", "rsq","adj.rsq" ,"n","f"))
