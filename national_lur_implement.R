# does not assign coefficient

source('~/Box Sync/from_dropbox/ACE hugh/Github_CAPS_spatial/make_lur.R', echo=TRUE)

library(tidyverse)
library(car)
library(ape)
library(DAAG)

LUR_input <- read_csv("qing_national_lur_input.csv")
LUR_input_01 <- LUR_input %>% select(-qing_ID) 
LUR_input_02 = LUR_input_01[!duplicated(lapply(LUR_input_01, summary))]
# remove vars with more than half zero
zero_filter = LUR_input_02 %>% map_dbl(~sum(.x == 0)/nrow(LUR_input_02))
LUR_input_f = LUR_input_02[zero_filter < 0.5]
ignore_list <- names(LUR_input_f)[1:321]
sx <- LUR_input_f

# COA ---------------------------------------------------------------------
# not 298, coz duplicates removal code change order already

COA <- make_lur(dat1 = LUR_input_f, response = "COA", dep_col = 322, special = ignore_list)

# validation 
COA_lm <- lm(formula("COA ~  + ndvi_winter_a00250 + elev_1k_at_elev + lu_forest_p03000 + lu_stream_p00300 + tl_s00750 + ll_a1_s01500 + lu_oth_urban_p00750 + lu_comm_p03000 + intersect_a1_a1_s03000 + imp_a01000 + pop_s01000 + ndvi_winter_a07500 + ll_a3_s00500 + tl_s15000"), sx)

COA_lm <- lm(formula("COA ~  + ndvi_winter_a00250 + elev_1k_at_elev + lu_forest_p03000 + lu_stream_p00300 + tl_s00750  "), sx)
summary(COA_lm)
r2 0.56

# adj 0.62
plot(COA_lm, which = 4)
car::vif(COA_lm)

# moran's I
# todo

# LOOCV R2
loocv_COA <- cv.lm(COA_lm$model, COA_lm, m = 70, legend.pos = "topright")
cor(loocv_COA$COA,loocv_COA$cvpred)**2
0.47

# mean studentized prediction residuals (sd used n-1)
M_COA<-rstudent(COA_lm)
mean(M_COA)
-0.00218
# root mean square of studentized
sqrt(mean(M_COA^2))
1.02


# HOA ---------------------------------------------------------------------

HOA <- make_lur(dat1 = LUR_input_f, response = "HOA", dep_col = 322, special = ignore_list)

# validation 

HOA_lm <- lm(formula("HOA ~  + ll_a3_s00300 + lu_transport_p00750 + ll_a1_s03000 + em_CO_s30000 + lu_resi_p00400 + lu_mix_urban_p15000 + lu_industrial_p03000 + lu_resi_p01000 + em_CO_s03000 + lu_industrial_p01500 + ll_a3_s03000 + satno2_2013_2015 + rlu_dev_med_p00100"), sx)
summary(HOA_lm)

HOA_lm <- lm(formula("HOA ~  + ll_a3_s00300 + lu_transport_p00750 + ll_a1_s03000  "), sx)
summary(HOA_lm)
0.50 and adj r2 0.47
plot(HOA_lm, which = 4)
car::vif(HOA_lm)

# moran's I
# todo

# LOOCV R2
loocv_HOA <- cv.lm(HOA_lm$model, HOA_lm, m=70, legend.pos = "topright")
cor(loocv_HOA$HOA,loocv_HOA$cvpred)**2
0.40

# mean studentized prediction residuals (sd used n-1)
M_HOA<-rstudent(HOA_lm)
mean(M_HOA)
0.0158
# root mean square of studentized
sqrt(mean(M_HOA^2))
1.06


# chi ---------------------------------------------------------------------

chi <- make_lur(dat1 = LUR_input_f, response = "chi", dep_col = 322, special = ignore_list) 

chi_lm <- lm(formula("chi ~  + ll_a1_s01000 + m_to_ry + tl_s05000  "), sx)
summary(chi_lm)

plot(chi_lm, which = 4)
car::vif(chi_lm)

# moran's I
# todo

# LOOCV R2
loocv_chi <- cv.lm(chi_lm$model, chi_lm, m=70, legend.pos = "topright")
cor(loocv_chi$chi,loocv_chi$cvpred)**2
0.74 (compared to r2 0.77 and adj 0.76)

# mean studentized prediction residuals (sd used n-1)
M_chi <- rstudent(chi_lm)
mean(M_chi)
0.00774
# root mean square of studentized
sqrt(mean(M_chi^2))
1.02


# previous zeros truck and road boxplot --------------------------------------------------
ind_plot <- LUR_input_02 %>% select(tl_s00300, ll_a1_s01500, ll_a3_s00300, tl_s00150, ll_a1_s01000, tl_s05000)
ind_plot %>% gather() %>% ggplot() + geom_boxplot(aes(x = key, y = value)) 

ggplot(ind_plot) + geom_boxplot(aes(x = 1, y = tl_s00300)) + ggpubr::theme_pubr()
ggplot(ind_plot) + geom_boxplot(aes(x = 1, y = ll_a1_s01500)) + ggpubr::theme_pubr()


