library(terra)
setwd("D:Personale/Studio/Magistrale/Tesi/zGW_Data/ISIMIP3a")

data1<-rast("cwatm_gswp3-w5e5_obsclim_histsoc_default_groundwstor_global_monthly_1901_2019.nc")
# Contiene GW storage di 1428 mesi a partire dal 01-01-1901

data2<-rast("cwatm_gswp3-w5e5_obsclim_histsoc_default_qr_global_monthly_1901_2019.nc")
# Contiene Total GW recharge di 1428 mesi a partire dal 01-01-1901

data3<-rast("cwatm_gswp3-w5e5_obsclim_histsoc_default_tws_global_monthly_1901_2019.nc")
# Contiene Total Water storage di 1428 mesi a partire dal 01-01-1901


# Total Water Storage per MENA (considerando il mese 1400)
a <- c(0,30000)
extent<-ext(-20, 60, 0, 39.5)
tws_1400_MENA<-crop(data3$tws_1400, extent)
plot(tws_1400_MENA, range=a)
