# default libs
library(tidyverse)
library(scales)

# read data
df_ipm <- read_csv("data/ipm_provinsi.csv") %>%
  gather(key=tahun, value=ipm, -1) %>%
  select(provinsi=1, everything()) %>%
  mutate(provinsi=case_when(grepl("Jakarta", .$provinsi)~"DKI Jakarta",
                            grepl("Yogyakarta", .$provinsi)~"DI Yogyakarta",
                            TRUE~.$provinsi),
         tahun=as.numeric(tahun),
         ipm=as.numeric(ipm)) %>%
  mutate(ipm=ifelse(ipm==0, NA, ipm))