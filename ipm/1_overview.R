source("0_read.R")

df_ipm_overview <- df_ipm %>%
  filter(!is.na(ipm), provinsi!="Indonesia") %>%
  group_by(tahun) %>%
  summarise(`Banyak Provinsi`=n(),
            `Rata-rata`=mean(ipm),
            `Median`=median(ipm),
            `Standar Deviasi`=sd(ipm),
            `Min`=min(ipm),
            `Maks`=max(ipm),
            `Rentang`=Maks-Min,
            `IQR`=IQR(ipm)) %>%
  inner_join(filter(df_ipm, provinsi=="Indonesia") %>%
               select(tahun, nasional=ipm)) %>%
  select(Tahun=tahun, `Banyak Provinsi`, Nasional=nasional, everything()) %>%
  mutate_each(funs(round(., 2)), Nasional:IQR)

write.csv(df_ipm_overview, "data/ipm_overview.csv", row.names=FALSE)
