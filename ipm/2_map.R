source("0_read.R")
library(viridis)

# peta indonesia 2015 http://www.gadm.org/
# devtools::install_github("rasyidstat/indonesia")
library(indonesia)

# sf spatial data frame
# install.packages("sf")
library(sf)

# geom_sf
# devtools::install_github("tidyverse/ggplot2")
library(ggplot2)


df_spatial <- id_map("indonesia", "provinsi")

df_spatial <- df_ipm %>%
  filter(tahun==2016) %>%
  left_join(df_spatial, by=c("provinsi"="nama_provinsi")) %>%
  st_as_sf()

df_spatial %>%
  filter(provinsi!="Indonesia") %>%
  ggplot() +
  geom_sf(aes(fill=ipm), color=NA) + 
  scale_fill_viridis("IPM") +
  labs(title="Peta Indeks Pembangunan Manusia di Indonesia",
       subtitle="Per Provinsi (Tahun 2016)",
       caption="Visualisasi data oleh: Rasyid Ridha (@rasyidstat)
                Sumber data: BPS
                http://rasyidridha.com") +
  theme_minimal(base_size = 13, base_family="DIN") +
  theme(line =               element_blank(),
        rect =               element_blank(),
        axis.text =          element_blank(),
        axis.ticks.length =  unit(0, "cm"), 
        legend.justification=c(0, 0),
        legend.position =    c(0, 0),
        legend.direction =   "horizontal",
        panel.spacing =      unit(0, "lines"),
        plot.margin =        unit(c(0, 0, 0, 0), "lines"),
        panel.grid.major =   element_line(colour = "white"))

ggsave("figures/ipm-2016-peta.png", width=8, height=4)

