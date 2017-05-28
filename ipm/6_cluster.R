source("0_read.R")

# data --------------------------------------------------------------------

model <- function(df){
  lm(ipm ~ tahun, data=df)
}

df_reg <- df_ipm %>%
  group_by(provinsi) %>%
  nest() %>%
  mutate(model=map(data, model))

df_reg <- df_reg %>%
  mutate(glance=map(model, broom::tidy)) %>%
  unnest(glance) %>%
  filter(term=="tahun") %>%
  select(provinsi, estimate)

df_master <- df_ipm %>%
  filter(tahun==2016) %>%
  mutate(tahun=paste0("ipm_", tahun)) %>%
  spread(tahun, ipm) %>%
  inner_join(df_reg) %>%
  inner_join(read_csv("data/ref_provinsi.csv")) %>%
  as.data.frame()

rownames(df_master) = df_master$provinsi_abb

# kmeans ------------------------------------------------------------------

wssplot <- function(data, n = 15, seed = 1234){
  wss <- (nrow(data) - 1)*sum(apply(data, 2, var))
  for (i in 2:n){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:n, wss, type = "b", xlab = "Number of Clusters", 
       ylab = "Within groups sum of squares")
}
wssplot(df_master[,2:3])

km <- kmeans(df_master[,2:3], 4)

# plot kmeans -------------------------------------------------------------

df_master <- cbind(df_master, data.frame(class=km$cluster))

df_master %>%
  filter(provinsi!="Indonesia") %>%
  ggplot(aes(ipm_2016, estimate)) + 
  geom_point(aes(color=factor(class))) + 
  ggrepel::geom_text_repel(aes(label=provinsi_abb), family="DIN") +
  labs(x="IPM 2016", y="Koefisien Kemiringan") +
  scale_color_discrete("Klaster") +
  theme_minimal(base_size=13, base_family="DIN") +
  theme(panel.grid.minor=element_blank(),
        legend.justification=c(1,1),
        legend.position=c(1,1))

ggsave("figures/ipm-cluster.png", width=8, height=6)

# spatial -----------------------------------------------------------------

library(sf)
library(indonesia)
library(ggplot2)

df_spatial <- id_map("indonesia", "provinsi")

df_spatial <- df_master %>%
  left_join(df_spatial, by=c("provinsi"="nama_provinsi")) %>%
  st_as_sf()

df_spatial %>%
  filter(provinsi!="Indonesia") %>%
  ggplot() +
  geom_sf(aes(fill=as.factor(class)), color=NA) + 
  scale_fill_discrete("Kluster") +
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

ggsave("figures/ipm-cluster-peta.png", width=8, height=3)
