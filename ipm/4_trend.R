source("0_read.R")

df_ipm <- df_ipm %>%
  filter(provinsi!="Indonesia") %>%
  left_join(df_ipm %>%
              filter(provinsi=="Indonesia") %>%
              select(tahun, ipm_nasional=ipm)) %>%
  inner_join(read_csv("data/ref_provinsi.csv")) %>%
  mutate(ipm_nasional=ifelse(is.na(ipm), NA, ipm_nasional),
         cat=ifelse(ipm>ipm_nasional, "Di atas\nIPM nasional", "Di bawah\nIPM nasional"),
         cat=ifelse(is.na(ipm), "Di atas\nIPM nasional", cat))

ggplot(df_ipm, aes(tahun, ipm, fill=cat)) + 
  geom_col() +
  geom_line(data=df_ipm, aes(tahun, ipm_nasional, color="IPM Nasional"), size=1, linetype="dashed") +
  theme_minimal(base_size=13, base_family="DIN") + 
  facet_wrap(~provinsi_abb, nrow=7, ncol=5, scales="free_x") +
  labs(title="Tren Indeks Pembangunan Manusia di Indonesia",
       subtitle="Per Provinsi (Tahun 2010 - 2016)",
       x="Tahun", y="Indeks Pembangunan Manusia",
       caption="Sumber data: BPS") +
  scale_x_continuous(breaks=seq(2010,2016,2)) +
  scale_y_continuous(limits=c(50,85), oob=rescale_none) +
  scale_fill_manual("", values=c("steelblue", "indianred")) +
  scale_color_manual(name="", values=c("IPM Nasional"="black")) +
  theme(panel.grid.minor = element_blank(),
        legend.justification=c(1,0),
        legend.position=c(1,0))

ggsave("figures/ipm-trend.png", width=10, height=16)
