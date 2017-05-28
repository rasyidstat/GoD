source("0_read.R")

df_ipm_2016 <- df_ipm %>%
  filter(tahun==2016) %>%
  mutate(ipm_scaled=round(ipm-70.18, 2),
         cat=ifelse(ipm_scaled<0, "Di bawah\nrata-rata nasional", "Di atas\nrata-rata nasional"),
         ipm_scaled=ifelse(ipm_scaled>0, paste0("+", ipm_scaled), ipm_scaled)) %>%
  filter(provinsi!="Indonesia")

ggplot(df_ipm_2016, aes(reorder(provinsi, ipm), ipm, fill=cat)) +
  geom_col() + coord_flip() + 
  theme_minimal(base_size=13, base_family="DIN") +
  geom_text(aes(label=ipm_scaled, color=cat), hjust=0, nudge_y=1, family="DIN") +
  labs(x="Provinsi", y="Indeks Pembangunan Manusia") +
  guides(color=F) +
  scale_y_continuous(limits=c(50,81), breaks=seq(0,100,5), oob=rescale_none) +
  scale_fill_manual("", values=c("steelblue", "indianred")) +
  scale_color_manual("", values=c("steelblue", "indianred")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),
        panel.grid.minor=element_blank())

ggsave("figures/ipm-compare.png", width=8, height=8)



# version 2

df_ipm_2016 <- df_ipm %>%
  filter(tahun==2016) %>%
  mutate(ipm_scaled=ipm-70.18) %>%
  filter(provinsi!="Indonesia") %>%
  mutate(cat=ifelse(ipm_scaled<0, "Di bawah\nIPM nasional", "Di atas\nIPM nasional"))

ggplot(df_ipm_2016, aes(reorder(provinsi, ipm_scaled), ipm_scaled, fill=cat)) +
  geom_col() + coord_flip() + 
  geom_hline(aes(yintercept=0, linetype="70.18"), size=0.75, alpha=0.6) +
  theme_minimal(base_size=13, base_family="DIN") +
  labs(x="Provinsi", y="Indeks Pembangunan Manusia\n(Selisih dengan IPM Nasional)",
       title="Indeks Pembangunan Manusia di Indonesia",
       subtitle="Per Provinsi (Tahun 2016)",
       caption="Sumber data: BPS") +
  scale_y_continuous(breaks=c(seq(-15-0.18,-5-0.18,5),0,seq(5-0.18,10-0.18,5)),
                     labels=paste0(c(seq(70.18-15-0.18,70.18-5-0.18,5),70.18,seq(70.18+5-0.18,70.18+10-0.18,5)), "\n (",
                                   c(seq(-15-0.18,-5-0.18,5),0,seq(5-0.18,10-0.18,5)), ")")) +
  scale_linetype_manual("IPM Nasional", values=2) +
  scale_fill_manual("", values=c("steelblue", "indianred")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),
        panel.grid.minor=element_blank())

ggsave("figures/ipm-compare-v2.png", width=10, height=8)
