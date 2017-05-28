source("0_read.R")

model <- function(df){
  lm(ipm ~ tahun, data=df)
}

df_model <- df_ipm %>%
  group_by(provinsi) %>%
  nest() %>%
  mutate(model=map(data, model))

df_coef <- df_model %>%
  mutate(glance=map(model, broom::tidy)) %>%
  unnest(glance) %>%
  mutate(sig=ifelse(p.value<0.05, TRUE, FALSE))

df_coef %>%
  filter(sig==TRUE, term=="tahun", provinsi!="Indonesia") %>%
  ggplot(aes(reorder(provinsi, estimate), estimate)) + 
  geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), 
                lwd=1, width=0) +
  geom_point() + 
  geom_hline(data=filter(df_coef, term=="tahun", provinsi=="Indonesia"),
             aes(yintercept=estimate, linetype=comma(estimate, digit=4)),
             size=0.75, color="steelblue") +
  coord_flip() +
  theme_minimal(base_size=13, base_family="DIN") +
  labs(x="Provinsi", y="Koefisien Kemiringan",
       title="Rata-rata Pertumbuhan IPM di Indonesia",
       subtitle="Per Provinsi (Tahun 2010 - 2016)",
       caption="Sumber data: BPS") +
  scale_linetype_manual("Nasional", values=2) +
  theme(panel.grid.minor=element_blank(),
        legend.justification=c(1,0),
        legend.position=c(1,0))

ggsave("figures/ipm-reg-slope.png", width=10, height=8)
