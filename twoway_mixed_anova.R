
#https://www.datanovia.com/en/lessons/mixed-anova-in-r/

library(tidyverse)
library(ggpubr)
library(rstatix)

dados_con_cae %>%
  group_by(Bote, grupo) %>%
  get_summary_stats(pico_total, type = "mean_sd")


bxp <- ggboxplot(
  dados_con_cae, x = "Bote", y = "pico_total",
  color = "grupo", palette = "jco"
)
bxp


dados_con_cae %>%
  group_by(Bote, grupo) %>%
  identify_outliers(pico_total)




ggqqplot(dados_con_cae, "pico_total", ggtheme = theme_bw()) +
  facet_grid(Bote ~ grupo)



box_m(dados_con_cae[, "pico_total", drop = FALSE], dados_con_cae$grupo)


#Two-way mixed ANOVA test

res.aov <- anova_test(
  data = dados_con_cae,
  dv = vel_pico_kinov,
  wid = participante,
  between = grupo,
  within = Bote,
  type = 3,
  detailed = T,
)
get_anova_table(res.aov)


#################################################### IMPULSO


dados_con_cae %>%
  group_by(Bote, grupo) %>%
  get_summary_stats(Impulso, type = "mean_sd")


res.aov_imp <- anova_test(
  data = dados_con_cae,
  dv = Impulso,
  wid = participante,
  between = grupo,
  within = Bote,
  type = 3,
  detailed = T
)
get_anova_table(res.aov_imp)

