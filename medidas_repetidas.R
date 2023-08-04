
library(tidyverse)
library(readxl)
library(writexl)
library(patchwork)
library(RVAideMemoire)
library(car)
library(gtsummary)
library(knitr)
library(effsize)
library(ggstatsplot)
library(irr)
library(ICC)
library(ggpubr)
library(ez)
library(rstatix)
library(outliers)
library(ggsignif)
library(easystats)
citation("rstatix")
citation("easystats")
citation("tidyverse")

#######################################              BOTECON      ##############################




dados_con_tot <- read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo 2/dados/Cinética/con_tot.xlsx")


dados_cae_tot <- read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo 2/dados/Cinética/cae_tot.xlsx")




##############################                     IMPULSO
##Análise descritiva dos dados (pacote: rstatix)


dados_con_limp %>%
  group_by(intencidade) %>%
  get_summary_stats(Impulso, type = c("common")) %>%
  write_xlsx("tabela_ex1_Impulso.xlsx")



# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_pot,
  x    = intencidade, 
  y    = pic_pot,
  type = "p",
  #grouping.var = bote,
  centrality.plotting = T,
  p.adjust.method = "bonferroni",
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0.3, color = "black"),
  violin.args = list(width = 1, alpha = 0.2, color = "black"),
  xlab = "Resistência externa (%1RM) ",            # define o nome do eixo x
  ylab = " Impulso (N.s)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_con_limp$Impulso), by = 20)  # define os pontos na escala do eixo y
 ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
    )

  
##################################
##############################                     Pico_de_potência_normalizdado


##Análise descritiva dos dados (pacote: rstatix)


dados_con_limp %>%
  group_by(intencidade) %>%
  get_summary_stats(Pico_de_potência_normalizdado, type = c("common")) %>%
  write_xlsx("tabela_ex1_Pico_de_potência.xlsx")



# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167



ggwithinstats(
  data = dados_con_limp,
  x = intencidade, 
  y = Pico_de_potência_normalizdado,
  type = "p",
  centrality.plotting = FALSE,
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  k = 3L,
  effsize.type = "eta",
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  xlab = "Resistência externa (%1RM)",            
  ylab = "Pico de potência (W/kg)",
  violin.args = list( alpha = 0.0, fill = "blue", color = "white") 
  ) +
  #  # Adicione o gráfico de boxplot
  geom_point(size = 3, alpha = 1, color = "black") +  # Adicione o gráfico de pontos acima do boxplot
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),
    panel.grid = element_blank()
  )

############################BoteCAE

glimpse(dados_cae_limp)

ggwithinstats(
  data = dados_cae_limp,
  x = intensidade, 
  y = Pico_de_potência_normalizdado,
  type = "p",
  centrality.plotting = FALSE,
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  k = 4L,
  effsize.type = "eta",
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  xlab = "Resistência externa (%1RM)",            
  ylab = "Pico de potência (W/kg)",
  violin.args = list( alpha = 0.0, fill = "blue", color = "white") 
) +
  #  # Adicione o gráfico de boxplot
  geom_point(size = 3, alpha = 1, color = "black") +  # Adicione o gráfico de pontos acima do boxplot
  scale_y_continuous(breaks = seq(0, 20, by = 4,limits = c(0, 20))) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),
    panel.grid = element_blank()
  )



























#########################  Potência_média_norma      #########################


glimpse(dados_con_limp)

##Análise descritiva dos dados (pacote: rstatix)


dados_con_limp %>%
  group_by(intencidade) %>%
  get_summary_stats(Potência_média_norma, type = c("common")) %>%
  write_xlsx("tabela_ex1_potência_media.xlsx")



# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_con_limp,
  x    = intencidade, 
  y    = Potência_média_norma,
  type = "np",
  centrality.plotting = F,
  p.adjust.method = "bonferroni",
  outlier.tagging = F,
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0.3, color = "black"),
  violin.args = list(width = 1, alpha = 0.2, color = "black"),
  xlab = "Resistência externa (%1RM)",            # define o nome do eixo x
  ylab = "Potência média (W/kg)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_con_limp$Potência_média_norma), by = 4)  # define os pontos na escala do eixo y
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
  )



####################################### Pico_de_velocidade 
glimpse(dados_con_limp)

##Análise descritiva dos dados (pacote: rstatix)


dados_con_limp %>%
  group_by(intencidade) %>%
  get_summary_stats(Pico_de_velocidade , type = c("common")) %>%
  write_xlsx("tabela_ex1_Pico_de_velocidade .xlsx")



# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_con_limp,
  x = intencidade, 
  y = Pico_de_força_direita,
  type = "p",
  centrality.plotting = FALSE,
  pairwise.comparisons = TRUE,
  p.adjust.method = "bonferroni",
  k = 3L,
  effsize.type = "eta",
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  xlab = "Resistência externa (%1RM)",            
  ylab = "Pico de velocidade CM (m/s)",
  violin.args = list( alpha = 0.0, fill = "blue", color = "white") 
) +
  #  # Adicione o gráfico de boxplot
  geom_point(size = 3, alpha = 1, color = "black") +  # Adicione o gráfico de pontos acima do boxplot
  scale_y_continuous(breaks = seq(0, 16, by = 2)) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),
    panel.grid = element_blank()
  )

############################################################################

######################### ################## BOTECAE      #########################

dados_cae <- read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo 2/dados/Cinética/dados_ex2_compildados_limpo.xlsx")

glimpse(dados_cae)

novos_niveis <- c("0%1RM", "5%1RM", "10%1RM", "20%1RM", "30%1RM")


novos_niveis_num <- c("0", "5", "10", "20", "30")

dados_cae_limp <- dados_cae%>% 
  mutate(intensidade = factor(intensidade, levels = unique(intensidade), labels = novos_niveis))%>%
  mutate(intensidade_num = factor(intensidade, levels = unique(intensidade), labels = novos_niveis_num))%>%
  mutate(id = as_factor(id))%>%
  mutate(dia = as_factor(dia)) %>% 
  select(id, dia, intensidade, Pico_de_potência_normalizdado, Potência_média_normal, pico_vel, Impulso, Lastro, Pico_de_força_direita, Pico_de_força_esquerda)

glimpse(dados_cae_limp)





##############################                     IMPULSO
##Análise descritiva dos dados (pacote: rstatix)


dados_cae_limp %>%
  group_by(intensidade) %>%
  get_summary_stats(Impulso, type = c("common")) %>%
  write_xlsx("tabela_ex2_Impulso.xlsx")


# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_cae_limp,
  x    = intensidade, 
  y    = Impulso,
  type = "np",
  centrality.plotting = F,
  p.adjust.method = "bonferroni",
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0.3, color = "black"),
  violin.args = list(width = 1, alpha = 0.2, color = "black"),
  xlab = "Resistência externa (%1RM) ",            # define o nome do eixo x
  ylab = " Impulso (N.s)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_cae_limp$Impulso), by = 20)  # define os pontos na escala do eixo y
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
  )


##################################
##############################                     Pico_de_potência_normalizdado


##Análise descritiva dos dados (pacote: rstatix)


dados_cae_limp %>%
  group_by(intensidade) %>%
  get_summary_stats(Pico_de_potência_normalizdado, type = c("common")) %>%
  write_xlsx("tabela_ex2_Pico_de_potência.xlsx")



# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_cae_limp,
  x    = intensidade, 
  y    = Pico_de_potência_normalizdado,
  type = "p",
  centrality.plotting = F,
  p.adjust.method = "bonferroni",
  outlier.tagging = F,
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0.3, color = "black"),
  violin.args = list(width = 1, alpha = 0.2, color = "black"),
  xlab = "Resistência externa (%1RM)",            # define o nome do eixo x
  ylab = " Pico de potência (W/kg)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_cae_limp$Pico_de_potência_normalizdado), by = 2)  # define os pontos na escala do eixo y
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
  )


#########################  Potência_média_norma      #########################


glimpse(dados_cae_limp)

##Análise descritiva dos dados (pacote: rstatix)


dados_cae_limp %>%
  group_by(intensidade) %>%
  get_summary_stats(Potência_média_normal , type = c("common")) %>%
  write_xlsx("tabela_ex2_potência_media.xlsx")



# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_cae_limp,
  x    = intensidade, 
  y    = Potência_média_normal ,
  type = "np",
  centrality.plotting = F,
  p.adjust.method = "bonferroni",
  outlier.tagging = F,
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0.3, color = "black"),
  violin.args = list(width = 1, alpha = 0.2, color = "black"),
  xlab = "Resistência externa (%1RM)",            # define o nome do eixo x
  ylab = "Potência média (W/kg)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_cae_limp$Potência_média_normal ), by = 2)  # define os pontos na escala do eixo y
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
  )



####################################### Pico_de_velocidade 
glimpse(dados_cae_limp)

##Análise descritiva dos dados (pacote: rstatix)





# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_cae_limp,
  x    = intensidade, 
  y    = pico_vel  ,
  type = "np",
  centrality.plotting = F,
  p.adjust.method = "bonferroni",
  outlier.tagging = F,
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0.3, color = "black"),
  violin.args = list(width = 1, alpha = 0.2, color = "black"),
  xlab = "Resistência externa (%1RM)",            # define o nome do eixo x
  ylab = " Pico de velocidade CM (m/s)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_cae_limp$pico_vel  ), by = 0.4)  # define os pontos na escala do eixo y
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
  )


####################################### Lastro


##Análise descritiva dos dados (pacote: rstatix)


dados_limp_lastro <- dados_con_limp %>%
  select(id, intencidade, Lastro_) %>% 
  filter(intencidade != "0%1RM")
  
  write_xlsx("tabela_lastro .xlsx")

glimpse(dados_limp_lastro)


# Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot'
# approach. Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

ggwithinstats(
  data = dados_limp_lastro,
  x    = intencidade, 
  y    = Lastro_ ,
  type = "np",
  centrality.plotting = F,
  p.adjust.method = "bonferroni",
  outlier.tagging = F,
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0, color = "black"),
  violin.args = list(width = 0.0, alpha = 0.0 ),
  xlab = "Resistência externa ",            # define o nome do eixo x
  ylab = "Lastro (kg)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_limp_lastro$Lastro_  ), by = 2)  # define os pontos na escala do eixo y
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
  )

##################################################TOTAL


##forçaCON

glimpse(dados_con_tot)

ggwithinstats(
  data = dados_con_tot,
  x    = intencidade, 
  y    = `pic-_for_tot`,
  type = "p",
  centrality.plotting = F,
  p.adjust.method = "bonferroni",
  outlier.tagging = F,
  point.args = list(size = 3, alpha = 1, color = "black"),
  ggsignif.args = list(textsize = 4, tip_length = 0.02, step_increase = 0.1),
  boxplot.args = list(width = 0.3, alpha = 0.3, color = "black"),
  violin.args = list(width = 1, alpha = 0.2, color = "black"),
  xlab = "Resistência externa (%1RM)",            # define o nome do eixo x
  ylab = " Pico de potência (W/kg)",            # define o nome do eixo y
) +
  scale_y_continuous(
    # ajusta os limites do eixo y
    breaks = seq(0, max(dados_cae_limp$Pico_de_potência_normalizdado), by = 2)  # define os pontos na escala do eixo y
  ) +
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black", face = "bold"),  # adiciona o negrito para os valores e legendas dos eixos
    panel.grid = element_blank(),
    
  )

