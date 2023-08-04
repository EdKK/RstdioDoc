library(ggstatsplot)
library(BlandAltmanLeh)
library(ggplot2)
library(tidyverse)
library(readxl)
library(writexl)
library(gridExtra)
library(grid)
library(ggpubr)
library(ggfortify)


####################### carrelação Velocidade CM e Velocidad L5

# Patil, I. (2021). Visualizations with statistical details:
#   The 'ggstatsplot' approach. Journal of Open Source Software,
# 6(61), 3167, doi:10.21105/joss.03167
# Hinkle DE, Wiersma W, Jurs SG. Applied Statistics for the Behavioral Sciences. 5th ed. Boston: Houghton Mifflin; 2003.
# 
# Jeffreys, H. 1961. Theory of Probability. 3rd ed. Oxford: Oxford University Press.
# 
# Raiola, Gaetano & Di tore, Pio. (2012). Statistical study on bodily communication skills in volleyball to improve teaching methods. Journal of Human Sport and Exercise. 7. 10.4100/jhse.2012.72.12.
# 


dados_ex1 <- read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/dados_ex1_compilados_limpo.xlsx")

glimpse(dados_ex1)


dados_ex1_cor <- dados_ex1%>%
  filter(dia == 2) %>% 
  mutate(grupo = as_factor(grupo)) %>%
  select(grupo, Pico_de_velocidade, vel_pico_kinov)

glimpse(dados_ex1_cor)

r1 <- cor(dados_ex1_cor$Pico_de_velocidade, dados_ex1_cor$vel_pico_kinov, method = "spearman")^2

ggscatterstats(
  data = dados_ex1_cor,
  x = Pico_de_velocidade,
  y = vel_pico_kinov,
  type = "np",
  xlab = "Pico de velocidade CM (m/s)",
  ylab = "Pico de velocidade L5 (m/s)",
  point.label.args = list(alpha = 0.7, size = 4, color = "black"),
  smooth.line.args = list(linewidth = 1.0, color = "black", method = "lm", formula = y ~ x),
  marginal = F
 ) +
  # adiciona o coeficiente de determinação (r2) ao gráfico
  annotate(
    "text",
    x = min(dados_ex1_cor$Pico_de_velocidade) + 0.1,
    y = max(dados_ex1_cor$vel_pico_kinov) - 0.1,
    label = paste0("R² = ", round(r1, 2)),
    size = 5
  ) +
  # personaliza a fonte, tamanho e peso dos eixos
  theme(
    axis.title = element_text(face = "bold", size = 14, family = "Arial"),
    axis.line.x = element_line(size = 0.5, color = "black"),
    axis.line.y = element_line(size = 0.5, color = "black"),
    axis.text = element_text(size = 14, family = "Arial", color = "black"),
    panel.grid = element_blank(),
  )




##################################  Gráfico de Bland-Altman

stats2 <- bland.altman.stats(dados_ex1_cor$vel_pico_kinov, dados_ex1_cor$Pico_de_velocidade)

plot2 <- bland.altman.plot(dados_ex1_cor$vel_pico_kinov, dados_ex1_cor$Pico_de_velocidade, graph.sys = "ggplot2")

# # Adicionar as linhas horizontais que representam as estatísticas de concordância
plot2 +
  geom_point(size = 2.5) + # Aqui está o argumento size para aumentar o tamanho dos pontos
  geom_hline(yintercept = stats2$mean.diff, linetype = "dashed", color = "red",
             show.legend = TRUE, aes(linetype = "Média das diferenças"), size = 1) +
  labs(x = "Média da velocidade (m/s)", y = "Diferença entre as velocidades (m/s)",
       linetype = "Legenda") +
  theme(axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 14, family = "Arial", color = "black"),
        axis.title = element_text(size = 14, family = "Arial", color = "black"),
        plot.title = element_text(size = 14, family = "Arial", color = "black"),
        plot.subtitle = element_text(size = 14, family = "Arial", color = "black"))

# 
# ####################################################
# 
# 
# ###################################################  BOTECAE
# 
 dados_cor_ex2 <- read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/dados_ex2_compildados_limpo.xlsx")
# 
# 
 glimpse(dados_cor_ex2)
# 
# 
dados_cor_ex2 <- dados_cor_ex2%>%
 filter(dia == 2) %>% 
   mutate(grupo = as_factor(grupo)) %>%
   select(grupo, Pico_de_velocidade , vel_pico_kinov, Pico_de_velocidade_2)
 

byf.shapiro(Pico_de_velocidade_2 ~ grupo, dados_cor_ex2)
leveneTest(Pico_de_velocidade_2 ~ grupo, dados_cor_ex2, center=mean)

 glimpse(dados_cor_ex2)
 
 r2 <- cor(dados_cor_ex2$Pico_de_velocidade_2, dados_cor_ex2$vel_pico_kinov, method = "spearman")^2
 
 ggscatterstats(
   data = dados_cor_ex2,
   x = Pico_de_velocidade_2,
   y = vel_pico_kinov,
   type = "np",
   xlab = "Pico de velocidade  CM (m/s)",
   ylab = "Pico de velocidade  L5 (m/s)",
   point.label.args = list(alpha = 0.7, size = 4, color = "black"),
   smooth.line.args = list(linewidth = 1.0, color = "black", method = "lm", formula = y ~ x),
   marginal = F
 ) +
   # adiciona o coeficiente de determinação (r2) ao gráfico
   annotate(
     "text",
     x = min(dados_cor_ex2$Pico_de_velocidade) + 0.1,
     y = max(dados_cor_ex2$vel_pico_kinov) - 0.1,
     label = paste0("R² = ", round(r2, 2)),
     size = 5
   ) +
   # personaliza a fonte, tamanho e peso dos eixos
   theme(
     axis.title = element_text(face = "bold", size = 14, family = "Arial"),
     axis.line.x = element_line(size = 0.5, color = "black"),
     axis.line.y = element_line(size = 0.5, color = "black"),
     axis.text = element_text(size = 14, family = "Arial", color = "black"),
     panel.grid = element_blank(),
   )
 
 
 
 
 ##################################  Gráfico de Bland-Altman
 
 stats_ex2 <- bland.altman.stats(dados_cor_ex2$vel_pico_kinov, dados_cor_ex2$Pico_de_velocidade_2)
 
 plot_ex2 <- bland.altman.plot(dados_cor_ex2$vel_pico_kinov, dados_cor_ex2$Pico_de_velocidade_2, graph.sys = "ggplot2")
 
 # # Adicionar as linhas horizontais que representam as estatísticas de concordância
 plot_ex2+
   geom_point(size = 2.5) + # Aqui está o argumento size para aumentar o tamanho dos pontos
   geom_hline(yintercept = stats_ex2$mean.diff, linetype = "dashed", color = "red",
              show.legend = TRUE, aes(linetype = "Média das diferenças"), size = 1) +
   labs(x = "Média da velocidade (m/s)", y = "Diferença entre as velocidades (m/s)",
        linetype = "Legenda") +
   theme(axis.line = element_line(color = "black"),
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         axis.text = element_text(size = 14, family = "Arial", color = "black"),
         axis.title = element_text(size = 14, family = "Arial", color = "black"),
         plot.title = element_text(size = 14, family = "Arial", color = "black"),
         plot.subtitle = element_text(size = 14, family = "Arial", color = "black"))