
library(tidyverse)
library(readxl)
library(irr)
library(ICC)
library(ggstatsplot) # for box-plots with statistics
library(rstatix)
library(psych)
library(reshape2)
library(ez)
library(rstatix)
library(easystats)

################################################   BOTECON    ###################################

####IMPULSO
conf_ex1_imp <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex1_imp.xlsx")

glimpse(impulso_D1)

icc_conf_ex1_imp <- icc(conf_ex1_imp[, 3:4], model = "twoway", type = "agreement", unit = "single")


##Reestruturando o banco de dados  para calculo do EPM
conf_ex1_imp_epm <- conf_ex1_imp %>% 
  select(participante,impulso_D1,impulso_D2) %>% 
  mutate(participante = as_factor(participante))

glimpse(conf_ex1_imp_epm)



conf_ex1_imp_epm_lim <- melt(conf_ex1_imp_epm,
                id = "conf_ex1_imp_epm_lim",
                measured = c("impulso_D1", "impulso_D2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_imp_epm_lim <- conf_ex1_imp_epm_lim %>%
  arrange(conf_ex1_imp_epm_lim)

glimpse(conf_ex1_imp_epm_lim)

#criando modelo anova
mod.ANOVA_ex1_imp <- ezANOVA(data = conf_ex1_imp_epm_lim,
                             dv = value,
                             wid = participante,
                             within = variable,
                             detailed = TRUE,
                             type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_ex1_imp 

################################### pico força total Botecon


conf_ex1_pic_tot <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex1_pico_total.xlsx")
glimpse(conf_ex1_pic_tot)
attach()


icc_ex1_pic_tot <- ICC(conf_ex1_pic_tot, missing = T, alpha = .05)

################################### pico força total Botecon


conf_ex2_pic_tot <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex2_pico_total.xlsx")
glimpse(conf_ex2_pic_tot)
attach()


icc_ex1_pic_tot <- ICC(conf_ex2_pic_tot, missing = T, alpha = .05)


##Reestruturando o banco de dados  para calculo do EPM

conf_ex1_pic_forDIR_epm <- conf_ex1_pic_for %>% 
  select(participante,Pico_de_força_direita, Pico_de_força_direita_D2) %>% 
  mutate(participante = as_factor(participante))

glimpse(conf_ex1_pic_forDIR_epm)


conf_ex1_pic_forDIR_epm_lim <- melt(conf_ex1_pic_forDIR_epm,
                                    id = "participante",
                                    measured = c("Pico_de_força_direita", "Pico_de_força_direita_D2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_pic_forDIR_epm_lim  <- conf_ex1_pic_forDIR_epm_lim  %>%
  arrange(conf_ex1_pic_forDIR_epm_lim)

glimpse(conf_ex1_pic_forDIR_epm_lim)



#criando modelo anova
mod.ANOVA_conf_ex1_pic_forDIR_epm_lim <- ezANOVA(data = conf_ex1_pic_forDIR_epm_lim,
                                                 dv = value,
                                                 wid = participante,
                                                 within = variable,
                                                 detailed = TRUE,
                                                 type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex1_pic_forDIR_epm_lim




####PICO DE FORÇA DIREIA ESQUERDA 

conf_ex1_pic_for <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex1_pico_força.xlsx")
glimpse(conf_ex1_pic_for)
 
icc_ex1_pic_for_dir <- icc(conf_ex1_pic_for[, 2:3], model = "twoway", type = "agreement", unit = "single")

##Reestruturando o banco de dados  para calculo do EPM

conf_ex1_pic_forDIR_epm <- conf_ex1_pic_for %>% 
  select(participante,Pico_de_força_direita, Pico_de_força_direita_D2) %>% 
  mutate(participante = as_factor(participante))

glimpse(conf_ex1_pic_forDIR_epm)


conf_ex1_pic_forDIR_epm_lim <- melt(conf_ex1_pic_forDIR_epm,
                             id = "participante",
                             measured = c("Pico_de_força_direita", "Pico_de_força_direita_D2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_pic_forDIR_epm_lim  <- conf_ex1_pic_forDIR_epm_lim  %>%
  arrange(conf_ex1_pic_forDIR_epm_lim)

glimpse(conf_ex1_pic_forDIR_epm_lim)



#criando modelo anova
mod.ANOVA_conf_ex1_pic_forDIR_epm_lim <- ezANOVA(data = conf_ex1_pic_forDIR_epm_lim,
                                          dv = value,
                                          wid = participante,
                                          within = variable,
                                          detailed = TRUE,
                                          type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex1_pic_forDIR_epm_lim



###ESQUERDA


icc_ex1_pic_for_esq <- icc(conf_ex1_pic_for[, 4:5], model = "twoway", type = "agreement", unit = "single")

##Reestruturando o banco de dados  para calculo do EPM

conf_ex1_pic_forESQ_epm <- conf_ex1_pic_for %>% 
  select(participante,Pico_de_força_esquerda,Pico_de_força_esquerda_D2) %>% 
  mutate(participante = as_factor(participante))


conf_ex1_pic_forESQ_epm_lim <- melt(conf_ex1_pic_forESQ_epm,
                             id = "participante",
                             measured = c("Pico_de_força_esquerda ", "Pico_de_força_esquerda_D2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_pic_forESQ_epm_lim <- conf_ex1_pic_forESQ_epm_lim %>%
  arrange(conf_ex1_pic_forESQ_epm_lim)

glimpse(conf_ex1_pic_forESQ_epm_lim)



#criando modelo anova
mod.ANOVA_ex1_pic_forESQ_epm_lim <- ezANOVA(data = conf_ex1_pic_forESQ_epm_lim,
                             dv = value,
                             wid = participante,
                             within = variable,
                             detailed = TRUE,
                             type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_ex1_pic_forESQ_epm_lim


######## Pico de Potência

conf_ex1_pic_pot <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex1_pot_pic.xlsx")
glimpse(conf_ex1_pic_pot)

icc_conf_ex1_pic_pot <- icc(conf_ex1_pic_pot[, 3:4], model = "twoway", type = "agreement", unit = "single")

conf_ex1_pic_pot_epm <- conf_ex1_pic_pot %>% 
  select(participante,Pico_de_potência_normalizdado_d1,Pico_de_potência_normalizdado_d2) %>% 
  mutate(participante = as_factor(participante))

glimpse(conf_ex1_imp_epm)


##Reestruturando o banco de dados  para calculo do EPM

conf_ex1_pic_pot_epm_lim <- melt(conf_ex1_pic_pot_epm,
                                 id = "participante",
                                 measured = c("Pico_de_potência_normalizdado_d1", "Pico_de_potência_normalizdado_d1"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_pic_pot_epm_lim  <- conf_ex1_pic_pot_epm_lim  %>%
  arrange(conf_ex1_pic_pot_epm_lim )

glimpse(conf_ex1_pic_pot_epm_lim )



#criando modelo anova
mod.ANOVA_ex1_pic_pot_epm <- ezANOVA(data = conf_ex1_pic_pot_epm_lim ,
                             dv = value,
                             wid = participante,
                             within = variable,
                             detailed = TRUE,
                             type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_ex1_pic_pot_epm 

####### Alcançe maximo 

conf_ex1_alac <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex1_alcac.xlsx")
glimpse(conf_ex1_alac)

icc_conf_ex1_alac <- icc(conf_ex1_alac[, 3:4], model = "twoway", type = "agreement", unit = "single")


##Reestruturando o banco de dados  para calculo do EPM

conf_ex1_alac_epm <- conf_ex1_alac %>% 
  select(participante,Alcançe_con_d1, Alcançe_con_d2) %>% 
  mutate(participante = as_factor(participante))

glimpse(conf_ex1_alac_epm)


conf_ex1_alac_epm_lim <- melt(conf_ex1_alac_epm,
                                 id = "participante",
                                 measured = c("Alcançe_con_d1", "Alcançe_con_d2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_alac_epm_lim  <- conf_ex1_alac_epm_lim  %>%
  arrange(conf_ex1_alac_epm_lim )

glimpse(conf_ex1_alac_epm_lim )



#criando modelo anova
mod.ANOVA_conf_ex1_alac_epm_lim <- ezANOVA(data = conf_ex1_alac_epm_lim,
                                     dv = value,
                                     wid = participante,
                                     within = variable,
                                     detailed = TRUE,
                                     type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex1_alac_epm_lim

####### deslocamento CM

conf_ex1_desl <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex1_desloc.xlsx")
glimpse(conf_ex1_desl)

icc_conf_ex1_alac <- icc(conf_ex1_desl[, 3:4], model = "twoway", type = "agreement", unit = "single")

##Reestruturando o banco de dados  para calculo do EPM

conf_ex1_desl_epm <- conf_ex1_desl %>% 
  select(participante,Desloc_vert_d1, Desloc_vert_d2) %>% 
  mutate(participante = as_factor(participante))

glimpse(conf_ex1_alac_epm)


conf_ex1_desl_epm_lim <- melt(conf_ex1_desl_epm,
                              id = "participante",
                              measured = c("Alcançe_con_d1", "Alcançe_con_d2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_desl_epm_lim  <- conf_ex1_desl_epm_lim  %>%
  arrange(conf_ex1_desl_epm_lim )

glimpse(conf_ex1_desl_epm_lim )



#criando modelo anova
mod.ANOVA_conf_ex1_desl_epm_lim <- ezANOVA(data = conf_ex1_desl_epm_lim,
                                           dv = value,
                                           wid = participante,
                                           within = variable,
                                           detailed = TRUE,
                                           type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex1_desl_epm_lim

###### pico de velocidade

conf_ex1_vel <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex1_vel_pic.xlsx")
glimpse(conf_ex1_vel )

icc_conf_ex1_vel  <- icc(conf_ex1_vel [, 3:4], model = "twoway", type = "agreement", unit = "single")

#icc_conf_ex1_vel4  <- icc(conf_ex1_vel [, 5:6], model = "twoway", type = "agreement", unit = "single")


##Reestruturando o banco de dados  para calculo do EPM

conf_ex1_vel_epm <- conf_ex1_vel %>% 
  select(participante,Pico_de_velocidade_d1, Pico_de_velocidade_d2) %>% 
  mutate(participante = as_factor(participante))

glimpse(conf_ex1_vel_epm)


conf_ex1_vel_epm_lim <- melt(conf_ex1_vel_epm,
                              id = "participante",
                              measured = c("Pico_de_velocidade_d1", "Pico_de_velocidade_d2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex1_vel_epm_lim  <- conf_ex1_vel_epm_lim  %>%
  arrange(conf_ex1_vel_epm_lim)

glimpse(conf_ex1_vel_epm_lim)



#criando modelo anova
mod.ANOVA_conf_ex1_vel_epm_lim <- ezANOVA(data = conf_ex1_vel_epm_lim,
                                           dv = value,
                                           wid = participante,
                                           within = variable,
                                           detailed = TRUE,
                                           type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex1_vel_epm_lim

 
################################################   BOTECAE      ########################



####IMPULSO
conf_ex2_imp <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex2_imp.xlsx")

glimpse(conf_ex2_imp)

icc_conf_ex2_imp <- icc(conf_ex2_imp[, 3:4], model = "twoway", type = "agreement", unit = "single")

##Reestruturando o banco de dados  para calculo do EPM


conf_conf_ex2_imp_epm <- conf_ex2_imp %>% 
  select(id,Impulso_D1,Impulso_D2) %>% 
  mutate(id = as_factor(id))

glimpse(conf_conf_ex2_imp_epm)



conf_conf_ex2_imp_epm_lim <- melt(conf_conf_ex2_imp_epm,
                             id = "id",
                             measured = c("Impulso_D1", "Impulso_D2"))

# Ordenando as colunas pelo sujeito experimental
conf_conf_ex2_imp_epm_lim <- conf_conf_ex2_imp_epm_lim %>%
  arrange(conf_conf_ex2_imp_epm_lim)

glimpse(conf_conf_ex2_imp_epm_lim)

#criando modelo anova
mod.ANOVA_ex2_imp_epm_lim <- ezANOVA(data = conf_conf_ex2_imp_epm_lim,
                             dv = value,
                             wid = id,
                             within = variable,
                             detailed = TRUE,
                             type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_ex2_imp_epm_lim 




####PICO DE FORÇA DIREIA

conf_ex2_pic_for <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex2_pico_força.xlsx")
glimpse(conf_ex2_pic_for)

icc_ex2_pic_for_dir <- icc(conf_ex2_pic_for[, 3:4], model = "twoway", type = "agreement", unit = "single")
##Reestruturando o banco de dados  para calculo do EPM


conf_ex2_pic_for_fir_epm <- conf_ex2_pic_for %>% 
  select(id,Pico_de_força_direita_1, Pico_de_força_direita_2) %>% 
  mutate(id = as_factor(id))

glimpse(conf_ex2_pic_for_fir_epm)



conf_ex2_pic_for_fir_epmlim <- melt(conf_ex2_pic_for_fir_epm,
                                  id = "id",
                                  measured = c("Pico_de_força_direita_1", "Pico_de_força_direita_2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex2_pic_for_fir_epmlim <- conf_ex2_pic_for_fir_epmlim %>%
  arrange(conf_ex2_pic_for_fir_epmlim)

glimpse(conf_ex2_pic_for_fir_epmlim)

#criando modelo anova
mod.ANOVA_conf_ex2_pic_for_fir_epmlim <- ezANOVA(data = conf_ex2_pic_for_fir_epmlim,
                                     dv = value,
                                     wid = id,
                                     within = variable,
                                     detailed = TRUE,
                                     type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex2_pic_for_fir_epmlim 





################ESQUERDA


icc_ex2_pic_for_esq <- icc(conf_ex2_pic_for[, 5:6], model = "twoway", type = "agreement", unit = "single")

##Reestruturando o banco de dados  para calculo do EPM


conf_ex2_pic_for_esq_epm <- conf_ex2_pic_for %>% 
  select(id,Pico_de_força_esquerda_d1,Pico_de_força_esquerda_d2) %>% 
  mutate(id = as_factor(id))

glimpse(conf_ex2_pic_for_esq_epm)



conf_ex2_pic_for_esq_epmlim <- melt(conf_ex2_pic_for_esq_epm,
                                  id = "id",
                                  measured = c("Pico_de_força_esquerda_d1", "Pico_de_força_esquerda_d2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex2_pic_for_esq_epmlim  <- conf_ex2_pic_for_esq_epmlim  %>%
  arrange(conf_ex2_pic_for_esq_epmlim)

glimpse(conf_ex2_pic_for_esq_epmlim)

#criando modelo anova
mod.ANOVA_conf_ex2_pic_for_esq_epmlim  <- ezANOVA(data = conf_ex2_pic_for_esq_epmlim ,
                                     dv = value,
                                     wid = id,
                                     within = variable,
                                     detailed = TRUE,
                                     type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex2_pic_for_esq_epmlim  




######## Pico de Potência

conf_ex2_pic_pot <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex2_pot_pic.xlsx")
glimpse(conf_ex2_pic_pot)

icc_conf_ex2_pic_pot <- icc(conf_ex2_pic_pot[, 3:4], model = "twoway", type = "agreement", unit = "single")


##Reestruturando o banco de dados  para calculo do EPM


conf_ex2_pic_pot_epm <- conf_ex2_pic_pot %>% 
  select(id,Pico_de_potência_normalizdado_d1,Pico_de_potência_normalizdado_d2) %>% 
  mutate(id = as_factor(id))

glimpse(conf_ex2_pic_pot_epm)



conf_ex2_pic_pot_epm_lim <- melt(conf_ex2_pic_pot_epm,
                                  id = "id",
                                  measured = c("Pico_de_potência_normalizdado_d1", "Pico_de_potência_normalizdado_d22"))

# Ordenando as colunas pelo sujeito experimental
conf_ex2_pic_pot_epm_lim <- conf_ex2_pic_pot_epm_lim %>%
  arrange(conf_ex2_pic_pot_epm_lim)

glimpse(conf_ex2_pic_pot_epm_lim)

#criando modelo anova
mod.ANOVA_conf_ex2_pic_pot_epm_lim <- ezANOVA(data = conf_ex2_pic_pot_epm_lim,
                                     dv = value,
                                     wid = id,
                                     within = variable,
                                     detailed = TRUE,
                                     type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex2_pic_pot_epm_lim 








####### Alcançe maximo 

conf_ex2_alac <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex2_alcac.xlsx")
glimpse(conf_ex2_alac)

icc_conf_ex2_alac <- icc(conf_ex2_alac[, 3:4], model = "twoway", type = "agreement", unit = "single")


##Reestruturando o banco de dados  para calculo do EPM


conf_ex2_alac_epm <- conf_ex2_alac %>% 
  select(id,alcance_d1,alcance_d2) %>% 
  mutate(id = as_factor(id))

glimpse(conf_ex2_alac_epm)



conf_ex2_alac_epm_lim <- melt(conf_ex2_alac_epm,
                                 id = "id",
                                 measured = c(" alcance_d1", " alcance_d2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex2_alac_epm_lim <- conf_ex2_alac_epm_lim %>%
  arrange(conf_ex2_alac_epm_lim)

glimpse(conf_ex2_alac_epm_lim)

#criando modelo anova
mod.ANOVA_conf_ex2_alac_epm_lim <- ezANOVA(data = conf_ex2_alac_epm_lim,
                                              dv = value,
                                              wid = id,
                                              within = variable,
                                              detailed = TRUE,
                                              type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex2_alac_epm_lim






####### deslocamento CM

conf_ex2_desl <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex2_desloc.xlsx")
glimpse(conf_ex2_desl)

icc_conf_ex2_alac <- icc(conf_ex2_desl[, 3:4], model = "twoway", type = "agreement", unit = "single")



##Reestruturando o banco de dados  para calculo do EPM


conf_ex2_deslepm <- conf_ex2_desl %>% 
  select(id,Desloc_vert_d1, Desloc_vert_d2) %>% 
  mutate(id = as_factor(id))

glimpse(conf_ex2_deslepm)



conf_ex2_deslepm_lim <- melt(conf_ex2_deslepm,
                              id = "id",
                              measured = c("Desloc_vert_d1", "Desloc_vert_d2"))

# Ordenando as colunas pelo sujeito experimental
conf_ex2_deslepm_lim  <- conf_ex2_deslepm_lim  %>%
  arrange(conf_ex2_deslepm_lim )

glimpse(conf_ex2_deslepm_lim  )

#criando modelo anova
mod.ANOVA_conf_ex2_deslepm_lim  <- ezANOVA(data = conf_ex2_deslepm_lim ,
                                           dv = value,
                                           wid = id,
                                           within = variable,
                                           detailed = TRUE,
                                           type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_ex2_deslepm_lim 






###### pico de velocidade
conf_ex2_vel <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_ex2_vel_pic.xlsx")
glimpse(conf_ex2_vel )

icc_conf_ex2_vel  <- icc(conf_ex2_vel [, 3:4], model = "twoway", type = "agreement", unit = "single")

#icc_conf_ex2_vel4  <- icc(conf_ex2_vel [, 5:6], model = "twoway", type = "agreement", unit = "single")





################################################### TESTE DE 1RM    ########################

conf_rm <-read_xlsx("C:/Users/Edgardo/OneDrive/Doutorado/DadosTese1/Resultados DOC/Estudo_1/dados/conf_rm.xlsx")
glimpse(conf_rm)


icc_ex1_rm <- icc(conf_rm[, 4:5], model = "twoway", type = "agreement", unit = "single")


##Reestruturando o banco de dados  para calculo do EPM


conf_rm_epm <- conf_rm  %>% 
  select(id, rm_d1,  rm_d2) %>% 
  mutate(id= as_factor(id))

glimpse(conf_rm_epm)



conf_rm_epmlim <- melt(conf_rm_epm,
                             id = "id",
                             measured = c(" rm_d1", " rm_d2"))

# Ordenando as colunas pelo sujeito experimental
conf_rm_epmlim  <- conf_rm_epmlim  %>%
  arrange(conf_rm_epmlim )

glimpse(conf_rm_epmlim)

#criando modelo anova
mod.ANOVA_conf_rm_epmlim  <- ezANOVA(data = conf_rm_epmlim ,
                                           dv = value,
                                           wid = id,
                                           within = variable,
                                           detailed = TRUE,
                                           type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III é o padrão no SPSS)


# Passo 5: Analisando os resultados do modelo
mod.ANOVA_conf_rm_epmlim 