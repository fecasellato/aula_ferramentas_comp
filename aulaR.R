library(tidyverse)

dados <- read.csv("Pokemon_full.csv")

glimpse(dados)
dplyr::glimpse(dados)
dados$name
dados$attack/dados$defense
dados$IMC <-dados$weight/dados$height^2
glimpse(dados)

#?selecionar colunas usando dplyr
select(dados, name, attack)

#? filtrar dados
filter(dados, height>10)

#?criar colunas
mutate(dados, rate_atdef = attack/defense)

#? resumir dados
summarise (dados, media_atk=mean(attack), sd_at=sd(attack))

#?media e desvio padrao apenas para altura > 10
df <- filter(dados, height > 10)
summarise(df, media_at = mean(attack), sd_at = sd(attack))

#?pull
pull(dados,name) #retorna um dataframe/coluna e não valores
select(dados,name) #retorno no console é uma coluna com nome (df/tabela de uma coluna só). Retorna VALORES


#!PIPE %>%

dados%>%mutate(
  soma_ad = attack+defense,
  soma2= height+weight
)%>%
  select(soma_ad, soma2)

dados%>%
  pull(attack)%>%max


#?agrupar dados

dados%>%
  group_by(type)%>%
  summarise(media_at = mean(attack), sd_at = sd(attack))
  


