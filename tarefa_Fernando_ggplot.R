library(tidyverse)

list.files()#?mostra qm está na pasta de trabalho
dados <- read.csv("Pokemon_full.csv")#?possivel qnd o .csv está na pasta de trabalho
glimpse(dados)#?mostrar o conjunto de dados

#!converter altura e peso em metros e kg de acordo com PokeAPI:
#	The weight of this Pokémon in hectograms.
#The height of this Pokémon in decimetres.
dados <- dados %>%
  mutate(
    height_m = height / 10,
    weight_kg = weight / 10
  )

glimpse(dados)

# ggplot(dados, aes(x = height_m, y = weight_kg, color = type)) +
#   geom_point(alpha = 0.5, size = 1.5)#alpha é opacidade dos pontos e size é o tamanho
# +
#   labs(
#     x = "Height (m)",
#     y = "Weight (kg)",
#     color = "Type"
#   ) +
#   theme_minimal()
#!ficou ruim, precisa filtrar os dados e colorir por tipo

#!filtrar ate medio porte:
pokemons_ate_medio_porte <- dados %>%
  filter(height_m < 5, weight_kg < 500)

#!definir as cores de cada tipo (pedi para a IA fazer):
cores_pokemon <- c(
  bug      = "#A8B820",  # verde oliva
  dark     = "#705848",  # marrom escuro
  dragon   = "#7038F8",  # roxo
  electric = "#F8D030",  # amarelo
  fairy    = "#EE99AC",  # rosa
  fighting = "#C03028",  # vermelho
  fire     = "#F08030",  # laranja
  flying   = "#A890F0",  # lilás
  ghost    = "#705898",  # roxo escuro
  grass    = "#78C850",  # verde claro
  ground   = "#E0C068",  # bege/terra
  ice      = "#98D8D8",  # azul-claro
  normal   = "#A8A878",  # cinza-esverdeado
  poison   = "#A040A0",  # roxo
  psychic  = "#F85888",  # rosa forte
  rock     = "#B8A038",  # marrom/amarelo
  steel    = "#B8B8D0",  # cinza metálico
  water    = "#6890F0"   # azul
)

#!gostei do plot de curvas de densidade para a idéia que eu estava de colocar a variável categórica "type" como uma "terceira dimensão" por meio da cor
ggplot(pokemons_ate_medio_porte,
       aes(x = height_m, y = weight_kg, color = type)) +

  #tirar o plot com pontos geom_point(alpha = 0.6, size = 1.8) +

  scale_color_manual(values = cores_pokemon) +
  
  #?plot de curvas de densidades usando o 2d kernel density estimation
  stat_density_2d(
    aes(color = type),
    linewidth = 0.8,
    bins = 3#?numero de contornos para traçar a curva, testei varios
  ) +

  #?escala log para explorar diferenças em ordem de grandeza, nao em valores absolutos:
  scale_x_log10() +
  scale_y_log10() +

  labs(
    title = "Pokémons de até médio porte (<5 m e <500 kg) - curvas de densidade por tipo",
    x = "log10 (Height (m))",
    y = "log10 (Weight (kg))",
    color = "Type"
  ) +

  theme_minimal()

