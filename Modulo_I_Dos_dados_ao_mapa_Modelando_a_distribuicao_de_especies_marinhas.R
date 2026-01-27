# ---------------------------------------------------------------------------- #
# Curso Online: Dos dados ao mapa: Modelando a distribuição                    #
# das espécies                                                                 #
#                                                                              #
# Criado por: Dr. Carlos de Oliveira                                           #
# Data: 27-01-2026                                                             #
# Contato: carlos.prof.bio@gmail.com                                           #
#                                                                              #
# Descrição: o script representa o processo geral de implementação de          #
# uma rotina de modelagem da adequabilidade ambiental, para espécies marinhas  # 
# no contexto da Modelagem de Distribuição de Especies (MDE).                  #
# Todos os procedimentos podem ser modificados conforme o escopo e necessidade # 
# da pesquisa.                                                                 #
#                                                                              #
# Notas:                                                                       #
# - eventuais erros podem surgir no script devido atualizações dos             #
# pacotes utilizados pelo script.                                              #
# - as pastas de origem e destino dos arquivos devem ser atualizadas           #
# conforme o computador onde serão realizados os processos de modelagem.       #
# ---------------------------------------------------------------------------- #

options(scipen = 999) # remover notação científica dos dados

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

#install.packages("spThin")
#install.packages("raster")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("dismo")

library(spThin)    # Realiza o "thinning" espacial, reduzindo a autocorrelação espacial em dados de ocorrência
library(raster)    # Manipulação, análise e visualização de dados espaciais no formato raster
library(tidyverse) # Conjunto de pacotes para manipulação, visualização e análise de dados (ggplot2, dplyr, tidyr, etc.)
library(dplyr)     # Manipulação de dados (selecionar colunas, filtrar linhas, criar variáveis, agrupar, sumarizar)

pal1 <- c("#3E49BB", "#3498DB", "yellow", "orange", "red", "darkred") # paleta de cores

# ---------------------------------------------------------------------------- #

# 01. Obter dados presença e processar dados ambientais e bióticos -----

## Download ou carregamento das ocorrências -----
sp_toninha <- dismo::gbif(
  genus = "Pontoporia",           # Define o gênero da espécie a buscar no GBIF
  species = "blainvillei",        # Define a espécie
  geo = TRUE,                     # Filtra apenas registros com coordenadas (lat/long)
  removeZeros = TRUE,             # Remove registros com coordenadas inválidas
  download = TRUE                 # Faz o download diretamente do GBIF
)

# ---------------------------------------------------------------------------- #

# sp_toninha <- read.csv("nome do arquivo.csv")             # Alternativa: ler ocorrências de um arquivo CSV
# sp_toninha <- readxl::read_excel("nome do arquivo.xlsx")  # Alternativa: ler ocorrências de um Excel

# ---------------------------------------------------------------------------- #

names(sp_toninha)    # Mostra os nomes das colunas do objeto 'sp'
sp_toninha$country   # Exibe todos os dados baixados
nrow(sp_toninha)     # Conta o número de linhas (registros) no dataframe

# ---------------------------------------------------------------------------- #

### Tratamento dos dados -----
sp_toninha <- sp_toninha %>%
  dplyr::filter(country %in% c("Brazil", "Argentina", "Uruguay")) %>%  # Mantém ocorrências nos países escolhidos
  dplyr::select(species, lon, lat)                                     # Mantém apenas colunas de interesse

nrow(sp_toninha)  # Número de registros após o filtro

# ---------------------------------------------------------------------------- #

sp_toninha <- sp_toninha %>%
  distinct() %>%  # Remove registros duplicados
  drop_na()       # Remove registros com valores faltantes

nrow(sp_toninha)      # Conta registros após limpeza

# ---------------------------------------------------------------------------- #

# Carrega pacotes para visualização
library(ggplot2)  # Pacote para gráficos
library(maps)     # Pacote para mapas simples

# Lista de países da América Latina que você quer exibir
latam_countries <- c("Brazil", "Argentina", "Bolivia", "Chile", "Colombia",
                     "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela",
                     "Guyana", "Suriname", "French Guiana")

# Obtém dados do mapa mundial e filtra apenas a América Latina
latam_map <- map_data("world") %>%
  filter(region %in% latam_countries)

# Plota o mapa da América Latina com os pontos
g1 <- ggplot() +
  geom_polygon(data = latam_map, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray60") +
  geom_point(data = sp_toninha, aes(x = lon, y = lat),
             color = "red", size = 2) +
  coord_fixed(1.3) +
  labs(
    title = "Ocorrências de Pontoporia blainvillei",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

g1

# ---------------------------------------------------------------------------- #

#install.packages("devtools")
#devtools::install_github("bio-oracle/biooracler")

library(biooracler)
