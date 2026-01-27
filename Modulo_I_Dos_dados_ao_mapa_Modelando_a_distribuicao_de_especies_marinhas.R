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

#install.packages("raster")
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("dismo")

library(raster)    # Manipulação, análise e visualização de dados espaciais no formato raster
library(tidyverse) # Conjunto de pacotes para manipulação, visualização e análise de dados (ggplot2, dplyr, tidyr, etc.)
library(dplyr)     # Manipulação de dados (selecionar colunas, filtrar linhas, criar variáveis, agrupar, sumarizar)

pal1 <- c("#3E49BB", "#3498DB", "yellow", "orange", "red", "darkred") # paleta de cores

# ---------------------------------------------------------------------------- #

# 01. Obter dados presença -----

## Download ou carregamento das ocorrências -----
sp_toninha_full <- dismo::gbif(
  genus = "Pontoporia",           # Define o gênero da espécie a buscar no GBIF
  species = "blainvillei",        # Define a espécie
  geo = TRUE,                     # Filtra apenas registros com coordenadas (lat/long)
  removeZeros = TRUE,             # Remove registros com coordenadas inválidas
  download = TRUE                 # Faz o download diretamente do GBIF
)

# ---------------------------------------------------------------------------- #

# Carrega pacotes
library(ggplot2)
library(maps)

# Dados do mapa mundial (sem filtro)
world_map <- map_data("world")

# Plot do mapa global com os pontos
g1 <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray60",
    linewidth = 0.2
  ) +
  geom_point(
    data = sp_toninha_full,
    aes(x = lon, y = lat),
    color = "red",
    size = 2
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Ocorrências de Pontoporia blainvillei",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

g1

# ---------------------------------------------------------------------------- #

# sp_toninha <- read.csv("nome do arquivo.csv")             # Alternativa: ler ocorrências de um arquivo CSV
# sp_toninha <- readxl::read_excel("nome do arquivo.xlsx")  # Alternativa: ler ocorrências de um Excel

# ---------------------------------------------------------------------------- #

names(sp_toninha_full)    # Mostra os nomes das colunas do objeto 'sp'
sp_toninha_full$country   # Exibe todos os dados baixados
nrow(sp_toninha_full)     # Conta o número de linhas (registros) no dataframe

# ---------------------------------------------------------------------------- #

### Tratamento dos dados -----
sp_toninha <- sp_toninha_full %>%
  dplyr::filter(country %in% c("Brazil", "Argentina", "Uruguay")) %>%  # Mantém ocorrências nos países escolhidos
  dplyr::select(species, lon, lat)                                     # Mantém apenas colunas de interesse

nrow(sp_toninha)  # Número de registros após o filtro

# ---------------------------------------------------------------------------- #

sp_toninha <- sp_toninha %>%
  distinct() %>%  # Remove registros duplicados
  drop_na()       # Remove registros com valores faltantes

nrow(sp_toninha)      # Conta registros após limpeza

# ---------------------------------------------------------------------------- #

# 02. Obter mapa da área de estudo (shapefile) -----

#install.packages("sf")
#install.packages("sp")

library(sf)        # Trabalhar com dados espaciais
library(sp)        # Trabalhar com dados espaciais


# Baixar Shapefile usando sf
oceans <- st_read("shapefile/goas_v01.shp")

# Baixar Shapefile usando sf
eez <- st_read("shapefile/eez_boundaries_v12.shp")

par(mfrow=c(1, 1))

# Plotar o shapefile com personalizações
plot(oceans$geometry, col = "lightblue")
plot(eez, col = "black", add=TRUE)

# Adicionar eixos
axis(2, at = seq(-90, 90, by = 20))
axis(1, at = seq(-180, 180, by = 20))

# Definir as coordenadas de recorte
coord_limit <- c(-90, -20, -89.975, 20)

# Converter o objeto oceans de sf para sp
oceans_sp <- as(oceans, "Spatial")

# Transformar os polígonos para o sistema de coordenadas desejado
oceans_cropped_1 <- spTransform(oceans_sp, CRS("+proj=longlat +datum=WGS84"))

# Criar uma extensão usando a função extent do pacote raster
ext_lim <- raster::extent(coord_limit[1], coord_limit[2], coord_limit[3], coord_limit[4])

# Usar a função crop do pacote raster para recortar
oceans_cropped <- raster::crop(oceans_cropped_1, ext_lim)

# Converter o objeto oceans de sf para sp
eez_sp <- as(eez, "Spatial")

# Transformar os polígonos para o sistema de coordenadas desejado
eez_cropped_1 <- spTransform(eez_sp, CRS("+proj=longlat +datum=WGS84"))

# Usar a função crop do pacote raster para recortar
eez_cropped <- raster::crop(eez_cropped_1, ext_lim)

# ---------------------------------------------------------------------------- #

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

points(sp_toninha$lon, sp_toninha$lat,
       pch = 16,
       col = "red",
       cex = 1)

# ---------------------------------------------------------------------------- #

# 03. Obter dados e processar dados ambientais -----

# https://www.bio-oracle.org/

#install.packages("devtools")
#devtools::install_github("bio-oracle/biooracler")

library(biooracler)
library(writexl)     # Salvar arquivos .xlsx

# ---------------------------------------------------------------------------- #

list_layers()                                     # Visualizar a descrição das camadas ambientais

list_layers("tas_baseline_2000_2020_depthsurf")   # Listar camada indivídual
  
camadas <- list_layers()                          # Salvar todas as camadas em uma variável

write_xlsx(camadas, "informacoes_camadas.xlsx")   # Salvar em .xlsx

info_layer("tas_baseline_2000_2020_depthsurf")    # Informação sobre camadas indivídual

# ---------------------------------------------------------------------------- #

chl_baseline_surf <- "chl_baseline_2000_2018_depthsurf" ### mg m-3
mld_baseline_surf <- "mlotst_baseline_2000_2019_depthsurf" ### m
tsm_baseline_surf <- "thetao_baseline_2000_2019_depthsurf" ### °C
sal_baseline_surf <- "so_baseline_2000_2019_depthsurf" ### PSU
swd_baseline_surf <- "swd_baseline_2000_2019_depthsurf" ### Graus
sws_baseline_surf <- "sws_baseline_2000_2019_depthsurf" ### m s**-1
produt_baseline_surf <- "phyc_baseline_2000_2020_depthsurf" ### Total Phytoplankton - MMol' 'M-3
bathy_baseline <- "terrain_characteristics" ### metros  
iron_baseline_surf <- "dfe_baseline_2000_2018_depthsurf" ###
nitrate_baseline_surf <- "no3_baseline_2000_2018_depthsurf" ###
oxygen_baseline_surf <- "o2_baseline_2000_2018_depthsurf" ###
ph_baseline_surf <- "ph_baseline_2000_2018_depthsurf" ###
phosphate_baseline_surf <- "po4_baseline_2000_2018_depthsurf" ###
silicate_baseline_surf <- "si_baseline_2000_2018_depthsurf" ###

info_layer("chl_baseline_2000_2018_depthsurf")
info_layer("terrain_characteristics")

time_bathy = c('1970-01-01T00:00:00Z', '1970-01-01T00:00:00Z')  # Intervalo temporal da batimetria - variável estática, sem variação temporal real
time = c('2000-01-01T00:00:00Z', '2000-01-01T00:00:00Z')        # Intervalo temporal das variáveis ambientais
latitude = c(-89.975, 20)                                       # Domínio espacial Sul global até 20°N
longitude = c(-90, 20)                                          # Domínio espacial 90°W até 20°E

# Listas de restrições (constraints) para consulta de dados.

constraints_bathy = list(time_bathy, latitude, longitude)
constraints = list(time, latitude, longitude)
names(constraints) = c("time", "latitude", "longitude")
names(constraints_bathy) = c("time", "latitude", "longitude")

constraints_bathy
constraints

info_layer("chl_baseline_2000_2018_depthsurf")
info_layer("terrain_characteristics")

variables_chl_baseline_surf = c("chl_mean")
variables_mld_baseline_surf = c("mlotst_mean")
variables_tsm_baseline_surf = c("thetao_mean")
variables_sal_baseline_surf = c("so_mean")
variables_swd_baseline_surf = c("swd_mean")
variables_sws_baseline_surf = c("sws_mean")
variables_produt_baseline_surf = c("phyc_mean")
variables_bathy_baseline = c("bathymetry_mean")
variables_iron_baseline_surf = c("dfe_mean")
variables_nitrate_baseline_surf = c("no3_mean")
variables_oxygen_baseline_surf = c("o2_mean")
variables_ph_baseline_surf = c("ph_mean")
variables_phosphate_baseline_surf = c("po4_mean")
variables_silicate_baseline_surf = c("si_mean")

chl_baseline_surf_2000_2010 <- download_layers(chl_baseline_surf, variables_chl_baseline_surf, constraints)
mld_baseline_surf_2000_2010 <- download_layers(mld_baseline_surf, variables_mld_baseline_surf, constraints)
tsm_baseline_surf_2000_2010 <- download_layers(tsm_baseline_surf, variables_tsm_baseline_surf, constraints)
sal_baseline_surf_2000_2010 <- download_layers(sal_baseline_surf, variables_sal_baseline_surf, constraints)
swd_baseline_surf_2000_2010 <- download_layers(swd_baseline_surf, variables_swd_baseline_surf, constraints)
sws_baseline_surf_2000_2010 <- download_layers(sws_baseline_surf, variables_sws_baseline_surf, constraints)
produt_baseline_surf_2000_2010 <- download_layers(produt_baseline_surf, variables_produt_baseline_surf, constraints)
bathy_baseline_2000_2010 <- download_layers(bathy_baseline, variables_bathy_baseline, constraints_bathy)
iron_baseline_2000_2010 <- download_layers(iron_baseline_surf, variables_iron_baseline_surf, constraints)
nitrate_baseline_2000_2010 <- download_layers(nitrate_baseline_surf, variables_nitrate_baseline_surf, constraints)
oxygen_baseline_2000_2010 <- download_layers(oxygen_baseline_surf, variables_oxygen_baseline_surf, constraints)
ph_baseline_2000_2010 <- download_layers(ph_baseline_surf, variables_ph_baseline_surf, constraints)
phosphate_baseline_2000_2010 <- download_layers(phosphate_baseline_surf, variables_phosphate_baseline_surf, constraints)
silicate_baseline_2000_2010 <- download_layers(silicate_baseline_surf, variables_silicate_baseline_surf, constraints)

chl_baseline_surf_2000_2010

# Criar RasterLayer a partir dos SpatRaster
chl_surf_raster <- raster(chl_baseline_surf_2000_2010)
mld_surf_raster <- raster(mld_baseline_surf_2000_2010)
tsm_surf_raster <- raster(tsm_baseline_surf_2000_2010)
sal_surf_raster <- raster(sal_baseline_surf_2000_2010)
swd_surf_raster <- raster(swd_baseline_surf_2000_2010)
sws_surf_raster <- raster(sws_baseline_surf_2000_2010)
produt_surf_raster <- raster(produt_baseline_surf_2000_2010)
bathy_raster <- raster(bathy_baseline_2000_2010)
iron_surf_raster <- raster(iron_baseline_2000_2010)
nitrate_surf_raster <- raster(nitrate_baseline_2000_2010)
oxygen_surf_raster <- raster(oxygen_baseline_2000_2010)
ph_surf_raster <- raster(ph_baseline_2000_2010)
phosphate_surf_raster <- raster(phosphate_baseline_2000_2010)
silicate_surf_raster <- raster(silicate_baseline_2000_2010)

# Empilhar os RasterLayer em um RasterStack
bio <- stack(chl_surf_raster, mld_surf_raster, tsm_surf_raster, sal_surf_raster, swd_surf_raster, sws_surf_raster, 
             produt_surf_raster, bathy_raster, iron_surf_raster, nitrate_surf_raster, phosphate_surf_raster, 
             silicate_surf_raster, ph_surf_raster, oxygen_surf_raster)

print(bio)

plot(bio)

# ---------------------------------------------------------------------------- #

bio <- crop(bio, oceans_cropped) # recorte da área de estudo
bio <- mask(bio, oceans_cropped) # máscara fora da área de estudo

names(bio)

# ---------------------------------------------------------------------------- #

# 04. Extrair valores das variáveis ambientais -----

sp_toninha_coord <- subset(sp_toninha, select = -species)  # Excluir coluna "species", manter somente "lat" e "lon"

nrow(sp_toninha_coord)

toninha_var <- raster::extract(bio, sp_toninha_coord)

nrow(toninha_var)
summary(toninha_var)

# ---------------------------------------------------------------------------- #

toninha_concat <- cbind(sp_toninha_coord, toninha_var) # Concatenar "sp_toninha_coord e toninha_var"

nrow(toninha_concat)
summary(toninha_concat)

# ---------------------------------------------------------------------------- #

toninha_sem_na <- na.omit(toninha_concat) # Excluir NAs

nrow(toninha_sem_na)

write_xlsx(
  toninha_sem_na,
  "toninha_sem_na.xlsx"
)

# ---------------------------------------------------------------------------- #

# Remove os dois pontos indesejados
toninha_sem_na <- toninha_sem_na[-c(103, 107), ]

# ---------------------------------------------------------------------------- #

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

points(toninha_sem_na$lon, toninha_sem_na$lat,
       pch = 16,
       col = "red",
       cex = 1)

# ---------------------------------------------------------------------------- #

nrow(toninha_sem_na)
nrow(sp_toninha)
nrow(sp_toninha_full)

# ---------------------------------------------------------------------------- #

# 05. Gerar as ausências/pseudoausências -----

# Converter o mapa recortado para sf

toninha_pres_sf <- st_as_sf(
  toninha_sem_na,
  coords = c("lon", "lat"),
  crs = 4326
)

oceans_sf <- st_as_sf(oceans_cropped)

# ---------------------------------------------------------------------------- #

# Sortear pontos candidatos (mais do que 111!)

set.seed(123)

candidatos <- st_sample(
  oceans_sf,
  size = 5000,        # quanto maior, melhor
  type = "random"
)

candidatos_sf <- st_as_sf(candidatos)

# ---------------------------------------------------------------------------- #

# Calcular distância até os pontos de presença

dist_matrix <- st_distance(candidatos_sf, toninha_pres_sf)

# distância mínima de cada candidato até qualquer presença
dist_min <- apply(dist_matrix, 1, min)

# Filtrar pontos com distância ≥ 4°
ausencias_sf <- candidatos_sf[dist_min >= 4, ]

# Selecionar 120 pontos
if (nrow(ausencias_sf) < 120) {
  stop("Poucos pontos disponíveis. Aumente o número de candidatos.")
}

ausencias_sf <- ausencias_sf %>%
  slice_sample(n = 120)

# ---------------------------------------------------------------------------- #

# Visualização final (checagem)
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add = TRUE)

plot(st_geometry(toninha_pres_sf), add = TRUE, col = "blue", pch = 16)
plot(st_geometry(ausencias_sf), add = TRUE, col = "red", pch = 16)

legend(
  "bottomleft",
  legend = c("Presença", "Ausência"),
  col = c("blue", "red"),
  pch = c(16, 16),
  bty = "n"
)

# ---------------------------------------------------------------------------- #

# Extrair latitude e longitude
ausencias_df <- ausencias_sf %>%
  st_coordinates() %>%
  as.data.frame()

colnames(ausencias_df) <- c("lon", "lat")

# ---------------------------------------------------------------------------- #

# Extrair valores das variaveis de ausência

toninha_ausencias <- raster::extract(bio, ausencias_sf)

summary(toninha_ausencias)
str(toninha_ausencias)

# ---------------------------------------------------------------------------- #

# Inserir coluna "species" com valor 0 para ausência e coluna "lon" e "lat" 

toninha_ausencias_df <- cbind(
  species = 0,
  ausencias_df[, c("lon", "lat")],
  toninha_ausencias
)

str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

toninha_ausencias_df <- na.omit(as.data.frame(toninha_ausencias_df))

str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

# Define semente para reprodutibilidade (opcional)
set.seed(123)

# Sorteia 5 linhas aleatórias para remover
linhas_remover <- sample(seq_len(nrow(toninha_ausencias_df)), 5)

# Remove as linhas sorteadas
toninha_ausencias_df <- toninha_ausencias_df[-linhas_remover, ]

str(toninha_ausencias_df)
str(toninha_sem_na)

colnames(toninha_ausencias_df)
colnames(toninha_sem_na)

# ---------------------------------------------------------------------------- #

# Concatenar dados de presença e ausências 

toninha_final <- rbind(
  toninha_sem_na,
  toninha_ausencias_df
)

str(toninha_final)

# ---------------------------------------------------------------------------- #

# Visualização base
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add = TRUE)

# Definir cores por presença/ausência
cols <- ifelse(toninha_final$species == 1, "blue", "red")

# Plotar pontos
points(
  toninha_final$lon,
  toninha_final$lat,
  col = cols,
  pch = 16,
  cex = 0.8
)

# Adicionar eixos y
valores_y <- c(20, 10, 0, -10, -20, -30, -40, -50, -60, -70, -80, -90)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-90, -80, -70, -60, -50, -40, -30, -20)
axis(1, at = valores_x)

# ---------------------------------------------------------------------------- #
write_xlsx(
  toninha_final,
  path = "dados_toninha_final.xlsx"
)
