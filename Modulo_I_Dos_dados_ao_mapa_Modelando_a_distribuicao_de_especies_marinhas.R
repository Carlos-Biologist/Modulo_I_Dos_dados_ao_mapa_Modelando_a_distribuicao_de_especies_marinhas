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

#Carregar pacotes

# install.packages("raster")     # Leitura, manipulação, análise e visualização de dados raster (camadas ambientais, mapas contínuos, etc.)
# install.packages("tidyverse")  # Conjunto de pacotes para ciência de dados (dplyr, tidyr, ggplot2, readr, etc.)
# install.packages("dplyr")      # Manipulação de dados tabulares: filtrar, selecionar, criar variáveis e resumir dados
# install.packages("dismo")      # Ferramentas para modelagem de distribuição de espécies (SDM),
# install.packages("psych")      # Funções estatísticas exploratórias, incluindo correlação, análise de colinearidade e PCA
# install.packages("devtools")   # Ferramentas para desenvolvimento e instalação de pacote
# library(devtools)
# devtools::install_github("biomodhub/biomod2", dependencies = TRUE)  # Instalação do biomod2 diretamente do GitHub
# devtools::install_github("bio-oracle/biooracler")  # Interface para acessar dados ambientais marinhos do Bio-ORACLE
# install.packages("sf")         # Manipulação moderna de dados espaciais vetoriais (shapefiles, geopackages)
# install.packages("sp")         # Estruturas clássicas de dados espaciais (SpatialPoints, SpatialPolygons)
# install.packages("readxl")     # Leitura de arquivos Excel (.xls e .xlsx)
# install.packages("writexl")    # Escrita de arquivos Excel (.xlsx)

library(raster)    
library(tidyverse) 
library(dplyr)     
library(psych)     
library(biomod2)
library(ggplot2)
library(maps)
library(sf)        
library(sp)        
library(biooracler)
library(writexl)   
library(readxl)

# ---------------------------------------------------------------------------- #

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
coord_limit <- c(-70, -35, -60, -10)

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
valores_y <- c(-10, -15, -20, -25, -30, -35, -40, -45, -50, -55, -60)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-70, -65, -60, -55, -50, -45, -40, -35)
axis(1, at = valores_x)

points(sp_toninha$lon, sp_toninha$lat,
       pch = 16,
       col = "red",
       cex = 1)

# ---------------------------------------------------------------------------- #

# 03. Obter dados e processar dados ambientais -----

# https://www.bio-oracle.org/

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
latitude = c(-60,-10)                                       # Domínio espacial Sul global até 20°N
longitude = c(-70, -35)                                          # Domínio espacial 90°W até 20°E

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

names(sp_toninha)
nrow(sp_toninha)

sp_toninha_coord <- subset(sp_toninha, select = -species)  # Excluir coluna "species", manter somente "lat" e "lon"

names(sp_toninha_coord)

toninha_var <- raster::extract(bio, sp_toninha_coord)      # Extrair valores das variáveis ambientais

summary(toninha_var)

# ---------------------------------------------------------------------------- #

toninha_concat <- cbind(sp_toninha_coord, toninha_var) # Concatenar "sp_toninha_coord e toninha_var"

summary(toninha_concat)

# ---------------------------------------------------------------------------- #

toninha_sem_na <- na.omit(toninha_concat) # Excluir NAs

str(toninha_sem_na)

write_xlsx(
  toninha_sem_na,
  "toninha_sem_na.xlsx"
)

# ---------------------------------------------------------------------------- #

# Remove os dois pontos indesejados
toninha_sem_na <- toninha_sem_na[-c(106), ]

# ---------------------------------------------------------------------------- #

# Plotar Shapefile recortado
plot(oceans_cropped, col = "lightblue")
plot(eez_cropped, add=TRUE)

# Adicionar eixos y
valores_y <- c(-10, -15, -20, -25, -30, -35, -40, -45, -50, -55, -60)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-70, -65, -60, -55, -50, -45, -40, -35)
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

# Filtrar pontos com distância ≥ 2°
ausencias_sf <- candidatos_sf[dist_min >= 2, ]

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

# Adicionar eixos y
valores_y <- c(-10, -15, -20, -25, -30, -35, -40, -45, -50, -55, -60)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-70, -65, -60, -55, -50, -45, -40, -35)
axis(1, at = valores_x)

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

# ---------------------------------------------------------------------------- #

# Inserir coluna "species" com valor 0 para ausência e coluna "lon" e "lat" 

toninha_ausencias_df <- cbind(
  species = 0,
  ausencias_df[, c("lon", "lat")],
  toninha_ausencias
)

str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

#toninha_ausencias_df <- na.omit(as.data.frame(toninha_ausencias_df))

#str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

# Define semente para reprodutibilidade (opcional)
set.seed(123)

# Sorteia 5 linhas aleatórias para remover
linhas_remover <- sample(seq_len(nrow(toninha_ausencias_df)), 9)

# Remove as linhas sorteadas
toninha_ausencias_df <- toninha_ausencias_df[-linhas_remover, ]

str(toninha_ausencias_df)

# ---------------------------------------------------------------------------- #

# Concatenar dados de presença e ausências 

colnames(toninha_ausencias_df)
colnames(toninha_sem_na)

# Inserir coluna "species" com valor 0 para ausência e coluna "lon" e "lat" 

toninha_sem_na <- cbind(
  species = 1,
  toninha_sem_na
)

colnames(toninha_ausencias_df)
colnames(toninha_sem_na)

# ---------------------------------------------------------------------------- #

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
valores_y <- c(-10, -15, -20, -25, -30, -35, -40, -45, -50, -55, -60)
axis(2, at = valores_y)
# Adicionar eixo x
valores_x <- c(-70, -65, -60, -55, -50, -45, -40, -35)
axis(1, at = valores_x)

# ---------------------------------------------------------------------------- #

write_xlsx(
  toninha_final,
  path = "dados_toninha_final.xlsx"
)

# ---------------------------------------------------------------------------- #

# 06. Verificar colinearidade -----

toninha_colin <- toninha_final %>%
  dplyr::select(-species, -lon, -lat, -phyc_mean, -no3_mean, -po4_mean, -o2_mean, -ph_mean, -dfe_mean, -si_mean)

pairs.panels(
  toninha_colin,
  cex = 6,        # tamanho geral da fonte (números, correlações)
  cex.labels = 1.5 # tamanho dos nomes das variáveis
)

# ---------------------------------------------------------------------------- #

# 07. Rodar SDM Biomod2 -----

# https://cran.r-project.org/web/packages/biomod2/biomod2.pdf

# ---------------------------------------------------------------------------- #

# Dados de presença / ausência -----

toninha_final <- read_xlsx("dados_toninha_final.xlsx")

toninha_colin <- toninha_final %>%
  dplyr::select(, -phyc_mean, -no3_mean, -po4_mean, -o2_mean, -ph_mean, -dfe_mean, -si_mean)

# Checagens básicas
str(toninha_colin)
summary(toninha_colin)

# ---------------------------------------------------------------------------- #

# Nome da espécie
myResp <- toninha_colin$species
myRespName <- "species"

# Coordenadas (data.frame simples)
myRespXY <- toninha_colin[, c("lon", "lat")]

# ---------------------------------------------------------------------------- #

# Dados ambientais -----

# Empilhar os RasterLayer em um RasterStack
bio_colin <- stack(chl_surf_raster, mld_surf_raster, tsm_surf_raster, sal_surf_raster, swd_surf_raster, sws_surf_raster, bathy_raster)

# Converter Brick → Stack
bio_colin <- raster::stack(bio_colin)

bio_colin

# ---------------------------------------------------------------------------- #

# Formatação BIOMOD (com ausências reais) -----

myBiomodData <- BIOMOD_FormatingData(
  resp.var  = myResp,              # ✅ vetor 0/1
  expl.var  = bio_colin,
  resp.xy   = myRespXY,
  resp.name = myRespName
)

myBiomodData
myBiomodData@coord
head(myBiomodData@data.env.var)
plot(myBiomodData)

# ---------------------------------------------------------------------------- #

ModelsTable # Visualizar algoritmos

#allModels <- c('ANN', 'CTA', 'DNN', 'FDA', 'GAM', 'GBM', 'GLM', 'MARS'
#               , 'MAXENT', 'MAXNET', 'RF', 'RFd', 'SRE', 'XGBOOST')

allModels <- c('GAM', 'GLM', 'RF', 'XGBOOST')

toninha_opt <- bm_ModelingOptions(
  data.type = 'binary',
  models = allModels,
  strategy = 'default',
  bm.format = myBiomodData
)

toninha_model <- BIOMOD_Modeling(
  bm.format    = myBiomodData,
  modeling.id = 'AllModels',
  models      = c('GAM','GLM','RF','XGBOOST'),
  CV.strategy = 'block',    # validação cruzada espacial (em blocos)
  CV.perc     = 0.7,
  OPT.strategy = 'default',
  metric.eval = c('TSS','AUCroc'),
  var.import  = 3,
  seed.val    = 42
)

# ---------------------------------------------------------------------------- #

# Obter scores de avaliação e importância das variáveis

bm_PlotEvalMean(
  toninha_model,
  metric.eval = c('TSS','AUCroc'),
  dataset = "validation",
  group.by = "algo",
  do.plot = TRUE
)

# ---------------------------------------------------------------------------- #

toninha_model_var_imp <- get_variables_importance(toninha_model)

# Calcula a média da importância das variáveis pelas colunas "expl.var"
mean_var_imp <- aggregate(var.imp ~ expl.var, data = toninha_model_var_imp, FUN = mean)

# Exibe o resultado
print(mean_var_imp)

# Ordena o dataframe mean_var_imp do mais importante para o menos importante
mean_var_imp <- mean_var_imp[order(mean_var_imp$var.imp, decreasing = TRUE), ]

# Gera o gráfico de barras invertido
barplot(mean_var_imp$var.imp, names.arg = mean_var_imp$expl.var, 
        xlab = "Variáveis Explicativas", ylab = "Importância",
        col = "blue")

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

## then call Projection function
toninha_projection <- BIOMOD_Projection(toninha_model,
                                        new.env = bio_colin,
                                        proj.name = 'current_new',
                                        selected.models = 'all',
                                        compress = FALSE,
                                        build.clamping.mask = FALSE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

mods <- get_built_models(toninha_model)

toninha_ens <- BIOMOD_EnsembleModeling(
  toninha_model,
  models.chosen = mods,
  em.by = "all",
  em.algo = c('EMmean'),
  metric.select = c('TSS'),
  metric.select.thresh = c(0.8),
  metric.eval = c("TSS"),
  var.import = 5,
  EMci.alpha = 0.05,
  EMwmean.decay = "proportional",
  nb.cpu = 1,
  seed.val = 123,
  do.progress = TRUE,
)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

rcurve_australis_ens <- 
  bm_PlotResponseCurves(
    toninha_ens,
    models.chosen = get_built_models(toninha_ens),
    new.env = get_formal_data(toninha_ens, "expl.var"),
    show.variables = get_formal_data(toninha_ens, "expl.var.names"),
    do.bivariate = FALSE,
    fixed.var = "mean",
    do.plot = TRUE,
    do.progress = TRUE)

# Get evaluation scores & variables importance
get_evaluations(toninha_ens)

get_variables_importance(toninha_ens)

# # Represent variables importance
bm_PlotVarImpBoxplot(bm.out = toninha_ens, group.by = c('expl.var', 'algo', 'algo'))

# Caminho para o arquivo
caminho_arquivo <- "C:/Fernando_Modelagem australis/sdm_australis_fernando/sp.cod/proj_current_new/proj_current_new_sp.cod.tif"

# Carregar a imagem TIFF
imagem_tiff <- raster(caminho_arquivo)

# Visualizar a imagem
plot(imagem_tiff, col = pal1)

atual <- imagem_tiff / 1000

# Visualizar a imagem
plot(atual, col = pal1)
plot(eez_cropped, add = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

toninha_ens_ens <- BIOMOD_EnsembleForecasting(toninha_ens,
                                              projection.output = toninha_projection,
                                              new.env = bio_cropped,
                                              selected.models = 'all',
                                              proj.name = "ensemble_new_current",
                                              binary.meth = "TSS")
# Caminho para o arquivo
caminho_arquivo_ens <- "C:/Fernando_Modelagem australis/sdm_australis_fernando/sp.cod/proj_ensemble_new_current/proj_ensemble_new_current_sp.cod_ensemble.tif"

# Carregar a imagem TIFF
imagem_tiff_ens <- raster(caminho_arquivo_ens)

# Visualizar a imagem
plot(imagem_tiff_ens, col = pal1)

atual_ens <- imagem_tiff_ens / 1000

# Visualizar a imagem
plot(atual_ens, col = pal1)
#plot(eez_cropped, add = TRUE)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Extrair as coordenadas de myBiomodData
coordenadas <- myBiomodData@coord

# Extrair os valores do raster nas coordenadas
valores_extraidos <- extract(atual_ens, coordenadas)

# Visualizar os primeiros valores extraídos
head(valores_extraidos)

# Se desejar salvar esses valores em um arquivo .xlsx, use o pacote openxlsx

# Criar um dataframe com as coordenadas e os valores extraídos
dados_extraidos <- data.frame(coordenadas, valores_extraidos)

head(dados_extraidos)

# Salvar os dados em um arquivo .xlsx
write.xlsx(dados_extraidos, file = "valores_extraidos_ensemble.xlsx")

## Download ou carregamento das ocorrências -----
adequab <- readxl::read_excel("australis_env_var.xlsx")

head(adequab)
summary(adequab)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Crie o gráfico de "curva de resposta"
ggplot(adequab, aes(x = chl_mean, y = Adequabilidade)) +
  geom_point() + 
  geom_smooth(method = "loess") + 
  labs(x = "Chl-a",
       y = "Adequabilidade") +
  ylim(0, 1.12) +
  theme_gray()

# Crie o gráfico de "curva de resposta"
ggplot(adequab, aes(x = dfe_mean, y = Adequabilidade)) +
  geom_point() + 
  geom_smooth(method = "loess") + 
  labs(x = "Ferro Dissolvido",
       y = "Adequabilidade") +
  ylim(0, 1) +
  theme_gray()

# Crie o gráfico de "curva de resposta"
ggplot(adequab, aes(x = bathymetry_mean, y = Adequabilidade)) +
  geom_point() + 
  geom_smooth(method = "loess") + 
  labs(x = "Batimetria",
       y = "Adequabilidade") +
  ylim(0, 1) +
  theme_gray()

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

# Defina uma função para remover outliers usando o método do IQR
remove_outliers <- function(df, col_name) {
  Q1 <- quantile(df[[col_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df[df[[col_name]] >= lower_bound & df[[col_name]] <= upper_bound, ]
}

# Remova os outliers da variável "so_mean.2"
adequab_clean_chl <- remove_outliers(adequab, "chl_mean")

# Crie o gráfico de "curva de resposta" sem os outliers na variável x
chl <- ggplot(adequab_clean_chl, aes(x = chl_mean, y = Adequabilidade)) +
  #geom_point() + 
  geom_smooth(method = "loess") + 
  geom_hline(yintercept = 0.5, linetype = "dashed") +  # Adiciona linha pontilhada em y = 0.5
  labs(x = "Chl-a",
       y = "Adequabilidade") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(breaks = seq(0, 3.0, by = 0.2))

plot(chl)

# Remova os outliers da variável "so_mean.2"
adequab_clean_iron <- remove_outliers(adequab, "dfe_mean")

# Crie o gráfico de "curva de resposta" sem os outliers na variável x
iron <- ggplot(adequab_clean_iron, aes(x = dfe_mean, y = Adequabilidade)) +
  #geom_point() + 
  geom_smooth(method = "loess") + 
  geom_hline(yintercept = 0.5, linetype = "dashed") +  # Adiciona linha pontilhada em y = 0.5
  labs(x = "Ferro dissolvido",
       y = "Adequabilidade") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))

plot(iron)

# Remova os outliers da variável "so_mean.2"
adequab_clean_bathy <- remove_outliers(adequab, "bathymetry_mean")

# Crie o gráfico de "curva de resposta" sem os outliers na variável x
bathy <- ggplot(adequab_clean_bathy, aes(x = bathymetry_mean, y = Adequabilidade)) +
  #geom_point() + 
  geom_smooth(method = "loess") + 
  geom_hline(yintercept = 0.5, linetype = "dashed") +  # Adiciona linha pontilhada em y = 0.5
  labs(x = "Batimetria",
       y = "Adequabilidade") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))

plot(bathy)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #

library(gridExtra)

# Combine os gráficos em um painel 2x2
grid.arrange(chl, iron, bathy, ncol = 3)

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #