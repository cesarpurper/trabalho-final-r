library(dplyr)
library(leaflet)
library(sf)
library(mapsBR)
library(rgeos)
library(rgdal)
library(janitor)
library(highcharter)
library(tidyr)


idese = readRDS("data/Idese Municipios 2007-2014.rds") %>% 
  clean_names() 

options(scipen = 999)
ideseTest = idese %>% 
  select(cod,nome, ano, bloco_educacao,bloco_saude,bloco_renda,idese) %>% 
  gather(bloco,valor,bloco_educacao,bloco_saude,bloco_renda,idese) %>% 
  group_by(cod,bloco) %>% 
  filter(ano !=2007) %>%
  mutate(
    difference = valor - lag(valor)
  ) %>% 
  filter(!is.na(difference)) %>%
  group_by(cod,nome,bloco) %>% 
  summarise(
    sumDiff = sum(difference)
  ) %>% 
  spread(bloco, sumDiff)


data(regMun) #carrega mapa com os municípios brasileiros

regMun = regMun %>%  
  st_as_sf() %>% 
  mutate(
    COD = as.factor(COD)
  )%>%
  mutate(NOME = iconv(NOME, from = "UTF-8", to = "latin1"))



xx <- regMun %>% 
  st_as_sf() %>% 
  inner_join(
    ideseTest,
    by = c("COD" = "cod")
  )

pal <- colorNumeric(
  palette = "RdYlBu",
  domain = NULL
)

leaflet(xx) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillOpacity = 0,
              weight      = 0.85,
              color       = "#000000") %>%
  
  addPolygons(color      = ~pal(idese),
              stroke      = F,
              weight      = 0.1,
              fillOpacity = 0.7) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~idese,
            title = "Vairação idese")

leaflet(xx) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillOpacity = 0,
              weight      = 0.85,
              color       = "#000000") %>%
  
  addPolygons(color      = ~pal(bloco_educacao),
              stroke      = F,
              weight      = 0.1,
              fillOpacity = 0.7) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~bloco_educacao,
            title = "Variação bloco_educacao")

leaflet(xx) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillOpacity = 0,
              weight      = 0.85,
              color       = "#000000") %>%
  
  addPolygons(color      = ~pal(bloco_saude),
              stroke      = F,
              weight      = 0.1,
              fillOpacity = 0.7) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~bloco_saude,
            title = "Variação bloco_saude")

leaflet(xx) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillOpacity = 0,
              weight      = 0.85,
              color       = "#000000") %>%
  
  addPolygons(color      = ~pal(bloco_renda),
              stroke      = F,
              weight      = 0.1,
              fillOpacity = 0.7) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~bloco_renda,
            title = "Variação bloco_renda")
