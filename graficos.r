library(dplyr)
library(leaflet)
library(sf)
library(mapsBR)
library(rgeos)
library(rgdal)
library(janitor)
library(highcharter)
library(tidyr)
library(ggplot2)


idese = readRDS("data/Idese Municipios 2007-2014.rds") %>% 
  clean_names() 

options(scipen = 999)
ideseTest = idese %>% 
  filter(cod != "4314548") %>% 
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

top5= ideseTest %>% 
  arrange(desc(idese)) %>% 
  head(5)

top5lixo = ideseTest %>% 
  arrange(idese) %>% 
  head(5)

top5 %>% 
  left_join(
    idese,
    by = c("cod" = "cod")
  ) %>% 
  gather(bloco,valor,bloco_educacao.y,bloco_saude.y,bloco_renda.y,idese.y) %>% 
  ggplot(aes(x = ano, valor, color = nome.y)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y")

top5lixo %>% 
  left_join(
    idese,
    by = c("cod" = "cod")
  ) %>% 
  gather(bloco,valor,bloco_educacao.y,bloco_saude.y,bloco_renda.y,idese.y) %>% 
  ggplot(aes(x = ano, valor, color = nome.y)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y")
   

