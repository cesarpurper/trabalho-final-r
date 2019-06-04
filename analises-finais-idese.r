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
  #filter(cod != "4314548") %>% 
  mutate(
    grupoPop = ifelse(populacao<2979, "Pequena", ifelse(populacao<14732, "Média", "Grande"))
  )%>% 
  na.omit() 

  #divisao dos quartis da populacao
quantile(ideseTest$populacao,probs = seq(0, 1, 0.25), na.rm = FALSE)


#comparacao dos blocos por tamanho de cidade
ideseTest %>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, bloco_saude,bloco_educacao, bloco_renda, idese) %>% 
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y") 


#compração do bloco educacao por tamanho de cidade
ideseTest %>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, bloco_educacao, bloco_educacao_ensino_fundamental, bloco_educacao_ensino_fundamental_anos_finais, bloco_educacao_ensino_fundamental_anos_iniciais, bloco_educacao_ensino_medio,bloco_educacao_escolaridade_adulta, bloco_educacao_pre_escola, idese) %>% 
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y") 

#compração do bloco saude por tamanho de cidade
ideseTest %>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, bloco_saude,bloco_saude_condicoes_gerais_de_saude, bloco_saude_condicoes_gerais_de_saude_obitos_por_causas_evitaveis, bloco_saude_condicoes_gerais_de_saude_obitos_por_causas_mal_definidas,bloco_saude_longevidade, bloco_saude_saude_materno_infantil, bloco_saude_saude_materno_infantil_consultas_pre_natal,bloco_saude_saude_materno_infantil_mortalidade_de_menores_de_5_anos, idese) %>% 
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y") 



#compração do bloco renda por tamanho de cidade
ideseTest %>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, bloco_renda, bloco_renda_apropriacao_da_renda, bloco_renda_geracao_da_renda, idese) %>% 
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y") 
