---
title: "Analise Idese"
author: "Alexandre - Cesar - Domingos"
date: "30 de maio de 2019"
output: 
  html_document:
    toc: true
    toc_float: true
    theme: "cosmo"
    highlight: "haddock"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, cache = TRUE, fig.align='center')

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
library(maptools)
library(spData)
library(stringr)
library(RColorBrewer)
library(DT)
library(tibble)
library(broom)

# carrega os dados do idese e separa nos quartis
idese = readRDS("../../data/Idese Municipios 2007-2014.rds") %>% 
  clean_names()

options(scipen = 999)
ideseTest = idese %>% 
  #filter(cod != "4314548") %>% 
  mutate(
    grupoPop = ifelse(populacao<2979, "Pequena", ifelse(populacao<14732, "Média", "Grande"))
  )%>% 
  na.omit()

#carrega os shapes dos municipios
data(regMun)
regMun = regMun %>%  
  st_as_sf() %>% 
  mutate(
    COD = as.factor(COD)
  )%>%
  mutate(NOME = iconv(NOME, from = "UTF-8", to = "latin1"))

idese_shapes <- regMun %>% 
  st_as_sf() %>% 
  inner_join(
    ideseTest,
    by = c("COD" = "cod")
  )




```

>**Quais são as cidades mais prósperas para viver?**
<center>
</br>
![](../../images/imagem_capa_2.jpg)
</center>

</br>

#**Dataset**#

O idese é um indicador socioeconomico baseado no IDH. Ele é dividido em três blocos: renda, educação e saúde. O índice é composto pela meda aritimética destes 3 blocos, já os blocos são compostos pela média aritimética dos seus sub-blocos.


* O bloco educação possui 5 indicadores, que são:
    + Taxa de Matrícula em Pré-Escola;
    + Nota da Prova Brasil dos alunos de anos iniciais (1º ao 5º ano);
    + Nota da Prova Brasil dos alunos de anos finais (6º ao 9º ano);
    + Taxa de matrícula no Ensino Médio;
    + Percentual da população adulta com pelo menos o Ensino Fundamental completo;

* O bloco renda é composto por 2 indicadores:
    + Renda Domiciliar per capita média;
    + PIB per capita;
  
* O bloco saúde é composto por outros 5 indicadores:
    + Taxa de Mortalidade de menores de 5 anos;
    + Número de consultas pré-natais por nascidos vivos;
    + Taxa de mortalidade por causas evitáveis;
    + Proporção de óbitos por causas mal definidas;
    + Taxa bruta de mortalidade padronizada;



#**Entendendo o cenário**#

Após uma analise inicial, ficou claro que com a quantidade de municipios no Rio Grande do Sul, seria muito dificil extrair cidades especificas e compara-las quanto a quais os fatores que a tornam uma cidade próspera ou não para se viver. Percebemos então, que a melhor forma de identificar comportamentos através das informações da base Idese seria utilizando a população para comparar cidades.


##Cidades vs População##

Analisando os dados da coluna população temos este resultado:.
```{r}
table = data.frame(quantile(ideseTest$populacao,probs = seq(0, 1, 0.25), na.rm = FALSE)) %>% 
  rownames_to_column(var = "perc") 
colnames(table)[2] = "valor"

table = table %>% 
  mutate(
    valor = as.integer(valor)
  ) %>% 
  spread(perc, valor) 
table = table[,c(1,3,4,5,2)]

table %>% 
  datatable(
    colnames = c('Mínimo', '1º quartil', 'Mediana', '3º quartil',   'Máximo'),
    rownames = F,
    options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")), # centralizando colunas
      bPaginate = FALSE,
      bInfo = FALSE,
      bFilter = FALSE,
      bSort = FALSE
    )
  )
```

Achamos ideal dividir a população em quartis, delimitando assim as cidades no espectro das 25% primeiras amostras como cidades pequenas, as que se encontram entre 25% e 75% das amostras como cidades médias, e as restantes, cidades grandes. 
<center>
![](../../images/imagem_quartil.png)
</center>

>Considerar a divisão acima para as próximas análises.




##Cidades vs População - Mapa##



```{r}
#mapa 1

cor_fator <- colorFactor(
  palette = c("#FF0000","#00FF00","#0000FF"), 
  unique(idese_shapes$grupoPop)
)	

tooltip <- sprintf("<strong>%s</strong><br>", 	
                   idese_shapes$nome)	

leaflet(idese_shapes) %>%	
  addProviderTiles(providers$CartoDB.Positron) %>%	
  addPolygons(fillOpacity = 0,	
              weight      = 0.85,	
              color       = "#000000") %>%	
  
  addPolygons(color      = ~cor_fator(grupoPop),	
              stroke      = F,	
              weight      = 0.1,	
              fillOpacity = 0.7,	
              popup       = tooltip) %>%	
  addLegend("bottomright",	
            pal    = cor_fator,	
            values = ~grupoPop,	
            title  = "Tam. Cidade")


```

Aqui vemos como fica essa divisão populacional no mapa do Rio Grande do Sul.


#**Entendendo os blocos**#

```{r}
#comparacao dos blocos por tamanho de cidade
ideseTest %>% 
  rename(
    "Bloco Saúde" = bloco_saude,
    "Bloco Educação" = bloco_educacao,
    "Bloco Renda" = bloco_renda,
    " Idese" = idese#gambiarra para o idese ser o primeiro coloquei o caracter alt+255 para ficar na frente de todos
  )%>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, "Bloco Saúde","Bloco Educação", "Bloco Renda", " Idese") %>% 
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y")+
  labs(
    colour = "Tamanho",
    x = "Ano",
    y = "Indicador"
    )
```
</br>

**- Educação :** Nos primeiros anos os três grupos variaram sem nenhum aumento significativo. A partir de 2010 houve uma grande crescente. Em todos os anos as cidades pequenas tiveram vantagem, sendo que apenas na amostra de 2011 as cidades grandes encostaram.</br>
</br>
**- Saúde :** Mesmo que por uma margem pequena, as cidades pequenas sempre tiveram vantagem neste bloco </br>
</br>
**- Renda :** Como esperado, as cidades grandes lideram neste quesito, mas não por muito. Os momentos de crescimento das cidades médias e pequenas são semelhantes, porém com intesidades diferentes, o que pode ter a ver com o tipo  de mercado predominante na região. Já o crescimento das cidades grandes se mantem constante durante todo o periodo. </br>

##Educação##

```{r}
#compração do bloco educacao por tamanho de cidade
ideseTest %>% rename(
    "Bloco Educação" = bloco_educacao,
    "Ensino Fundamental" = bloco_educacao_ensino_fundamental,
    "Ens. Fund. Anos Finais" = bloco_educacao_ensino_fundamental_anos_finais,
    "Ens. Fund. Anos Iniciais" = bloco_educacao_ensino_fundamental_anos_iniciais,
    "Ensino Médio" = bloco_educacao_ensino_medio,
    "Escolaridade Adulta" = bloco_educacao_escolaridade_adulta,
    "Pré Escola" = bloco_educacao_pre_escola,
    " Idese" = idese#gambiarra para o idese ser o primeiro coloquei o caracter alt+255 para ficar na frente de todos
  )%>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, "Bloco Educação", "Ensino Fundamental", "Ens. Fund. Anos Finais", "Ens. Fund. Anos Iniciais", "Ensino Médio","Escolaridade Adulta", "Pré Escola", " Idese") %>% #gambiarra para o idese ser o primeiro coloquei o caracter alt+255 para ficar na frente de todos
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y") +
  labs(
    colour = "Tamanho",
    x = "Ano",
    y = "Indicador"
    )
```
<br/>
No geral, os blocos variam de maneira bem semelhante com excessão dos indices de "Escolaridade Adulta" e "Pré Escola", cujos a diferença entre eles é bem pequena.
No bloco "Escolaridade Adulta", é possível ver claramente a vantagem de cidades grandes , isso pode se dar pelo fato das melhores universidades ficarem nas capitais.
Já no bloco "Pré Escola", as cidades pequenas levam maior vantagem, talvez devido ao grupo de crianças com idade para tal, seja menor e mais controlado.

</br>

##Saúde##

```{r}
#compração do bloco saude por tamanho de cidade
ideseTest %>% 
  rename(
    "Bloco Saúde" = bloco_saude,
    "Condições Gerais" = bloco_saude_condicoes_gerais_de_saude,
    "Óbitos por Causa Evitável" = bloco_saude_condicoes_gerais_de_saude_obitos_por_causas_evitaveis,
    "Óbitos Causas Mal Def." = bloco_saude_condicoes_gerais_de_saude_obitos_por_causas_mal_definidas,
    "Longevidade" = bloco_saude_longevidade,
    "Materno Infantil" = bloco_saude_saude_materno_infantil,
    "Consultas Pré Natal" = bloco_saude_saude_materno_infantil_consultas_pre_natal,
    "Mort. Menores 5 Anos" = bloco_saude_saude_materno_infantil_mortalidade_de_menores_de_5_anos,
    " Idese" = idese#gambiarra para o idese ser o primeiro coloquei o caracter alt+255 para ficar na frente de todos
  ) %>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, "Bloco Saúde","Condições Gerais", "Óbitos por Causa Evitável", "Óbitos Causas Mal Def.","Longevidade", "Materno Infantil", "Consultas Pré Natal","Mort. Menores 5 Anos", " Idese") %>% 
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y") +
  labs(
    colour = "Tamanho",
    x = "Ano",
    y = "Indicador"
    )
```
</br>

Neste bloco a margem entre os três é bem pequena, na maioria dos indicadores a diferença n passa de 0,1. A excessão fica com o quesito "Longevidade", no qual as cidades pequenas e medias tem uma vantagem maior quanto as grandes, indicando talvez que uma vida mais tranquila é um fator decisivo para uma vida longeva.
</br>



##Renda##


```{r}
#compração do bloco renda por tamanho de cidade
ideseTest %>% 
  rename(
    " Bloco Renda" = bloco_renda,
    "Apropriação de Renda" = bloco_renda_apropriacao_da_renda,
    "Geração da Renda" = bloco_renda_geracao_da_renda,
    "  Idese" = idese#gambiarra para o idese ser o primeiro coloquei o caracter alt+255 para ficar na frente de todos
  ) %>% 
  group_by(grupoPop, ano) %>% 
  summarise_all(mean)%>% 
  gather(bloco,valor, " Bloco Renda", "Apropriação de Renda", "Geração da Renda", "  Idese") %>% 
  ggplot(aes(x = ano, valor, color = grupoPop)) +
  geom_line()+
  geom_point()+
  facet_wrap(~bloco, scales = "free_y") +
  labs(
    colour = "Tamanho",
    x = "Ano",
    y = "Indicador"
    )
```

</br>

Nestes blocos é possível verificar uma grande variação entre o ano de 2012 e 2013 nas cidades pequenas e médias enquanto a cidade grande não esboçou tamanho aumento. Em 2012 o Rio grande do Sul teve um <a href="https://www.fee.rs.gov.br/pib/em-2013-o-rio-grande-do-sul-liderou-o-crescimento-do-produto-interno-bruto-pib-na-nova-serie-entretanto-perdeu-a-quarta-posicao-entre-as-maiores-economias-do-pais/">aumento no PIB muito significativo de 3,9%</a>, tendo como o setor mais beneficiado a agricultura. Isso explica o grande aumento nas cidades pequenas e médias, visto que a maior fonte de renda das mesmas provém do agronegócio.
</br>


#**Afinal, onde prosperar?**#


Com base nas analises e nos dados apresentados, é possível afirmar que as cidades pequenas e médias são as melhores escolhas para uma vida prospera e longeva.
Tendo em vista o favorecimento do agronegócio do nosso governo atual, essa tendencia não deve mudar para os próximos anos.
No quesito educação, as cidades grandes tem um grande favorecimento quando se fala de ensino superior, portanto caso a academia seja um objetivo de vida, a cidade grande eventualmente poderá ser uma escolha.
