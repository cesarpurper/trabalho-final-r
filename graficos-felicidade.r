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



mundo = spData::world %>% 
  mutate(
    name_long = as.vector(str_trim(name_long))
  )

mundo$name_long[mundo$name_long == 'South Sudan'] <- 'Sudan'

felicidade = read.csv("data/felicidade_mundo.csv", encoding = "UTF-8") %>% 
  clean_names() %>% 
  mutate(
    happiness_score = as.double(happiness_score),
    name = as.character(name)
  )%>% 
  mutate(
    name = str_trim(name)
  ) 


felicidade$name[felicidade$name == 'Côte d\'Ivoire'] = "Ivory Coast"
felicidade$name[felicidade$name == 'Russia'] <- 'Russian Federation'
felicidade$name[felicidade$name == 'Laos'] <- 'Myanmar'
felicidade$name[felicidade$name == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'

felicidade$name = as.vector(felicidade$name)


grouped = felicidade %>% 
  inner_join(
    mundo,
    by = c("name" =  "name_long")
  )%>% 
  st_as_sf() 


grouped %>% 
  gather(bloco,valor, economic_liberty, property_rights, government_integrity, judicial_effectiveness, tax_burden, government_spending, fiscal_health, business_freedom, labor_freedom, monetary_freedom, trade_freedom, investment_freedom, financial_freedom) %>% 
  group_by(continent, index_year, bloco,happiness_score) %>% 
  summarise(
    valor = mean(valor)
  ) %>% 
  ggplot(aes(x = happiness_score, valor)) +
  geom_point()+
  geom_smooth(se = FALSE, method = lm)+
  facet_wrap(~bloco, scales= "free_y")

##mapa mundi com cores por felicidade

mapa_felicidade_ratio = grouped %>% 
  filter(!is.na(happiness_score)) %>% 
  group_by(name) %>% 
  summarise_all(mean)

pal <- colorNumeric(
  palette = "Greens",
  domain = mapa_felicidade_ratio$happiness_score
)

tooltip <- sprintf("<strong>%s</strong><br>
                   <p>%s</p>", 
                   mapa_felicidade_ratio$name, 
                   mapa_felicidade_ratio$happiness_score)

leaflet(mapa_felicidade_ratio) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillOpacity = 0,
              weight      = 0.85,
              color       = "#000000") %>%
  
  addPolygons(color      = ~pal(happiness_score),
              stroke      = F,
              weight      = 0.1,
              fillOpacity = 0.8,
              popup       = tooltip) %>%
  addLegend("bottomright",
            pal    = pal,
            values = ~happiness_score,
            title  = "Felicidade")


##grid com os gráficos do highchart

tooltip <- c("<br/>Nome : <b>{point.name}</b><br/>
             <br/>Felicidade : {point.happiness_score}<br/>")

HCDataset = grouped %>% 
  mutate(
    name = paste(name, index_year,sep=" - "),
    happiness_score = round(happiness_score, digits=2)
  )

lm.model.property <- augment(lm(property_rights ~ happiness_score, data = HCDataset))

HC_propertyRights = HCDataset%>%
  hchart("scatter", hcaes(x = happiness_score, y = property_rights, group = continent)) %>%
  hc_add_series(lm.model.property, "line", hcaes(x = happiness_score, y = .fitted), color = "black", enableMouseTracking = FALSE,showInLegend = FALSE) %>% 
  hc_yAxis(max = 100) %>% 
  hc_tooltip(pointFormat = tooltip)


lm.model.business_freedom <- augment(lm(business_freedom ~ happiness_score, data = HCDataset))
HC_businessFreedom = HCDataset%>%
  hchart("scatter", hcaes(x = happiness_score, y = business_freedom, group = continent)) %>%
  hc_add_series(lm.model.business_freedom, "line", hcaes(x = happiness_score, y = .fitted), color = "black", enableMouseTracking = FALSE,showInLegend = FALSE) %>% 
  hc_yAxis(max = 100) %>% 
  hc_tooltip(pointFormat = tooltip)

lm.model.government_spending <- augment(lm(government_spending ~ happiness_score, data = HCDataset))
HC_govSpending = HCDataset%>%
  hchart("scatter", hcaes(x = happiness_score, y = government_spending, group = continent)) %>%
  hc_add_series(lm.model.government_spending, "line", hcaes(x = happiness_score, y = .fitted), color = "black", enableMouseTracking = FALSE,showInLegend = FALSE) %>% 
  hc_yAxis(max = 100) %>% 
  hc_tooltip(pointFormat = tooltip)

lm.model.government_integrity <- augment(lm(government_integrity ~ happiness_score, data = HCDataset))
HC_govIntegrity = HCDataset%>%
  hchart("scatter", hcaes(x = happiness_score, y = government_integrity, group = continent)) %>%
  hc_add_series(lm.model.government_integrity, "line", hcaes(x = happiness_score, y = .fitted), color = "black", enableMouseTracking = FALSE,showInLegend = FALSE) %>% 
  hc_yAxis(max = 100) %>% 
  hc_tooltip(pointFormat = tooltip)

lst <- list(HC_propertyRights,HC_businessFreedom,HC_govIntegrity,HC_govSpending)
hw_grid(lst, rowheight = 300,ncol = 2)
           

               
