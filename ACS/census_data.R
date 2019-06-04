library(pacman)

p_load(tidyverse, tidycensus, leaflet, sf, data.table)

#B03001_003E Total Hispanic Population
#B01003_001 Total Population
#B08126_001 Total
#B08126_002 Total Ag, Fishing and Forrestry 
#B05006_001 Place of birth foreign born
#B06007_004 Speak Spanish:!!Speak English "very well"
#B06007_003 Speak Spanish: 
#B06007_002 Speaks only english
#B06007_005 Speak Spanish:!!Speak English less than "very well"

wa_pop <- get_acs(geography = "tract", 
                  variables = c("B03001_003E", "B08126_002", "B05006_001",
                                "B06007_004", "B06007_002", "B06007_005"), 
                  state = c("WA"),
                  summary_var = c("B01003_001"),
                  geometry = T) 

wa_pop$county = unlist(lapply(strsplit(wa_pop$NAME, ", "), FUN = function(x) x[2]))
wa_pop$tract = unlist(lapply(strsplit(wa_pop$NAME, ", "), FUN = function(x) x[1]))

wa_pop$moe = NULL

wa_pop = spread(wa_pop, "variable", "estimate", drop=T)

#save
setwd("./rImported")
saveRDS(wa_pop,file="censusPop.RDS")

# wa_pop = readRDS("censusPop.RDS")

lookuptable = data.table(Description = 
                           c("Total Hispanic Population", "Total Ag, Fishing and Forrestry",
                             "Foreign Place of Birth", "Speaks Spanish; English Very Well",
                             "Speaks only English",
                             "Speaks Spanish; English less than Very Well"), 
                         colname_val = 
                           c("B03001_003", "B08126_002", "B05006_001",
                             "B06007_004", "B06007_002", "B06007_005"), 
                         summary_comp = 
                           "summary_est")

ind = 1

numer= wa_pop[, lookuptable[ind, ]$colname_val]
denom = wa_pop[, lookuptable[ind, ]$summary_comp]

st_geometry(numer) = NULL
st_geometry(denom) = NULL

colnames(numer) = "numerator"
colnames(denom) = "denominator"

wa_pop$numerator = numer
wa_pop$denominator = denom


pal <- colorNumeric(palette = "viridis", domain = wa_pop$numerator/wa_pop$denominator * 100, n = 10)

wa_pop %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ paste(tract, "<br>",county, "<br>" , lookuptable[ind]$Description, unlist(numerator),"<br>","Total Population",unlist(denominator) ),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = pal(unlist(wa_pop$numerator/wa_pop$denominator * 100))) %>%
  addLegend("bottomright", 
            pal = pal, 
            na.label = "",
            labFormat = labelFormat(suffix = "%"),
            values = unlist(wa_pop$numerator/wa_pop$denominator * 100),
            title = paste(lookuptable[ind,]$Description),
            opacity = 1)

wa_estimates <- get_estimates(geography = "county", state = "WA",year = 2017, time_series=T,
                              product = "characteristics", breakdown = "HISP", geometry = T, output="wide")
