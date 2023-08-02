## Interactive map
library(leaflet)

# get population
pop <- lga$pop %>% 
  group_by(Year, LGA_Code) %>% 
  summarise(N = sum(N),
            .groups = "drop") %>% 
  group_by(LGA_Code) %>% 
  summarise(N = mean(N))

# get dataset
mapping_data <- left_join(map, pop, by = c("LGA_COD" = "LGA_Code")) %>% 
  left_join(.,mutate(lga$seifa_ra, LGA_Code = as.character(LGA_Code)), by = c("LGA_COD" = "LGA_Code")) %>% 
  mutate(IRSD_5 = as.factor(IRSD_5))

# map labels
labels <- sprintf("<strong> %s \nPopulation: %g", 
                  mapping_data$LGA_NAM, mapping_data$N) %>%
  lapply(htmltools::HTML) # to get the line breaks to work as 
# <br/> is a html command

pal <- colorFactor(topo.colors(5), domain = mapping_data$IRSD_5) # number of bins

leaflet(mapping_data) %>% 
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(IRSD_5), # the variable we want to use to fill
    fillOpacity = 0.8, # how bright are the fill colors
    color = "grey", # color of the boundaries
    weight = 0.8, # weight of the boundaries
    # how thick are the outlines of areas when we highlight them
    highlightOptions = highlightOptions(weight=3), 
    # labels as defined with the sprintf function
    label = labels, 
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"
      ))) %>% 
  addLegend(
    pal = pal, 
    values = ~IRSD_5, 
    opacity = 1, # how easy is it to see through the legend
    title = "IRSD", # title for legend
    position = "bottomright"
  )
