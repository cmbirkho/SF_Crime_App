
library(leaflet)
library(rgdal)

# https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=GeoJSON

sfHoods <- readOGR("SF Find Neighborhoods.geojson")

pal <- colorFactor("viridis", sfHoods$name)

leaflet(sfHoods) %>%
    addTiles() %>%
    addPolygons(stroke = FALSE,
                smoothFactor = 0.5, 
                fillOpacity = 1,
                fillColor = ~ pal(sfHoods$name))



