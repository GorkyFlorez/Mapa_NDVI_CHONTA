library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(cptcity)
library(leaflet)
library(leafem)
library(leaflet.extras)
NDVI <- raster("Raster/Lazo2021_NDVI_2021_12_14_16_34_20.tif")

NDVI.pa        <-  rasterToPoints(NDVI)
NDVI.pa_a      <-  data.frame(NDVI.pa)

viz_ndvi <- list(palette = cpt("grass_ndvi"))
find_cpt("grass")

col = cpt(pal = "grass_ndvi")
#------------------------------------------------------------------------
Mapa =ggplot()+
 geom_raster(data = NDVI.pa_a , aes(x,y,fill = Lazo2021_NDVI_2021_12_14_16_34_20)) + 
  scale_fill_gradientn(colors = cpt(pal = "grass_evi"), name="NDVI") +
  scale_x_continuous(name=expression(paste("Longitude (",degree,")"))) +
  scale_y_continuous(name=expression(paste("Latitude (",degree,")")))+
  theme_bw()+
  theme(legend.position = c(0.95,0.14),
        panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        legend.background = element_blank(),
        legend.text = element_text(size=8,face=2),
        legend.title = element_text(size=11,face=2),
        panel.background = element_rect(fill = "aliceblue"))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -69.65, y = -12.96, hjust = 0, vjust = 0, lineheight = .9,
           label = "Author: Gorky Florez (@gflorezc) Original Idea: Aprende R desde cero, Geometries: RStudio Data: ING-Peru, 2022;",
           size = 3, family = "serif", color = "grey50")+
  # title
  annotate(geom = "text", x = -69.65, y = -12.80, hjust = 0, vjust = 1, 
           label = "Microcuenca Chonta:  NDVI Índice de \nvegetación de diferencia normalizada",
           size = 7, family="serif", color = "grey20")+
  annotate(geom = "text", x = -69.61,, y = -12.942, hjust = 1, vjust = 0,
           label = "Codigo en Githab", fontface = 2,
           size = 3, family = "serif", color = "grey20")+
  annotate(geom = "text", x = -69.6,, y = -12.948, hjust = 1, vjust = 0,
           label = "https://github.com/GorkyFlorez/Mapa_NDVI_CHONTA",
           size = 3, family ="serif", color = "#bb3e03")+
  # date
  annotate(geom = "text", x = -69.47, y = -12.78, hjust = 0, vjust = 0,
           label = "2022", fontface = 2,
           size = 5, family = "serif", color = "#35978f")

#ggsave("Mapas/NDVI-Chonta.png", Mapa, width = 11, height = 11.76, 
# dpi = 900, type = "cairo-png")
#------------------------------------------------------------------------
m="https://www.osgeo.org/wp-content/uploads/leaflet.png"

LeaMapa = leaflet() %>%
  addTiles() %>%
  addRasterImage(NDVI, project = TRUE,colors = cpt(pal = "grass_ndvi"))

leaflet() %>%
  addControl(html = "<p><strong><em>Índice de vegetación de diferencia normalizada- Chonta</em></strong></p>",
             position = "topright")%>%
  addLogo(m,url = "https://images.vexels.com/media/users/3/143561/isolated/preview/afa3aa927b63061e3b0222b7dab9cdbf-ubicaci--n-n--utica-norte-flecha-vintage-by-vexels.png",
          position = "topleft",
          offset.x = 50,
          offset.y = 10,
          width = 150,
          height = 100)%>%
  addRasterImage(NDVI, project = TRUE,colors = cpt(pal = "grass_ndvi"), group = "NDVI - Chonta")%>%
  addLayersControl(baseGroups = c("OSM", "Satellite","OTM","Toner","Terrain","Terrain.ESRI", "Toner Lite","CartoDB.Positron"),
                   overlayGroups = c("NDVI - Chonta" ),
                   position = "topright",
                   options = layersControlOptions(collapsed = T))%>%
  addProviderTiles(providers$OpenStreetMap, group = "OSM")%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")%>%
  addProviderTiles(providers$OpenTopoMap, group = "OTM")%>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Terrain.ESRI") %>%

  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addMiniMap(tiles = providers$Esri.WorldImagery,toggleDisplay = TRUE)%>%
  addScaleBar(position = "bottomright",options = scaleBarOptions(maxWidth = 100,
                                                                 metric = TRUE,
                                                                 imperial = TRUE,
                                                                 updateWhenIdle = TRUE)) %>%
  addDrawToolbar(targetGroup = "Graficos",editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))%>%
  addMeasure(position = "topleft",
             primaryLengthUnit = "meters",
             primaryAreaUnit = "sqmeters",
             activeColor = "#3D535D",
             completedColor = "#7D4479")%>% 
  addSearchGoogle() %>% 
  addControlGPS() %>% 
  addResetMapButton()
