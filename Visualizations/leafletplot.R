library(leaflet)


sitecodebook <- "/projectnb/dietzelab/vfeagins/Programming/NVDI_Modeling/GOESdownloadSites.csv" #This is the site codebook




Sites <- read.csv(sitecodebook)

sitenames <- Sites$siteName


leaflet(data = Sites) %>% 
  addTiles() %>% 
  addMarkers(~Long, ~Lat, label = ~siteName)
