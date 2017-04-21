

world <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")
# is text
world <- jsonlite::fromJSON(world, simplifyVector = FALSE)

# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_corrientes_maritimas
marine <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_corrientes_maritimas&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# marine <- geojsonio::as.json(marine)


# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_limites_placas
plates <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_limites_placas&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# plates <- geojsonio::as.json(plates)

# http://cedeusdata.geosteiniger.cl/layers/geonode:mundo_volcanes
volcano <- getContent("http://cedeusdata.geosteiniger.cl/geoserver/wfs?srsName=EPSG%3A4326&typename=geonode%3Amundo_volcanes&outputFormat=json&version=1.0.0&service=WFS&request=GetFeature")
# volcano <- geojsonio::as.json(volcano)

hailData <- stormData[stormData$EVTYPE == "HAIL" & stormData$STATE == "PA",]
#hailData <- hailData[complete.cases(hailData),]

haildf <- data.frame(
  names = hailData$COUNTYNAME,
  lat = hailData$LATITUDE/100,
  lon = hailData$LONGITUDE/100,
  z = hailData$FATALITIES#,
  #color = colorize(z),
  #sequence =
)
haildf <- haildf[complete.cases(haildf),]

hcmap("custom/usa-and-canada", showInLegend = FALSE) %>%
  hc_add_series(data = haildf, type = "mapbubble",
                minSize = 0, maxSize = 30)
%>%
  hc_motion(enabled = TRUE, series = 1,# labels = 1:n,
            loop = TRUE, autoPlay = TRUE,
            updateInterval = 1000, magnet = list(step =  1)) %>%
  hc_plotOptions(series = list(showInLegend = FALSE))

library(rworldmap)
newmap <- getMap(resolution = "low")
plot(newmap)
points(haildf$lon,haildf$lat, col = "red", cex = .6)

set.seed(1234)

n <- 20
z <-  sample(1:n)
sequences <- map2(1:n, z, function(x, y){ ifelse(x == 1:n, y, 0) })

df <- data_frame(
  lat = runif(n, -180, 180),
  lon = runif(n, -180, 180),
  z = z,
  color = colorize(z),
  sequence = sequences
)




hcmap() %>%
  hc_add_series(data = df, type = "mapbubble",
                minSize = 0, maxSize = 30) %>%
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE,
            updateInterval = 1000, magnet = list(step =  1)) %>%
  hc_plotOptions(series = list(showInLegend = FALSE))

