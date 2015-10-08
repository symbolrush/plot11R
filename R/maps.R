


mapDtService <- function(simEvents, simEventsBefore) {
  simEvents0 <- simEventsBefore[simEventsBefore$priority %in% c(1,2), ]
  simEvents1 <- simEvents[simEvents$priority %in% c(1,2), ]
  simEventsNewInTime <- simEvents1[simEvents1$dtService < simEvents0$dtService, ]
  simEventsNewInTime <- simEventsNewInTime[simEventsNewInTime$dtService < 15*60, ]
  simEventsNewNotInTime <- simEvents1[simEvents1$dtService > simEvents0$dtService, ]
  simEventsNewNotInTime <- simEventsNewNotInTime[simEventsNewNotInTime$dtService >= 15*60, ]

  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = simEventsNewInTime$missions$lat,
                     lng = simEventsNewInTime$missions$lng,
                     popup = "Neu innerhalb 15 Min. erreicht",
                     color = 'green',
                     stroke = FALSE,
                     radius = 10,
                     fillOpacity = 0.8) %>%
    addCircleMarkers(lat = simEventsNewNotInTime$missions$lat,
                     lng = simEventsNewNotInTime$missions$lng,
                     popup = "Neu nicht mehr erreicht",
                     color = 'red',
                     stroke = FALSE,
                     radius = 10,
                     fillOpacity = 0.8) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
}

mapPoAClustered <- function(lat, lng, popup) {
  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = lat,
                     lng = lng,
                     popup = popup,
                     color = fhsblue(),
                     stroke = FALSE,
                     radius = 10,
                     fillOpacity = 0.8,
                     clusterOptions = markerClusterOptions()) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
}

mapVehiclesClustered <- function(lat, lng, popup) {
  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = lat,
                     lng = lng,
                     popup = popup,
                     color = fhsblue(),
                     stroke = FALSE,
                     radius = 10,
                     fillOpacity = 0.8,
                     clusterOptions = markerClusterOptions()) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
}
