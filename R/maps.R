#' mapOrganisationPrio12(): Die famose Karte mit einem Punkt für jeden
#' Rettungsdienst. Die Farbe zeigt die Hilfsfristeinhaltung.
#'
#' @param simScenario
#'
#' @return map A leaflet object
mapOrganisationPrio12 <- function(simScenario) {
  organisation <- c("RD Luzern", "RD Sursee", "RD Wolhusen")
  lat <- c()
  lng <- c()
  percentage <- c()
  nrOfEvents <- c()
  color <- c()
  j <- 1
  for (i in organisation) {
    lat[j] <- mean(simScenario$vehicles$lat[simScenario$vehicles$organisation == i])
    lng[j] <- mean(simScenario$vehicles$lng[simScenario$vehicles$organisation == i])
    percentage[j] <- plot11R::kpiEventsInTimeByOrganisationPrio12(simScenario, i)
    nrOfEvents[j] <- plot11R::kpiEventsByOrganisationPrio12(simScenario, i)
    color[j] <- plot11R::cpDtService(percentage[j])
    j <- j+1
  }

  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(
      lat = lat,
      lng = lng,
      stroke = FALSE,
      radius = nrOfEvents/50,
      color = color,
      popup = paste0(organisation, ': ',nrOfEvents, ' Events, davon ', round(percentage, 0), '% innerhalb 15min. erreicht.'),
      fillOpacity = 0.6) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
}




mapDtService <- function(simEvents, center_lat, center_lng) {
  library(leaflet)
  leaflet(data = simEvents, width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(
      lat = ~lat,
      lng = ~lng,
      stroke = FALSE,
      radius = 8,
      color = ifelse(simEvents$dtService < 15*60, 'green', 'red'),
      group = ifelse(simEvents$dtService < 15*60, 'green', 'red'),
      fillOpacity = 0.2) %>%
    setView(lat = center_lat, lng = center_lng, zoom = 12) %>%
  addLayersControl(
    baseGroups = c("OSM", "Stamen.TonerLite"),
    overlayGroups = c('green', 'red'),
    options = layersControlOptions(collapsed = FALSE)
  )
}



mapDtServiceDeltaInTime <- function(simEvents, simEventsBefore) {
  simEventsNewInTime <- plot11R::tableEventsNewInTime(simEvents, simEventsBefore)
  simEventsNewNotInTime <- plot11R::tableEventsNewNotInTime(simEvents, simEventsBefore)

  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = simEventsNewInTime$lat,
                     lng = simEventsNewInTime$lng,
                     group = "Neu erreichte Ereignisse",
                     popup = "Neu innerhalb 15 Min. erreicht",
                     color = 'green',
                     stroke = FALSE,
                     radius = 8,
                     fillOpacity = 0.2) %>%
    addCircleMarkers(lat = simEventsNewNotInTime$lat,
                     lng = simEventsNewNotInTime$lng,
                     group = "Neu nicht erreichte Ereignisse",
                     popup = "Neu nicht mehr erreicht",
                     color = 'red',
                     stroke = FALSE,
                     radius = 8,
                     fillOpacity = 0.2) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite"),
      overlayGroups = c("Neu erreichte Ereignisse", "Neu nicht erreichte Ereignisse"),
      options = layersControlOptions(collapsed = FALSE)
    )
}


mapDtServiceDelta <- function(simEvents, simEventsBefore) {
  simEventsNewFaster <- plot11R::tableEventsNewFaster(simEvents, simEventsBefore)
  simEventsNewSlower <- plot11R::tableEventsNewSlower(simEvents, simEventsBefore)

  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = simEventsNewFaster$lat,
                     lng = simEventsNewFaster$lng,
                     group = "Neu schneller",
                     popup = "Neu schneller",
                     color = 'blue',
                     stroke = FALSE,
                     radius = 8,
                     fillOpacity = 0.2) %>%
    addCircleMarkers(lat = simEventsNewSlower$lat,
                     lng = simEventsNewSlower$lng,
                     group = "Neu langsamer",
                     popup = "Neu langsamer",
                     color = 'yellow',
                     stroke = FALSE,
                     radius = 8,
                     fillOpacity = 0.2) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite"),
      overlayGroups = c("Neu schneller", "Neu langsamer"),
      options = layersControlOptions(collapsed = FALSE)
    )
}



mapPoAClustered <- function(simEvents) {
  library(leaflet)
  leaflet(data = simEvents, width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = ~lat,
                     lng = ~lng,
                     popup = "popup",
                     color = fhsblue(),
                     stroke = FALSE,
                     radius = 10,
                     fillOpacity = 0.8,
                     clusterOptions = markerClusterOptions()) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
}


mapVehiclesClustered <- function(simVehicles) {
  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addCircleMarkers(lat = simVehicles$lat,
                     lng = simVehicles$lng,
                     popup = paste(simVehicles$name),
                     color = fhsblue(),
                     stroke = FALSE,
                     radius = 10,
                     fillOpacity = 0.8,
                     clusterOptions = markerClusterOptions())
}
