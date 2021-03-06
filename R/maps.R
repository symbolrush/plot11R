#' mapOrganisationPrio12(): Die famose Karte mit einem Punkt für jeden
#' Rettungsdienst. Die Farbe zeigt die Hilfsfristeinhaltung.
#'
#' @param simScenario
#'
#' @return map A leaflet object
mapOrganisationPrio12 <- function(simScenario) {
  # organisation <- c("RD Luzern", "RD Sursee", "RD Wolhusen")
  organisation <- c("Sano", "STS", "FMI", "SNBe", "RSE", "ARB", "HJB", "SRO")
  lat <- c()
  lng <- c()
  percentage <- c()
  nrOfMissions <- c()
  # nrOfEvents <- c()
  color <- c()
  j <- 1
  for (i in organisation) {
    lat[j] <- mean(
      simScenario$vehicles$lat[simScenario$vehicles$organisation == i])
    lng[j] <- mean(
      simScenario$vehicles$lng[simScenario$vehicles$organisation == i])
    percentage[j] <- round(
      plot11R::kpiMissionsInTimeByOrganisationPrio12(simScenario, i), 0)
    nrOfMissions[j] <- plot11R::kpiMissionsByOrganisationPrio12(simScenario, i)
    color[j] <- plot11R::cpDtService(percentage[j])
    j <- j + 1
  }

  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(
      lat = lat,
      lng = lng,
      stroke = FALSE,
      # radius = nrOfEvents/50,
      radius = nrOfMissions/150,
      color = color,
      popup = paste0(organisation, ': ',nrOfMissions,
                     ' Einsätze, davon ', percentage,
                     '% innerhalb 15min. erreicht.'),
      fillOpacity = 0.6) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite")
    )
}



#' mapDtServicePoA: Die famose Karte aufgelöst nach Einsatzort (für Bern
#' bedeutet das aufgelöst nach Ort)
#'
#' @param simMissions
#'
#' @return mapDtServicePoA A leaflet map
mapDtServicePoA <- function(simMissions) {
  lat <- unique(simMissions$lat)
  lng <- c()
  nrOfMissions <- c()
  percentage <- c()
  color <- c()

  for (i in 1:length(lat)) {
    lng[i] <- simMissions$lng[
      simMissions$lat == lat[i]][1]
    nrOfMissions[i] <- nrow(
      filterPrio12(simMissions[simMissions$lat == lat[i],]))
    percentage[i] <- plot11R::kpiMissionsInTime(
        filterPrio12(simMissions[simMissions$lat == lat[i], ]))
    color[i] <- plot11R::cpDtService(percentage[i])
  }

  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = lat,
                     lng = lng,
                     popup = paste0(
                       nrOfMissions, " Einsätze, davon ",
                       percentage, "% innerhalb 15 Min. erreicht."),
                     color = color,
                     stroke = FALSE,
                     radius = nrOfMissions/150 + 5,
                     fillOpacity = 0.7) %>%
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



mapDtServiceDeltaInTime <- function(simMissions, simMissionsRef) {
  simMissionsNewInTime <- plot11R::tableMissionsNewInTime(
    simMissions, simMissionsRef)
  simMissionsNewNotInTime <- plot11R::tableMissionsNewNotInTime(
    simMissions, simMissionsRef)

  nrOfMissionsNewInTime <- c()
  nrOfMissionsNewNotInTime <- c()
  latNewInTime <- unique(simMissionsNewInTime$lat)
  lngNewInTime <- c()
  latNewNotInTime <- unique(simMissionsNewNotInTime$lat)
  lngNewNotInTime <- c()
  for (i in 1:length(latNewInTime)) {
    lngNewInTime[i] <- simMissionsNewInTime$lng[
      simMissionsNewInTime$lat == latNewInTime[i]][1]
    nrOfMissionsNewInTime[i] <- nrow(
      simMissionsNewInTime[simMissionsNewInTime$lat == latNewInTime[i],]) - nrow(
        simMissionsNewNotInTime[
          simMissionsNewNotInTime$lat == latNewInTime[i],]
      )
  }
  for (i in 1:length(latNewNotInTime)) {
    lngNewNotInTime[i] <- simMissionsNewNotInTime$lng[
      simMissionsNewNotInTime$lat == latNewNotInTime[i]][1]
    nrOfMissionsNewNotInTime[i] <- nrow(
      simMissionsNewNotInTime[
        simMissionsNewNotInTime$lat == latNewNotInTime[i],]) - nrow(
          simMissionsNewInTime[simMissionsNewInTime$lat == latNewNotInTime[i],]
        )
  }


  nrOfMissionsNewInTime <- ifelse(nrOfMissionsNewInTime > 0, nrOfMissionsNewInTime, 0)
  nrOfMissionsNewNotInTime <- ifelse(nrOfMissionsNewNotInTime > 0, nrOfMissionsNewNotInTime, 0)





#   simEventsNewInTime <- plot11R::tableEventsNewInTime(
#  simEvents, simEventsBefore)
#   simEventsNewNotInTime <- plot11R::tableEventsNewNotInTime(
#  simEvents, simEventsBefore)

  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = latNewNotInTime,
                     lng = lngNewNotInTime,
                     group = "Neu nicht innerhalb 15 Min. erreichte Einsätze",
                     color = 'red',
                     popup = paste0(nrOfMissionsNewNotInTime, " Einsätze sind neu nicht innerhalb 15 min. erreicht"),
                     stroke = FALSE,
                     radius = nrOfMissionsNewNotInTime,
                     fillOpacity = 0.7) %>%
    addCircleMarkers(lat = latNewInTime,
                     lng = lngNewInTime,
                     group = "Neu innerhalb 15 Min. erreichte Einsätze",
                     color = 'green',
                     popup = paste0(nrOfMissionsNewInTime, " Einsätze sind neu innerhalb 15 min. erreicht"),
                     stroke = FALSE,
                     radius = nrOfMissionsNewInTime,
                     fillOpacity = 0.7) %>%
    addLayersControl(
      baseGroups = c("OSM", "Stamen.TonerLite"),
      overlayGroups = c("Neu innerhalb 15 Min. erreichte Einsätze",
                        "Neu nicht innerhalb 15 Min. erreichte Einsätze"),
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

#' mapDtToPoA(): Die blau/gelb Karte aufgelöst nach PoA
#'
#' @param simMissions A set of simMissions
#' @param simMissionsRef A set of simMissions
#'
#' @return mapDtToPoA A leaflet map
mapDtToPoA <- function(simMissions, simMissionsRef) {
  simMissionsNewFaster <- plot11R::tableMissionsNewFaster(
    simMissions, simMissionsRef)
  simMissionsNewSlower <- plot11R::tableMissionsNewSlower(
    simMissions, simMissionsRef)

  nrOfMissionsFaster <- c()
  nrOfMissionsSlower <- c()
  latFaster <- unique(simMissionsNewFaster$lat)
  lngFaster <- c()
  latSlower <- unique(simMissionsNewSlower$lat)
  lngSlower <- c()
  for (i in 1:length(latFaster)) {
    lngFaster[i] <- simMissionsNewFaster$lng[
      simMissionsNewFaster$lat == latFaster[i]][1]
    nrOfMissionsFaster[i] <- nrow(
      simMissionsNewFaster[simMissionsNewFaster$lat == latFaster[i],]) - nrow(
        simMissionsNewSlower[simMissionsNewSlower$lat == latFaster[i],])
  }
  for (i in 1:length(latSlower)) {
    lngSlower[i] <- simMissionsNewSlower$lng[
      simMissionsNewSlower$lat == latSlower[i]][1]
    nrOfMissionsSlower[i] <- nrow(
      simMissionsNewSlower[simMissionsNewSlower$lat == latSlower[i],]) - nrow(
        simMissionsNewFaster[simMissionsNewFaster$lat == latSlower[i],])
  }
  nrOfMissionsFaster <- ifelse(nrOfMissionsFaster > 0, nrOfMissionsFaster, 0)
  nrOfMissionsSlower <- ifelse(nrOfMissionsSlower > 0, nrOfMissionsSlower, 0)


  library(leaflet)
  leaflet(width = "100%") %>%
    addTiles(group = "OSM") %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(lat = latSlower,
                     lng = lngSlower,
                     group = "Neu langsamer",
                     color = 'yellow',
                     popup = paste0(nrOfMissionsSlower, " Einsätze sind neu langsamer"),
                     stroke = FALSE,
                     radius = nrOfMissionsSlower/nrow(simMissionsNewFaster)*300,
                     fillOpacity = 0.7) %>%
    addCircleMarkers(lat = latFaster,
                     lng = lngFaster,
                     group = "Neu schneller",
                     color = fhsblue(),
                     popup = paste0(nrOfMissionsFaster, " Einsätze sind neu schneller"),
                     stroke = FALSE,
                     radius = nrOfMissionsFaster/nrow(simMissionsNewFaster)*300,
                     fillOpacity = 0.7) %>%
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
                     color = 'gold',
                     stroke = FALSE,
                     radius = 5,
                     fillOpacity = 0.8,
                     clusterOptions = markerClusterOptions(iconCreateFunction = JS("function (cluster) {
    var childCount = cluster.getChildCount();
    var c = ' marker-cluster-';
    c += 'medium';
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
  }")))
}
