#' tableDtService(): Liefert eine Tabelle mit ausgesuchten Spalten zweier Sets
#' von simMissions
#'
#' @param simMissions A set of simMissions
#' @param simMissionsBefore A set of simMissions
#'
#' @return tableDtService A data.frame
tableDtService <- function(simScenario, simScenarioRef) {
  simMissions <- simScenario$missions
  simMissionsBefore <- simScenarioRef$missions
  missions <- simMissions[
    simMissions$dtToPoA != simMissionsBefore$dtToPoA, ]
  missionsBefore <- simMissionsBefore[
    simMissionsBefore$dtToPoA != simMissions$dtToPoA, ]
  waytime <- c()
  waytimeBefore <- c()
  cache <- cacheR::loadCache("Bern")
  for (i in 1:nrow(missions)) {
    waytime[i] <- nine11R::getWaytime(
      cache,
      simScenario$vehicles$lat[missions$vehicleId[i]],
      simScenario$vehicles$lng[missions$vehicleId[i]],
      missions$lat[i],
      missions$lng[i]
    )$waytime
    waytimeBefore[i] <- nine11R::getWaytime(
      cache,
      simScenarioRef$vehicles$lat[missionsBefore$vehicleId[i]],
      simScenarioRef$vehicles$lng[missionsBefore$vehicleId[i]],
      missionsBefore$lat[i],
      missionsBefore$lng[i]
    )$waytime
 }


  return(data.frame(
    id = missions$id,
    priority = missions$priority,
    organisation = missions$organisation,
    vehicleIdBefore = missionsBefore$vehicleId,
    vehicleNameBefore = simScenarioRef$vehicles$name[missionsBefore$vehicleId],
    vehicleId = missions$vehicleId,
    vehicleName = simScenario$vehicles$name[missions$vehicleId],
    dtToPoABefore = missionsBefore$dtToPoA,
    dtToPoA = missions$dtToPoA,
    waytimeBefore = waytimeBefore,
    waytime = waytime,
    lat = missions$lat,
    lng = missions$lng
  ))
}

#' tableDtServicePrio12
#'
#' @param simScenario
#' @param simScenarioRef
#'
#' @return tableDtServicePrio12 A data.frame
tableDtServicePrio12 <- function(simScenario, simScenarioRef){
  simScenario$missions <- filterPrio12(simScenario$missions)
  simScenarioRef$missions <- filterPrio12(simScenarioRef$missions)
  tableDtService(simScenario, simScenarioRef)
}



tableEventsNewInTime <- function(simEvents, simEventsBefore) {
  inTime <- ifelse(simEvents$dtService < 15*60, TRUE, FALSE)
  inTimeOld <- ifelse(simEventsBefore$dtService < 15*60, TRUE, FALSE)
  return(simEvents[inTime & !inTimeOld, ])
}


tableEventsNewNotInTime <- function(simEvents, simEventsBefore) {
  inTime <- ifelse(simEvents$dtService < 15*60, TRUE, FALSE)
  inTimeOld <- ifelse(simEventsBefore$dtService < 15*60, TRUE, FALSE)
  return(simEvents[!inTime & inTimeOld, ])
}


tableEventsDifferenceDtService <- function(simEvents, simEventsBefore) {
  return(simEvents[simEvents$dtService != simEventsBefore$dtService, ])
}

tableEventsNewFaster <- function(simEvents, simEventsBefore) {
  return(simEvents[simEvents$dtService < simEventsBefore$dtService, ])
}

tableEventsNewSlower <- function(simEvents, simEventsBefore) {
  return(simEvents[simEvents$dtService > simEventsBefore$dtService, ])
}
