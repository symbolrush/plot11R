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
#   for (i in 1:nrow(missions)) {
#     waytime[i] <- nine11R::getWaytimeOSRM(
#       missions$lat[i],
#       missions$lng[i],
#       simScenario$vehicles$lat[missions$vehicleId],
#       simScenario$vehicles$lng[missions$vehicleId]
#     )
#     waytimeBefore[i] <- nine11R::getWaytimeOSRM(
#       missionsBefore$lat[i],
#       missionsBefore$lng[i],
#       simScenarioRef$vehicles$lat[missionsBefore$vehicleId],
#       simScenarioRef$vehicles$lng[missionsBefore$vehicleId]
#     )
#  }


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
#     waytimeBefore = waytimeBefore,
#     waytime = waytime,
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
