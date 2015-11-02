#' tableNrOfMissionsByOrganisation(): Tabelle mit den explizit gemachten
#' Verschiebungen zwischen den Rettungsdiensten.
#'
#' @param simScenario A simScenario
#' @param simScenarioRef A simScenario
#'
#' @return tableNrOfMissionsByOrganisation A data.frame
tableNrOfMissionsByOrganisation <- function(simScenario, simScenarioRef) {
  simScenario$missions <- filterPrio12(simScenario$missions)
  simScenarioRef$missions <- filterPrio12(simScenarioRef$missions)
  organisation = sort(unique(simScenario$missions$organisation))
  table <- data.frame(
    Rettungsdienst = organisation
  )
  for (i in 1:length(organisation)) {
    nrOfMissions <- c(1:length(organisation))
    vehicleIdOrgI <- simScenario$vehicles$id[
      simScenario$vehicles$organisation == organisation[i]]
    for (j in 1:length(organisation)) {
      vehicleIdOrgJ <- simScenarioRef$vehicles$id[
        simScenarioRef$vehicles$organisation == organisation[j]]
      if (organisation[i] != organisation[j]) {
        BeforeOrgI <- simScenarioRef$missions$id[
          simScenarioRef$missions$vehicleId %in% vehicleIdOrgI]
        BeforeOrgJ <- simScenarioRef$missions$id[
          simScenarioRef$missions$vehicleId %in% vehicleIdOrgJ]
        AfterOrgI <- simScenario$missions$id[
          simScenario$missions$vehicleId %in% vehicleIdOrgI]
        AfterOrgJ <- simScenario$missions$id[
          simScenario$missions$vehicleId %in% vehicleIdOrgJ]

        won <- simScenario$missions
        won <- won[won$id %in% AfterOrgJ, ]
        won <- won[won$id %in% BeforeOrgI, ]

        lost <- simScenario$missions
        lost <- lost[lost$id %in% BeforeOrgJ, ]
        lost <- lost[lost$id %in% AfterOrgI, ]

        nrOfMissions[j] <- nrow(won) - nrow(lost)
      } else {
        nrOfMissions[j] <- "x"
      }
    }
    table <- cbind(table, nrOfMissions)
  }
  names(table) <- c("Rettungsdienst", organisation, "Total")
  return(table)
}



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

#' tableMissionsNewInTime(): Alle Einsätze, die neu innerhalb der Frist erreicht
#' werden als Tabelle
#'
#' @param simMissions A set of simMissions
#' @param simMissionsBefore A set of simMissions
#'
#' @return simMissionsNewInTime A set of simMissions
tableMissionsNewInTime <- function(simMissions, simMissionsBefore) {
  dtService <- simMissions$dtLaunch + simMissions$dtToPoA
  dtServiceBefore <- simMissionsBefore$dtLaunch + simMissionsBefore$dtToPoA
  inTime <- ifelse(dtService < 15*60, TRUE, FALSE)
  inTimeBefore <- ifelse(dtServiceBefore < 15*60, TRUE, FALSE)
  return(simMissions[inTime & !inTimeBefore, ])
}


tableEventsNewInTime <- function(simEvents, simEventsBefore) {
  inTime <- ifelse(simEvents$dtService < 15*60, TRUE, FALSE)
  inTimeOld <- ifelse(simEventsBefore$dtService < 15*60, TRUE, FALSE)
  return(simEvents[inTime & !inTimeOld, ])
}


tableMissionsNewNotInTime <- function(simMissions, simMissionsBefore) {
  dtService <- simMissions$dtLaunch + simMissions$dtToPoA
  dtServiceBefore <- simMissionsBefore$dtLaunch + simMissionsBefore$dtToPoA
  inTime <- ifelse(dtService < 15*60, TRUE, FALSE)
  inTimeOld <- ifelse(dtServiceBefore < 15*60, TRUE, FALSE)
  return(simMissions[!inTime & inTimeOld, ])
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

tableMissionsNewFaster <- function(simMissions, simMissionsBefore) {
  return(simMissions[simMissions$dtToPoA < simMissionsBefore$dtToPoA, ])
}

tableEventsNewSlower <- function(simEvents, simEventsBefore) {
  return(simEvents[simEvents$dtService > simEventsBefore$dtService, ])
}

tableMissionsNewSlower <- function(simMissions, simMissionsBefore) {
  return(simMissions[simMissions$dtToPoA > simMissionsBefore$dtToPoA, ])
}
