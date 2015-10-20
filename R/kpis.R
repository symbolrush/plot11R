#' kpiEventsInTime: Calculates the percentage of events which
#' have a dtService of less than 15min. In other words: which are in time.
#'
#' @param simEvents A set of simEvents
#'
#' @return EventsInTime A percentage of the fraction of the events in time
kpiEventsInTime <- function(simEvents) {
  return(
    round(
      nrow(simEvents[simEvents$dtService < 15*60,])/nrow(simEvents)*100, 0))
}

#' kpiEventsInTimeDelta: Calculates the delta of the percentage of the events
#' in time between two sets of simEvents. Rounded to 1 decimalplace.
#'
#' @param simEvents
#' @param simEventsBefore
#'
#' @return EventsInTimeDelta A numeric
kpiEventsInTimeDelta <- function(simEvents, simEventsBefore) {
  percentage <- nrow(simEvents[simEvents$dtService < 15*60,])/nrow(simEvents)*100
  percentageBefore <- nrow(
    simEventsBefore[simEventsBefore$dtService < 15*60,])/nrow(simEventsBefore)*100
  round(percentage - percentageBefore, 1)
}

kpiEventsNewInTime <- function(simEvents, simEventsBefore) {
  nrow(plot11R::tableEventsNewInTime(simEvents, simEventsBefore))
}

kpiEventsNewNotInTime <- function(simEvents, simEventsBefore) {
  nrow(plot11R::tableEventsNewNotInTime(simEvents, simEventsBefore))
}

kpiMissionsExternalServiceNeeded <- function(simMissions) {
  length(simMissions$vehicleId[simMissions$vehicleId %in% c(1000,2000)])
}

kpiMissionsByOrganisation <- function(simScenario, organisation) {
  vehicleId <- simScenario$vehicles$id[simScenario$vehicles$organisation == organisation]
  length(simScenario$missions$vehicleId[simScenario$missions$vehicleId %in% vehicleId])
}

kpiEventsInTimeByOrganisationPrio12 <- function(simScenario, organisation) {
  vehicleId <- simScenario$vehicles$id[simScenario$vehicles$organisation == organisation]
  eventId <- unique(simScenario$missions$eventId[simScenario$missions$vehicleId %in% vehicleId])
  simEvents <- simScenario$events[simScenario$events$id %in% eventId, ]
  simEvents <- filterPrio12(simEvents)
  nrow(simEvents[simEvents$dtService < 15*60, ])/nrow(simEvents)*100
}

kpiEventsByOrganisationPrio12 <- function(simScenario, organisation) {
  vehicleId <- simScenario$vehicles$id[simScenario$vehicles$organisation == organisation]
  eventId <- unique(simScenario$missions$eventId[simScenario$missions$vehicleId %in% vehicleId])
  simEvents <- simScenario$events[simScenario$events$id %in% eventId, ]
  simEvents <- filterPrio12(simEvents)
  nrow(simEvents)
}

#' kpiEventsFaster(): Anzahl der Events, welche schneller als im
#' zugrundeliegenden Referenzszenario erreicht wurden.
#'
#' @param simEvents
#' @param simEventsRef
#'
#' @return kpiEventsFaster
kpiEventsFaster <- function(simEvents, simEventsRef) {
  nrow(tableEventsNewFaster(simEvents, simEventsRef)) -
    nrow(tableEventsNewSlower(simEvents, simEventsRef))
}

#' kpiEventsFasterPercentage(): Prozentsatz der Events, welche schneller als
#' im zugrundeliegenden Referenzszenario erreicht wurden.
#'
#' @param simEvents
#' @param simEventsRef
#'
#' @return kpiEventsFasterPercentage A number
kpiEventsFasterPercentage <- function(simEvents, simEventsRef) {
  round(kpiEventsFaster(simEvents, simEventsRef)/nrow(simEvents)*100, 1)
}
