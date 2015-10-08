#' kpiEventsInTime: Calculates the percentage of events of priority 1&2 which
#' have a dtService of less than 15min. In other words: which are in time.
#' Rounded to 1 decimalplace.
#'
#' @param simEvents A set of simEvents
#'
#' @return EventsInTime A percentage of the fraction of the events in time
kpiEventsInTime <- function(simEvents) {
  simEvents <- simEvents[simEvents$priority %in% c(1,2), ]
  return(
    round(
      nrow(simEvents[simEvents$dtService < 15*60,])/nrow(simEvents)*100, 1))
}

#' kpiEventsInTimeDelta: Calculates the delta of the percentage of the events
#' in time between two sets of simEvents. Rounded to 1 decimalplace.
#'
#' @param simEvents
#' @param simEventsBefore
#'
#' @return EventsInTimeDelta A numeric
kpiEventsInTimeDelta <- function(simEvents, simEventsBefore) {
  simEvents0 <- simEventsBefore[simEventsBefore$priority %in% c(1,2), ]
  simEvents1 <- simEvents[simEvents$priority %in% c(1,2), ]
  percentage1 <- nrow(
    simEvents1[simEvents1$dtService < 15*60,])/nrow(simEvents1)*100
  percentage0 <- nrow(
    simEvents0[simEvents0$dtService < 15*60,])/nrow(simEvents0)*100
  round(percentage1 - percentage0, 1)
}

kpiEventsNewInTime <- function(simEvents, simEventsBefore) {
#   simEvents0 <- simEventsBefore[simEventsBefore$priority %in% c(1,2), ]
#   simEvents1 <- simEvents[simEvents$priority %in% c(1,2), ]
#   simEvents1 <- simEvents1[simEvents1$dtService < simEvents0$dtService, ]
#   simEvents1 <- simEvents1[simEvents1$dtService < 15*60, ]
  return("Bullshit")
}

kpiEventsNewNotInTime <- function(simEvents, simEventsBefore) {
  simEvents0 <- simEventsBefore[simEventsBefore$priority %in% c(1,2), ]
  simEvents1 <- simEvents[simEvents$priority %in% c(1,2), ]
  simEvents1 <- simEvents1[simEvents1$dtService > simEvents0$dtService, ]
  simEvents1 <- simEvents1[simEvents1$dtService >= 15*60, ]
}
