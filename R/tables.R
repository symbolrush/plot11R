tableEventsDifferenceDtService <- function(simEvents, simEventsBefore) {
  return(simEvents[simEvents$dtService != simEventsBefore$dtService, ])
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
