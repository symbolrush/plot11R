#' fhsblue: brings you the funky fhsblue and makes all graphics corporate design
#' compatible.
#'
#' @return fhsblue The famous color of all colors
fhsblue <- function() {
  rgb(0, 102, 153, maxColorValue = 255)
}


#' cpDtService: color palette to paint dots according to their level of service
#' time (DtService).
#'
#' @param x A number between 0 and 100 (percentage)
#'
#' @return color A string containing the name of a color
cpDtService <- function(x){
  if (is.na(x)) {'blue'}
  else if(x < 70) {'red'}
  else if(x < 80) {'orange'}
  else if(x < 90) {'yellow'}
  else {'green'}
}


#' filterPrio12: Provisorische Funktion für das Filtern aller Events mit
#' Priorität 1 und 2. Achtung: Die Logik beim rausfiltern ist nicht in allen
#' Teilen von plot11R konsistent gelöst. Teilweise geschieht die Filterung
#' explizit vor dem Funktionisaufruf, teilweise implizit in der Funktion
#'
#' @param simEvents
#'
#' @return simEventsPrio12 A subset of simEvents with events of priority 1 & 2
filterPrio12 <- function(simEvents) {
  return(simEvents[simEvents$priority %in% c(1,2), ])
}


#' filterPrio1: Provisorische Funktion für das Filtern aller Events mit
#' Priorität 1. Achtung: Die Logik beim rausfiltern ist nicht in allen
#' Teilen von plot11R konsistent gelöst. Teilweise geschieht die Filterung
#' explizit vor dem Funktionisaufruf, teilweise implizit in der Funktion
#'
#' @param simData A set of simMissions or simEvents
#'
#' @return simEventsPrio12 A subset of simEvents with events of priority 1 & 2
filterPrio1 <- function(simData) {
  return(simData[simData$priority == 1, ])
}




filterLUKS <- function(simMissions) {
  simMissions[!(simMissions$vehicleId %in% c(23:26,1000)),]
}

