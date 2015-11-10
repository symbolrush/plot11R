#' boxDtLaunchByVehicleName(): Druckt einen Boxplot der Ausrückzeiten für alle
#' Stützpunkte in simData$vehicles
#'
#' @param simScenario A simScenario
#'
#' @return ok
boxDtLaunchByVehicleName <- function(simScenario) {
  for (i in unique(simScenario$vehicles$name)) {
    # Für alle ausser den externen Simulationsdienst
    if (i != 2000) {
      boxplot(
        simScenario$missions$dtLaunch[
          simScenario$missions$vehicleId %in% simScenario$vehicles$id[
            simScenario$vehicles$name == i]]/60,
        ylim = c(0,10),
        main = i
      )
    }
  }
}


#' boxDtLaunchByOrganisation(): Druckt einen Boxplot der Ausrückzeiten für alle
#' Stützpunkte in simData$vehicles
#'
#' @param simScenario A simScenario
#'
#' @return ok
boxDtLaunchByOrganisation <- function(simMissions) {
  for (i in unique(simMissions$organisation)) {
    boxplot(
        simMissions$dtLaunch[
          simMissions$organisation == i]/60,
        ylim = c(0,10),
        main = i,
        col = fhsblue(),
        ylab = "Ausrückzeit in Minuten",
        range = 0
      )
  }
}
