#' histDtService: that famous (but not very useful) histogram of all events.
#'
#' @param simData A set of simEvents or simMissions
#'
#' @return plotted histogram
histDtService <- function(simData) {
  if ("simMissions" %in% class(simData)) {
    dtService <- simData$dtLaunch + simData$dtToPoA
  } else {
    dtService <- simData$dtService
  }
  # dtService in Minutes
  dtService <- dtService/60
  # Only look at simData with dtService between 0 and 60 min.
  dtService <- dtService[dtService >= 0]
  dtService <- dtService[dtService < 60]
  # plot it
  hist(
    dtService,
    breaks = 60,
    xlim = c(0,60),
    col = c(rep('green',15),rep('red',45)),
    main = "Hilfsfrist auf den Ereignissen",
    ylab = "Anzahl Ereignisse",
    xlab = "Hilfsfrist [min]"
  )
}




#' histTAlarmHour:
#'
#' @param simEvents A set of simMissions
#'
#' @return plotted histogram
histTAlarmHour <- function(simMissions) {
  tAlarmHour <- plyr::count(trunc(simTimeR::simTime(simMissions$tAlarm)/60/60))
  barplot(
    tAlarmHour$freq,
    main = "Einsatzverteilung über den Tag",
    ylab = "Anzahl Einsätze",
    xlab = "Stunde",
    col = fhsblue(),
    names.arg = c(0:23)
  )
}


#' histTAlarmDay:
#'
#' @param simEvents A set of simMissions
#'
#' @return plotted histogram
histTAlarmDay <- function(simMissions) {
  tAlarmDay <- plyr::count(simTimeR::simDate(simMissions$tAlarm))
  barplot(
    tAlarmDay$freq,
    main = "Einsatzverteilung über das Jahr, nach Tag aufgelöst",
    ylab = "Anzahl Einsätze",
    xlab = "Tag",
    col = fhsblue()
  )
}

#' histTAlarmWeekday:
#'
#' @param simMissions A set of simMissions
#'
#' @return plotted histogram
histTAlarmWeekday <- function(simMissions) {
  t <- plyr::count(simTimeR::simWeekday(simMissions$tAlarm))
  barplot(
    c(
      t$freq[t$x == "Mo"],
      t$freq[t$x == "Di"],
      t$freq[t$x == "Mi"],
      t$freq[t$x == "Do"],
      t$freq[t$x == "Fr"],
      t$freq[t$x == "Sa"],
      t$freq[t$x == "So"]
      ),
    main = "Einsatzverteilung nach Wochentag",
    ylab = "Anzahl Einsätze",
    xlab = "Tag",
    col = fhsblue(),
    names.arg = c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")
  )
}


#' histTAlarmMonth:
#'
#' @param simMissions A set of simMissions
#'
#' @return plotted histogram
histTAlarmMonth <- function(simMissions) {
  hist(
    simMissions$tAlarm/60/60/24/30.5,
    axes = FALSE,
    breaks = 12,
    xlab = "Monat",
    ylab = "Anzahl Einsätze",
    main = "Einsatzverteilung über das Jahr, nach Monat aufgelöst",
    col = fhsblue())
  axis(1,
       at = c(0.5:11.5),
       labels = c("Jan","Feb","Mar","Apr","Mai","Jun",
                  "Jul","Aug","Sep","Okt","Nov","Dez"))
  axis(2)
}

