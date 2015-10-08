#' histDtService: that famous (but not very useful) histogram of all events.
#'
#' @param simEvents A set of simEvents
#'
#' @return plotted histogram
histDtService <- function(simEvents) {
  dtService <- simEvents$dtService/60
  dtService <- dtService[dtService >= 0]
  dtService <- dtService[dtService < 60]
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


