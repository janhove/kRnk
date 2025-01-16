#' Expected shortfall
#'
#' Schätzt expected shortfall anhand einer Simulation.
#'
#' @param alpha Alpha.
#' @param qdist Quantilfunktion.
#' @param rdist Funktion, um Zufallsdaten aus Verteilung zu generieren.
#' @param ... Parameter für `qdist` und `rdist`.
#' @param B Anzahl generierter Zufallsdaten.
#'
#' @return Expected shortfall.
#' @export
expected_shortfall <- function(alpha, qdist, rdist, ..., B = 1e6) {
  VaR <- qdist(alpha, ...)
  sim_data <- rdist(B, ...)
  mean(sim_data[sim_data > VaR])
}
