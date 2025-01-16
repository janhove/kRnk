#' Rentenbarwert
#'
#' Berechnet den Rentenbarwart.
#'
#' @param x Alter der Versichertenkohorte.
#' @param lx Grösse der Versichertenkohorte.
#' @param i Zinssatz
#' @param pj Vektor mit Überlebungswahrscheinlichkeiten. Soll bei Alter 0 anfangen.
#'
#' @return Rentenbarwert
#' @export
rentenbarwert <- function(x, lx = 1, i, pj) {
  v <- 1 / (1 + i)
  pj <- c(1, pj[(x+1):length(pj)])
  ellj <- lx * cumprod(pj)
  vj <- v^(x + (0:(length(pj) - 1)))
  Nx <- sum(ellj * vj)
  Nx / v^x
}

#' Leistungsbarwert
#'
#' Berechnet den Leistungsbarwert.
#'
#' @param x Alter der Versichertenkohorte.
#' @param lx Grösse der Versichertenkohorte.
#' @param i Zinssatz
#' @param pj Vektor mit Überlebungswahrscheinlichkeiten nach Alter. Soll bei Alter 0 anfangen.
#' @param Kj Vektor mit erwarteten Kopfschäden nach Alter. Soll bei Alter 0 anfangen.
#'
#' @return Leistungsbarwert
#' @export
leistungsbarwert <- function(x, lx = 1, i, pj, Kj) {
  v <- 1 / (1 + i)
  pj <- c(1, pj[(x+1):length(pj)])
  ellj <- lx * cumprod(pj)
  vj <- v^(x + (0:(length(pj)-1)))
  Kj <- Kj[(x+1):length(Kj)]
  Ux <- sum(ellj * vj * Kj)
  Ux / v^x
}

#' Eintrittsalterpraemie
#'
#' @param x Alter der Versichertenkohorte.
#' @param i Zinssatz.
#' @param pj Vektor mit Überlebungswahrscheinlichkeiten nach Alter. Soll bei Alter 0 anfangen.
#' @param Kj Vektor mit erwarteten Kopfschäden nach Alter. Soll bei Alter 0 anfangen.
#'
#' @return Eintrittsalterprämie.
#' @export
eintrittsalterspraemie <- function(x, i, pj, Kj) {
  leistungsbarwert(x, 1, i, pj, Kj) /
    rentenbarwert(x, 1, i, pj)
}

#' Deckungskapital
#'
#' @param x Alter der Versichertenkohorte.
#' @param m Anzahl Jahre in die Zukunft.
#' @param lx Grösse der Versichertenkohorte.
#' @param i Zinssatz.
#' @param pj Vektor mit Überlebungswahrscheinlichkeiten nach Alter. Soll bei Alter 0 anfangen.
#' @param Kj Vektor mit erwarteten Kopfschäden nach Alter. Soll bei Alter 0 anfangen.
#'
#' @return Deckungskapital nach `m` Jahren.
#' @export
deckungskapital <- function(x, m, lx, i, pj, Kj) {
  lxm <- (lx * cumprod(c(1, pj[(x + 1):length(pj)])))[m + 1]
  leistungsbarwert(x + m, lxm, i, pj, Kj) -
    eintrittsalterspraemie(x, i, pj, Kj) * rentenbarwert(x + m, lxm, i, pj)
}
