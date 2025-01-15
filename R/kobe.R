#' Individuelle Kostenbeteiligung berechnen
#'
#' @param s Leistung.
#' @param f Franchise.
#' @param sb Selbstbehalt zwischen 0 und 1.
#' @param m Maximaler Selbstbehalt
#'
#' @return Kostenbeteiligung.
#' @export
k <- function(s, f, sb, m) { # Formel 31, S. 184
  if (s < f) return(s)
  if (f <= s && s < f + m / sb) return(f + (s-f)*sb)
  f + m
}

#' Kostenbeteiligung anhand von Kostenklassen berechnen.
#'
#' @param e Vektor mit Anteilen der Kostenklassen.
#' @param s Vektoren mit individuellen Leistungen pro Kostenklasse.
#' @param f Franchise.
#' @param sb Selbstbehalt zwischen 0 und 1.
#' @param m Maximaler Selbstbehalt
#'
#' @return Kostenbeteiligung.
#' @export
k_avg <- function(e, s, f, sb, m) { # Formel 34, S. 186
  avg <- 0
  for (i in 1:length(e)) {
    avg <- avg + e[i]*k(s[i], f, sb, m)
  }
  avg
}

#' Kostenbeteiligungsanteil anhand von Kostenklassen berechnen.
#'
#' @param e Vektor mit Anteilen der Kostenklassen.
#' @param s Vektoren mit individuellen Leistungen pro Kostenklasse.
#' @param f Franchise.
#' @param sb Selbstbehalt zwischen 0 und 1.
#' @param m Maximaler Selbstbehalt
#'
#' @return Kostenbeteiligungsanteil an Gesamtleistung.
#' @export
kobe_anteil <- function(e, s, f, sb, m) {
  k_avg(e, s, f, sb, m) / sum(e * s)
}
