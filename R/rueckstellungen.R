#' Abwicklungsdreieck konstruieren
#'
#' @param data Datensatz mit Angaben zu den Leistungen (brutto oder netto)
#'             sowie dem Behandlungs- und Abrechnungsjahr.
#' @param betrag Name der Brutto- oder Nettoleistungen in `data`.
#' @param behandlungsjahr Name des Behandlungsjahrs in `data`.
#' @param abrechnungsjahr Name des Abrechnungsjahrs in `data`.
#'
#' @return Matrix, die das Abwechslungsdreieck darstellt.
#' @export
abw3eck <- function(data, betrag, behandlungsjahr, abrechnungsjahr) {
  dreieck <- data |>
    dplyr::group_by({{ behandlungsjahr }}, {{ abrechnungsjahr }}) |>
    dplyr::summarise(total_betrag = sum({{ betrag }}),
                     .groups = "drop")
  colnames(dreieck) <- c("behandlungsjahr", "abrechnungsjahr", "total_betrag")
  dreieck <- dreieck |>
    tidyr::pivot_wider(id_cols = behandlungsjahr,
                       values_from = total_betrag,
                       names_from = abrechnungsjahr)
  dreieck_m <- as.matrix(dreieck[, -1])
  rownames(dreieck_m) <- dreieck$behandlungsjahr
  dreieck_m
}

#' Matrixzeilen links aussetzen
#'
#' @param M Zu verschiebende Matrix.
#'
#' @return Matrix mit links ausgesetzten Zeilen.
#' @export
shift_left <- function(M) {
  shift_row_left <- function(row) {
    if (!is.na(row[[1]])) {
      return(row)
    }
    shift_row_left(c(row[-1], NA))
  }
  for (i in 1:nrow(M)) {
    M[i, ] <- shift_row_left(M[i, ])
  }
  colnames(M) <- seq(0, ncol(M) - 1, by = 1)
  M
}

#' Matrixzeilen rechts aussetzen
#'
#' @param M Zu verschiebende Matrix.
#'
#' @return Matrix mit rechts ausgesetzten Zeilen.
#' @export
shift_right <- function(M) {
  shift_row_right <- function(row, leading, trailing) {
    c(rep(NA, leading), row, rep(NA, trailing))
  }
  ell <- ncol(M)
  M_shifted <- matrix(nrow = ell, ncol = 2 * ell - 1)
  for (i in 1:ell) {
    M_shifted[i, ] <- shift_row_right(M[i, ], i - 1, ell - i)
  }
  colnames(M_shifted) <- as.numeric(rownames(M)[1]) + 0:(2*ell-2)
  rownames(M_shifted) <- rownames(M)
  M_shifted
}

#' X-Matrix zu S-Matrix konvertieren.
#'
#' X-Matrizen enthalten die Leistungen pro Zeiteinheit;
#' S-Matrizen enthalten die kumulierten Leistungen pro Zeiteinheit.
#'
#' @param X Zu konvertierende X-Matrix.
#'
#' @return Zu `X` passende S-Matrix. Diese Matrix ist links eingerückt.
#' @export
to_S <- function(X) {
  apply(shift_left(X), 1, cumsum) |> t()
}

#' S-Matrix zu X-Matrix konvertieren.
#'
#' X-Matrizen enthalten die Leistungen pro Zeiteinheit;
#' S-Matrizen enthalten die kumulierten Leistungen pro Zeiteinheit.
#'
#' @param S Zu konvertierende S-Matrix.
#'
#' @return Zu `S` passende X-Matrix, links eingerückt.
#' @export
to_X <- function(S) {
  S <- shift_left(S)
  ell <- ncol(S)
  for (row in 1:ell) {
    for (col in 2:ell) {
      S[row, col] <- S[row, col] - sum(S[row, 1:(col-1)])
    }
  }
  S
}

#' Gesamtrückstellungen berechnen
#'
#' Aus S-Matrix das total der Rückstellungen berechnen.
#'
#' @param S S-Matrix.
#'
#' @return Total Rückstellungen.
#' @export
rueckstellung <- function(S) {
  last_cols <- (ncol(S)+1):(2*ncol(S)-1)
  X <- S |>
    to_X() |>
    shift_right()
  X[, last_cols] |> sum(na.rm = TRUE)
}

#' Phi-Faktoren berechnen
#'
#' Anhand von S-Matrix die Phi-Faktoren berechnen.
#' Diese schätzen, um welchen Faktor die kumulierten
#' Leistungen im Vergleich zum vorigen Abrechnungsmonat
#' zunehmen.
#'
#' @param S S-Matrix.
#'
#' @return Total Rückstellungen.
#' @export
phi <- function(S) { # Formel S. 151
  ell <- ncol(S)
  phis <- rep(NA, ell)
  for (i in 2:ell) {
    phis[i] <- sum(S[1:(ell-i+1), i]) / sum(S[1:(ell-i+1), i-1])
  }
  phis
}

#' Chain ladder-Algorithmus
#'
#' Chain ladder auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#'
#' @return Mit chain ladder erweiterte S-Matrix.
#' @export
chain_ladder <- function(S) { # Formel S. 152
  phis <- phi(S)
  ell <- ncol(S)
  for (row in 1:ell) {
    for (col in 1:ell) {
      if (is.na(S[row, col])) {
        S[row, col] <- S[row, col - 1] * phis[col]
      }
    }
  }
  S
}

#' Bornhuetter-Ferguson-Algorithmus
#'
#' Bornhuetter-Ferguson auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#' @param gamma Gamma-Schätzungen.
#' @param alpha Alpha-Schätzungen
#'
#' @return Mit Bornhuetter-Ferguson erweiterte S-Matrix.
#' @export
bf <- function(S, gamma, alpha) {
  ell <- ncol(S)
  for (row in 2:ell) {
    for (col in (ell - row + 2):ell) {
      S[row, col] <- S[row, col - 1] + alpha[row] * (gamma[col] - gamma[col - 1])
    }
  }
  S
}

#' Benktander-Hovinen-Algorithmus
#'
#' Benktander-Hovinen auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#' @param gamma Gamma-Schätzungen.
#' @param alpha Alpha-Schätzungen
#'
#' @return Mit Benktander-Hovinen erweiterte S-Matrix.
#' @export
bh <- function(S, gamma, alpha) {
  alpha_bf <- bf(S, gamma, alpha)[, ncol(S)]
  bf(S, gamma, alpha_bf)
}

#' Mack-Methode
#'
#' Mack auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#' @param pis Pi-Schätzungen
#'
#' @return Mit Mack erweiterte S-Matrix.
#' @export
mack <- function(S, pis) {
  compute_sigmas <- function(X, pis) {
    ell <- ncol(X)
    sigmas <- rep(0, ell)
    for (i in 1:ell) {
      sigmas[i] <- sum(X[1:(ell-i+1), i]) / sum(pis[1:(ell-i+1)])
    }
    sigmas
  }
  X <- S |> to_X()
  sigma_ad <- colSums(X, na.rm = TRUE) / rev(cumsum(pi)) # S. 158
  gamma_ad <- cumsum(sigma_ad) / sum(sigma_ad)           # S. 159
  alpha_ld <- rowSums(X, na.rm = TRUE) / rev(gamma_ad)   # S. 161
  sigmas_ad <- compute_sigmas(X, alpha_ld)
  gamma_mack <- cumsum(sigmas_ad) / sum(sigmas_ad)       # S. 159
  kappa_cc <- sum(apply(S, 1, max, na.rm = TRUE)) / sum(rev(gamma_ad) * pi) # S. 162
  alpha_mack <- pi * kappa_cc                            # S. 163
  alpha_mack <- alpha_ld * sum(sigmas_ad)                # S. 163; verwende alpha_ld für pi
  bf(S, gamma_mack, alpha_mack)
}

#' Chain ladder via Bornhuetter-Ferguson
#'
#' Chain ladder via Bornhuetter-Ferguson auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#'
#' @return Mit CL/BF erweiterte S-Matrix.
#' @export
chain_ladder_bf <- function(S) {
  phi_cl <- phi(S) # S. 156
  gamma_cl <- c((1 / cumprod(rev(phi_cl[2:length(phi_cl)]))) |> rev(), 1) # S. 157
  alpha_ld <- rev(rev(apply(S, 1, max, na.rm = TRUE)) / gamma_cl) # S. 161
  bf(S, gamma_cl, alpha_ld)
}

#' Panning-Verfahren
#'
#' Panning-Verfahren auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#'
#' @return Mit Panning erweiterte S-Matrix.
#' @export
panning <- function(S) {
  # S. 158
  gamma_pa <- function(beta) {
    cumsum(beta) / sum(beta)
  }
  # S. 157
  beta_pa <- function(X) {
    ell <- ncol(X)
    betas <- vector(length = ell)
    for (i in 1:ell) {
      betas[i] <- sum(X[1:(ell-i+1), i] * X[1:(ell-i+1), 1]) / sum(X[1:(ell-i+1), 1]^2)
    }
    betas
  }
  # S. 164
  alpha_pa <- function(X) {
    X[, 1] * sum(beta_pa(X))
  }
  bf(S, gamma_pa(beta_pa(to_X(S))), alpha_pa(to_X(S)))
}

#' Loss development-Verfahren
#'
#' Loss development-Verfahren auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#' @param gamma Gamma-Schätzungen.
#'
#' @return Mit Loss development erweiterte S-Matrix.
#' @export
loss_dev <- function(S, gamma) {
  alpha_ld <- apply(S, 1, max, na.rm = TRUE) / rev(gamma)
  bf(S, gamma, alpha_ld)
}

#' Additives Verfahren
#'
#' Additives Verfahren auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#' @param pis Pi-Schätzungen.
#'
#' @return Mit additivem Verfahren erweiterte S-Matrix.
#' @export
additiv <- function(S, pis) {
  X <- S |>
    to_X()
  sigma_ad <- colSums(X, na.rm = TRUE) / rev(cumsum(pis)) # S. 158
  gamma_ad <- cumsum(sigma_ad) / sum(sigma_ad) # S. 159
  alpha_ad <- pis * sum(sigma_ad)
  bf(S, gamma_ad, alpha_ad)
}

#' Cape cod-Verfahren
#'
#' Cape cod auf S-Matrix anwenden.
#'
#' @param S S-Matrix.
#' @param gamma Gamma-Schätzungen.
#' @param pis Pi-Schätzungen.
#'
#' @return Mit cape cod erweiterte S-Matrix.
#' @export
cape_cod <- function(S, gamma, pis) {
  kappa_cc <- sum(apply(S, 1, max, na.rm = TRUE)) / sum(rev(gamma) * pis)
  alpha_cc <- pis * kappa_cc
  bf(S, gamma, alpha_cc)
}

#' Rückstellungszusammenfassung
#'
#' Unterschiedliche Algorithmen auf S-Matrix anwenden.
#' Wenn `alpha`, `gamma` nicht mitgeliefert werden, werden diese anhand
#' des Chain-Ladders geschätzt. Falls `loss_dev()` mit Chain-Ladder-gamma-Schätzungen
#' verwendet wird, ergibt dies die Chain-Ladder-Rückstellungsschätzung.
#' Falls `bf()` mit Chain-Ladder-alpha- und -gamma-Schätzungen verwendet wird,
#' ergibt dies ebenfalls die Chain-Ladder-Rückstellungsschätzung.
#'
#' @param S S-Matrix.
#' @param alpha A-priori alpha-Schätzungen. Falls `NA` werden sie anhand des Chain Ladders geschätzt.
#' @param gamma A-priori gamma-Schätzungen. Falls `NA` werden sie anhand des Chain Ladders geschätzt.
#' @param pis Volumenmass.
#'
#' @return Tibble mit anhand unterschiedlicher Algorithmen geschätztem Rückstellungsbedarf.
#' @export
zsf <- function(S, alpha = NA, gamma = NA, pis) {
  if (is.na(gamma)) {
    phi_cl <- phi(S)
    gamma <- c((1 / cumprod(rev(phi_cl[2:length(phi_cl)]))) |> rev(), 1)
  }
  if (is.na(alpha)) {
    alpha <- apply(S |> chain_ladder(), 1, max)
  }
  tibble::tibble(
    Methode = "Chain ladder",
    Rückstellungsbedarf = S |>
      chain_ladder() |>
      rueckstellung()) |>
    tibble::add_row(Methode = "Additives Verfahren (bf)",
            Rückstellungsbedarf = S |> additiv(pis = pis) |> rueckstellung()) |>
    tibble::add_row(Methode = "Panning (bf)",
            Rückstellungsbedarf = S |> panning() |> rueckstellung()) |>
    tibble::add_row(Methode = "Bornhuetter-Ferguson",
            Rückstellungsbedarf = S |> bf(gamma, alpha) |> rueckstellung()) |>
    tibble::add_row(Methode = "Cape Cod",
            Rückstellungsbedarf = S |> cape_cod(gamma, pis) |> rueckstellung()) |>
    tibble::add_row(Methode = "Mack",
            Rückstellungsbedarf = S |> mack(pis) |> rueckstellung()) |>
    tibble::add_row(Methode = "Loss-development",
            Rückstellungsbedarf = S |> loss_dev(gamma) |> rueckstellung())
}
