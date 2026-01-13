#' Obliczanie wag metodą Entropii Shannona
#'
#' @description Wyznacza obiektywne wagi kryteriów na podstawie danych,
#' mierząc stopień rozproszenia wartości. Im większa zmienność, tym wyższa waga.
#'
#' @param macierz_decyzyjna Rozmyta macierz (wynik funkcji `przygotuj_dane_mcda` z pliku przygotowanie_danych.R).
#' @return Wektor numeryczny wag sumujący się do 1.
#' @export
oblicz_wagi_entropii <- function(macierz_decyzyjna) {
  # Od-rozmycie (defuzzification) macierzy do obliczen entropii (srednia z l, m, u)
  n_kolumn <- ncol(macierz_decyzyjna)
  macierz_ostra <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn/3)
  k <- 1
  for(j in seq(1, n_kolumn, 3)) {
    # Od-rozmycie: (l + 4m + u) / 6
    macierz_ostra[, k] <- (macierz_decyzyjna[, j] + 4*macierz_decyzyjna[, j+1] + macierz_decyzyjna[, j+2]) / 6
    k <- k + 1
  }
  # Normalizacja (P_ij)
  sumy_kolumn <- colSums(macierz_ostra, na.rm = TRUE) #na.rm = TRUE usuwa braki wartości
  sumy_kolumn[sumy_kolumn == 0] <- 1
  P <- sweep(macierz_ostra, 2, sumy_kolumn, "/")
  # Obliczanie Entropii (E_j)
  k_const <- 1 / log(nrow(macierz_decyzyjna))
  E <- numeric(ncol(P))
  for(j in 1:ncol(P)) {
    p_vals <- P[, j]
    p_pos <- p_vals[p_vals > 0] # Ignorujemy zera dla logarytmu
    if(length(p_pos) == 0) {
      E[j] <- 1
    } else {
      E[j] <- -k_const * sum(p_pos * log(p_pos))
    }
  }

  d <- 1 - E #d_j
  if(sum(d) == 0) return(rep(1/length(d), length(d)))
  w <- d / sum(d)

  return(w) #w_j
}

#' @title Wewnętrzny procesor wag
#' @description Decyduje, skąd wziąć wagi (Ręczne vs BWM).
#' @keywords internal
.pobierz_finalne_wagi <- function(macierz, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze) {

  n_kryteriow <- ncol(macierz) / 3

  # Opcja 1: Wagi podane ręcznie
  if (!missing(wagi) && !is.null(wagi)) {
    if (length(wagi) == n_kryteriow) {
      return(rep(wagi, each = 3)) # wagi ostre na rozmyte (w, w, w)
    }
    if (length(wagi) != ncol(macierz)) {
      stop("Długość wektora 'wagi' musi odpowiadać liczbie kolumn macierzy (3 * n_kryteriow) lub liczbie kryteriów.")
    }
    return(wagi)
  }

  # Opcja 2: Obliczenie BWM
  if (!missing(bwm_najlepsze) && !missing(bwm_najgorsze)) {
    if (missing(bwm_kryteria)) {
      if (!is.null(attr(macierz, "nazwy_kryteriow"))) {
        bwm_kryteria <- attr(macierz, "nazwy_kryteriow")
      } else {
        bwm_kryteria <- paste0("C", 1:n_kryteriow)
        message("Nie znaleziono nazw kryteriów. Używam domyślnych: ", paste(bwm_kryteria, collapse=", "))
      }
    }

    message("Obliczanie wag metodą BWM...")
    wynik_bwm <- oblicz_wagi_bwm(bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
    wagi_ostre <- wynik_bwm$wagi_kryteriow

    if (length(wagi_ostre) != n_kryteriow) stop("Liczba wag z BWM nie zgadza się z liczbą kryteriów w macierzy.")
    # Konwersja na wagi rozmyte (w, w, w)
    wagi_rozmyte <- rep(wagi_ostre, each = 3)
    return(wagi_rozmyte)
  }
  stop("Musisz podać wektor 'wagi' LUB parametry 'bwm_najlepsze' i 'bwm_najgorsze'.")
}

#' Implementacja Fuzzy VIKOR dla macierzy rozmytej (TFN)
#' Oblicza wskaźniki S (użyteczność grupy), R (indywidualny żal) oraz Q (indeks kompromisu).
#'
#' @description Implements Fuzzy VIKOR with BWM integration. Returns an object for plotting.
#' @param macierz_decyzyjna Macierz rozmyta (m x 3n) w układzie (l1,m1,u1, l2,m2,u2, ...).
#' @param typy_kryteriow Wektor znakowy "max" (benefit) lub "min" (cost).
#' @param v Waga strategii grupowej (domyślnie 0.5).
#' @param wagi Wektor wag (length = n_kryteriow lub length = 3*n_kryteriow).
#' @param bwm_kryteria Wektor z nazwami kryteriów (jeśli BWM).
#' @param bwm_najlepsze Wektor (best-to-others) dla BWM.
#' @param bwm_najgorsze Wektor (others-to-worst) dla BWM.
#' @return `fuzzy_vikor_res`
#' @export
#'
fuzzy_vikor <- function(macierz_decyzyjna,
                        typy_kryteriow,
                        v = 0.5,
                        wagi = NULL,
                        bwm_kryteria,
                        bwm_najlepsze,
                        bwm_najgorsze)
{
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)
  n_kolumn <- ncol(macierz_decyzyjna)
  typy_rozmyte <- character(n_kolumn)

  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  # 1. Rozwiązania Idealne
  ideal_poz <- ifelse(typy_rozmyte == "max", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))
  ideal_neg <- ifelse(typy_rozmyte == "min", apply(macierz_decyzyjna, 2, max), apply(macierz_decyzyjna, 2, min))

  # 2. Normalizacja liniowa
  m_dec <- matrix(0, nrow = nrow(macierz_decyzyjna), ncol = n_kolumn)
  for (i in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[i] == "max") {
      mianownik <- ideal_poz[i+2] - ideal_neg[i]
      if(mianownik == 0) mianownik <- 1e-9

      # Wzór dla Benefit: (f* - f_ij) / (f* - f-)
      m_dec[, i]   <- (ideal_poz[i]   - macierz_decyzyjna[, i+2]) /mianownik
      m_dec[, i+1] <- (ideal_poz[i+1] - macierz_decyzyjna[, i+1]) /mianownik
      m_dec[, i+2] <- (ideal_poz[i+2] - macierz_decyzyjna[, i])   /mianownik

    } else {
      mianownik <- ideal_neg[i+2] - ideal_poz[i]
      if(mianownik == 0) mianownik <- 1e-9

      # Wzór dla Cost: (f_ij - f*) / (f- - f*)
      m_dec[, i]   <- (macierz_decyzyjna[, i]   - ideal_poz[i+2]) /mianownik
      m_dec[, i+1] <- (macierz_decyzyjna[, i+1] - ideal_poz[i+1]) /mianownik
      m_dec[, i+2] <- (macierz_decyzyjna[, i+2] - ideal_poz[i])   /mianownik
    }
  }

  W_diag <- diag(finalne_wagi)
  m_dec_wazona <- m_dec %*% W_diag

  # 3. Wartości S (suma) i R (max)
  S_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  R_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)

  S_rozmyte[,1] <- apply(m_dec_wazona[, seq(1, n_kolumn, 3), drop=FALSE], 1, sum)
  S_rozmyte[,2] <- apply(m_dec_wazona[, seq(2, n_kolumn, 3), drop=FALSE], 1, sum)
  S_rozmyte[,3] <- apply(m_dec_wazona[, seq(3, n_kolumn, 3), drop=FALSE], 1, sum)

  R_rozmyte[,1] <- apply(m_dec_wazona[, seq(1, n_kolumn, 3), drop=FALSE], 1, max)
  R_rozmyte[,2] <- apply(m_dec_wazona[, seq(2, n_kolumn, 3), drop=FALSE], 1, max)
  R_rozmyte[,3] <- apply(m_dec_wazona[, seq(3, n_kolumn, 3), drop=FALSE], 1, max)

  # 4. Indeks Q
  s_star <- min(S_rozmyte[,1])
  s_minus <- max(S_rozmyte[,3])
  r_star <- min(R_rozmyte[,1])
  r_minus <- max(R_rozmyte[,3])

  mianownik_s <- s_minus - s_star
  mianownik_r <- r_minus - r_star
  if (mianownik_s == 0) mianownik_s <- 1
  if (mianownik_r == 0) mianownik_r <- 1

  Q_rozmyte <- matrix(0, nrow(macierz_decyzyjna), 3)
  x <- (S_rozmyte - s_star) / mianownik_s
  y <- (R_rozmyte - r_star) / mianownik_r
  Q_rozmyte <- v * x + (1 - v) * y

  # Odrozmycie (defuzzification)
  S <- (S_rozmyte[,1] + 2*S_rozmyte[,2] + S_rozmyte[,3]) / 4
  R <- (R_rozmyte[,1] + 2*R_rozmyte[,2] + R_rozmyte[,3]) / 4
  Q <- (Q_rozmyte[,1] + 2*Q_rozmyte[,2] + Q_rozmyte[,3]) / 4

  ramka_wynikow <- data.frame(
    Alternatywa = 1:nrow(macierz_decyzyjna),
    S = S,
    R = R,
    Q = Q,
    ranking = rank(Q, ties.method = "first")
  )

  wynik <- list(
    wyniki = ramka_wynikow,
    detale = list(S_rozmyte = S_rozmyte, R_rozmyte = R_rozmyte, Q_rozmyte = Q_rozmyte),
    parametry = list(v = v)
  )

  class(wynik) <- "fuzzy_vikor_res"
  return(wynik)
}

