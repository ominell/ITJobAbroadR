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

#' @title Wewnętrza funkcja MULTIMOORA - teoria dominacji
#' @description Agreguje trzy wewnętrzne rankingi metody MULTIMOORA  (RS, RP, MF).
#' @keywords internal
.teoria_dominacji_multimoora <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)
  macierz_rang <- cbind(r1, r2, r3)
  dostepne <- rep(TRUE, n)

  for (poz in 1:n) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf

    # Znajdź kandydatów z najlepszą (najniższą) rangą w każdej pod metodzie
    c1 <- which.min(obecna_macierz[, 1])
    c2 <- which.min(obecna_macierz[, 2])
    c3 <- which.min(obecna_macierz[, 3])
    kandydaci <- c(c1, c2, c3)

    # Mechanizm głosowania (Voting Mechanism)
    czestosc <- table(kandydaci)
    zwyciezca <- as.numeric(names(czestosc)[which.max(czestosc)])

    # Rozstrzyganie remisu
    if (length(czestosc) == 3) {
      sumy <- rowSums(macierz_rang[kandydaci, ])
      zwyciezca <- kandydaci[which.min(sumy)]
    }

    finalny_ranking[zwyciezca] <- poz
    dostepne[zwyciezca] <- FALSE
  }
  return(finalny_ranking)
}

#' Rozmyta Metoda MULTIMOORA
#'
#' @description Implementacja metody Fuzzy MULTIMOORA. Składa się z:
#' 1. Ratio System (RS)
#' 2. Reference Point (RP)
#' 3. Full Multiplicative Form (FMF)
#' Finalny ranking powstaje przez agregację Teorią Dominacji.
#'
#' @inheritParams fuzzy_vikor
#' @return `fuzzy_multimoora_res`
#' @export
fuzzy_multimoora <- function(macierz_decyzyjna,
                               typy_kryteriow,
                               wagi = NULL,
                               bwm_kryteria,
                               bwm_najlepsze,
                               bwm_najgorsze) {

  if (!is.matrix(macierz_decyzyjna)) stop("'macierz_decyzyjna' musi być macierzą.")

  # 1. Obliczenie wag
  finalne_wagi <- .pobierz_finalne_wagi(macierz_decyzyjna, wagi, bwm_kryteria, bwm_najlepsze, bwm_najgorsze)

  n_wierszy <- nrow(macierz_decyzyjna)
  n_kolumn <- ncol(macierz_decyzyjna)

  # Rozszerzenie typów kryteriów
  typy_rozmyte <- character(n_kolumn)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    typy_rozmyte[j:(j+2)] <- typy_kryteriow[k]
    k <- k + 1
  }

  # 2. Normalizacja Wektorowa
  norm_macierz <- matrix(0, nrow = n_wierszy, ncol = n_kolumn)
  for (i in seq(1, n_kolumn, 3)) {
    mianownik <- sqrt(sum(macierz_decyzyjna[,i]^2 + macierz_decyzyjna[,i+1]^2 + macierz_decyzyjna[,i+2]^2))
    if (mianownik == 0) mianownik <- 1
    norm_macierz[,i]   <- macierz_decyzyjna[,i]   / mianownik
    norm_macierz[,i+1] <- macierz_decyzyjna[,i+1] / mianownik
    norm_macierz[,i+2] <- macierz_decyzyjna[,i+2] / mianownik
  }

  # --- CZĘŚĆ A: System Ilorazowy (RS) ---
  rs_wazona <- norm_macierz
  for (j in 1:n_kolumn) {
    rs_wazona[, j] <- norm_macierz[, j] * finalne_wagi[j]
  }

  rs_rozmyte <- matrix(0, nrow = n_wierszy, ncol = 3)
  for (j in seq(1, n_kolumn, 3)) {
    if (typy_rozmyte[j] == 'max') {
      rs_rozmyte[,1] <- rs_rozmyte[,1] + rs_wazona[, j]
      rs_rozmyte[,2] <- rs_rozmyte[,2] + rs_wazona[, j+1]
      rs_rozmyte[,3] <- rs_rozmyte[,3] + rs_wazona[, j+2]
    } else {
      rs_rozmyte[,1] <- rs_rozmyte[,1] - rs_wazona[, j+2]
      rs_rozmyte[,2] <- rs_rozmyte[,2] - rs_wazona[, j+1]
      rs_rozmyte[,3] <- rs_rozmyte[,3] - rs_wazona[, j]
    }
  }
  def_rs <- rowMeans(rs_rozmyte)
  rank_rs <- rank(-def_rs, ties.method = "first")

  # --- CZĘŚĆ B: Punkt Odniesienia (RP) ---
  punkt_ref <- numeric(n_kolumn)
  for (j in 1:n_kolumn) {
    if (typy_rozmyte[j] == 'max') punkt_ref[j] <- max(rs_wazona[,
                                                                j])
    else punkt_ref[j] <- min(rs_wazona[, j])
  }

  dystanse <- matrix(0, nrow = n_wierszy, ncol = n_kolumn/3)
  k <- 1
  for (j in seq(1, n_kolumn, 3)) {
    d_l <- (rs_wazona[, j]   - punkt_ref[j])^2
    d_m <- (rs_wazona[, j+1] - punkt_ref[j+1])^2
    d_u <- (rs_wazona[, j+2] - punkt_ref[j+2])^2
    dystanse[, k] <- sqrt(d_l + d_m + d_u)
    k <- k + 1
  }
  def_rp <- apply(dystanse, 1, max)
  rank_rp <- rank(def_rp, ties.method = "first")

  # --- CZĘŚĆ C: Forma Multiplikatywna (FMF) ---
  iloczyn_zysk <- matrix(1, nrow = n_wierszy, ncol = 3)
  iloczyn_koszt <- matrix(1, nrow = n_wierszy, ncol = 3)

  for (j in seq(1, n_kolumn, 3)) {
    w <- finalne_wagi[j+1]
    trojka <- norm_macierz[, j:(j+2)]
    if (typy_rozmyte[j] == 'max') {
      iloczyn_zysk[,1] <- iloczyn_zysk[,1] * (trojka[,1]^w)
      iloczyn_zysk[,2] <- iloczyn_zysk[,2] * (trojka[,2]^w)
      iloczyn_zysk[,3] <- iloczyn_zysk[,3] * (trojka[,3]^w)
    } else {
      iloczyn_koszt[,1] <- iloczyn_koszt[,1] * (trojka[,1]^w)
      iloczyn_koszt[,2] <- iloczyn_koszt[,2] * (trojka[,2]^w)
      iloczyn_koszt[,3] <- iloczyn_koszt[,3] * (trojka[,3]^w)
    }
  }
  iloczyn_koszt[iloczyn_koszt == 0] <- 1e-9

  fmf_rozmyte <- matrix(0, nrow = n_wierszy, ncol = 3)
  fmf_rozmyte[,1] <- iloczyn_zysk[,1] / iloczyn_koszt[,3]
  fmf_rozmyte[,2] <- iloczyn_zysk[,2] / iloczyn_koszt[,2]
  fmf_rozmyte[,3] <- iloczyn_zysk[,3] / iloczyn_koszt[,1]

  def_fmf <- rowMeans(fmf_rozmyte)
  rank_fmf <- rank(-def_fmf, ties.method = "first")

  # Agregacja
  finalny_ranking <- .teoria_dominacji_multimoora(rank_rs, rank_rp, rank_fmf)

  wyniki_df <- data.frame(
    Alternatywa = 1:n_wierszy,
    RS_Wynik = def_rs,
    RS_Ranking = rank_rs,
    RP_Wynik = def_rp,
    RP_Ranking = rank_rp,
    FMF_Wynik = def_fmf,
    FMF_Ranking = rank_fmf,
    Ranking_MM = finalny_ranking
  )

  wynik <- list(wyniki = wyniki_df, metoda = "MULTIMOORA")
  class(wynik) <- "fuzzy_multimoora_res"
  return(wynik)
}
