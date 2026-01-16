#' @title Wewnętrzny parser składni
#' @description Zamienia tekst typu: "Kryterium =~ zmienna1 + zmienna2" na listę w R.
#' @keywords internal
.parsuj_skladnie_mcda <- function(skladnia) {
  czysta_skladnia <- gsub("\n", "", skladnia)
  linie <- strsplit(czysta_skladnia, ";")[[1]]
  mapowanie <- list()

  for (linia in linie) {
    if (trimws(linia) == "") next
    czesci <- strsplit(linia, "=~")[[1]]

    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      mapowanie[[nazwa_kryterium]] <- elementy
    }
  }
  return(mapowanie)
}

#' @title Wewnętrzny Skaler Saaty'ego
#' @description Skaluje dowolny wektor (ciągły, Likert, dyskretny) na skalę Saaty'ego 1-9.
#' @keywords internal
.skaluj_do_saaty <- function(wektor) {
  if (any(wektor < 0, na.rm = TRUE)) stop("Wykryto wartości ujemne w danych wejściowych.")

  #Obsluga kodow bledow (np. 99) i brakow danych (NA)
  wektor[is.na(wektor) | wektor == 99] <- 0

  maska_poprawne <- wektor > 0
  wartosci <- wektor[maska_poprawne]

  if (length(wartosci) == 0) return(wektor)

  min_w <- min(wartosci)
  max_w <- max(wartosci)

  #Skalowanie
  if (min_w == max_w) {
    wektor[maska_poprawne] <- 1
  } else {
    wektor[maska_poprawne] <- 1 + (wartosci - min_w) * (8 / (max_w - min_w))
  }

  return(wektor)
}

#' @title Wewnętrzna funkcja rozmywająca (Fuzzifier)
#' @description Zamienia liczbę rzeczywistą (Crisp) na Trójkątną Liczbę Rozmytą (TFN).
#' Wektor przyjmuje postać (l, m, u), gdzie m = x, l = x-1, u = x+1.
#' @keywords internal
.rozmyj_wektor <- function(wektor) {

  l <- pmax(1, wektor - 1)
  m <- wektor
  u <- pmin(9, wektor + 1)

  jest_zerem <- (wektor == 0)
  l[jest_zerem] <- 0; m[jest_zerem] <- 0; u[jest_zerem] <- 0

  return(cbind(l, m, u))
}


#' Przygotowanie danych do rozmytej analizy MCDA
#'
#' @description Funkcja przekształca surowe dane ankietowe w rozmytą macierz decyzyjną.
#' Oblicza wyniki zmiennych kompozytowych na podstawie składni, skaluje je do przedziału 1-9,
#' agreguje odpowiedzi ekspertów (jeśli dotyczy) i dokonuje rozmycia (fuzzification).
#'
#' @param dane Ramka danych (data frame) zawierająca surowe zmienne.
#' @param skladnia Tekst definiujący kryteria, np.:
#' "A1 =~ oferty_IT;
#'  A2 =~ dojazd_godz;
#'  A3 =~ koszt_mieszkania"
#' @param kolumna_alternatyw Nazwa kolumny identyfikującej alternatywy.
#' Jeśli NULL, każdy wiersz traktowany jest jako osobna alternatywa.
#' @param funkcja_agregacji Funkcja używana do scalania opinii ekspertów (domyślnie: mean).
#'
#' @return Macierz o wymiarach ($m \times 3n$), gdzie m to liczba alternatyw.
#' @export
przygotuj_dane_mcda <- function(dane,
                                skladnia,
                                kolumna_alternatyw = "Alternatywa",
                                funkcja_agregacji = mean) {

  if (!is.data.frame(dane)) stop("Argument 'dane' musi być ramką danych (data frame).")

  # 1. Parsowanie składni
  mapowanie <- .parsuj_skladnie_mcda(skladnia)
  nazwy_kryteriow <- names(mapowanie)

  # 2. Obliczanie zmiennych kompozytowych i skalowanie (dla każdego wiersza/eksperta)
  tymczasowe_wyniki <- data.frame(row_id = 1:nrow(dane))

  for (kryt in nazwy_kryteriow) {
    zmienne <- mapowanie[[kryt]]
    brakujace <- zmienne[!zmienne %in% names(dane)]
    if (length(brakujace) > 0) stop(paste("Brakuje zmiennych w danych:", paste(brakujace, collapse=", ")))
    if (length(zmienne) > 1) {
      surowy_wynik <- rowMeans(dane[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy_wynik <- dane[[zmienne]]
    }

    # Skalowanie do 1-9
    tymczasowe_wyniki[[kryt]] <- .skaluj_do_saaty(surowy_wynik)

  }

  # 3. Agregacja (Eksperci -> Alternatywy)
  if (!is.null(kolumna_alternatyw)) {
    if (!kolumna_alternatyw %in% names(dane)) stop("Nie znaleziono kolumny alternatyw w danych.")

    tymczasowe_wyniki$ID_Alternatywy <- dane[[kolumna_alternatyw]]

    dane_aggr <- aggregate(. ~ ID_Alternatywy, data = tymczasowe_wyniki[, -1], FUN = funkcja_agregacji)

    dane_aggr <- dane_aggr[order(dane_aggr$ID_Alternatywy), ]
    nazwy_wierszy <- dane_aggr$ID_Alternatywy
    macierz_wynikow <- as.matrix(dane_aggr[, nazwy_kryteriow])
  } else {
    macierz_wynikow <- as.matrix(tymczasowe_wyniki[, nazwy_kryteriow])
    nazwy_wierszy <- 1:nrow(macierz_wynikow)
  }

  # 4. Rozmywanie
  lista_decyzyjna <- list()

  for (i in seq_along(nazwy_kryteriow)) {
    kryt <- nazwy_kryteriow[i]
    lista_decyzyjna[[kryt]] <- .rozmyj_wektor(macierz_wynikow[, i])
  }

  finalna_macierz <- do.call(cbind, lista_decyzyjna)
  rownames(finalna_macierz) <- nazwy_wierszy

  attr(finalna_macierz, "nazwy_kryteriow") <- nazwy_kryteriow

  return(finalna_macierz)
}
