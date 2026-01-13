#' Przykładowe dane do macierzy decyzyjnej MCDA
#'
#' Zbiór danych zawierający symulowane odpowiedzi lub pomiary dla 10 alternatyw
#' ocenianych w ramach 10 zmiennych reprezentujących kryteria.
#' Zbiór przeznaczony do użycia z funkcją `przygotuj_dane_mcda()`.
#'
#' @format Ramka danych (data frame) z 50 wierszami i 12 zmiennymi:
#' \describe{
#'
#'   \item{EkspertID}{Identyfikator eksperta (1–5). Każdy ekspert ocenia wszystkie kraje.}
#'
#'   \item{Alternatywa}{Nazwa kraju będącego alternatywą decyzyjną.}
#'
#'   \item{oferty_IT}{Liczba ofert pracy IT (zmienna ciągła).}
#'
#'   \item{dojazd_godz}{Średni czas podróży z Polski (godziny, zmienna ciągła).}
#'
#'   \item{koszt_mieszkania}{Średni koszt wynajmu mieszkania (EUR/miesiąc, zmienna ciągła).}
#'
#'   \item{odleglosc_km}{Odległość geograficzna od Polski (km, zmienna ciągła).}
#'
#'   \item{dostep_jedzenie_uslugi}{Liczba punktów usług/żywienia w okolicy (zmienna ciągła).}
#'
#'   \item{formalnosci_dni}{Czas formalności związanych z pracą (dni, zmienna ciągła).}
#'
#'   \item{roznica_czasu}{Różnica strefy czasowej względem Polski (0–2, zmienna dyskretna).}
#'
#'   \item{komunikacja_lokalna}{Ocena jakości komunikacji lokalnej (skala Likerta 1–7).}
#'
#'   \item{nauka_jezyka}{Możliwość nauki języka (skala Likerta 1–5). Zawiera wartość 99 jako kod błędu/braku odpowiedzi.}
#'
#'   \item{przyjaznosc_kultury}{Ocena przyjazności kultury (skala Likerta 1–7).}
#'
#' }
#'
#' @usage data(mcda_dane_surowe)
#' @name mcda_dane_surowe
NULL
