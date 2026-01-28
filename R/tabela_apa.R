#' @title Generowanie Tabeli APA
#' @description
#' Funkcja przekształca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta Ranking)
#' w sformatowaną tabelę zgodną ze standardem APA, gotową do publikacji w Wordzie.
#'
#' @param x Obiekt wynikowy z funkcji pakietu (np. `fuzzy_vikor_res`).
#' @param tytul Opcjonalny tytuł tabeli.
#' @return Obiekt klasy `flextable` gotowy do druku lub zapisu do Worda.
#' @importFrom rempsyc nice_table
#' @importFrom flextable autofit save_as_docx
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod("tabela_apa")
}

#' @export
tabela_apa.fuzzy_vikor_res <- function(x, tytul = "Wyniki metody Fuzzy VIKOR") {
  df <- x$wyniki

  names(df) <- c("Alternatywa", "S (Grupa)", "R (Zal)", "Q (Kompromis)", "Ranking")

  df$`S (Grupa)`     <- round(df$`S (Grupa)`, 3)
  df$`R (Zal)`       <- round(df$`R (Zal)`, 3)
  df$`Q (Kompromis)` <- round(df$`Q (Kompromis)`, 4)

  rempsyc::nice_table(
    df,
    title = c("Tabela 2", tytul),
    note = c("Uwaga. S: użyteczność grupy, R: indywidualny żal, Q: indeks kompromisu (im mniej tym lepiej).")
  )
}

#' @export
tabela_apa.fuzzy_multimoora_res <- function(x, tytul = "Wyniki MULTIMOORA") {
  df <- x$wyniki[, c("Alternatywa", "RS_Ranking", "RP_Ranking", "FMF_Ranking", "Ranking_MM")]
  names(df) <- c("Alternatywa", "Rank Ratio", "Rank Ref.Point", "Rank Mult.Form", "MULTIMOORA")
  rempsyc::nice_table(df, title = c("Tabela", tytul))
}

#' @export
tabela_apa.list <- function(x, tytul = "Meta-Ranking (Konsensus)") {
  # Obsługa Meta-Rankingu
  if(is.null(x$porownanie)) stop("To nie jest obiekt meta-rankingu.")
  df <- x$porownanie
  # Usuwamy "podłogi" z nazw kolumn (np. Meta_Suma -> Meta Suma)
  names(df) <- gsub("_", " ", names(df))
  rempsyc::nice_table(
    df,
    title = c("Tabela 4", tytul),
    note = c("Zestawienie rang uzyskanych różnymi metodami oraz rankingi konsensusu.")
  )
}



