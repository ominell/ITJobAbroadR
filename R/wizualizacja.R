#' Wewnętrzny motyw graficzny
#'
#' @description Przedstawia wyniki VIKOR przy użyciu wykresu bąbelkowego.
#' @import ggplot2
#' @import ggrepel
#'
#' @keywords internal
.motyw_wykresu <- function() {
  list(
    theme_light(base_size = 12),
    scale_fill_gradient(low = "#DCA84B", high = "#58A9A5"),
    scale_size_continuous(range = c(4, 16)),
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold")
    )
  )
}

#' Wizualizacja Fuzzy VIKOR
#'
#' @param x Wynik z fuzzy_vikor.
#' @param ... Dodatkowe argumenty przekazywane do funkcji plot.
#' @method plot fuzzy_vikor_res
#' @export
plot.fuzzy_vikor_res <- function(x, ...) {
  df <- x$wyniki

  # 1. Odwrócenie S i normalizacja
  s_min <- min(df$S); s_max <- max(df$S)
  df$Wydajnosc <- ((s_max - df$S) / (s_max - s_min)) * 100

  # 2. Wielkość bąbla (odwrócone Q)
  q_inv <- 1 - ((df$Q - min(df$Q)) / (max(df$Q) - min(df$Q)))
  df$Rozmiar <- (q_inv + 0.1)^3

  # 3. Ćwiartkowanie
  sr_wyd <- median(df$Wydajnosc, na.rm=TRUE)
  sr_ryzyko <- median(df$R, na.rm=TRUE)

  ggplot(df, aes(x = Wydajnosc, y = R)) +
    # Tło dla strefy Lidera (Prawa dolna ćwiartka: Duża wydajność, Małe ryzyko)
    annotate("rect", xmin=sr_wyd, xmax=Inf, ymin=-Inf, ymax=sr_ryzyko, fill="#E8F5E9", alpha=0.5) +
    annotate("rect", xmin=sr_wyd, xmax=-Inf, ymin=Inf, ymax=sr_ryzyko, fill="red", alpha=0.1) +
    # Linie podziału
    geom_vline(xintercept = sr_wyd, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = sr_ryzyko, linetype = "dashed", color = "grey50") +
    # Etykiety stref
    annotate("text", x = max(df$Wydajnosc), y = min(df$R), label = "STABILNY LIDER\n(Wysoka Efekt., Niskie Ryzyko)",
           hjust=1, vjust=0, size=3, fontface="bold.italic",color="darkgreen") +
    annotate("text", x = min(df$Wydajnosc), y = max(df$R), label = "UNIKAĆ\n(Niska Efekt., Wysokie Ryzyko)",
           hjust=0, vjust=1, size=3, fontface="italic", color="#B71C1C") +
    # Bąble
    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = paste0("Kraj ", Alternatywa)), box.padding = 0.5) +
    scale_x_continuous(expand = expansion(mult = 0.2)) +

    labs(
        title = "Analiza Fuzzy VIKOR",
        subtitle = "Zielona Strefa = Najlepszy kompromis",
        x = "Indeks Wydajności Grupy (odwrócone S)",
        y = "Indeks Żalu (R)",
        size = "Dominacja\n(odwrócone Q)",
        fill = "Wynik"
      ) +
      .motyw_wykresu()
}

#' Wizualizacja Fuzzy MULTIMOORA
#' @export
plot.fuzzy_multimoora_res <- function(x, ...) {
  df <- x$wyniki
  df$Sila <- (max(df$Ranking_MM) - df$Ranking_MM + 1)^2

  ggplot(df, aes(x = RS_Wynik, y = RP_Wynik)) +
    annotate("rect", xmin = median(df$RS_Wynik), xmax = Inf, ymin = -Inf, ymax = median(df$RP_Wynik), fill = "#E8F5E9", alpha = 0.5) +
    geom_point(aes(size = Sila, fill = as.factor(Ranking_MM)), shape = 21, color = "black") +
    geom_text_repel(aes(label = paste0("Kraj ", Alternatywa))) +

    .motyw_wykresu() +
    scale_fill_brewer(palette = "BrBG", direction = -1) +
    labs(
        title = "Analiza Fuzzy MULTIMOORA",
        x = "System Ilorazowy (Max)",
        y = "Punkt Odniesienia (Min)")
}


#' ------------- Tabele APA ----------------------
#' @title Generowanie Tabeli APA
#' @description
#' Funkcja przekształca wyniki analizy MCDA w sformatowaną tabelę
#' zgodną ze standardem APA, gotową do publikacji w Wordzie.
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
    title = c("Tabela 1", tytul),
    note = c("Uwaga. S: użyteczność grupy, R: indywidualny żal, Q: indeks kompromisu (im mniej tym lepiej).")
  )
}

#' @export
tabela_apa.fuzzy_multimoora_res <- function(x, tytul = "Wyniki MULTIMOORA") {
  df <- x$wyniki[, c("Alternatywa", "RS_Ranking", "RP_Ranking", "FMF_Ranking", "Ranking_MM")]
  names(df) <- c("Alternatywa", "Rank Ratio", "Rank Ref.Point", "Rank Mult.Form", "MULTIMOORA")
  rempsyc::nice_table(df, title = c("Tabela 2", tytul))
}

#' @export
tabela_apa.list <- function(x, tytul = "Meta-Ranking (Konsensus)") {
  if(is.null(x$porownanie)) stop("To nie jest obiekt meta-rankingu.")
  df <- x$porownanie
  names(df) <- gsub("_", " ", names(df))
  rempsyc::nice_table(
    df,
    title = c("Tabela 3", tytul),
    note = c("Zestawienie rang uzyskanych różnymi metodami oraz rankingi konsensusu.")
  )
}
