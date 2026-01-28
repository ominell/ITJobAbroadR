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
    scale_fill_gradient(low = "#EDE791", high = "#4C656E"),
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
        y = "Indeks Ryzyka / Żalu (R)",
        size = "Dominacja",
        fill = "Wynik"
      ) +
      .motyw_wykresu()
}

#' Mapa Strategiczna MULTIMOORA
#' @export
plot.fuzzy_multimoora_res <- function(x, ...) {
  df <- x$wyniki
  df$Sila <- (max(df$Ranking_MM) - df$Ranking_MM + 1)^2

  ggplot(df, aes(x = RS_Wynik, y = RP_Wynik)) +
    annotate("rect", xmin = median(df$RS_Wynik), xmax = Inf, ymin = -Inf, ymax = median(df$RP_Wynik), fill = "#E8F5E9", alpha = 0.5) +
    geom_point(aes(size = Sila, fill = as.factor(Ranking_MM)), shape = 21, color = "black") +
    geom_text_repel(aes(label = paste0("Alt ", Alternatywa))) +

    .motyw_wykresu() +
    scale_fill_brewer(palette = "RdYlGn", direction = -1) +
    labs(
        title = "Mapa MULTIMOORA",
        x = "System Ilorazowy (Max)",
        y = "Punkt Odniesienia (Min)")
}
