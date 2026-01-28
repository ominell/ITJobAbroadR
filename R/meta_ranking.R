#' @title Teoria Dominacji dla Rankingu
#' @description Funkcja pomocnicza do konsensusu.
#' @keywords internal
oblicz_dominacje_ranking <- function(rank_mat) {
  n <- nrow(rank_mat)
  finalny_ranking <- rep(0, n)
  dostepne <- rep(TRUE, n)

  for (obecna_poz in 1:n) {
    obecna_mac <- rank_mat
    obecna_mac[!dostepne, ] <- Inf

    # Kto ma najlepszą rangę w każdej metodzie?
    kandydaci <- apply(obecna_mac, 2, which.min)
    freq_table <- table(kandydaci)

    max_votes <- max(freq_table)
    zwyciezcy <- as.numeric(names(freq_table)[freq_table == max_votes])

    if (length(zwyciezcy) == 1) {
      zwyciezcy_ind <- zwyciezcy
    } else {
      # Remis: wybierz tego z najmniejszą sumą rang
      sums <- rowSums(rank_mat[zwyciezcy, , drop = FALSE])
      zwyciezcy_ind <- zwyciezcy[which.min(sums)]
    }

    finalny_ranking[zwyciezcy_ind] <- obecna_poz
    dostepne[zwyciezcy_ind] <- FALSE
  }
  return(finalny_ranking)
}

#' @title Rozmyty Meta-Ranking
#' @description Agreguje wyniki: VIKOR, MULTIMOORA.
#' @param macierz_decyzyjna Rozmyta macierz decyzyjna.
#' @param typy_kryteriow Wektor typów kryteriów ("min", "max").
#' @param wagi Wektor wag (opcjonalny).
#' @param bwm_najlepsze,bwm_najgorsze Parametry BWM.
#' @param v Parametr VIKOR.
#'
#' @export
fuzzy_meta_ranking <- function(macierz_decyzyjna,
                               typy_kryteriow,
                               wagi = NULL,
                               bwm_najlepsze = NULL,
                               bwm_najgorsze = NULL,
                               v = 0.5) {

  # 1. Wagi (jeśli brak -> Entropia)
  if (is.null(wagi) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze)))
  {
    message("Brak wag. Obliczam Entropię...")
    wagi_surowe <- oblicz_wagi_entropii(macierz_decyzyjna)
    wagi <- rep(wagi_surowe, each = 3)
  }

  # 2. Uruchomienie Metod
  args_base <- list(macierz_decyzyjna = macierz_decyzyjna, typy_kryteriow = typy_kryteriow)
  if (!is.null(wagi)) args_base$wagi <- wagi
  if (!is.null(bwm_najlepsze)) {
    args_base$bwm_najlepsze <- bwm_najlepsze
    args_base$bwm_najgorsze <- bwm_najgorsze
  }

  # VIKOR, TOPSIS, WASPAS
  res_vikor  <- do.call(fuzzy_vikor, c(args_base, list(v = v)))

  # MULTIMOORA
  res_mm <- do.call(fuzzy_multimoora, args_base)

  # 3. Zestawienie Wyników
  rank_matrix <- cbind(
    res_vikor$results$Ranking,
    res_mm$wyniki$Ranking_MM
  )
  colnames(rank_matrix) <- c("VIKOR", "MMOORA")

  # 4. Agregacja
  # A. Suma
  rank_sum <- rank(rowSums(rank_matrix), ties.method = "first")

  # B. Dominacja
  rank_dom <- oblicz_dominacje_ranking(rank_matrix)

  # C. RankAggreg
  ra_input <- t(apply(rank_matrix, 2, order)) # Konwersja na listę indeksów
  n_alt <- nrow(macierz_decyzyjna)

  if (n_alt <= 10) {
    ra <- RankAggreg::BruteAggreg(ra_input, n_alt, distance = "Spearman")
  } else {
    ra <- RankAggreg::RankAggreg(ra_input, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  # Konwersja wyniku RA na wektor rang
  rank_ra <- numeric(n_alt)
  top_list <- ra$top.list
  if (is.numeric(top_list)) {
    for(i in 1:n_alt) rank_ra[top_list[i]] <- i
  } else {
    for(i in 1:n_alt) rank_ra[top_list[i]] <- i # Jeśli nazwy są indeksami
  }

  # 5. Wynik końcowy
  comp_df <- data.frame(
    Alternative = rownames(macierz_decyzyjna),
    R_VIKOR = rank_matrix[,1],
    R_MMOORA = rank_matrix[,2],
    Meta_Sum = rank_sum,
    Meta_Dominance = rank_dom,
    Meta_Aggreg = rank_ra
  )

  return(list(comparison = comp_df, correlations = cor(comp_df[,-1], method="spearman")))
}
