set.seed(112)

# Tworzymy ramke danych symulujaca problem decyzyjny (wybor kraju)
# Zalozmy, ze mamy 10 alternatyw (krajów) i 5 ekspertów

kraje <- c("Niemcy", "Holandia", "Hiszpania", "Czechy", "Szwecja", "Irlandia", "Portugalia", "Austria", "Włochy", "Francja")

n_eks <- 5
n_alt <- 10

mcda_dane_surowe <- data.frame(
  # --- Identyfikatory ---
  EkspertID = rep(1:n_eks, each = n_alt),
  Alternatywa = rep(kraje, times = n_eks),

  # --- Kryterium 1: Warunki pracy ---
  war_oferty = runif(n_alt * n_eks, 500, 5000),
  war_siec = sample(1:5, n_alt * n_eks, replace = TRUE),
  war_elast = sample(1:5, n_alt * n_eks, replace = TRUE),

  # --- Kryterium 2: Koszty (dane ciągłe, EUR/miesiąc) ---
  koszt_mieszkanie = runif(n_alt * n_eks, 600, 2500),
  koszt_jedzenie = runif(n_alt * n_eks, 600, 2500),
  koszt_transport = runif(n_alt * n_eks, 50, 1000),

  # --- Kryterium 3: Atrakcyjność miejsca (skala Likerta 1-7) ---
  atr_kultura = sample(1:7, n_alt * n_eks, replace = TRUE),
  atr_jezyk = sample(c(1:7, 99), n_alt * n_eks, replace = TRUE, prob = c(rep(0.12, 7), 0.16)),
  atr_obcy = sample(1:7, n_alt * n_eks, replace = TRUE),
  atr_zdrowie = sample(1:7, n_alt * n_eks, replace = TRUE),

  # --- Kryterium 4: Możliwości rozwoju (skala Likerta 1-7) ---
  rozwoj_zaw = sample(1:7, n_alt * n_eks, replace = TRUE),
  rozwoj_osob = sample(1:7, n_alt * n_eks, replace = TRUE),

  # --- Kryterium 5: Podobieństwo strefy czasowej (skala dyskretna, różnica względem Polski 0-2h) ---
  roznica_czasu = sample(0:2, n_alt * n_eks, replace = TRUE)
)

usethis::use_data(mcda_dane_surowe, overwrite = TRUE)
