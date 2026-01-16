set.seed(112)

# Tworzymy ramke danych symulujaca problem decyzyjny (wybor kraju)
# Zalozmy, ze mamy 10 alternatyw (krajów) i 5 ekspertów
# Mamy 10 kryteriów

#Przykładowe kraje
kraje <- c("Niemcy", "Holandia", "Hiszpania", "Czechy", "Szwecja",
           "Irlandia", "Portugalia", "Austria", "Włochy", "Francja")

n_alt <- 10 #liczba alternatyw
n_eks <- 5 #liczba ekspertów

mcda_dane_surowe <- data.frame(
  # --- Identyfikatory ---
  EkspertID = rep(1:n_eks, each = n_alt), # Symulacja 5 ekspertow oceniajace każde z 10 panstw
  Alternatywa = rep(kraje, times = n_eks), # Powtórzone 5 razy dla każdego eksperta

  # Dane ciągłe
  # --- Kryterium 1: Liczba ofert pracy IT (ilość) ---
  oferty_IT = runif(n_alt * n_eks, 500, 5000),
  # --- Kryterium 2: Dostępność dojazdu z Polski (średni czas podróży w h) ---
  dojazd_godz = runif(n_alt * n_eks, 1, 10),
  # --- Kryterium 3: Koszt mieszkania (EUR/miesiąc) ---
  koszt_mieszkania = runif(n_alt * n_eks, 600, 2500),
  # --- Kryterium 4: Bliskość geograficzna (km) ---
  odleglosc_km = runif(n_alt * n_eks, 300, 2500),
  # --- Kryterium 5: Dostęp do jedzenia/usług (ilość) ---
  dostep_jedzenie_uslugi = runif(n_alt * n_eks, 50, 500),

  # Skala dyskretna
  # --- Kryterium 6: Podobieństwo strefy czasowej (różnica względem Polski 0-2h) ---
  roznica_czasu = sample(0:2, n_alt * n_eks, replace = TRUE),

  # Skala Likerta
  # --- Kryterium 7: Łatwość zatrudnienia dla objokrajowców (1-7) ---
  zatrudnienie_obcy = sample(1:7, n_alt * n_eks, replace = TRUE),
  # --- Kryterium 8: Komunikacja lokalna (1–7) ---
  komunikacja_lokalna = sample(1:7, n_alt * n_eks, replace = TRUE),
  # --- Kryterium 9: Możliwość nauki języka (1–5) ---
  nauka_jezyka = sample(c(1:5, 99), n_alt * n_eks, replace = TRUE, prob = c(rep(0.18, 5), 0.1)),
  # --- Kryterium 10: Przyjazność kultury (1–7) ---
  przyjaznosc_kultury = sample(1:7, n_alt * n_eks, replace = TRUE)
)

usethis::use_data(mcda_dane_surowe, overwrite = TRUE)
