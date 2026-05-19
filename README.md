
# ITJobAbroadR

<!-- badges: start -->

<!-- badges: end -->

**ITJobAbroadR** to pakiet R do przeprowadzenia pełnej analizy MCDA
(Multi‑Criteria Decision Analysis) w kontekście wyboru kraju do pracy w
branży IT.

------------------------------------------------------------------------

## Funkcje pakietu

- Przygotowanie danych rozmytych
- Wyznaczenie wag przy użyciu **Best-Worst Method (BWM)**
- Implementacja metod MCDA:
- **Fuzzy VIKOR**
- **Fuzzy MultiMOORA**
- Wizualizacje wyników
- Meta-ranking

------------------------------------------------------------------------

## Instalacja

Możesz zainstalować wersję deweloperską z serwisu GitHub:

``` r
# install.packages("devtools")
devtools::install_github("ominell/ITJobAbroadR")
```

## Szybki Start

Oto podstawowy przykład użycia pakietu z wykorzystaniem wbudowanych
danych.

``` r
library(ITJobAbroadR)

# 1. Wczytaj dane 
data("mcda_dane_surowe") 

# 2. Przygotuj macierz rozmytą 
skladnia <- "Warunki =~ war_oferty + war_siec + war_elast;
             Koszt =~ koszt_mieszkanie + koszt_jedzenie + koszt_transport;
             Atrakcyjnosc =~ atr_kultura + atr_jezyk + atr_obcy + atr_zdrowie;
             Rozwoj =~ rozwoj_zaw + rozwoj_osob;
             Strefa_czasowa =~ roznica_czasu"

macierz_rozmyta <- przygotuj_dane_mcda(mcda_dane_surowe, skladnia, kolumna_alternatyw = "Alternatywa") 

# 3. Oblicz ranking metodą Fuzzy VIKOR
res_vikor <- fuzzy_vikor(macierz_decyzyjna = macierz_rozmyta,
                     typy_kryteriow= c("max","min","max","max","min"),
                     bwm_kryteria = c("Warunki", "Koszt", "Atrakcyjnosc", "Rozwoj", "Strefa_czasowa"), 
                     bwm_najlepsze = c(1,3,2,6,8),
                     bwm_najgorsze = c(8,5,6,3,1)
)
#> Obliczanie wag metodą BWM...

# 4. Wyświetl wynik 
print(res_vikor$wyniki)
#>    Alternatywa          S          R         Q ranking
#> 1            1 0.33202427 0.16358810 0.4644608       4
#> 2            2 0.34483969 0.13696039 0.4398026       2
#> 3            3 0.36810142 0.17348032 0.4907298       6
#> 4            4 0.05192986 0.04862186 0.2174769       1
#> 5            5 0.32926279 0.16227770 0.4618262       3
#> 6            6 0.44637225 0.29070491 0.6557096      10
#> 7            7 0.28029815 0.20398230 0.4883419       5
#> 8            8 0.40827222 0.23870839 0.5811138       9
#> 9            9 0.48625950 0.19831534 0.5682357       8
#> 10          10 0.33316222 0.22567344 0.5349486       7

# 5. Wyświetl mapę decyzyjną 
plot(res_vikor)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Raportowanie wyników

Możesz wygenerować tabele w stylu APA z wynikami.

``` r
tabela_apa(res_vikor)
```

<img src="man/figures/README-tabela_apa-1.png" width="100%" />
