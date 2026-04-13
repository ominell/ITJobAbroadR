
# ITJobAbroadR

<!-- badges: start -->

<!-- badges: end -->

**ITJobAbroadR** to pakiet R przeprowadzenie pełnej analizy MCDA
(Multi‑Criteria Decision Analysis) w kontekście wyboru kraju do pracy w
branży IT.

## Funkcje pakietu

- **Przygotowanie danych rozmytych**
- **Best-Worst Method (BWM)** - ważenie kryteriów
- **2 metody MCDA:**
- Fuzzy VIKOR
- MultiMOORA
- **Wizualizacje wyników**

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
# Definiujemy, które kolumny tworzą kryteria 
skladnia <- "Warunki =~ war_oferty + war_siec + war_elast;
             Koszt =~ koszt_mieszkanie + koszt_jedzenie + koszt_transport;
             Atrakcyjnosc =~ atr_kultura + atr_jezyk + atr_obcy + atr_zdrowie;
             Rozwoj =~ rozwoj_zaw + rozwoj_osob;
             Strefa_czasowa =~ roznica_czasu"

macierz_rozmyta <- przygotuj_dane_mcda(mcda_dane_surowe, skladnia, kolumna_alternatyw = "Alternatywa") 

# 3. Oblicz ranking metodą Fuzzy VIKOR
res_vikor <- fuzzy_vikor(macierz_decyzyjna = macierz_rozmyta,
                     typy_kryteriow= c("min","max","max","min","min"),
                     bwm_kryteria = c("Warunki", "Koszt", "Atrakcyjnosc", "Rozwoj", "Strefa_czasowa"), 
                     bwm_najlepsze = c(1,3,2,6,8),
                     bwm_najgorsze = c(8,5,6,3,1)
)
#> Obliczanie wag metodą BWM...

# 4. Wyświetl wynik 
print(res_vikor$wyniki) 
#>    Alternatywa         S          R         Q ranking
#> 1            1 0.3459050 0.14454410 0.4392888       6
#> 2            2 0.3462842 0.17117182 0.4703025       8
#> 3            3 0.3147758 0.13465188 0.4141762       5
#> 4            4 0.3913298 0.29070491 0.6285342      10
#> 5            5 0.4547699 0.25052115 0.6098063       9
#> 6            6 0.0930141 0.06372314 0.2347465       1
#> 7            7 0.2145565 0.11009372 0.3417719       4
#> 8            8 0.2365612 0.09947639 0.3391229       3
#> 9            9 0.3202589 0.17155513 0.4593323       7
#> 10          10 0.1890987 0.09333401 0.3111909       2

# 5. Wyświetl mapę decyzyjną 
plot(res_vikor) 
```

<img src="man/figures/README-example-1.png" width="100%" />
