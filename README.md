
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
skladnia <- "Oferty =~ oferty_IT;
             Czas =~ dojazd_godz;
             Koszt =~ koszt_mieszkania;
             Odległosc =~ odleglosc_km;
             Dogodnosci =~ dostep_jedzenie_uslugi;
             Strefa_czasowa =~ roznica_czasu;
             Dostepnosc =~ zatrudnienie_obcy;
             Transport =~ komunikacja_lokalna;
             Jezyk =~ nauka_jezyka;
             Kultura =~ przyjaznosc_kultury"

macierz_rozmyta <- przygotuj_dane_mcda(mcda_dane_surowe, skladnia, kolumna_alternatyw = "Alternatywa") 

# 3. Oblicz ranking metodą Fuzzy VIKOR
res_vikor <- fuzzy_vikor(macierz_decyzyjna = macierz_rozmyta,
                     typy_kryteriow= c("min", "max", "min", "min", "max", "min", "min", "min", "min", "min"),
                     bwm_kryteria = c("Oferty", "Czas", "Koszt","Odleglosc", "Dogodnosci", "Strefa_czasowa", "Dostepnosc", "Transport", "Jezyk", "Kultura"), 
                     bwm_najlepsze = c(1,7,4,5,6,8,2,3,6,5),
                     bwm_najgorsze = c(8,2,3,3,2,1,6,4,2,3)
)
#> Obliczanie wag metodą BWM...

# 4. Wyświetl wynik 
print(res_vikor$wyniki) 
#>    Alternatywa         S          R         Q ranking
#> 1            1 0.2657205 0.09855034 0.3855698       6
#> 2            2 0.2194784 0.11519149 0.3893203       7
#> 3            3 0.3585538 0.09300367 0.4249667       8
#> 4            4 0.4010886 0.19936634 0.6264429      10
#> 5            5 0.4075190 0.17205589 0.5838204       9
#> 6            6 0.2264975 0.07417146 0.3239161       4
#> 7            7 0.2581199 0.07381699 0.3399214       5
#> 8            8 0.2423257 0.06632465 0.3190100       2
#> 9            9 0.1933356 0.07962489 0.3156907       1
#> 10          10 0.2488578 0.06480586 0.3198814       3

# 5. Wyświetl mapę decyzyjną 
plot(res_vikor) 
```

<img src="man/figures/README-example-1.png" width="100%" />
