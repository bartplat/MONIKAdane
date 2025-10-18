#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja przechowująca nazwę i adres (z RSPO) szkoły, której
#' dotyczy raport.
#' @param x ramka danych pośrednich P4
#' @return list
#' @importFrom dplyr %>%
#' @importFrom tibble is_tibble
#' @export
dane_szkoly <- function(x) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            c("nazwa_szk", "adres_szk") %in% names(x))

  if (length(unique(x$nazwa_szk)) > 1 | length(unique(x$adres_szk)) > 1) {
    naz <- ""
    adr <- ""
    warning("Zmienne `nazwa_szk` i `adres_szk` zawierają więcej niż 1 unikalną wartość, więc zwrócone zostaną puste wartości tekstowe")
  } else {
    naz <- unique(x$nazwa_szk)
    adr <- unique(x$adres_szk)
  }

  list(
    nazwa <- naz,
    adres <- adr
  ) %>%
    return()
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę absolwentów na danym
#' poziomie agregacji.
#' @param x ramka danych pośrednich P4
#' @return numeric
#' @importFrom dplyr %>% select count n_distinct
#' @importFrom tibble is_tibble
#' @export
l_abs <- function(x) {
  stopifnot(is.data.frame(x) | is_tibble(x))

  x <- x %>%
    select(id_abs, rok_abs)

  if (n_distinct(x$id_abs) != nrow(x)) {
    powt <- x %>%
      count(id_abs, rok_abs) %>%
      filter(n > 1) %>%
      nrow()
    warning(paste("W zbiorze danych występują zdublowane wartości par `id_abs` i `rok_abs` w liczbie: "), powt)
  }

  x %>%
    n_distinct() %>%
    return()
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę kobiet wśród
#' absolwentów, którzy zostali objęci monitoringiem na danym poziomie agregacji.
#' @param x ramka danych pośrednich P4
#' @return numeric
#' @importFrom dplyr %>% select filter .data n_distinct
#' @export
l_kobiet <- function(x) {
  x %>%
    select(id_abs, rok_abs, .data$plec) %>%
    filter(plec == "K") %>%
    n_distinct() %>%
    return()
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja oblicza wskaźnik opisujący liczbę absolwentów na danym
#' poziomie agregacji, o których pozyskano informacje z poszczególnych
#' rejestrów.
#' @param x ramka danych pośrednich P4
#' @return list
#' @importFrom dplyr %>% reframe .data
#' @export
l_abs_zrodla <- function(x) {
  x %>%
    reframe(
      n_sio = sum(abs_w_sio, na.rm = TRUE),
      n_polon = sum(abs_w_polon, na.rm = TRUE),
      n_cke = sum(abs_w_cke, na.rm = TRUE),
      n_zus = sum(abs_w_zus, na.rm = TRUE)
    ) %>%
    as.list() %>%
    return()
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów o danym statusie
#' edukacyjno-zawodowym (wskaźnik S3) w danym miesiącu dla raportu dla danego
#' roku.
#' @details Wyróżniamy następujące statusy (bez KUZ i KKZ):
#' \itemize{
#'  \item{tylko_ucz}{Tylko nauka}
#'  \item{ucz_prac}{Nauka i praca}
#'  \item{tylko_prac}{Tylko praca}
#'  \item{bezrob}{Bezrobotny}
#'  \item{brak_danych}{Brak danych o aktywności}
#' }
#' Bezrobotni kontynuujący naukę niestacjonarnie nie są wykluczani z kategorii
#' „Nauka”, więc określając ich status w danym miesiącu traktujemy ich jako
#' osoby uczące się. W sytuacji gdy w tym samym miesiącu absolwent miał różne
#' statusy, przypisujemy mu „bardziej aktywny” z tych statusów (np. „naukę” a
#' nie „bezrobocie”).
#' Wszystkie powyższe statusy są od tej edycji dodawane do tabel pośrednich p3,
#' a ta funkcja jedynie zlicza odsetek ich występowania.
#' @param x ramka danych pośrednich P3
#' @param rok_od rok początku okresu, dla którego ma być policzony wskaźnik
#' @param mies_od miesiąc początku okresu, dla którego ma być policzony wskaźnik
#' @param rok_do rok końca okresu, dla którego ma być policzony wskaźnik
#' @param mies_do miesiąc końca okresu, dla którego ma być policzony wskaźnik
#' @return list
#' @importFrom dplyr %>% .data filter reframe n_distinct
#' @importFrom tibble is_tibble
#' @export
status_S3_mies <- function(x, rok_od, mies_od, rok_do, mies_do) {
  stopifnot(
    is.data.frame(x) | is_tibble(x),
    rok_od %in% c(2023, 2024),
    mies_od %in% c(1:12),
    rok_do %in% c(2023, 2024),
    mies_do %in% c(1:12),
    "status" %in% names(x)
  )
  
  l_od <- data_na_okres(rok = rok_od, mies = mies_od)
  l_do <- data_na_okres(rok = rok_do, mies = mies_do)
  
  x %>%
  filter(
    okres %in% seq(l_od, l_do, by = 1)
  ) %>%
  reframe(
    n = n_distinct(id_abs),
    tylko_ucz = sum(status == "Tylko nauka") / n,
    ucz_prac = sum(status == "Nauka i praca") / n,
    tylko_prac = sum(status == "Tylko praca") / n,
    bezrob = sum(status == "Bezrobocie") / n,
    brak_danych = sum(status == "Brak danych o aktywności") / n
  ) %>%
  as.list() %>%
  return()
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów o danym statusie
#' edukacyjno-zawodowym wśród grup absolwentów wydzielonych ze względu na zawód,
#' w którym się kształcili w szkole.
#' @details Zwracana lista służy jako wsad do tabeli w szkolnym raporcie
#' automatycznym. Jeśli lista będąca wsadem tabeli generowałaby pustą
#' tabelę, to zwracana jest lista o `n` równym 0 oraz z brakiem danych dla
#' pozostałych elementów listy.
#' @param x ramka danych pośrednich P3
#' @param rok_od rok początku okresu, dla którego ma być policzony wskaźnik
#' @param mies_od miesiąc początku okresu, dla którego ma być policzony wskaźnik
#' @param rok_do rok końca okresu, dla którego ma być policzony wskaźnik
#' @param mies_do miesiąc końca okresu, dla którego ma być policzony wskaźnik
#' @return list
#' @importFrom dplyr %>% group_by .data ungroup filter mutate select
#' @importFrom tibble as_tibble
#' @export
zawody_status_S3 <- function(x, rok_od, mies_od, rok_do, mies_do) {
  stopifnot(
    is.data.frame(x),
    "nazwa_zaw" %in% names(x),
    rok_od %in% c(2023, 2024),
    mies_od %in% c(1:12),
    rok_do %in% c(2023, 2024),
    mies_do %in% c(1:12)
  )
  
  if (any(unique(x$typ_szk) %in% c(
    "Branżowa szkoła I stopnia",
    "Technikum",
    "Szkoła policealna",
    "Branżowa szkoła II stopnia"
  ))) {
    tab <- x %>%
      group_by(.data$nazwa_zaw) %>%
      status_S3_mies(rok_od, mies_od, rok_do, mies_do) %>%
      as_tibble() %>%
      ungroup() %>%
      arrange(desc(n))
    
    tot <- x %>%
      filter(.data$nazwa_zaw %in% unique(tab$nazwa_zaw)) %>%
      status_S3_mies(rok_od, mies_od, rok_do, mies_do) %>%
      as_tibble() %>%
      mutate(nazwa_zaw = "Ogółem") %>%
      select(nazwa_zaw, n:brak_danych)
    
    if (nrow(tab) == 0) {
      return(list(
        n = 0,
        tylko_ucz = NA_real_,
        ucz_prac = NA_real_,        
        tylko_prac = NA_real_,
        bezrob = NA_real_,
        brak_danych = NA_real_
      ))
    } else {
      rbind(tab, tot) %>%
      as.list() %>%
      return()
    }
  } else {
    return(list(
        n = 0,
        tylko_ucz = NA_real_,
        ucz_prac = NA_real_,        
        tylko_prac = NA_real_,
        bezrob = NA_real_,
        brak_danych = NA_real_
      ))
  }
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca wskaźnik E2 - Sposoby kontynuowania edukacji.
#' Domyślnym okresem, w którym sprawdzany jest odsetek kontynuujących naukę jest
#' grudzień roku ukończenia szkoły (zmienna `p4$typ_szk_kont6`).
#' Od bieżącej edycji monitoringu informacja ta dołączona jest do tabeli p4 jako
#' zmienna `p4$typ_szk_kont6`, gdzie wartość "6" oznacza 6 miesięcy od
#' ukończenia szkoły (6 miesięcy od czerwca, czyli grudzień).
#' @param x ramka danych pośrednich P4
#' @return list
#' @importFrom dplyr %>% filter reframe n_distinct
#' @export
E2_nauka_kontyn <- function(x) {
  stopifnot(
    is.data.frame(x),
    "typ_szk_kont6" %in% names(x)
  )
  
  nka <- n_distinct(x$id_abs)
  
  x <- x %>% 
    pull(typ_szk_kont6) %>% 
    as.data.frame() %>% 
    mutate(`Brak kontynuacji nauki` = ifelse(rowSums(., na.rm = TRUE) > 0, 0, 1)) %>%
    colSums(na.rm = TRUE) %>% 
    as.list()
  
  x <- c(nka, x)

  return(x)
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca odsetek pracujących w okresie od września do
#' grudnia roku ukończenia szkoły w podziale na pobierających i nie
#' pobierających naukę.
#' @param x ramka danych pośrednich P4
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się (TRUE) czy nie uczących się (FALSE)
#' @return list
#' @importFrom dplyr %>% reframe n
#' @export
Z4_ods_prac_mies <- function(x, nauka) {
  stopifnot(
    is.data.frame(x),
    c("praca_nauka_r0_wrzgru", "praca_bez_nauki_r0_wrzgru") %in% names(x),
    is.logical(nauka)
  )

  if (nauka) {    
    x %>% 
      reframe(
        n = n(),
        p0 = sum(praca_nauka_r0_wrzgru == "Brak pracy") / n,
        czesc = sum(praca_nauka_r0_wrzgru == "Praca przez część okresu") / n,
        p100 = sum(praca_nauka_r0_wrzgru == "Praca przez cały okres") / n
      ) %>% 
        as.list() %>% 
        return()
  } else {
    x %>% 
      reframe(
        n = n(),
        p0 = sum(praca_bez_nauki_r0_wrzgru == "Brak pracy") / n,
        czesc = sum(praca_bez_nauki_r0_wrzgru == "Praca przez część okresu") / n,
        p100 = sum(praca_bez_nauki_r0_wrzgru == "Praca przez cały okres") / n
      ) %>% 
        as.list() %>% 
        return()
  }
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów wykonujących dane formy
#' pracy w danym miesiącu (domyślnie jest to grudzień). Wskaźnik może być
#' liczony albo dla absolwentów pracujących i kontynuujących naukę (\code{nauka
#' = TRUE}) lub dla absolwentów pracujących i nie kontynuujących nauki
#' (\code{nauka = FALSE}). Wyróżnione formy pracy:
#' \itemize{
#'  \item{\code{ucz_uop}}{Zatrudnienie na podstawie UOP oraz brak innej formy},
#'  \item{\code{ucz_samoz}}{Prowadzący działalność gosp. (samozatrudnienie)
#'  oraz brak innej formy},
#'  \item{\code{ucz_inna}}{Tylko forma inna niż samozatrudnienie i niż umowa o
#'  pracę (np. umowa-zlecenie)},
#'  \item{\code{ucz_wiecej}}{Więcej niż jedna forma, czyli: Zatrudnienie na
#'  podstawie UOP oraz prowadzący działalność gosp., Zatrudnienie na podstawie
#'  UOP oraz zatrudnienie w innej formie, Prowadzący działalność gosp.
#'  (samozatrudnienie) oraz zatrudnienie w innej formie, Zatrudnienie na
#'  podstawie UOP oraz działalność gosp. oraz zatrudnienie w innej formie}
#'  \item{\code{NA}{Brak danych - dla absolwentów, którzy w zmiennej `praca`
#'  posiadają wartość `0` lub brak danych (czyli pozostałe wartości spoza
#'  zakresu od 1 do 7)}}
#' }
#' @param x ramka danych pośrednich P3
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return list
#' @importFrom dplyr %>% filter .data reframe pull n_distinct
#' @export
Z8_formy_prac_mies <- function(x, rok, mies = 12, nauka) {
  stopifnot(is.data.frame(x),
            rok %in% c(2023, 2024),
            mies %in% c(1:12),
            is.logical(nauka),
            c("okres", "nauka2", "nauka_szk_abs") %in% names(x))

  x <- x %>%
    filter(okres == data_na_okres(mies, rok)) %>%
    mutate(forma = case_when(
      praca == 1 ~ "uop",
      praca == 2 ~ "samoz",
      praca == 3 ~ "inna",
      praca %in% c(4:7) ~ "wiecej",
      TRUE ~ NA_character_))

  if (nrow(x) == 0) {
    return(list(n = 0))
  } else {
    if (nauka) {
      nka <- x %>%
        filter(
          (nauka2 == 1 | nauka_szk_abs == 1) & praca > 0
        ) %>%
        pull(id_abs) %>%
        n_distinct()
      if (nka == 0) {
        return(
          list(
            n = 0,
            ucz_uop = 0,
            ucz_samoz = 0,
            ucz_inna = 0,
            ucz_wiecej = 0
          )
        )
      } else {
        x %>%
          reframe(
            n = nka,
            ucz_uop = sum((nauka2 == 1 | nauka_szk_abs== 1) & forma == "uop", na.rm = TRUE) / nka,
            ucz_samoz = sum((nauka2 == 1 | nauka_szk_abs == 1) & forma == "samoz", na.rm = TRUE) / nka,
            ucz_inna = sum((nauka2 == 1 | nauka_szk_abs == 1) & forma == "inna", na.rm = TRUE) / nka,
            ucz_wiecej = sum((nauka2 == 1 | nauka_szk_abs == 1) & forma == "wiecej", na.rm = TRUE) / nka) %>%
          as.list() %>%
          return()
      }
    } else {
      nka <- x %>%
        filter(nauka2 == 0 & praca > 0) %>%
        pull(id_abs) %>%
        n_distinct()
      if (nka == 0) {
        return(
          list(n = 0,
            nieucz_uop = 0,
            nieucz_samoz = 0,
            nieucz_inna = 0,
            nieucz_wiecej = 0
          )
        )
      } else {
        x %>%
          reframe(
            n = nka,
            nieucz_uop = sum(nauka2 == 0 & forma == "uop", na.rm = TRUE) / nka,
            nieucz_samoz = sum(nauka2 == 0 & forma == "samoz", na.rm = TRUE) / nka,
            nieucz_inna = sum(nauka2 == 0 & forma == "inna", na.rm = TRUE) / nka,
            nieucz_wiecej = sum(nauka2 == 0 & forma == "wiecej", na.rm = TRUE) / nka) %>%
          as.list() %>%
          return()
      }
    }
  }
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca odsetek absolwentów, którzy w danym miesiącu
#' (domyślnie jest to wrzesień) kontynuowali zatrudnienie u pracodawcy, który
#' wcześniej (tj. kiedy byli jeszcze uczniami) zatrudniał ich jako pracowników
#' młodocianych. Wskaźnik liczony jest tylko dla absolwentów branżowych szkół 1.
#' stopnia. Wyróżnianych jest 5 form kontynuowania pracy u pracodawcy
#' zatrudniającego poprzednio jako młodocianego:
#' \itemize{
#'  \item{\code{nieucz_niekontuop}}{Nieuczący się, nie kontynuujący pracy u
#'  danego pracodawcy}
#'  \item{\code{nieucz_kont_uop}}{Nieuczący się, kontynuujący pracę u danego
#'  pracodawcy na podstawie umowy o pracę}
#'  \item{\code{nieucz_kont_inne}}{Nieuczący się, kontynuujący pracę u danego
#'  pracodawcy w formie innej niż umowa  o pracę}
#'  \item{\code{ucz_niekontuop}}{Uczący się, nie kontynuujący pracy u danego
#'  pracodawcy na podstawie umowy o pracę}
#'  \item{\code{ucz_kontuop}}{Uczący się, kontynuujący pracę u danego
#'  pracodawcy na podstawie umowy o pracę}
#' }
#' @param x ramka danych pośrednich P3
#' @param rok rok lub zakres lat osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się (TRUE) czy nie uczących się (FALSE)
#' @return list
#' @importFrom dplyr %>% filter reframe n_distinct
#' @export
Z9_kont_mlod <- function(x, rok, mies = 9, nauka) {
  stopifnot(
    is.data.frame(x),
    "kont_mlodoc_prac" %in% names(x),
    rok %in% c(2023, 2024),
    mies %in% c(1:12),
    is.logical(nauka)
  )
  
  if (any(unique(x$typ_szk) == "Branżowa szkoła I stopnia")) {
    x <- x %>%
    filter(
      .data$okres == data_na_okres(mies, rok),
      !is.na(.data$kont_mlodoc_prac)
    )
    
    if (nrow(x) == 0) {
      return(list(n = 0))
    } else {
      if (nauka) {
        nka <- x %>%
        filter(kont_mlodoc_prac %in% c(4:6)) %>% 
        pull(.data$id_abs) %>%
        n_distinct()
        
        x %>%
        reframe(
          n = nka,
          niekont_uop = sum(.data$kont_mlodoc_prac == 4) / nka,
          kont_uop = sum(.data$kont_mlodoc_prac == 5) / nka,
          kont_inne = sum(.data$kont_mlodoc_prac == 6) / nka
        ) %>%
        as.list() %>%
        return()
      } else {
        nka = x %>%
        filter(kont_mlodoc_prac %in% c(1:3)) %>% 
        pull(.data$id_abs) %>%
        n_distinct()
        
        x %>%
        reframe(
          n = nka,
          niekont_uop = sum(.data$kont_mlodoc_prac == 1) / nka,
          kont_uop = sum(.data$kont_mlodoc_prac == 2) / nka,
          kont_inne = sum(.data$kont_mlodoc_prac == 3) / nka
        ) %>%
        as.list() %>%
        return()
      }
    }
  }
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca średni względny miesięczny przychód z pracy
#' \strong{etatowej} (na Umowie o Pracę - UOP) w danym okresie w odniesieniu do
#' zarobków w zamieszkiwanym powiecie. Domyślnie jest to okres między wrześniem
#' a grudniem roku przekazanego w argumentach funkcji, ale te wartości można
#' zmieniać. Średnia liczona jest oddzielnie dla uczących i nie uczących się
#' absolwentów.
#' @details Relatywna średnia zarobków liczona jest w kilku etapach. W pierwszym
#' kroku, dla każdego absolwenta liczony jest stosunek wartości wynagrodzenia na
#' UOP w danym miesiącu do średniego wynagrodzenia w powiecie, w którym
#' uczęszczał do szkoły według GUS. Następnie, dla każdego absolwenta liczona
#' jest ze stosunków zarobków absolwenta w poszczególnych miesiącach do średnich
#' zarobków w powiecie. Ze zbioru osobo-miesięcy przechodzimy na zbiór osób,
#' gdzie każdy wiersz odpowiada absolwentowi, a iloraz zarobków absolwenta i
#' zarobków w powiecie jest uśredniony - średni odsetek dla wszystkich miesięcy,
#' w których dochód był niezerowy. Następnie na tych indywidualnych wartościach
#' liczone są statystyki opisowe dla zadanego poziomu agregacji.
#' @param x ramka danych pośrednich P4
#' @param nauka wartość TRUE/FALSE określająca czy status ma być liczony dla
#' absolwentów uczących się czy nie uczących się
#' @return list
#' @importFrom dplyr %>% reframe
#' n_distinct
#' @export
W3_sr_doch_uop = function(x, nauka) {
  stopifnot(
    is.data.frame(x),
    c("sr_wynagr_uop_nauka_r0_wrzgru", "sr_wynagr_uop_bez_nauki_r0_wrzgru") %in% names(x),
    is.logical(nauka)
  )
  
  if (nauka) {    
    x %>%
      reframe(
        n = n_distinct(id_abs),
        sred = round(mean(sr_wynagr_uop_nauka_r0_wrzgru, na.rm = TRUE), 2),
        q5 = unname(round(quantile(sr_wynagr_uop_nauka_r0_wrzgru, 0.05, na.rm = TRUE), 2)),
        q25 = unname(round(quantile(sr_wynagr_uop_nauka_r0_wrzgru, 0.25, na.rm = TRUE), 2)),
        med = unname(round(quantile(sr_wynagr_uop_nauka_r0_wrzgru, 0.5, na.rm = TRUE), 2)),
        q75 = unname(round(quantile(sr_wynagr_uop_nauka_r0_wrzgru, 0.75, na.rm = TRUE), 2)),
        q95 = unname(round(quantile(sr_wynagr_uop_nauka_r0_wrzgru, 0.95, na.rm = TRUE), 2))
      ) %>%
      as.list() %>%
      return()
    } else {
      x %>%
      reframe(
        n = n_distinct(id_abs),
        sred = round(mean(sr_wynagr_uop_bez_nauki_r0_wrzgru, na.rm = TRUE), 2),
        q5 = unname(round(quantile(sr_wynagr_uop_bez_nauki_r0_wrzgru, 0.05, na.rm = TRUE), 2)),
        q25 = unname(round(quantile(sr_wynagr_uop_bez_nauki_r0_wrzgru, 0.25, na.rm = TRUE), 2)),
        med = unname(round(quantile(sr_wynagr_uop_bez_nauki_r0_wrzgru, 0.5, na.rm = TRUE), 2)),
        q75 = unname(round(quantile(sr_wynagr_uop_bez_nauki_r0_wrzgru, 0.75, na.rm = TRUE), 2)),
        q95 = unname(round(quantile(sr_wynagr_uop_bez_nauki_r0_wrzgru, 0.95, na.rm = TRUE), 2))
      ) %>%
      as.list() %>%
      return()
    }
  }
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca na potrzeby szablonu raportu rozkład liczby
#' miesięcy bezrobocia rejestrowanego wśród absolwentów od września do grudnia w
#' roku ukończenia szkoły.
#' @param x ramka danych pośrednich P4
#' @return list
#' @importFrom dplyr %>% count filter arrange mutate select
#' across
#' @export
B2_ods_bezrob = function(x) {
  stopifnot(
    is.data.frame(x),
    "bezrobocie_r0_wrzgru" %in% names(x)
  )
  
  wzor <- data.frame(
    "bezrobocie_r0_wrzgru" = c("Brak bezrobocia", "1 miesiąc", "2 miesiące", "3 miesiące", "4 miesiące"),
    "n" = rep(0, times = 5)
  )
  
  mies_bezrob <- x %>% 
    count(bezrobocie_r0_wrzgru)
  
  mies_bezrob <- rbind(
    mies_bezrob,
    wzor %>% 
      filter(!bezrobocie_r0_wrzgru %in% mies_bezrob$bezrobocie_r0_wrzgru)
  ) %>% 
    arrange(bezrobocie_r0_wrzgru) %>% 
    mutate(pct = n / sum(n))

  nka <- sum(mies_bezrob$n)

  c(n = nka,
    mies_bezrob %>% 
      select(-n) %>% 
      as.list()) %>% 
    return()
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca rozkład liczebności wyuczonych zawodów wśród
#' absolwentów w podziale na branże. Dodatkowo, funkcja liczy liczebności
#' absolwentów w branżach, a informacja ta służy jako podstawa do definiowania
#' warunków w szablonie raportu dla szkół branżowych 1. stopnia.
#' @details UWAGA: Wynikiem działania funkcji jest rozkład liczebności, w którym
#' nie odfiltrowuje się obserwacji poniżej 10 absolwentów, więc należy to zrobić
#' na poziomie szablonu raportu.
#' @param x ramka danych pośrednich P4
#' @return list
#' @importFrom dplyr %>% filter .data count mutate
#' @export
liczebnosc_branze_ucz = function(x) {
  stopifnot(
    is.data.frame(x),
    "branza" %in% names(x)
  )

  if (any(unique(x$typ_szk) == "Branżowa szkoła I stopnia")) {
    x <- x %>%
      filter(!(is.na(branza)))

    if (nrow(x) == 0) {
      return(
        list(
          n = 0
        )
      )
    } else {
      x %>%
        count(branza) %>%
        mutate(odsetek = n / sum(n)) %>% 
        as.list() %>% 
        return()
    }
  } else {
    return(
        list(
          n = 0
        )
      )
  }
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca rozklad zmeinnej z tabeli P2 przekazanej w
#' argumencie funkcji. Zmienne, dla których zaprojektowano funkjcę to:
#' \itemize{
#'  \item{\code{dyscyplina_wiodaca_kont}}{Dyscyplina studiów, w której absolwent
#'  kontynuował naukę}
#'  \item{\code{dziedzina_kont}}{Dziedzina studiów, w której absolwent
#'  kontynuował naukę na studiach}
#'  \item{\code{branza_kont}}{Branża zawodu KZSB, w której absolwent kontynuował
#'  naukę w szkole kształcącej w zawodzie}
#' }
#' @param x ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dziedzinie/dyscyplinie/branży (tabela danych pośrednich
#' P2 lub zawierająca analogiczne informacje oraz te same nazwy kolumn co tabela
#' P2)
#' @param ROK rok osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie jest
#' to grudzień
#' @param zmienna zmienna, której rozkład ma zwrócic funkcja
#' @param plc płeć absolwenta przekazana jako wartość tekstowa ("K" lub "M") -
#' dla rozkładów przez płeć
#' @return list
#' @importFrom dplyr %>% filter count mutate rename
#' @importFrom tibble is_tibble
#' @export
rozklad_liczebnosc = function(x, ROK = 2024, mies = 12, zmienna, plc = NULL) {
  stopifnot(
    is.data.frame(x) | is_tibble(x),
    c("dyscyplina_wiodaca_kont", "branza_kont", "dziedzina_kont") %in% names(x),
    is.numeric(ROK),
    ROK %in% 2024,
    is.numeric(mies),
    mies %in% c(1:12)
  )
  
  if (!is.null(plc)) {
    stopifnot(
      "plec" %in% names(x),
      is.character(plc),
      plc %in% c("K", "M")
    )
  }

  x <- x %>%
  filter(
    rok %in% ROK,
    miesiac %in% mies,
    !(is.na({{ zmienna }})),
    (is.null(plc) | plec %in% plc)
  )
  
  if (nrow(x) == 0) {
    return(list(kont = NA_character_))
  } else {  
    x %>%
    count({{ zmienna }}) %>%
    mutate(
      pct = n / sum(n),
      .keep = "unused"
    ) %>% 
    rename(kont = 1) %>% 
    as.list() %>% 
    return()
  }
}
#' @title Wskaźniki zagregowane dla monitoringu karier - dane administracyjne
#' @description Funkcja licząca rozkład liczebności danej zmiennej (funkcję
#' zaprojektowano do pracy ze zmiennymi `p2$branza` oraz
#' `p2$dyscyplina_wiodaca_kont`) absolwentów w podziale na wykonywany zawód. 
#' Funkcja liczy wskaźnik tylko dla absolwentów techników, szkół policealnych i 
#' branżowych szkół 2. stopnia.
#' @param x ramka danych zawierająca informację o kontynuowaniu
#' kształcenia w danej dyscyplinie lub branży (tabela danych pośrednich P2 lub
#' zawierająca analogiczne informacje oraz te same nazwy kolumn co tabela P2)
#' @param ROK rok osiągnięcia statusu absolwenta
#' @param mies miesiąc, dla którego ma być policzony wskaźnik - domyślnie jest
#' to grudzień
#' @param zmienna 
#' @return list
#' @importFrom dplyr %>% filter .data count mutate select left_join n_distinct
#' slice_max join_by
#' @importFrom tibble is_tibble
#' @export
rozklad_zawody <- function(x, ROK = 2024, mies = 12, zmienna) {
  stopifnot(
    is.data.frame(x) | is_tibble(x),
    c("dyscyplina_wiodaca_kont", "branza_kont") %in% names(x),
    is.numeric(ROK),
    ROK %in% 2024,
    is.numeric(mies),
    mies %in% c(1:12)
  )

  if (any(unique(x$typ_szk) %in% c("Technikum",
                                   "Szkoła policealna",
                                   "Branżowa szkoła II stopnia"))) {
    
    x <- x %>%
      filter(
        rok %in% ROK,
        miesiac %in% mies,
        !(is.na({{ zmienna }}))
      )
    
    if (nrow(x) == 0) {
      return(list(kont = NA_character_))
    } else {
      nki <- x %>%
        count(nazwa_zaw, name = "n_total") %>%
        filter(n_total >= 10)
      
      if (nrow(nki) == 0) {
        return(list(kont = NA_character_))
      }
      
      x %>%
        filter(nazwa_zaw %in% nki$nazwa_zaw) %>%
        count(nazwa_zaw, {{ zmienna }}) %>%
        group_by(nazwa_zaw) %>% 
        mutate(
          pct = n / sum(n),
          .keep = "unused"
        ) %>% 
        ungroup() %>% 
        pivot_wider(
          id_cols = {{ zmienna }},
          names_from = nazwa_zaw,
          values_from = pct,
          values_fill = 0
        ) %>% 
        rename(kont = 1) %>% 
        as.list() %>% 
        return()
    }
  } else {
    return(list(kont = NA_character_))
  }
}
  