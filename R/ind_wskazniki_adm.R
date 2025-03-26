#' @title Wskaźniki indywidualne na potrzeby monitoringu karier - dane
#' administracyjne
#' @description Funkcja obliczająca status edukacyjno-zawodowy (wskaźnik S3) dla
#' danego osobo-miesiąca (lub osoby).
#' @details W tej edycji pojawia się nowy status - bezrobocie rejestrowane,
#' który został wyodrębniony ze statusu `neet`. Wyróżniamy następujące statusy
#' (bez KUZ i KKZ):
#' \itemize{
#'  \item{tylko_ucz}{tylko nauka}
#'  \item{ucz_prac}{nauka i praca}
#'  \item{tylko_prac}{tylko praca}
#'  \item{bezrob}{zarejestrowany jako bezrobotny}
#'  \item{neet}{brak nauki i brak pracy lub brak informacji o tychże}
#' }
#' Bezrobotni kontynuujący naukę niestacjonarnie nie są wykluczani z kategorii
#' „Nauka”, więc określając ich status w danym miesiącu traktujemy ich jako
#' osoby uczące się. W sytuacji gdy w tym samym miesiącu absolwent miał różne
#' statusy, przypisujemy mu „bardziej aktywny” z tych statusów (np. „naukę” a
#' nie „bezrobocie”).
#' Funkcja domyślnie zwraca skrócone nazwy statusów, ale można je zmienić na
#' pełne, zatwierdzone przez DSKKZ, za pomocą argumentu \code{etykiety}.
#' @param x zbiór osobo-miesięcy (zwykle ramka danych pośrednich P3)
#' @param etykiety wartość logiczna opisująca czy etykiety statusów mają być w
#' skróconej (FALSE) czy pełnej (TRUE) formie
#' @return vector
#' @importFrom dplyr case_when %>%
#' @importFrom tibble is_tibble
#' @export
ind_status_S3 = function(x, etykiety = FALSE) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            c("nauka2", "nauka_szk_abs", "praca", "status_nieustalony", "bezrobocie") %in% names(x))

  status = case_when(
    (x$nauka2 == 1 | x$nauka_szk_abs == 1) & (x$praca == 0 | x$status_nieustalony == 1) ~ "tylko_ucz",
    (x$nauka2 == 1 | x$nauka_szk_abs == 1) & x$praca > 0 ~ "ucz_prac",
    (x$nauka2 == 0 & x$nauka_szk_abs == 0) & x$praca > 0 ~ "tylko_prac",
    x$bezrobocie == 1 & x$nauka2 == 0 & x$nauka_szk_abs == 0 & (x$praca == 0 | x$status_nieustalony == 1) ~ "bezrob",
    (x$bezrobocie == 0 | x$status_nieustalony == 1 | x$praca == 0) & x$nauka2 == 0 & x$nauka_szk_abs == 0 ~ "neet")

  if (etykiety) {
    status = case_when(
      status == "tylko_ucz" ~ "Tylko nauka",
      status == "ucz_prac" ~ "Nauka i praca",
      status == "tylko_prac" ~ "Tylko praca",
      status == "bezrob" ~ "Bezrobocie",
      status == "neet" ~ "Brak danych o aktywności",
      TRUE ~ status)
  }

  return(status)
}
#' @title Wskaźniki indywidualne na potrzeby monitoringu karier - dane
#' administracyjne
#' @description Funkcja obliczająca formę wykonywanej pracy na podstawie
#' zmiennej `praca` z tabeli pośredniej `P3`.
#' Wyróżnione formy pracy:
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
#' @param x zbiór osobo-miesięcy (zwykle ramka danych pośrednich P3)
#' @return vector
#' @importFrom dplyr case_when %>%
#' @export
ind_Z8_formy_prac = function(x) {
  stopifnot(is.data.frame(x),
            "praca" %in% names(x))

  case_when(x$praca == 1 ~ "uop",
            x$praca == 2 ~ "samoz",
            x$praca == 3 ~ "inna",
            x$praca %in% c(4:7) ~ "wiecej",
            TRUE ~ NA_character_) %>%
    return()
}
