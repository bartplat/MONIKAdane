#' @title Sprawdzanie poprawności danych
#' @description
#' Funkcja sprawdza czy dana tabela pośrednia posiada kolumny, które są kluczowe
#' dla poprawnego przetwarzania tabel pośrednich oraz do generowania wskaźników
#' zagregowanych.
#' @param data ramka danych, której poprawność nazw kolumn ma być sprawdzona.
#' Zwykle jest to tabela pośrednia w rozumieniu tabel używanych w Monitoringu
#' Karier
#' @param tabela wartość tekstowa oznaczająca która tabela pośrednia ma być
#' sprawdzona
#' @param force wartość logiczna: `FALSE` - funkcja zatrzymuje swoje działanie
#' po napotkaniu błędu, `TRUE` - funkcja kontynuuje działanie mimo napotkania
#' błędu
#' @importFrom tibble is_tibble
#' @export
check_kolumny_tabele <- function(data,
                                 tabela = c("p1", "p2", "p3", "p4", "p5"),
                                 force = FALSE) {
  # check na nazwy zmiennych w poszczególnych tabelach pośrednich
  # na formaty zmiennych itp.
  # check na braki danych??
  stopifnot(
    is.data.frame(data) | is_tibble(data),
    is.character(tabela),
    length(tabela) > 0,
    any(tabela %in% c("p1", "p2", "p3", "p4", "p5")),
    is.logical(force)
  )
  
  # uzupełnić o wymagane kolumny (szczególnie p2:p4)
  nazwy_kolumn = list(
    "p1" = c("id_abs", "rok_abs"),
    "p2" = c("id_abs", "rok_abs", "branza_kont", "dziedzina_kont", "dyscyplina_kont"),
    "p3" = c("id_abs", "rok_abs", "okres", "nauka2", "nauka_szk_abs", "praca", "status_nieustalony", "bezrobocie"),
    "p4" = c("id_abs", "rok_abs", "typ_szk", "plec", ),
    "p5" = c("id_abs", "rok_abs")
  )
  
  cat("\n", format(Sys.time(), "%H:%M:%S"), " - Sprawdzanie poprawności nazw kolumn w tabelach pośrednich.", sep = "")
  for (i in tabela) {
    if (all(nazwy_kolumn[[i]] %in% names(i))) {
      cat("\nTabela ", i, "ma wszystkie wymagane kolumny.", sep = "")
    } else {
      braki = setdiff(nazwy_kolumn[[i]], names(i))
      ifelse(force,
             warning(cat("\nW tabeli ", i, " brakuje następujących kolumn, które są wymagane do poprawnego przetwarzania danych: ",
                         paste(braki, collapse = ", "),
                         ". Ze względu na `force = TRUE` sprawdzanie nie zostanie przerwane.",
                         sep = "")),
             stop(cat("\nW tabeli ", i, " brakuje następujących kolumn, które są wymagane do poprawnego przetwarzania danych: ",
                      paste(braki, collapse = ", "),
                      ".",
                      sep = "")))
    }
  }
}
