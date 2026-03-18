#' @title Sprawdzanie poprawności oczekiwanych wartości zmiennych tabel
#' pośrednich
#' @description 
#' @param df obiekt-tabela pośrednia
#' @param tabela_p rodzaj tabeli pośredniej
#' @param force_zgodnosc_nazw wartość logiczna określająca czy nazwa obiektu
#' przekazana w argumencie `df` wygląda na zgodną z rodzajem tabeli pośredniej
#' np. jeśli w tym argumencie wpisano wartość "p2", to obiekt przekazany do
#' argumentu `df` powinien mieć w nazwie cyfrę 2 np. "p2" lub "tab_p2", etc.
#' @return logical
#' @export
skip_lokalne_dane <- function() {
  wymagane_tabele <- c("p1", "p2", "p3", "p4", "p5", "p6")
  braki <- wymagane_tabele[!sapply(wymagane_tabele, exists, envir = .GlobalEnv, inherits = TRUE)]
  
  if (length(braki) > 0) {
    skip(paste("Brak tabel pośrednich:", paste(braki, collapse = ", ")))
  }
}
