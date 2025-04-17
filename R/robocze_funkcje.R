#' @title Zmiana ukośników w ścieżce na Windowsowe
#' @description
#' Funkcja przyjmuje ścieżkę pliku jako ciąg znaków i zamienia wszystkie ukośniki
#' z formatu Unixowego (`/`) na format Windowsowy (`\\`).#'
#' @param path Ścieżka pliku jako ciąg znaków (string). Musi być niepusty.
#' @return Zwraca tekst z poprawionymi ukośnikami, gotowy do użycia w Windows.
#' @examples
#' normal_windows_path("folder/podfolder/plik.txt")
#' # [1] "folder\\podfolder\\plik.txt"
#' @export
normal_windows_path <- function(path) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("Argument 'path' musi być niepustym ciągiem znaków.")
  }
  gsub("/", "\\\\", path)
}
