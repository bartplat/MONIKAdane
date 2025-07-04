% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wskazniki_zagregowane.R
\name{zlacz_partial}
\alias{zlacz_partial}
\title{Łączenie częściowych zbiorów wskaźników zagregowanych}
\usage{
zlacz_partial(sciezka_partial, sciezka_zapisu, rodzaj_wsk)
}
\arguments{
\item{sciezka_partial}{ścieżka w formacie tekstowym, w ktorej znajdują się
częściowe zbiory danych}

\item{sciezka_zapisu}{ścieżka do folderu w formacie tekstowym, w którym ma
być zapisany połączony zbiór}

\item{rodzaj_wsk}{wektor tekstowy określający dla jakiego rodzaju wskaźników
należy przeprowadzić łącznie zbiorów. Możliwe wartości to \code{"szk_god"} lub
\code{"szkozaw"}. Dla poozstałych wartości funkcja nie zwraca nic lub zwraca błąd
(jeżeli wartość jest spoza zbioru sensownych wartości).}
}
\value{
zapis wskaźników zagregowanych
}
\description{
Funkcja służy do scalenia częściowych zbiorów, które powstają podczas pracy
skryptów \code{.bat} (zwracanych przez funkcję \code{\link[=skrypt_bat]{skrypt_bat()}}). Zbiory są łączone,
a czasem dodawane są dodatkowe zmienne.
}
