% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ind_wskazniki_adm.R
\name{ind_status_S3}
\alias{ind_status_S3}
\title{Wskaźniki indywidualne na potrzeby monitoringu karier - dane
administracyjne}
\usage{
ind_status_S3(x, etykiety = FALSE)
}
\arguments{
\item{x}{zbiór osobo-miesięcy (zwykle ramka danych pośrednich P3)}

\item{etykiety}{wartość logiczna opisująca czy etykiety statusów mają być w
skróconej (FALSE) czy pełnej (TRUE) formie}
}
\value{
vector
}
\description{
Funkcja obliczająca status edukacyjno-zawodowy (wskaźnik S3) dla
danego osobo-miesiąca (lub osoby).
}
\details{
W tej edycji pojawia się nowy status - bezrobocie rejestrowane,
który został wyodrębniony ze statusu \code{neet}. Wyróżniamy następujące statusy
(bez KUZ i KKZ):
\itemize{
\item{tylko_ucz}{tylko nauka}
\item{ucz_prac}{nauka i praca}
\item{tylko_prac}{tylko praca}
\item{bezrob}{zarejestrowany jako bezrobotny}
\item{neet}{brak nauki i brak pracy lub brak informacji o tychże}
}
Bezrobotni kontynuujący naukę niestacjonarnie nie są wykluczani z kategorii
„Nauka”, więc określając ich status w danym miesiącu traktujemy ich jako
osoby uczące się. W sytuacji gdy w tym samym miesiącu absolwent miał różne
statusy, przypisujemy mu „bardziej aktywny” z tych statusów (np. „naukę” a
nie „bezrobocie”).
Funkcja domyślnie zwraca skrócone nazwy statusów, ale można je zmienić na
pełne, zatwierdzone przez DSKKZ, za pomocą argumentu \code{etykiety}.
}
