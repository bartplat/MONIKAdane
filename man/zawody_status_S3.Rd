% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agr_wskazniki_adm.R
\name{zawody_status_S3}
\alias{zawody_status_S3}
\title{Wskaźniki zagregowane dla monitoringu karier - dane administracyjne}
\usage{
zawody_status_S3(x, rok_od, mies_od, rok_do, mies_do)
}
\arguments{
\item{x}{ramka danych pośrednich P3}

\item{rok_od}{rok początku okresu, dla którego ma być policzony wskaźnik}

\item{mies_od}{miesiąc początku okresu, dla którego ma być policzony wskaźnik}

\item{rok_do}{rok końca okresu, dla którego ma być policzony wskaźnik}

\item{mies_do}{miesiąc końca okresu, dla którego ma być policzony wskaźnik}
}
\value{
list
}
\description{
Funkcja licząca odsetek absolwentów o danym statusie
edukacyjno-zawodowym wśród grup absolwentów wydzielonych ze względu na zawód,
w którym się kształcili w szkole.
}
\details{
Zwracana lista służy jako wsad do tabeli w szkolnym raporcie
automatycznym. Jeśli lista będąca wsadem tabeli generowałaby pustą
tabelę, to zwracana jest lista o \code{n} równym 0 oraz z brakiem danych dla
pozostałych elementów listy. Funkcja wykorzystuje inną funkcję z pakietu,
czyli \code{\link{status_S3_mies}}.
}
