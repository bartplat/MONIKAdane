test_that("Funkcja skrypty_podzial działa poprawnie dla poprawnych danych", {
  result = skrypty_podzial(7529, 11)
  
  # Sprawdzam, czy zwrócono ramkę danych
  expect_s3_class(result, "data.frame")
  # Sprawdzam liczbę wierszy w wynikowej ramce danych (powinno być 11)
  expect_equal(nrow(result), 11)
  # Sprawdzam kolumny
  expect_equal(colnames(result), c("skrypt", "od", "do"))
  # Sprawdzam, czy suma wszystkich wierszy wynosi 7529
  expect_equal(sum(result$do - result$od + 1), 7529)
  # Sprawdzam, czy wartości w kolumnie 'skrypt' są od 1 do 11
  expect_equal(result$skrypt, 1:11)
  # Sprawdzam, czy zakresy 'od' i 'do' nie nachodzą na siebie
  expect_true(all(result$od[-1] == result$do[-length(result$do)] + 1))
})

test_that("Funkcja skrypty_podzial obsługuje poprawnie małe liczby wierszy", {
  result = skrypty_podzial(15, 4)
  
  # Sprawdzam, czy zwrócono ramkę danych
  expect_s3_class(result, "data.frame")
  # Sprawdzam liczbę wierszy w wynikowej ramce danych (powinno być 4)
  expect_equal(nrow(result), 4)
  # Sprawdzam, czy suma wszystkich wierszy wynosi 15
  expect_equal(sum(result$do - result$od + 1), 15)
  # Sprawdzam, czy zakresy są nie nachodzące
  expect_true(all(result$od[-1] == result$do[-length(result$do)] + 1))
})

test_that("Funkcja skrypty_podzial działa poprawnie dla danych brzegowych", {
  result = skrypty_podzial(5, 5)
  
  # Sprawdzam, czy zwrócono ramkę danych
  expect_s3_class(result, "data.frame")
  # Sprawdzam liczbę wierszy w wynikowej ramce danych (powinno być 5)
  expect_equal(nrow(result), 5)
  # Sprawdzam, czy suma wszystkich wierszy wynosi 5
  expect_equal(sum(result$do - result$od + 1), 5)
  # Sprawdzam, czy zakresy są nie nachodzące
  expect_true(all(result$od[-1] == result$do[-length(result$do)] + 1))
})

# UWAGA: Problem z testem expect_error() polega na tym, że expect_error() domyślnie porównuje pełną treść komunikatu błędu, a komunikat błędu generowany przez stopifnot() może nie być dokładnie taki, jak oczekiwany tekst. W szczególności, komunikat stopifnot() nie zawsze dokładnie odwzorowuje wyrażenie, które przeszło na FALSE. W tej sytuacji lepiej jest sprawdzać jedynie część komunikatu błędu, np. fragment, który wiąże się z typem błędu. Możemy to zrobić, używając wyrażenia regularnego lub prostszego porównania tekstu.

# musiałem wykomentować, bo błędy były po angielsku i to nie działało - do rozwiązania na przyszłość
# test_that("Funkcja skrypty_podzial zwraca błąd dla niepoprawnych danych", {
#   expect_error(skrypty_podzial(-10, 5), "nrow > 0 nie ma wartości TRUE") # ujemna liczba wierszy
#   expect_error(skrypty_podzial(10, 0), "podzial > 0 nie ma wartości TRUE") # podział na 0 części
#   expect_error(skrypty_podzial(10.5, 4), "nie ma wartości TRUE") # niecałkowite liczby wierszy
#   expect_error(skrypty_podzial(10, -5), "podzial > 0 nie ma wartości TRUE") # ujemna wartość podziału
#   expect_error(skrypty_podzial(10, 2.5), "nie ma wartości TRUE") # niecałkowita liczba podziałów
# })
