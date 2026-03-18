skip_lokalne_dane()

test_that("Czy funkcja zwraca błąd dla źle podanego typu tabeli pośredniej", {
  expect_error(is_wymagane_kolumny(p4, "p8"), "Argument `tabela_p` ma nieprawidłową wartość.")

})

test_that("Czy funkcja działa poprawnie dla poprawnych nazw zmiennych w tabelach pośrednich", {
  expect_true(is_wymagane_kolumny(p1, "p1"))
  expect_true(is_wymagane_kolumny(p2, "p2"))
  expect_true(is_wymagane_kolumny(p3, "p3"))
  expect_true(is_wymagane_kolumny(p4, "p4"))
  expect_true(is_wymagane_kolumny(p5, "p5"))
  expect_true(is_wymagane_kolumny(p6, "p6"))
})

test_that("Czy argument `force_zgodnosc_nazw` działa w pożądany sposób", {
  tabela_p4 <- p4
  expect_true(is_wymagane_kolumny(tabela_p4, "p4", force_zgodnosc_nazw = FALSE))
  expect_true(is_wymagane_kolumny(tabela_p4, "p4", force_zgodnosc_nazw = TRUE))
  rm(tabela_p4)

  tabela_cztery <- p4
  tabela_4 <- p4
  expect_true(is_wymagane_kolumny(tabela_cztery, "p4", force_zgodnosc_nazw = FALSE))
  expect_error(
    is_wymagane_kolumny(tabela_cztery, "p4", force_zgodnosc_nazw = TRUE),
    "^Błąd: Nazwa przekazanej tabeli to:"
  )
  expect_true(is_wymagane_kolumny(tabela_4, "p4", force_zgodnosc_nazw = FALSE))
  expect_error(
    is_wymagane_kolumny(tabela_4, "p4", force_zgodnosc_nazw = TRUE),
    "^Błąd: Nazwa przekazanej tabeli to:"
  )
  rm(list = c("tabela_cztery", "tabela_4"))
})

test_that("Czy funkcja działa dla niepoprawnych nazw kolumn", {
  tabela_p4 <- p4
  colnames(tabela_p4)[9:11] <- c("Typ_Szkoly", "czy_szkola_specjalna", "typ_szk_czy_mlodociany_pracownik") 
  expect_warning(
    expect_false(is_wymagane_kolumny(tabela_p4, "p4")),
    "^Przekazana tabela nie ma wszystkich wymaganych kolumn. Dla tabeli p4 brakuje następujących kolumn: typ_szk, szk_specjalna, typ_szk_mlodoc"
  )
})