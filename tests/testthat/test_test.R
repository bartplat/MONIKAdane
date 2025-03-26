test_that("Czy tabele pośrednie ładują się z zewnętrznej lokalizacji", {
  test_data <- local_test_data()
  # skip_if(is.null(test_data), message = "Nie udało się załadować danych z zewnętrznej lokalizacji.")
  
  tabele_nazwy = c("p1", "p2", "p3", "p4", "p5")
  
  test_that("Tabele pośrednie zostały załadowane do środowiska testowego", {
    expect_in(ls(), tabele_nazwy)
  })
})
