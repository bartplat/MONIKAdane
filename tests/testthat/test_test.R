test_that("Czy tabele pośrednie ładują się z zewnętrznej lokalizacji", {
  test_data <- local_test_data()
  skip_if(is.null(test_data), "Nie udało się załadować danych z zewnętrznej lokalizacji.")
  
  # result <- your_package_function(test_data$dataset1)
  # expect_equal(result$some_value, expected_value)
  
  # # Now you can safely use the data
  # result <- your_package_function(test_data$dataset1)
  # expect_equal(result$some_value, expected_value)
})
