test_that("normal_windows_path poprawnie zamienia / na \\", {
  expect_equal(normal_windows_path("folder/podfolder/plik.txt"), "folder\\podfolder\\plik.txt")
  expect_equal(normal_windows_path("plik.txt"), "plik.txt")
  expect_equal(normal_windows_path("folder//plik.txt"), "folder\\\\plik.txt")
})

test_that("normal_windows_path rzuca błąd dla nieprawidłowego argumentu", {
  expect_error(normal_windows_path(123))
  expect_error(normal_windows_path(NA))
  expect_error(normal_windows_path(c("a", "b")))
})
