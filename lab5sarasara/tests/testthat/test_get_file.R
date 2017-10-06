context("get_file")


test_that("Selects correct election type", {
  expect_identical(get_file("Riksdagsval")[[1]], "Riksdagsval")
  expect_identical(get_file("Landstingsval")[[1]], "Landstingsval")
  expect_identical(get_file("Kommunval")[[1]], "Kommunval")
})

test_that("Selects correct data", {
  expect_identical(get_file("Riksdagsval")[[2]][26:28,5], c(2278, 907, 502))
  expect_length(get_file("Landstingsval")[[2]][,1], 289)
  expect_equal(get_file("Kommunval")[[2]][104:105,4], c("Lomma", "Svedala"))
})


test_that("get_file stops with argument error", {
  expect_error(get_file(c("Riksdagsval", "Landstingsval")))
  expect_error(get_file("Hello!"))
})

