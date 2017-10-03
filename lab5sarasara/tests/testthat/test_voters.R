context("voters")


test_that("Collects the correct files", {
  expect_output(str(voters(election="Riksdagsval")[[2]][6:8,5]), "1801306 1801307 1801308")
})




test_that("error with faulty input", {
  expect_error(voters("kmnval"))
  expect_error(voters(c("Riksdagsval","Landstingsval")))
})

