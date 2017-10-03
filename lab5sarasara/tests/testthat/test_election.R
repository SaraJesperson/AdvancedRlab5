context("election")


test_that("Collects the correct files", {
  expect_output(str(election("Riksdagsval","Kommun")[[2]]), "290 obs.")
  expect_output(str(election("Landstingsval","Kommun")[[2]]), "526 variables")
  expect_output(str(election("Kommunval","Kommun")[[2]]), "14 15 17 20")
})



test_that("error with faulty input", {
  expect_error(election("kmnval","Kommun"))
  expect_error(election(election=c("Riksdagsval","Landstingsval"),type="Valdistrikt"))
})
