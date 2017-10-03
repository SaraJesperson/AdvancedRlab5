context("election")


test_that("Collects the correct files", {
  expect_output(str(election("Riksdagsval","Kommun")[[2]][12:13,24]), "0.49 0.96")
  expect_output(str(election("Landstingsval","Valdistrikt")[[2]][12:13,23]), "1 2")
  expect_output(str(election("Kommunval","Kommun")[[2]][35:37,7]), "365 862 2760")
})



test_that("error with faulty input", {
  expect_error(election("kmnval","Kommun"))
  expect_error(election(election=c("Riksdagsval","Landstingsval"),type="Valdistrikt"))
})
