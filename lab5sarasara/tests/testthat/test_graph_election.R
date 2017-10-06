context("graph_election")


test_that("Correct labels", {
  expect_identical(graph_election("Lund", "Riksdagsval")$labels$title, "Election results 2014 in municipality Lund")
  expect_identical(graph_election("Lund", "Landstingsval")$labels$subtitle, "Landstingsval")
  expect_identical(graph_election("Ystad", "Landstingsval")$labels$y, "Percent")
})

test_that("Data is correct", {
  expect_identical(as.character(graph_election("Robertsfors", "Kommunval")$data[1:4,1]), 
                   c("Moderaterna", "Centerpartiet", "Folkpartiet", "Kristedemokraterna"))
  expect_identical(graph_election("Robertsfors", "Riksdagsval")$data[2:5,2], c(16.77, 2.88, 4.59, 42.31))
})

test_that("grap_election stops with error", {
  expect_error(graph_election("Sara", "Riksdagsval"))
  expect_error(graph_elction(c("Mora","Orsa"), "Riksdagsval"))
})