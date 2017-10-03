context("graph_election")



test_that("grap_election stops with error", {
  expect_error(graph_election("Sara"))
  expect_error(graph_elction(c("Mora","Orsa")))
})