context("get_file")


test_that("Collects the correct file", {
 expect_output(str(get_file()[2,4]), "Karlskrona")
#  expect_output(str(get_file(12)), "http://www.val.se/val/val2014/alkon/alkandur_L.skv")
#  expect_output(str(get_file(17)), "http://www.val.se/val/val2014/alkon/L/rostberattigade.skv")
})




test_that("get_file stops with argument error", {
  expect_error(get_file(20))
  expect_error(get_file(c(1,6)))
  expect_error(get_file("Hello!"))
})


