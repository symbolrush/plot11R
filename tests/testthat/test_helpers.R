test_that("fhsblue() works", {
  expect_equal(fhsblue(), "#006699")
})

test_that("cpDtService() works", {
  expect_equal(cpDtService(90), 'green')
  expect_equal(cpDtService(80), 'yellow')
  expect_equal(cpDtService(70), 'orange')
  expect_equal(cpDtService(60), 'red')
  expect_equal(cpDtService(NA), 'blue')
})

test_that("filterPrio12() works", {
  simEvents <- data.frame(
    id = c(1,2,3,4),
    priority = c(3,1,2,4)
  )
  expect_equal(nrow(filterPrio12(simEvents)), 2)
})
