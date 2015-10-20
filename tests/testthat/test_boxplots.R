test_that("boxDtLaunchByVehicleName() works", {
  load("../../data/simScenario.RData")
  expect_equal(boxDtLaunchByVehicleName(simScenario), "OK")
})
