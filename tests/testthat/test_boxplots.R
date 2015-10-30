test_that("boxDtLaunchByVehicleName() works", {
  load("../../data/simScenario.RData")
  expect_equal(boxDtLaunchByVehicleName(simScenario), "OK")
})

test_that("boxDtLaunchByOrganisation() works", {
  load("../../data/simScenario.RData")
  expect_equal(boxDtLaunchByOrganisation(simScenario), "OK")
})
