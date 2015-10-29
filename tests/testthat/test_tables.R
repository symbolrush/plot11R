test_that("tableDtService() works", {
  load("../../data/simScenario.RData")
  load("../../data/simScenarioRef.RData")
  expect_equal(
    nrow(tableDtService(simScenario, simScenarioRef)), 5047)
})
