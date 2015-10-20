test_that("kpiEventsInTime() works", {
  load("../../data/simScenario.RData")
  expect_equal(kpiEventsInTime(simScenario$events[1:4,]), 75)
  expect_equal(kpiEventsInTime(simScenario$events), 72)
})
