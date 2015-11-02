# test_that("tableDtService() works", {
#   load("../../data/simScenario.RData")
#   load("../../data/simScenarioRef.RData")
#   expect_equal(
#     nrow(tableDtService(simScenario, simScenarioRef)), 5047)
# })

test_that("tableNrOfMissionsByOrganisationU() works", {
  load("../../data/simScenario.RData")
  load("../../data/simScenarioRef.RData")
  simScenario$missions <- simScenario$missions[1:40,]
  simScenarioRef$missions <- simScenarioRef$missions[1:40,]
#   simScenario$missions <- simScenario$missions
#   simScenarioRef$missions <- simScenarioRef$missions
  table <- tableNrOfMissionsByOrganisation(simScenario,simScenarioRef)
  print(table)
})
