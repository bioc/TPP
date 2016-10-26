load(system.file("example_data/2D_example_data/panobinostat_2DTPP_smallExample.RData", package="TPP"))
filePath <- system.file("example_data", package="TPP")

test_that(desc="evalConfig", code={
  configTable <- tpp2dEvalConfigTable(panobinostat_2DTPP_config)
  expect_identical(configTable, panobinostat_2DTPP_config)
})

test_that(desc="evalErConfig", code={
  expect_error(tpp2dEvalConfigTable(configTable=NULL))
})