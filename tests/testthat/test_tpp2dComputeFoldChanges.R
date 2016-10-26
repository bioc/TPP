load(system.file("example_data/2D_example_data/panobinostat_2DTPP_smallExample.RData", package="TPP"))
load(system.file("example_data/2D_example_data/referenceReadInData.RData", package="TPP"))
load(system.file("example_data/2D_example_data/referenceFcData.RData", package="TPP"))

test_that(desc="evalCompFChead", code={
  foldChanges <- tpp2dComputeFoldChanges(configTable = panobinostat_2DTPP_config, 
                                         data = headData2d, 
                                         fcStr = "rel_fc_protein_",
                                         intensityStr = "sumionarea_protein_")  
  expect_identical(foldChanges, headData2dFc)
})

test_that(desc="evalCompFCtail", code={
  foldChanges <- tpp2dComputeFoldChanges(configTable = panobinostat_2DTPP_config, 
                                         data = tailData2d, 
                                         fcStr = "rel_fc_protein_",
                                         intensityStr = "sumionarea_protein_")  
  expect_identical(tail(foldChanges), tailData2dFc)
})

test_that(desc="evalCompFCErr", code={
  expect_error(tpp2dComputeFoldChanges(configTable = panobinostat_2DTPP_config, 
                                       data = headData2d, 
                                       intensityStr = NULL))  
})
