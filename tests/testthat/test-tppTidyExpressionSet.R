test_that(".tppTidyExpressionSet converts an ExpressionSet to long format", {
  expressionData <- matrix(
    c(1, 2, 3, 4),
    nrow=2,
    dimnames=list(
      c("protein_a", "protein_b"),
      c("sample_1", "sample_2")
    )
  )
  eset <- Biobase::ExpressionSet(assayData=expressionData)
  
  result <- .tppTidyExpressionSet(eset)
  
  expect_named(result, c("gene", "sample", "value"))
  expect_equal(nrow(result), 4)
  expect_equal(
    result,
    tibble::tibble(
      gene=c(
        "protein_a", "protein_a", "protein_b", "protein_b"
      ),
      sample=c("sample_1", "sample_2", "sample_1", "sample_2"),
      value=c(1, 3, 2, 4)
    )
  )
})

test_that(".tppTidyExpressionSet validates its input", {
  expect_error(
    .tppTidyExpressionSet(data.frame(value=1)),
    "must be an ExpressionSet"
  )
})