test_that("ssDecomp returns TSS = MSS + RSS", {
  result <- ssDecomp(data = spruce.df, formula = Height ~ BHDiameter)
  expect_equal(result$TSS, result$MSS + result$RSS, tolerance = 1e-8)
  expect_equal(result$Rsquared, summary(result$model)$r.squared, tolerance = 1e-8)
})

test_that("ssDecomp errors on non-data.frame input", {
  expect_error(ssDecomp(data = 1:10, formula = y ~ x))
})
