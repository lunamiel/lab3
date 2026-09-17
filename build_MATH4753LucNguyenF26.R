# build_MATH4753LucNguyenF26.R
#
# Run this from RStudio with your working directory set to the folder
# that contains SPRUCE.csv (Session > Set Working Directory > To Source
# File Location, or just make sure getwd() shows your lab3 folder).
#
# This builds the MATH4753LucNguyenF26 package FROM SCRATCH on your own
# machine and installs it, no downloaded .tar.gz required (that's what
# was getting corrupted).

if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2")
if (!requireNamespace("testthat", quietly = TRUE)) install.packages("testthat")

pkg <- "MATH4753LucNguyenF26"
unlink(pkg, recursive = TRUE)
dir.create(file.path(pkg, "R"), recursive = TRUE)
dir.create(file.path(pkg, "data"), recursive = TRUE)
dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE)

# ---- DESCRIPTION ----
writeLines(c(
'Package: MATH4753LucNguyenF26',
'Title: Helper Functions for MATH 4753 Labs',
'Version: 0.0.0.9000',
'Authors@R: ',
'    person("Lucia", "Nguyen", email = "lucia.nguyen@ou.edu", role = c("aut", "cre"))',
'Description: A small collection of helper functions written for MATH 4753',
'    at the University of Oklahoma, starting with a reusable sum of squares',
'    decomposition plot for simple linear regression models.',
'License: MIT + file LICENSE',
'Encoding: UTF-8',
'Roxygen: list(markdown = TRUE)',
'RoxygenNote: 7.3.1',
'Depends: ',
'    R (>= 2.10)',
'LazyData: true'
), file.path(pkg, "DESCRIPTION"))

# ---- LICENSE ----
writeLines(c(
"YEAR: 2026",
"COPYRIGHT HOLDER: Lucia Nguyen"
), file.path(pkg, "LICENSE"))

# ---- R/ssDecomp.R ----
writeLines('#\' Four panel sum of squares decomposition for simple linear regression
#\'
#\' Fits a simple linear regression from a data frame and formula and
#\' reproduces the classic decomposition of TSS into MSS and RSS as a
#\' 2 by 2 base R plot (fitted line, residual deviations, model
#\' deviations, total deviations) in one function call. The lab version
#\' of this task requires four nearly identical hand written plotting
#\' blocks per data set; this function generalizes that pattern to any
#\' simple linear model, prints a numeric check that TSS = MSS + RSS,
#\' and returns the sums of squares, R squared, and the fitted model so
#\' they can be reused instead of retyped.
#\'
#\' @param data A data frame containing the response and predictor.
#\' @param formula A model formula with one numeric predictor, e.g.
#\'   \\code{Height ~ BHDiameter}.
#\' @param col_mss Colour used for the model sum of squares segments.
#\'   Defaults to \\code{"Red"}.
#\' @param col_tss Colour used for the total sum of squares segments.
#\'   Defaults to \\code{"Green"}.
#\' @param pt_bg Fill colour for the plotted points. Defaults to \\code{"Blue"}.
#\'
#\' @return Invisibly, a named list with elements \\code{model} (the fitted
#\'   \\code{lm} object), \\code{RSS}, \\code{MSS}, \\code{TSS}, and
#\'   \\code{Rsquared}. Also produces a 2 by 2 base R plot as a side effect.
#\'
#\' @examples
#\' ssDecomp(data = spruce.df, formula = Height ~ BHDiameter)
#\'
#\' @importFrom stats model.frame fitted lm
#\' @importFrom graphics par layout plot abline segments
#\' @export
ssDecomp <- function(data, formula, col_mss = "Red", col_tss = "Green", pt_bg = "Blue") {
  # fit the model here so the caller only has to pass data + formula,
  # matching the data = ..., etc call style used throughout this lab
  model <- lm(formula, data = data)

  # pull the response and the single predictor straight out of the model
  mf <- model.frame(model)
  y <- mf[[1]]
  x <- mf[[2]]
  xname <- names(mf)[2]
  yname <- names(mf)[1]

  yhat <- fitted(model)
  ybar <- mean(y)

  # the three sums of squares and R squared, computed once and reused
  # across all four panels instead of recomputed per plot
  RSS <- sum((y - yhat) ^ 2)
  MSS <- sum((yhat - ybar) ^ 2)
  TSS <- sum((y - ybar) ^ 2)
  Rsquared <- MSS / TSS

  # restore the caller\'s plotting parameters when this function exits
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))

  ylim <- c(0, 1.1 * max(y))
  xlim <- c(0, 1.1 * max(x))

  # panel 1: scatter plot with the fitted least squares line
  plot(x, y, pch = 21, bg = pt_bg, xlab = xname, ylab = yname,
       xlim = xlim, ylim = ylim, main = "Fitted line")
  abline(model)

  # panel 2: residual deviations, y minus yhat (RSS)
  plot(x, y, pch = 21, bg = pt_bg, xlab = xname, ylab = yname,
       xlim = xlim, ylim = ylim, main = "RSS")
  abline(model)
  segments(x, y, x, yhat)

  # panel 3: model deviations, yhat minus ybar (MSS)
  plot(x, y, pch = 21, bg = pt_bg, xlab = xname, ylab = yname,
       xlim = xlim, ylim = ylim, main = "MSS")
  abline(model)
  abline(h = ybar)
  segments(x, ybar, x, yhat, col = col_mss)

  # panel 4: total deviations, y minus ybar (TSS)
  plot(x, y, pch = 21, bg = pt_bg, xlab = xname, ylab = yname,
       xlim = xlim, ylim = ylim, main = "TSS")
  abline(h = ybar)
  segments(x, y, x, ybar, col = col_tss)

  # print a quick numeric check that TSS = MSS + RSS, this is the
  # "did I do the algebra right" step the lab asks for by hand
  cat(sprintf(
    "RSS = %.4f\\nMSS = %.4f\\nTSS = %.4f\\nR-squared (MSS/TSS) = %.4f\\nTSS - (MSS + RSS) = %.6f\\n",
    RSS, MSS, TSS, Rsquared, TSS - (MSS + RSS)
  ))

  invisible(list(model = model, RSS = RSS, MSS = MSS, TSS = TSS, Rsquared = Rsquared))
}', file.path(pkg, "R", "ssDecomp.R"))

# ---- R/data.R ----
writeLines('#\' Spruce tree height and diameter data
#\'
#\' Height and breast height diameter measurements for 36 spruce trees,
#\' as used in MATH 4753 Lab 3 (see MS 10.52).
#\'
#\' @format A data frame with 36 rows and 2 variables:
#\' \\describe{
#\'   \\item{BHDiameter}{Breast height diameter, in cm.}
#\'   \\item{Height}{Tree height, in m.}
#\' }
#\' @source MATH 4753, University of Oklahoma, Lab 3.
"spruce.df"', file.path(pkg, "R", "data.R"))

# ---- data/spruce.df.rda ----
spruce.df <- read.csv("SPRUCE.csv")
save(spruce.df, file = file.path(pkg, "data", "spruce.df.rda"))

# ---- tests ----
writeLines('library(testthat)
library(MATH4753LucNguyenF26)

test_check("MATH4753LucNguyenF26")', file.path(pkg, "tests", "testthat.R"))

writeLines('test_that("ssDecomp returns TSS = MSS + RSS", {
  result <- ssDecomp(data = spruce.df, formula = Height ~ BHDiameter)
  expect_equal(result$TSS, result$MSS + result$RSS, tolerance = 1e-8)
  expect_equal(result$Rsquared, summary(result$model)$r.squared, tolerance = 1e-8)
})

test_that("ssDecomp errors on non-data.frame input", {
  expect_error(ssDecomp(data = 1:10, formula = y ~ x))
})', file.path(pkg, "tests", "testthat", "test-ssDecomp.R"))

# ---- document (builds NAMESPACE + man pages) ----
roxygen2::roxygenise(pkg)

# ---- install straight from the local source folder, no tarball needed ----
devtools::install(pkg, upgrade = FALSE)

cat("\nDone. Now run: library(MATH4753LucNguyenF26)\n")

