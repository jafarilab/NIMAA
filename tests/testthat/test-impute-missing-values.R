test_that("multiplication works", {
  data <- matrix(c(1, 1, 2, NA, 1, 2, 1, 1, NA, 1, 1, 2, 1, 1, 2), nrow = 3)
  colnames(data) <- letters[1:5]
  rownames(data) <- LETTERS[1:3]
  expect_true(!NA %in% (imputeMissingValue(data, method = c("als"))))
})
