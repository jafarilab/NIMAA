test_that("four shapes works", {
  data <- NIMAA::beatAML
  mat <- NIMAA::el2IncMatrix(data, print_skim = F)
  sub_matrices <- extractSubMatrix(mat, print_skim = F, row.vars = "patient_id", col.vars = "inhibitor")
  expect_equal(length(sub_matrices), 4)
  expect_equal(dim(sub_matrices$Square), c(96, 96))
  expect_equal(dim(sub_matrices$Rectangular_row), c(22, 113))
  expect_equal(dim(sub_matrices$Rectangular_col), c(510, 1))
  expect_equal(dim(sub_matrices$Rectangular_element_max), c(140, 87))
})
