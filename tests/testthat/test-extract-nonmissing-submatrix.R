test_that("four shapes works", {
  beatAML_data <- NIMAA::beatAML[1:10000,]
  beatAML_incidence_matrix <- NIMAA::el2IncMatrix(beatAML_data)
  sub_matrices <- extractSubMatrix(beatAML_incidence_matrix, col.vars = "patient_id", row.vars = "inhibitor")
  expect_equal(length(sub_matrices), 4)
  expect_equal(dim(sub_matrices$Square), c(66, 66))
  expect_equal(dim(sub_matrices$Rectangular_row), c(6, 105))
  expect_equal(dim(sub_matrices$Rectangular_col), c(99, 2))
  expect_equal(dim(sub_matrices$Rectangular_element_max), c(59, 79))
})
