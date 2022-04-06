test_that("weighted edgelist to incidence matrix works", {
  el1 <- data.frame(
    nominal_var_1 = c("d", "e", "e", "b", "a", "a"),
    nominal_var_2 = c("J", "N", "O", "R", "R", "L")
     )
  el2 <- data.frame(
    nominal_var_1 = c("d", "e", "e", "b", "a", "a"),
    nominal_var_2 = c("J", "N", "O", "R", "R", "L"),
       numeric_val = c(4, 5, 5, 8, 7, 7)
     )
  inc_mat1 <- nominalAsBinet(el1)
  inc_mat2 <- nominalAsBinet(el2)
  expect_equal(diag(inc_mat1), c(1, 1, NA, 1))
  expect_equal(diag(inc_mat2), c(4, 5, NA, 7))

})
