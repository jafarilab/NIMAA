test_that("weighted edgelist to incidence matrix works", {
  el1 <- data.frame(
       Part1 = c("d", "e", "e", "b", "a", "a"),
       Part2 = c("J", "N", "O", "R", "R", "L")
     )
  el2 <- data.frame(
       Part1 = c("d", "e", "e", "b", "a", "a"),
       Part2 = c("J", "N", "O", "R", "R", "L"),
       Value = c(4, 5, 5, 8, 7, 7)
     )
  inc_mat1 <- el2IncMatrix(el1)
  inc_mat2 <- el2IncMatrix(el2)
  expect_equal(diag(inc_mat1), c(1, 1, NA, 1))
  expect_equal(diag(inc_mat2), c(4, 5, NA, 7))

})
