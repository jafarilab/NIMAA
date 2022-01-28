test_that("weighted edgelist to incidence matrix works", {
  el <- data.frame(
    A = c(1, 2, 3, 4, 5, 6),
    B = c("a", "b", "c", "e", "f", "g"),
    m = c(1, 2, 1, 2, 1, 2)
  )
  inc_mat <- el2IncMatrix(el, print_skim = F)
  expect_equal(diag(inc_mat), c(1, 2, 1, 2, 1, 2))
})
