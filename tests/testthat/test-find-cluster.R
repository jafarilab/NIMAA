test_that("find cluster works", {
  data <- matrix(c(1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0), nrow = 3)
  colnames(data) <- letters[1:5]
  rownames(data) <- LETTERS[1:3]
  cls <- findCluster(
       data,
       part = 1,
       method = "all",
       normalization = FALSE,
       rm_weak_edges = TRUE,
       comparison = TRUE
    )
  expect_equal(names(V(cls$graph)), c("a", "b", "c", "d", "e"))
  expect_equal(E(cls$graph)$weight, rep(1, 4))
})
