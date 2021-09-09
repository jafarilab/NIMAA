test_that("results has three elements", {
  g <- igraph::make_graph(c(1, 2, 3, 4, 1, 3), directed = F)
  V(g)$name <- c(1, 2, 3, 4)
  expect_equal(length(analyseNetwork(g)), 3)
})
