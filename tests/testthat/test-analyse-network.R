test_that("results has three elements", {
  g1 <- igraph::make_graph(c(1, 2, 3, 4, 1, 3), directed = FALSE)
  igraph::V(g1)$name <- c("n1", "n2", "n3", "n4")
  g2 <- igraph::sample_gnm(10, 23)
  igraph::V(g2)$name <- letters[1:10]
  expect_equal(length(analyseNetwork(g1)), 3)
  expect_equal(length(analyseNetwork(g2)), 3)
})
