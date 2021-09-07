projectGraph <- function(inc_mat, dim = 0, method = "rm_after_pro") {
  # ProjectGraph
  #
  # Args:
  #   inc_mat: incidence matrix.
  #   dim: 0:both 1:proj1 2:proj2
  # Returns:
  #

  if (!method %in% c("rm_before_pro", "rm_after_pro")) {
    stop("Please input the correct projection method: \"rm_before_pro\" or \"rm_after_pro\"")
  }
  inc_mat <- as.matrix(inc_mat)
  inc_mat[is.na(inc_mat)] <- 0

  ## method1 remove all edge with weight <0
  if (method == "rm_before_pro") {
    inc_mat[inc_mat < 0] <- 0
  }

  if (dim == 0) {
    am1 <- t(inc_mat) %*% inc_mat
    am2 <- inc_mat %*% t(inc_mat)

    if (method == "rm_after_pro") {
      am1[am1 < 0] <- 0
      am2[am2 < 0] <- 0
    }

    proj1 <- igraph::graph_from_adjacency_matrix(am1, mode = "undirected", weighted = TRUE, diag = FALSE)
    proj2 <- igraph::graph_from_adjacency_matrix(am2, mode = "undirected", weighted = TRUE, diag = FALSE)

    result <- list("proj1" = proj1, "proj2" = proj2)
  } else if (dim == 1) {
    am1 <- t(inc_mat) %*% inc_mat

    if (method == "rm_after_pro") {
      am1[am1 < 0] <- 0
    }

    result <- igraph::graph_from_adjacency_matrix(am1, mode = "undirected", weighted = TRUE, diag = FALSE)
  } else if (dim == 2) {
    am2 <- inc_mat %*% t(inc_mat)

    if (method == "rm_after_pro") {
      am2[am2 < 0] <- 0
    }

    result <- igraph::graph_from_adjacency_matrix(am2, mode = "undirected", weighted = TRUE, diag = FALSE)
  } else {
    stop("Input should be 0,1,2")
  }

  return(result)
}


createBipartiteGrpahWithigraph <- function(inc_mat) {
  # CreateBipartiteGrpahWithigraph
  #
  # Args:
  #   inc_mat
  # Returns:
  #   igraph graph

  # load the prerequisite package
  el <- incMat2el(inc_mat)

  colnames(el)[3] <- "weight"

  g <- igraph::graph.data.frame(el, directed = FALSE)
  igraph::V(g)$type <- igraph::V(g)$name %in% el[, 2] # the second column of edges is TRUE type
  igraph::E(g)$weight <- as.numeric(el[, 3])

  return(g)
}


incMat2el <- function(inc_mat) {
  el <- inc_mat %>%
    as.matrix() %>%
    as.data.frame.table() %>%
    tidyr::drop_na()
  colnames(el)[3] <- "weight"
  return(el)
}
