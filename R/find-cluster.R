#' Find clusters in one-partite graph
#' @description This function will extract the clusters in one projection of the
#'   bipartite graph of the given incidence matrix.
#'
#' @details  This function will perform optional pre-processing on the input
#'   incidence matrix, such as normalization. Then use the matrix to perform
#'   bipartite graph projection, and perform optional pre-processing in one of
#'   the specified parts, such as removing edges with lower weights, that is,
#'   weak edges. The removal method and threshold selection can also be
#'   specified, and for the remaining You can choose to keep the original weight
#'   or set all of them to 1. For the graphs obtained after processing,
#'   implement some clustering methods in \href{https://igraph.org/r/}{igraph}
#'   to obtain the classification results. In addition, if there is an input of
#'   external features (prior knowledge), the function will also compare the
#'   clustering results obtained with external features regard similarity.
#'
#' @param inc_mat A matrix including valid values and NAs.
#' @param dim An integer, 1 or 2, indicating which one-partite projection should
#'   be used. Default is 1
#' @param method A string array indicating the clustering methods. Defalut is
#'   "all" which means all clutering methods in this function will be used,
#'   other options are conbinations of "walktrap", "multi level", "infomap",
#'   "label propagation", "leading eigenvector", "spinglass", "fast greedy".
#' @param normalization A logical, whether to normalize the weights. Default is
#'   TRUE.
#' @param rm_weak_edges A logical, whether to remove the weak edges. Default is
#'   TRUE.
#' @param rm_method A string indicating the weak edges removing method, if
#'   'rm_weak_edges' is False, then this argument will be ignored. Default is
#'   'delete', which means delete weak edges from graph, other option is
#'   'as_zero', set the weak edges' weights to 0.
#' @param threshold A string indicating the weak edges threshold selection
#'   method, if 'rm_weak_edges' is False, then this argument will be ignored..
#'   Default is 'median', other option is 'keep_connected', removing edges in
#'   ascending order of weight until the last one that keep the graph connected.
#' @param set_remaining_to_1 A logical, whether to set the remaining edges'
#'   weight to 1. Default is TRUE.
#' @param extra_feature A dataframe has only one column indicating the
#'   membership of each nodes (rownames).
#' @param comparison A logical, whether to compare different clustering methods'
#'   result. Default is TRUE.
#'
#' @return A list containing the clustering results.
#'
#' @import igraph
#' @importFrom  stats median
#' @importFrom  tibble rownames_to_column
#' @importFrom  tidyr pivot_longer
#' @import tidytext
#' @export
#'
#' @examples
#' # generate a incidence matrix
#' data <- matrix(c(1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1), nrow = 3)
#' colnames(data) <- letters[1:5]
#' rownames(data) <- LETTERS[1:3]
#'
#' # run findCluster() to do clustering
#' cls <- findCluster(
#'   data,
#'   dim = 1,
#'   method = "all",
#'   normalization = FALSE,
#'   rm_weak_edges = TRUE,
#'   comparison = FALSE
#' )
findCluster <- function(inc_mat,
                        dim = 1,
                        method = "all",
                        normalization = TRUE,
                        rm_weak_edges = TRUE,
                        rm_method = "delete",
                        threshold = "median",
                        set_remaining_to_1 = TRUE,
                        extra_feature = NULL,
                        comparison = TRUE) {
  # Normalization
  if (normalization) {
    inc_mat <- normalize(inc_mat)
  }

  # Projection
  projection_g <- projectGraph(inc_mat, dim = dim)

  # Remove weak edges
  if (rm_weak_edges) {
    if (threshold == "keep_connected") {
      projection_g <- remove_weak_edges_connected(g = projection_g, rm_method = rm_method)
    } else if (threshold == "median") {
      projection_g <- remove_weak_edges_median(g = projection_g, rm_method = rm_method)
    }
  }

  if (set_remaining_to_1) {
    igraph::E(projection_g)$weight[which(igraph::E(projection_g)$weight > 0)] <- 1
  }

  clustering_methods <- c(
    "walktrap",
    "multi level",
    "infomap",
    "label propagation",
    "leading eigenvector",
    "spinglass",
    "fast greedy",
    "all"
  )

  # add a argument for user to choose the threshhold().
  if (FALSE %in% (method %in% clustering_methods)) {
    stop("Please specify the clustering method.")
  }

  result <- list()

  result$graph <- projection_g
  if ("walktrap" %in% method || "all" %in% method) {
    walktrap_cl <- igraph::cluster_walktrap(graph = projection_g)
    result$walktrap <- walktrap_cl
  }

  if ("multi level" %in% method || "all" %in% method) {
    louvain_cl <- igraph::cluster_louvain(graph = projection_g)
    result$louvain <- louvain_cl
  }

  if ("infomap" %in% method || "all" %in% method) {
    infomap_cl <- igraph::cluster_infomap(graph = projection_g)
    result$infomap <- infomap_cl
  }

  if ("label propagation" %in% method || "all" %in% method) {
    label_prop_cl <- igraph::cluster_label_prop(graph = projection_g)
    result$label_prop <- label_prop_cl
  }

  if ("leading eigenvector" %in% method || "all" %in% method) {
    leading_eigen_cl <- igraph::cluster_leading_eigen(graph = projection_g)
    result$leading_eigen <- leading_eigen_cl
  }

  if ("spinglass" %in% method || "all" %in% method) {
    if (igraph::is_connected(projection_g)) {
      spinglass_cl <- igraph::cluster_spinglass(graph = projection_g)
      result$spinglass <- spinglass_cl
    } else {
      warning("cluster_spinglass cannot work with unconnected graph")
    }
  }

  if ("fast greedy" %in% method || "all" %in% method) {
    fast_greedy_cl <- igraph::cluster_fast_greedy(graph = projection_g)
    result$fast_greedy <- fast_greedy_cl
  }
  # change weights from strength to distance
  igraph::E(projection_g)$weight <- 1 / igraph::E(projection_g)$weight
  distance_matrix <- igraph::distances(projection_g)
  distance_matrix[distance_matrix == Inf] <- 1 / .Machine$double.eps
  result$distance_matrix <- distance_matrix
  result$clustering_args <- list(
    "normalization" = normalization,
    "rm_weak_edges" = rm_weak_edges,
    "rm_method" = rm_method,
    "threshold" = threshold,
    "set_remaining_to_1" = set_remaining_to_1
  )
  if (comparison) {
    comparison <- compareClusters(clusters = result, extra_feature = extra_feature)
    result$comparison <- comparison
  }
  return(result)
}

normalize <- function(x) {
  x <- as.matrix(x)
  x[which(x > 0)] <- (x[which(x > 0)] / max(x[which(x > 0)]))
  if (TRUE %in% (x < 0)) {
    x[which(x < 0)] <- -(x[which(x < 0)] / min(x[which(x < 0)]))
  }
  return(x)
}

remove_weak_edges_connected <- function(g, rm_method) {
  # get the weight list, set up 2 pivots
  weight_list <- sort(igraph::E(g)$weight)
  left_pivot <- 1
  right_pivot <- length(weight_list)

  # check remove only 1 edge with the minimum weight, the graph is still connected or not
  if (!igraph::is_connected(igraph::delete_edges(g, which(igraph::E(g)$weight <= (weight_list[1]))))) {
    print("Not connected before removing! Use the original graph.")
    return(g)
  }

  # use binary search to find the 'dividing' member
  while (left_pivot < right_pivot - 1) {
    mid <- floor((left_pivot + right_pivot) / 2) # the median (or nearly median) index
    # cut those edges with weights less than or equal to mid
    g_cut_off <- delete_edges(g, which(igraph::E(g)$weight <= (weight_list[mid])))
    if (igraph::is_connected(g_cut_off)) {
      left_pivot <- mid
    } else if (!igraph::is_connected(g_cut_off)) {
      right_pivot <- mid
    }
  }

  # remove all edges with weights lower than right-pivot (two methods)
  g_after_cut <- two_methods_remove_weak_edges(g, bar = weight_list[right_pivot], rm_method = rm_method)
  return(g_after_cut)
}

remove_weak_edges_median <- function(g, rm_method) {
  med <- stats::median(E(g)$weight)
  g_after_cut <- two_methods_remove_weak_edges(g, bar = med, rm_method = rm_method)
  return(g_after_cut)
}

two_methods_remove_weak_edges <- function(g, bar, rm_method) {
  if (rm_method == "delete") {
    # delete edges
    g_after_cut <- igraph::delete_edges(g, which(igraph::E(g)$weight < bar))
  } else if (rm_method == "as_zero") {
    # set to zero (epsilon)
    g_after_cut <- igraph::E(g)$weight[which(igraph::E(g)$weight < bar)] <- .Machine$double.eps
  }

  return(g_after_cut)
}

compareClusters <- function(clusters,
                            extra_feature = NULL,
                            print_indices = T) {
  g <- clusters$graph
  dist_mat <- clusters$distance_matrix
  communities <- base::within(clusters, rm("graph", "distance_matrix", "clustering_args"))

  result <- data.frame()
  for (method in names(communities)) {
    mod <- igraph::modularity(communities[[method]])
    if (mod < 0) {
      mod <- 0
    }
    result[method, "modularity"] <- mod
    score <- scoreCluster(community = communities[[method]], graph = g, distance_matrix = dist_mat)
    result[method, "avg.silwidth"] <- score$fpc_stats$avg.silwidth
    result[method, "coverage"] <- score$coverage
    if (!is.null(extra_feature)) {
      suppressWarnings(validation <- validateCluster(dist_mat = dist_mat, extra_feature = extra_feature, community = communities[[method]]))
      corr_rad <- validation$corrected.rand
      if (corr_rad < 0) {
        corr_rad <- 0
      }
      result[method, "corrected.rand"] <- corr_rad

      jac_simi <- validation$jaccard_similarity
      if (jac_simi < 0) {
        jac_simi <- 0
      }
      result[method, "jaccard_similarity"] <- jac_simi
    }
  }
  result <- as.data.frame(t(result))
  if (print_indices) {
    print(knitr::kable(result))
  }

  # to avoid devtools::check() notes... meaningless
  indices <- clustering_method <- value <- NULL

  result_without_rownames <- tibble::rownames_to_column(result, "indices")
  print_df <- tidyr::pivot_longer(result_without_rownames, !indices, names_to = "clustering_method", values_to = "value")
  fig <- ggplot(print_df, aes(tidytext::reorder_within(clustering_method, value, indices), value, label = round(value, 2), fill = clustering_method)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    geom_text(nudge_x = 0.1) +
    tidytext::scale_x_reordered() +
    facet_wrap(~indices, scales = "free_y") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  print(fig)
  return(result)
}
