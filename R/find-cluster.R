#' Find clusters in projected unipartite networks
#' @description This function looks for the clusters in the projected unipartite networks of the bipartite network (the incidence matrix) that was given to it.
#'
#' @details  This function performs optional preprocessing, such as normalization, on the input incidence matrix (bipartite network). The matrix is then used to perform bipartite network projection and optional preprocessing on one of the projected networks specified, such as removing edges with low weights (weak edges). Additionally, the user can specify the removal method, threshold value, or binarization of the weights. For the networks obtained after processing, this function implements some clustering methods in \href{https://igraph.org/r/}{igraph} such as "walktrap" and "infomap", to detect the communities within the network. Furthermore, if external features (prior knowledge) are provided, the function compares the clustering results obtained with the external features in terms of similarity as an external validation of clustering. Otherwise, several internal validation criteria such as modularity and coverage are only represented to compare the clustering results.
#'
#' @param inc_mat An incidence matrix.
#' @param part An integer, 1 or 2, indicating which unipartite projection should be used. The default is 1.
#' @param method A string array indicating the clustering methods. The defalut is "all", which means all available clustering methods in this function are utilized. Other options are combinations of "walktrap", "multi level", "infomap", "label propagation", "leading eigenvector", "spinglass", and "fast greedy".
#' @param normalization A logical value indicating whether edge weights should be normalized before the computation proceeds. The default is TRUE.
#' @param rm_weak_edges A logical value indicating whether weak edges should be removed before the computation proceeds. The default is TRUE.
#' @param rm_method A string indicating the weak edges removing method. If `rm_weak_edges` is False, then this argument is ignored. The default is `delete`, which means deleting weak edges from the network. The other option is `as_zero`, which sets the weak edges' weights to 0.
#' @param threshold A string indicating the weak edge threshold selection method. If `rm_weak_edges` is False, then this argument is ignored. By default, `median` is used. The other option is `keep_connected`, which prevents the network from being unconnected and removes edges in ascending order of weights.
#' @param set_remaining_to_1 A logical value indicating whether the remaining edges' weight should be set to 1. The default is TRUE.
#' @param extra_feature A data frame object that shows the group membership of each node based on prior knowledge.
#' @param comparison A logical value indicating whether clustering methods should be compared to each other using internal measures of clustering, including modularity, average silluoutte width, and coverage. The default value is TRUE.
#'
#' @return A list containing the igraph object of the projected network, the clustering results of each method on the projected network separately, along with a comparison between them. The applied clustering arguments and the network's distance matrix are also included in this list for potential use in the next steps. In the case of weighted projected networks, the distance matrix is obtained by inverting the edge weights. The comparison of selected clustering methods is also presented as bar plots simultaneously.
#'
#' @import igraph
#' @importFrom  stats median
#' @importFrom  tibble rownames_to_column
#' @importFrom  tidyr pivot_longer
#' @import tidytext
#' @export
#'
#' @examples
#' # generate an incidence matrix
#' data <- matrix(c(1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0), nrow = 3)
#' colnames(data) <- letters[1:5]
#' rownames(data) <- LETTERS[1:3]
#'
#' # run findCluster() to do clustering
#' cls <- findCluster(
#'   data,
#'   part = 1,
#'   method = "all",
#'   normalization = FALSE,
#'   rm_weak_edges = TRUE,
#'   comparison = TRUE
#' )
findCluster <- function(inc_mat,
                        part = 1,
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
  projection_g <- projectGraph(inc_mat, dim = part)

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
    score <- scoreCluster(community = communities[[method]], graph = g, dist_mat = dist_mat)
    result[method, "avg.silwidth"] <- score$fpc_stats$avg.silwidth
    result[method, "coverage"] <- score$coverage
    if (!is.null(extra_feature)) {
      suppressWarnings(validation <- validateCluster(community = communities[[method]], extra_feature = extra_feature, dist_mat = dist_mat))
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
