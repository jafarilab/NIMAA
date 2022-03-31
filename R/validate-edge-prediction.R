#' Validate and compare edge prediction methods.
#'
#' @description This function compares the imputation approaches for predicting edges using the clustering result of a submatrix with non-missing values as a benchmark.  This function performs the same analysis as the `findCluster` function on every imputed incidence matrices independently. Then, using different similarity measures, all imputation approaches are compared to each other, revealing how edge prediction methods affects network communities (clusters). The best method should result in a higher degree of similarity (common node membership) to the non-missing submatrix as a benchmark.
#'
#' @param imputation A list or a matrix containing the results of imputation method (s).
#' @param refer_community An igraph community object obtained through \code{\link{findCluster}} using a given method.
#' @param clustering_args A list indicating the clustering arguments values used in \code{\link{findCluster}} for a given method. This list is retrievable from the output of \code{\link{findCluster}}.
#'
#' @return A list containing the following indices: Jaccard similarity, Dice similarity coefficient, Rand index, Minkowski (inversed), and Fowlkes-Mallows index. The higher value indicates the greater similarity between the imputed dataset and the benchmark.
#'
#' @importFrom  tibble rownames_to_column
#' @importFrom  tidyr pivot_longer
#' @import tidytext
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:10000,]
#'
#' # convert to incidence matrix
#' beatAML_incidence_matrix <- el2IncMatrix(beatAML_data)
#'
#' # do clustering
#' cls <- findCluster(beatAML_incidence_matrix, part = 1)
#'
#' # predict the edges by imputation the wights
#' imputed_beatAML <- predictEdge(beatAML_incidence_matrix)
#'
#' # validate the edge prediction
#' validateEdgePrediction(imputation = imputed_beatAML,
#' refer_community = cls$fast_greedy,
#' clustering_args = cls$clustering_args
#' )
validateEdgePrediction <- function(imputation,
                               refer_community,
                               clustering_args) {
  if (is.matrix(imputation)) {
    imputation <- list("imputation" = imputation)
  }

  comparison <- data.frame(row.names = names(imputation))
  for (method_name in names(imputation)) {
    sub_inc_mat <- imputation[[method_name]][, refer_community$names]
    cluster2 <- findCluster(
      inc_mat = sub_inc_mat,
      part = 1,
      method = refer_community$algorithm,
      normalization = clustering_args$normalization,
      rm_weak_edges = clustering_args$rm_weak_edges,
      rm_method = clustering_args$rm_method,
      threshold = clustering_args$threshold,
      set_remaining_to_1 = clustering_args$set_remaining_to_1,
      comparison = F
    )
    cluster2 <- base::within(cluster2, rm("graph", "distance_matrix", "clustering_args"))[[1]]
    indices <- calculateIndices(cluster2, refer_community)
    comparison[method_name, "Jaccard_similarity"] <- indices$Jaccard_similarity
    comparison[method_name, "Dice_similarity_coefficient"] <- indices$Dice_similarity_coefficient
    comparison[method_name, "Rand_index"] <- indices$Rand_index
    comparison[method_name, "Minkowski (inversed)"] <- indices$Minkowski_inversed
    comparison[method_name, "Fowlkes_Mallows_index"] <- indices$Fowlkes_Mallows_index
  }
  print(knitr::kable(comparison))

  # to avoid devtools::check() notes... meaningless
  imputation_method <- value <- index <- NULL

  comparison <- tibble::rownames_to_column(comparison, "imputation_method")
  print_df <- tidyr::pivot_longer(comparison, !imputation_method, names_to = "index", values_to = "value")
  fig <- ggplot(print_df, aes(tidytext::reorder_within(imputation_method, value, index), value, label = round(value, 2), fill = imputation_method)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    geom_text(nudge_y = -0.1) +
    tidytext::scale_x_reordered() +
    facet_wrap(~index, scales = "free") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())
  print(fig)

  return(comparison)
}

calculateIndices <- function(community1, community2) {
  community_member_vector <- community1$membership
  community_membership_mat <- matrix(nrow = community1$vcount, ncol = community1$vcount)
  for (i in 1:(community1$vcount - 1)) {
    for (j in (i + 1):community1$vcount) {
      community_membership_mat[i, j] <- ifelse(community_member_vector[i] == community_member_vector[j], 1, 0)
    }
  }
  community_member_vector2 <- community2$membership
  community_membership_mat2 <- matrix(nrow = community2$vcount, ncol = community2$vcount)
  for (i in 1:(community2$vcount - 1)) {
    for (j in (i + 1):community2$vcount) {
      community_membership_mat2[i, j] <- ifelse(community_member_vector2[i] == community_member_vector2[j], 1, 0)
    }
  }

  j_table <- table(paste0(as.vector(community_membership_mat), as.vector(community_membership_mat2)))
  if (is.na(j_table["00"])) {
    j_table["00"] <- 0
  }

  if (is.na(j_table["01"])) {
    j_table["01"] <- 0
  }

  if (is.na(j_table["10"])) {
    j_table["10"] <- 0
  }

  if (is.na(j_table["11"])) {
    j_table["11"] <- 0
  }
  result <- list()
  result$Jaccard_similarity <- as.numeric(j_table["11"] / (j_table["10"] + j_table["01"] + j_table["11"]))
  result$Dice_similarity_coefficient <- as.numeric((2 * j_table["11"]) / (2 * j_table["11"] + j_table["01"] + j_table["10"]))
  result$Rand_index <- as.numeric((j_table["11"] + j_table["00"]) / (j_table["11"] + j_table["01"] + j_table["10"] + j_table["00"]))
  result$Minkowski_inversed <- as.numeric(sqrt((j_table["01"] + j_table["11"]) / (j_table["01"] + j_table["10"])))
  result$Fowlkes_Mallows_index <- as.numeric(j_table["11"] / sqrt((j_table["11"] + j_table["01"]) * (j_table["11"] + j_table["10"])))
  return(result)
}
