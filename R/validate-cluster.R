#' Validate the accuracy of our clustering of the projection comparing to users'
#' input.
#' @description This function will verify the similarity between different
#'   clustering methods and external features (prior knowledge), which is to
#'   measure the results of clustering methods.
#'
#' @param dist_mat  A matrix, the distance of graph, usually got from
#' @param extra_feature A dataframe has only one column indicating the
#'   membership of each nodes (rownames).
#' @param community  An igraph community object.
#'
#' @return A list containing the measured differences between clustering methods
#'   and reference clustering, corrected rand index and Jaccard similarity.
#' @import fpc
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' data <- NIMAA::beatAML
#' # convert to incidence matrix
#' inc_mat <- el2IncMatrix(data, print_skim = FALSE)
#'
#' # run findCluster() to do clustering
#' cls <- findCluster(
#' inc_mat,
#' dim = 1,
#' method = c('infomap','walktrap'),
#' normalization = FALSE,
#' rm_weak_edges = TRUE,
#' comparison = FALSE)
#'
#' # generate some external_feature
#' external_feature <- data.frame(row.names = cls$infomap$names)
#' external_feature[,'membership'] <- paste('group',
#' sample(c(1,2,3,4),
#' nrow(external_feature),
#' replace = TRUE))
#'
#' # validation
#' validateCluster(dist_mat = cls$distance_matrix,
#' community = cls$walktrap,
#' extra_feature = external_feature)

validateCluster <- function(dist_mat,
                            extra_feature,
                            community) {
  result <- list()

  colnames(extra_feature) <- "membership"
  result$corrected.rand <- fpc::cluster.stats(d = dist_mat, clustering = community$membership, alt.clustering = extra_feature$membership)$corrected.rand


  # CHECK extra_feature and community

  extra_feature <- extra_feature[community$names, ]

  # get the membership matrix
  extra_membership_mat <- matrix(nrow = length(extra_feature), ncol = length(extra_feature))
  for (i in 1:(length(extra_feature) - 1)) {
    for (j in (i + 1):length(extra_feature)) {
      extra_membership_mat[i, j] <- ifelse(extra_feature[i] == extra_feature[j], 1, 0)
    }
  }

  community_member_vector <- community$membership
  community_membership_mat <- matrix(nrow = community$vcount, ncol = community$vcount)
  for (i in 1:(community$vcount - 1)) {
    for (j in (i + 1):community$vcount) {
      community_membership_mat[i, j] <- ifelse(community_member_vector[i] == community_member_vector[j], 1, 0)
    }
  }

  j_table <- table(paste0(as.vector(extra_membership_mat), as.vector(community_membership_mat)))

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

  jaccard_similarity <- as.numeric(j_table["11"] / (j_table["10"] + j_table["01"] + j_table["11"]))
  result$jaccard_similarity <- jaccard_similarity

  return(result)
}
