#' Validate the cluster analysis in a projected network based on additional external measures.
#' @description This function calculates the similarity of a given clustering method to the provided ground truth as external features (prior knowledge). This function provides external cluster validity measures including `corrected.rand` and `jaccard similarity`. This function requires the community object, igraph object and distance matrix returned by \code{\link{findCluster}} to analyze.
#'
#' @param community  An igraph community object.
#' @param extra_feature A data frame object that shows the group membership of each node based on prior knowledge.
#' @param dist_mat  A matrix containing the distance of nodes in the network. This matrix can be retrieved by the output of \code{\link{findCluster}} to analyze.
#'
#' @return A list containing the similarity measures for the clustering results and the ground truth represented as an external features, i.e., corrected Rand and Jaccard indices.
#'
#' @import fpc
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:10000,]
#'
#' # convert to incidence matrix
#' beatAML_incidence_matrix <- nominalAsBinet(beatAML_data)
#'
#' # do clustering
#' cls <- findCluster(beatAML_incidence_matrix,
#'   part = 1, method = c('infomap','walktrap'),
#'   normalization = FALSE, rm_weak_edges = TRUE,
#'   comparison = FALSE)
#'
#' # generate a random external_feature
#' external_feature <- data.frame(row.names = cls$infomap$names)
#' external_feature[,'membership'] <- paste('group',
#' sample(c(1,2,3,4), nrow(external_feature),
#' replace = TRUE))
#'
#' # validate clusters using random external feature
#' validateCluster(community = cls$walktrap,
#' extra_feature = external_feature,
#' dist_mat = cls$distance_matrix)

validateCluster <- function(community,
                            extra_feature,
                            dist_mat) {
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
