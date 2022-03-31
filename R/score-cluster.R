#' Score the clusters in a projected network based on additional internal measures.
#' @description This function provides additional internal cluster validity measures such as entropy and coverage. The concept of scoring is according to the weight fraction of all intra-cluster edges relative to the total weight of all edges in the graph. This function requires the community object, igraph object and distance matrix returned by \code{\link{findCluster}} to analyze.
#'
#' @seealso \code{\link[fpc]{cluster.stats}}, \code{\link{findCluster}}
#' @param community An igraph community object.
#' @param graph An igraph graph object.
#' @param dist_mat A matrix containing the distance of nodes in the network. This matrix can be retrieved by the output of \code{\link{findCluster}} to analyze.
#'
#' @return A list of various scores.
#'
#' @import fpc
#' @import igraph
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
#' cls <- findCluster(beatAML_incidence_matrix,
#'   part = 1, method = "infomap", normalization = FALSE,
#'   rm_weak_edges = TRUE, comparison = FALSE)
#'
#' # get the scoring result
#' scoreCluster(community = cls$infomap, graph = cls$graph,
#'   dist_mat = cls$distance_matrix)
#'
scoreCluster <- function(community, graph, dist_mat) {
  result <- list()

  fpc_stats <- fpc::cluster.stats(d = dist_mat, clustering = community$membership)
  result$fpc_stats <- fpc_stats

  result$coverage <- calculateCoverage(graph, community)
  return(result)
}

calculateCoverage <- function(graph, community) {
  inner_edges_weights_sum <- sum(igraph::E(graph)[!igraph::crossing(community, graph)]$weight)
  all_weights_sum <- sum(igraph::E(graph)$weight)
  return(inner_edges_weights_sum / all_weights_sum)
}
