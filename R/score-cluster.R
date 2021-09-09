#' Score the clusters in one projection of the bipartite graph.
#' @description This function will use the community object, graph object and
#'   distance matrix to analyze, mainly using the cluster.stats function in the
#'   fpc package, in addition, it also calculates a ‘coverage’ indicator.
#'
#' @seealso \code{\link[fpc]{cluster.stats}}, \code{\link{findCluster}}
#' @param community An igraph community object.
#' @param graph An igraph graph object.
#' @param distance_matrix A matrix, the distance of graph, usually got from
#'   \code{\link{findCluster}}
#'
#' @return A list of various scores.
#'
#' @import fpc
#' @import igraph
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' data <- NIMAA::beatAML[1:1000,]
#' # convert to incidence matrix
#' inc_mat <- el2IncMatrix(data, print_skim = FALSE)
#'
#' # run findCluster() to do clustering
#' cls <- findCluster(
#'   inc_mat,
#'   dim = 1,
#'   method = "infomap",
#'   normalization = FALSE,
#'   rm_weak_edges = TRUE,
#'   comparison = FALSE
#' )
#' # get the scoring result
#' scoreCluster(
#'   community = cls$infomap,
#'   graph = cls$graph,
#'   distance_matrix = cls$distance_matrix
#' )
scoreCluster <- function(community, graph, distance_matrix) {
  result <- list()

  fpc_stats <- fpc::cluster.stats(d = distance_matrix, clustering = community$membership)
  result$fpc_stats <- fpc_stats

  result$coverage <- calculateCoverage(graph, community)
  return(result)
}

calculateCoverage <- function(graph, community) {
  inner_edges_weights_sum <- sum(igraph::E(graph)[!igraph::crossing(community, graph)]$weight)
  all_weights_sum <- sum(igraph::E(graph)$weight)
  return(inner_edges_weights_sum / all_weights_sum)
}
