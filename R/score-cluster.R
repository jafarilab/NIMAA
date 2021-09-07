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
#' \dontrun{
#' # load the beatAML data and get the incidence matrix
#' beatAML_data <- NIMAA::beatAML
#' beatAML_incidence_matrix <- plotInput(beatAML_data)
#'
#' # extract the sub-matrix
#' sub_matrices <- extractSubMatrix(
#' beatAML_incidence_matrix,
#' shape = c("Square") # the shapes you want to extract
#' )
#'
#' # do clustering
#' cls2 <- findCluster(
#' sub_matrices$Rectangular_element_max, # the sub-matrix
#' dim = 2 # set to 2 to use the other part of graph
#' )
#'
#' # get the scoring result
#' scoring_result <- scoreCluster(
#' community = cls2$infomap,
#' graph = cls2$graph,
#' distance_matrix = cls2$distance_matrix)
#' }
scoreCluster <- function(community, graph, distance_matrix) {

  result <- list()

  fpc_stats <- fpc::cluster.stats(d=distance_matrix, clustering=community$membership)
  result$fpc_stats <- fpc_stats

  result$coverage <- calculateCoverage(graph,community)
  return(result)
}

calculateCoverage <- function(graph, community){
  inner_edges_weights_sum <- sum(igraph::E(graph)[!igraph::crossing(community,graph)]$weight)
  all_weights_sum <- sum(igraph::E(graph)$weight)
  return(inner_edges_weights_sum/all_weights_sum)
}
