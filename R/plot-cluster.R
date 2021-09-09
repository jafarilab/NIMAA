#' Plot the clusters in one projection of the bipartite graph.
#' @description This function will visualize the input igraph cluster objects
#'   and igraph graph objects with membership properties. Different memberships
#'   will be represented by dots of different colors.
#'
#' @param graph A igraph graph object.
#' @param cluster A igraph cluster object, usually got from \code{\link{findCluster}}
#' @param ... Pass to \code{\link[networkD3]{forceNetwork}}
#'
#' @seealso \code{\link[networkD3]{forceNetwork}}
#'
#' @return A networkD3 object, the plot of cluster.
#'
#' @import igraph
#' @import networkD3
#' @export
#'
#' @examples
#' # generate a incidence matrix
#' data <- matrix(c(0, 1, 0, 1, 0,
#' 1, 0, 0, 0, 0,
#' 1, 0, 1, 1, 1), nrow = 3,byrow = TRUE)
#' colnames(data) <- letters[1:5]
#' rownames(data) <- LETTERS[1:3]
#'
#' # run findCluster() to do clustering
#' cls <- findCluster(
#'   data,
#'   dim = 1,
#'   method = "all",
#'   normalization = FALSE,
#'   rm_weak_edges = FALSE,
#'   comparison = FALSE
#' )
#' # plot the cluster
#' plotCluster(graph = cls$graph, cluster = cls$louvain)
plotCluster <- function(graph, cluster, ...) {
  members <- igraph::membership(cluster)

  # Convert to object suitable for networkD3
  graph_d3 <- networkD3::igraph_to_networkD3(graph, group = members)

  # Create force directed network plot
  fig <- networkD3::forceNetwork(
    Links = graph_d3$links, Nodes = graph_d3$nodes,
    Source = "source", Target = "target",
    NodeID = "name", Group = "group", ...
  )
  return(fig)
}
