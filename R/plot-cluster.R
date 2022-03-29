#' Plot the clusters in one projection of the bipartite network
#'
#' @description This function makes an interactive network figure that shows nodes in which nodes belonging to the same cluster are colored the same and nodes belonging to other clusters are colored differently. This function has been customized to use the output of \code{\link{findCluster}}.
#'
#' @param graph An igraph object.
#' @param cluster An igraph cluster object.
#' @param ... Pass to \code{\link[networkD3]{forceNetwork}}
#'
#' @seealso \code{\link[networkD3]{forceNetwork}}
#'
#' @return A networkD3 object.
#'
#' @import igraph
#' @import networkD3
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
#'   comparison = FALSE
#' )
#' # plot the cluster with Louvain method
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
