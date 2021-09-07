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
#'
#' plotCluster(graph=cls2$graph,cluster = cls2$louvain)
#' }
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
