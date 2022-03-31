#' General properties of the network
#'
#' @description Generic function for network properties, allowing you to get a quick overview of the network topology.
#'
#' @details The following measurements are calculated in one step using the \href{https://igraph.org/r/}{igraph} package to analyze the input graph object: the Degree, Betweenness, Closeness, Kleinberg's hub score and Eigenvector centrality of nodes (vertices), the Betweenness centrality of edges, number of nodes, edges and components, the edge density, global Eigenvector centrality value and global Kleinberg's hub centrality score.
#'
#'
#' @param graph An `igraph` object.
#'
#' @return A list containing `vertices` and `edges` centrality values as well as `general_stats` for the whole network.
#'
#' @seealso \code{\link[igraph]{vcount}}, \code{\link[igraph]{ecount}},
#'   \code{\link[igraph]{edge_density}}, \code{\link[igraph]{count_components}},
#'   \code{\link[igraph]{degree}},\code{\link[igraph]{betweenness}},
#'   \code{\link[igraph]{edge_betweenness}},\code{\link[igraph]{closeness}},
#'   \code{\link[igraph]{eigen_centrality}},\code{\link[igraph]{hub_score}}.
#'
#' @import igraph
#' @export
#'
#' @examples
#' # generate a toy graph
#' g1 <- igraph::make_graph(c(1, 2, 3, 4, 1, 3), directed = FALSE)
#' igraph::V(g1)$name <- c("n1", "n2", "n3", "n4")
#'
#' # generate random graph according to the Erdos-Renyi model
#' g2 <- igraph::sample_gnm(10, 23)
#' igraph::V(g2)$name <- letters[1:10]
#'
#' # run analyseNetwork
#' analyseNetwork(g1)
#' analyseNetwork(g2)
analyseNetwork <- function(graph) {
  vertices <- data.frame(row.names = igraph::V(graph)$name)
  edges <- data.frame(row.names = paste0(igraph::as_edgelist(graph)[, 1], " - ", igraph::as_edgelist(graph)[, 2]))
  general_stats <- list()
  result <- list()

  # vcount	Order (number of vertices) of a graph
  general_stats$vertices_amount <- igraph::vcount(graph)

  # ecount
  general_stats$edges_amount <- igraph::ecount(graph)

  # edge_density
  general_stats$edge_density <- igraph::edge_density(graph)

  # components_number
  general_stats$components_number <- igraph::count_components(graph)

  # degree
  vertices[, "degree"] <- igraph::degree(graph)

  # betweenness(), Vertex betweenness centrality
  vertices[, "betweenness_centrality"] <- igraph::betweenness(graph, directed = FALSE)

  # edge_betweenness()	Edge betweenness centrality
  edges[, "betweenness_centrality"] <- igraph::edge_betweenness(graph, directed = FALSE)

  # weights
  edges[, "weights"] <- igraph::E(graph)$weight

  # closeness()	Closeness centrality of vertices
  vertices[, "closeness_centrality"] <- igraph::closeness(graph)

  # eigen_centrality	Find Eigenvector Centrality Scores of Network Positions
  vertices[, "eigen_centrality"] <- igraph::eigen_centrality(graph)$vector
  general_stats$eigen_centrality_value <- igraph::eigen_centrality(graph)$value

  # hub_score	Kleinberg's hub centrality scores.
  vertices[, "hub_score"] <- igraph::hub_score(graph)$vector
  general_stats$hub_score_value <- igraph::hub_score(graph)$value

  result$vertices <- vertices
  result$edges <- edges
  result$general_stats <- general_stats
  return(result)
}
