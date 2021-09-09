#' Plot the graph based on input data
#'
#' @description This function converts the input incidence matrix into a
#'   bipartite graph, and uses the igraph package to draw an figure.
#'
#' @param inc_mat A matrix, the incidence matrix of graph.
#' @param dim An integer value, chosen between plotting the original bipartite
#'   graph or its projection. Default is 0 which plot the original bipartite
#'   graph. other options: 1, 2
#' @param verbose A logical, if TRUE, the plot is saved as the .png file in the
#'   working directory. Default is FALSE.
#' @param vertex.label.display A Boolean var, if TRUE then show the label of
#'   each vertex. Default is FALSE.
#' @param layout A function from igraph package, plot layout. Default is
#'   layout.bipartite, thus the nodes on the top side are the variables in rows,
#'   nodes on the bottom side are those in columns.
#' @param vertex.label.cex A numeric, vertex labels' size. Default is 0.3.
#' @param vertex.size A numeric, vertex size. Default is 4.
#' @param edge.width A numeric, edge width. Default is 0.1.
#' @param edge.color A string, edge color. Default is pink.
#' @param vertex.shape A vector, shapes for two different sets of vertices.
#'   Default is c("square", "circle"), the first one is for the nodes in rows,
#'   the second one is for nodes in columns, if the \code{dim} is not 0, then
#'   only the first one will be used.
#' @param vertix.color A vector, colors for two different sets of vertices.
#'   Default is c("steel blue", "orange"), the first one is for the nodes in
#'   rows, the second one is for nodes in columns, if the \code{dim} is not 0,
#'   then only the first one will be used..
#'
#' @return An igraph graph object.
#' @import igraph
#' @importFrom scales rescale
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:1000,]
#'
#' # plot beatAML data and convert to incidence matrix
#' beatAML_incidence_matrix <- el2IncMatrix(beatAML_data, print_skim = FALSE)
#'
#' # plot with the vertex label showing
#' plotBipartite(inc_mat = beatAML_incidence_matrix, vertex.label.display = TRUE)
plotBipartite <- function(inc_mat, dim = 0, verbose = FALSE,
                          vertex.label.display = FALSE,
                          layout = layout.bipartite,
                          vertex.shape = c("square", "circle"),
                          vertix.color = c("steel blue", "orange"),
                          vertex.label.cex = 0.3,
                          vertex.size = 4,
                          edge.width = 0.4,
                          edge.color = "pink") {
  G <- createBipartiteGrpahWithigraph(inc_mat)
  igraph::V(G)$shape <- vertex.shape[igraph::V(G)$type + 1]
  igraph::V(G)$color <- vertix.color[igraph::V(G)$type + 1]
  if (dim == 0) {
    if (vertex.label.display == TRUE) {
      igraph::plot.igraph(G,
        vertex.label.cex = vertex.label.cex,
        vertex.size = vertex.size,
        edge.width = scales::rescale(igraph::E(G)$weight, to = c(0, edge.width)),
        layout = layout,
        edge.color = edge.color
      )
    }
    if (vertex.label.display != TRUE) {
      igraph::plot.igraph(G,
        vertex.label = NA,
        vertex.size = vertex.size,
        edge.width = scales::rescale(igraph::E(G)$weight, to = c(0, edge.width)),
        layout = layout,
        edge.color = edge.color
      )
    }
  }
  return(G)
}
