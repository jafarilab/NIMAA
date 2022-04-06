#' Plot the bipartite network and the corresponding projected networks
#'
#' @description This function converts the incidence matrix into an igraph object and provides network visualization in multiple ways.
#'
#' @param inc_mat A matrix, the incidence matrix of bipartite network.
#' @param part An integer value indicates whether the original bipartite network or its projection should be plotted. The default value of 0 represents the original bipartite network. Other possibilities are 1 and 2 for two projected networks.
#' @param verbose A logical value, if it is \code{TRUE}, the plot is saved as the .png file in the working directory. The default value is \code{FALSE}.
#' @param vertex.label.display A logical value, if it is \code{TRUE}, then the label of each vertex is shown in the output graph. The default value is \code{FALSE}.
#' @param layout A function from igraph package. The default is `layout.bipartite`, so the nodes on the top side are the variables in rows, and the nodes on the bottom side are those in columns.
#' @param vertex.label.cex A numeric value used to define the size of vertex labels. The default value is 0.3.
#' @param vertex.size A numeric value used to define the size of vertex. The default value is 4.
#' @param edge.width A numeric value used to define the edge width. The default value is 0.1.
#' @param edge.color A string used to define the color of edges. The default value is "pink".
#' @param vertex.shape A string vector to define the shapes for two different sets of vertices. The default value is `c("square", "circle")`, the first string value is for the nodes in rows, and the second one is for nodes in columns. If the \code{part} is not 0, then only the first string value is used.
#' @param vertix.color A string vector to define the colors for two different sets of vertices. The default value is `c("steel blue", "orange")`, the first string value is for the nodes in rows, and the second one is for nodes in columns. If the \code{part} is not 0, then only the first string value is used.
#'
#' @return An igraph network object with visualization.
#' @import igraph
#' @importFrom scales rescale
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:10000,]
#'
#' # convert to incidence matrix
#' beatAML_incidence_matrix <- nominalAsBinet(beatAML_data)
#'
#' # plot with the vertex label showing
#' plotBipartite(inc_mat = beatAML_incidence_matrix, vertex.label.display = TRUE)
plotBipartite <- function(inc_mat, part = 0, verbose = FALSE,
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
  if (part == 0) {
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
