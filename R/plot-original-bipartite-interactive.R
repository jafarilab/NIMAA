#' Use the incidence matrix to plot an interactive bipartite graph figure
#'
#' @description This function converts the input incidence matrix into a
#'   bipartite graph, and uses the visNetwork package to draw an interactive
#'   figure.
#'
#' @details This function realizes interactive visualization. The user can input
#'   a simple incidence matrix, and then get a dynamic graph with a bipartite
#'   graph layout, in which two parts use different colors and shapes to
#'   represent nodes.
#'
#' @seealso \code{\link[visNetwork]{visNetwork}}
#' @param inc_mat A matrix, the incidence matrix of graph.
#'
#' @return An visNetwork interactive figure, details in \code{\link[visNetwork]{visNetwork}}
#'
#' @import visNetwork
#' @import igraph
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:1000,]
#'
#' # plot beatAML data and convert to incidence matrix
#' beatAML_incidence_matrix <- el2IncMatrix(beatAML_data, print_skim = FALSE)
#'
#' plotBipartiteInteractive(inc_mat = beatAML_incidence_matrix)
plotBipartiteInteractive <- function(inc_mat) {
  G <- createBipartiteGrpahWithigraph(inc_mat)
  igraph::V(G)$Group <- ifelse(igraph::V(G)$type == TRUE, ## if type is True, give it a 'Group' 1
    1,
    2
  )
  igraph::V(G)$shape <- c("square", "circle")[igraph::V(G)$type + 1]
  igraph::V(G)$color <- c("lightblue", "salmon")[igraph::V(G)$type + 1]

  G_vis <- visNetwork::toVisNetworkData(G) # convert the graph (or use visIgraph)

  names <- sort(G_vis$nodes$id) # for our dropdown box

  vis_plot <- visNetwork::visNetwork(
    nodes = G_vis$nodes,
    edges = G_vis$edges,
    main = "Original",
    submain = "data",
    footer = "---"
  ) %>%
    visNetwork::visIgraphLayout(
      layout = "layout.bipartite", # or use igraph's `layout_*`s in quotes
      smooth = FALSE, # set to F when bogged by bigger graphs
      physics = F # set to F when bogged by bigger graphs
    ) %>%
    visNetwork::visNodes(size = 30) %>%
    visNetwork::visEdges(color = list(highlight = "lightgray"), width = 0.001, smooth = F) %>%
    visNetwork::visOptions(
      selectedBy = "Group",
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE,
        labelOnly = TRUE
      ),
      nodesIdSelection = list(
        enabled = TRUE,
        values = names
      )
    ) %>%
    visNetwork::visLegend(width = 0.1) %>%
    visNetwork::visPhysics(
      repulsion = list(springlength = 50), # usually will take some tweaking
      maxVelocity = 2,
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -1000),
      timestep = 0.25
    )

  return(vis_plot)
}
