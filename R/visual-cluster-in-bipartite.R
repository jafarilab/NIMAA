#' Plot the bipartite graph with color coding for different cluster in different
#' projections.
#'
#' @description This function will use sankey to draw two graphs, the left and
#'   right parts are two projections, and in each projection, a node corresponds
#'   to a group in the clustering result.
#' @param data A a dataframe containing the missing values, at least 2 nominal
#'   columns.
#' @param community_left An igraph community object, one projection of the
#'   graph, will be showed on the left side.
#' @param community_right An igraph community object, the other projection of the
#'   graph, will be showed on the right side.
#' @param name_left A string, the name of left community.
#' @param name_right A string, the name of right community.
#'
#' @return A Data frame containing plotting datas.
#' @seealso \code{\link[plotly]{plot_ly}}
#'
#' @importFrom  dplyr inner_join group_by summarize
#' @import RColorBrewer
#' @export
#'
#' @examples
#' # load part of the beatAML data and get the incidence matrix
#' beatAML_data <- NIMAA::beatAML[1:1000,]
#' beatAML_incidence_matrix <- el2IncMatrix(beatAML_data)
#'
#' # extract the sub-matrix
#' sub_matrices <- extractSubMatrix(
#' beatAML_incidence_matrix,
#' shape = c("Rectangular_element_max") # the shapes you want to extract
#' )
#'
#' # do clustering
#' cls <- findCluster(
#' sub_matrices$Rectangular_element_max, # the sub-matrix
#' dim = 1
#' )
#'
#' cls2 <- findCluster(
#' sub_matrices$Rectangular_element_max, # the sub-matrix
#' dim = 2
#' )
#'
#' visualClusterInBipartite(
#' data = beatAML_data,
#' community_left = cls2$leading_eigen,
#' community_right = cls$fast_greedy,
#' name_left = 'patient_id',
#' name_right = 'inhibitor')
visualClusterInBipartite <- function(data,
                                     community_left,
                                     community_right,
                                     name_left = "Left",
                                     name_right  = "Right") {
  # to avoid devtools::check() notes... meaningless
  group_left <- group_right <- value <- NULL

  group_mapping_list_left <- data.frame(group_left = paste0(name_left, " Group ",community_left$membership), node = community_left$names)
  group_mapping_list_right <- data.frame(group_right = paste0(name_right, " Group ",community_right$membership), node = community_right$names)

  data_cn <- colnames(data)
  colnames(data) <- c("var1", "var2", "value")

  if (FALSE %in% (data[,'var1'] %in% group_mapping_list_left[,'node'])) {
    colnames(data) <- c("var2", "var1", "value")
  }
  print_df <- dplyr::inner_join(data, group_mapping_list_left, by = c("var1" = "node")) %>%
    dplyr::inner_join(y = group_mapping_list_right, by = c("var2" = "node")) %>%
    dplyr::group_by(group_left, group_right) %>%
    dplyr::summarize(value = sum(value), .groups = "keep")

  left_df <-  dplyr::group_by(print_df, group_left)%>%
    dplyr::summarize(value = sum(value), .groups = "keep")
  right_df <-  dplyr::group_by(print_df, group_right)%>%
    dplyr::summarize(value = sum(value), .groups = "keep")

  node_number_left <- table(group_mapping_list_left[, "group_left"])
  node_number_right <- table(group_mapping_list_right[, "group_right"])
  customdata <- paste(c(node_number_left, node_number_right, "node(s)"))

  after_rank_left <- match(node_number_left, sort(unique(node_number_left)))
  after_rank_right <- match(node_number_right, sort(unique(node_number_right)))
  node_colors <- c(RColorBrewer::brewer.pal(n=3,name='Blues')[after_rank_left],RColorBrewer::brewer.pal(n=3,name='PuRd')[after_rank_left])
  fig <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    arrangement = "perpendicular",
    node = list(
      label = c(unique(print_df$group_left), unique(print_df$group_right)),
      customdata = customdata,
      pad = 15,
      thickness = 20,
      color = node_colors,
      line = list(
        color = "black",
        width = 0.5
      ),
      hovertemplate = "%{customdata}"
    ),
    link = list(
      source = as.integer(factor(print_df$group_left, labels = "")) - 1,
      target = as.integer(factor(print_df$group_right, labels = "")) + length(unique(print_df$group_left)) - 1,
      value = print_df$value,
      label = as.character(interaction(print_df[, c("group_left", "group_right")], sep = "<-->")),
      hovertemplate = "%{label}"
    )
  )
  print(fig)
  return(print_df)
}
