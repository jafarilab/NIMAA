#' Plot the given data with heatmap figure.
#'
#' @description This function plot the input data in heatmap format. The row and
#'   column are two nominal variables. The color/value of heatmap could be the
#'   weights or logical value showing valid/missing connection.
#'
#' @details This function mainly convert data in the form of edge list into
#'   matrix data and visualize it. In addition, it will also calculate some
#'   basic matrix properties and the proportion of missing data.
#'
#' @seealso \code{\link{el2IncMatrix}}
#'
#' @param x A a dataframe containing the missing values, at least 2 nominal
#'   columns.
#' @param index_nominal  A vector containing two numbers, which are the indexes
#'   for the nominal columns (the first value indicating the rows objects and
#'   the second value indicating the column object). By default is c(1,2), i.e.,
#'   the first two columns are nomianl columns.
#' @param index_numeric An integer, the index for numeric values. (this is value
#'   for selecting the column which contains our numeric values and we change it
#'   to the matrix for missing value investigation and imputation).
#' @param palette A string or number. Color palette used for the heatmap. By
#'   default is 'Blues'. (Find the option in the manual of
#'   [scale_fill_distiller()]).
#' @param verbose A Boolean value, If \code{TRUE}, the plot will be saved as the
#'   .png file in the working directory. By default is \code{FALSE}.
#' @param plot_weight A Boolean value, If \code{TRUE}, the plot will have
#'   corresponding colors based on weights, otherwise plot the binary/logical
#'   matrix figure. By default is \code{FALSE}.
#' @param print_skim A Boolean value, If \code{TRUE}, then the funtion will
#'   print \code{\link[skimr]{skim}} information in console.
#'
#' @return A matrix, the incidence matrix got from input data.
#' @importFrom crayon green
#' @importFrom dplyr setdiff
#' @importFrom plotly ggplotly
#' @importFrom purrr is_empty
#' @import ggplot2
#' @export
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:1000,]
#'
#' beatAML_incidence_matrix <- plotInput(beatAML_data,index_numeric=3)
plotInput <- function(x,
                      index_nominal = c(1, 2),
                      index_numeric = NULL,
                      palette = "Blues",
                      verbose = FALSE,
                      plot_weight = FALSE,
                      print_skim = TRUE) {
  if (purrr::is_empty(colnames(x))) {
    stop("The input data frame should include column names")
  }

  if (!(index_nominal[1] %in% c(1:ncol(x)) && index_nominal[2] %in% c(1:ncol(x)) && length(index_nominal) == 2)) {
    stop("The nominal indecies should be a vector containing two indices of columns of the input data frame")
  }

  if (!is.null(index_numeric) && (index_numeric %in% c(1:ncol(x)) && length(index_nominal) == 1)) {
    stop("The numerical index should be the index column of the input data frame")
  }

  # select the non-specified column as the numeric column if there are 3 columns in input data
  # add a all 1 column as weight if there are 2 columns in input data
  if (is.null(index_numeric) && ncol(x) >= 3) {
    index_numeric <- dplyr::setdiff(c(1:ncol(x)), index_nominal)
    warning(paste("No index_numeric specified, the No.", index_numeric, "column was chosen as numeric column"))
  } else if (is.null(index_numeric) && ncol(x) == 2) {
    # if there is only 2 columns in input data, add a new column with all 1s as weights.
    index_numeric <- 3
    x <- as.data.frame(x)
    x <- cbind(x, connection = 1)
  }

  if (!sapply(x[index_numeric], is.numeric)) {
    stop("numeric column contains non-numeric data")
  }

  # convert input dataframe to incidence matrix
  x <- as.data.frame(x)
  inc_mat <- el2IncMatrix(el = x, index_nominal = index_nominal, index_numeric = index_numeric, print_skim = print_skim)

  col_size <- nrow(unique(x[index_nominal[1]]))
  row_size <- nrow(unique(x[index_nominal[2]]))

  # create plot figure title text
  title_text <- paste0("Size: ", row_size, "rows x ", col_size, "columns")

  # if plot_weight is FALSE, change all weights to 1.
  if (!plot_weight) {
    x[index_numeric] <- 1
    colnames(x)[index_numeric] <- "Connection"
  }

  cn <- colnames(x)

  # print the figure showing where are NA data
  p <- ggplot2::ggplot(data = x, aes_string(x = cn[index_nominal[1]], y = cn[index_nominal[2]], fill = cn[index_numeric])) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_distiller(direction = -1, palette = palette) +
    ggplot2::labs(title = title_text) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = element_text(color = "Black", size = 14, face = "bold"),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.background = element_blank()
    )
  if (verbose) {
    ggplot2::ggsave(paste0(cn[index_nominal[1]], "-", cn[index_nominal[2]], ".png"), width = col_size, height = row_size, dpi = 200, units = "mm", limitsize = FALSE)
  }

  na_rate <- round(1 - (nrow(x) / (row_size * col_size)), 4)
  summary_text <- crayon::green$bold(
    "\nNa/missing values Proportion: \t", na_rate, "\n"
  )
  cat(summary_text)
  print(plotly::ggplotly(p))

  return(inc_mat)
}
