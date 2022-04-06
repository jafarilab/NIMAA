#' Plot the incidence matrix.
#'
#' @description This function converts a nominal data edge list to an incidence matrix and provides some information about that.
#'
#' @details This function mainly converts data in the form of edge list into matrix data. It also returns the incidence matrix object, the dimensions and proportion of missing values, as well as the matrix's image for visualization.
#'
#' @seealso \code{\link{nominalAsBinet}}
#'
#' @param x A data frame containing at least 2 nominal columns and an optional numeric column.
#' @param index_nominal  A vector made up of two numbers that serve as nominal columns. The first value indicates the incidence matrix's row name, while the second value represents the incidence matrix's column name. It is c (1,2) by default, which implies that the first column in the x data frame is the incidence matrix's row, and the second column in the x data frame is the incidence matrix's column.
#' @param index_numeric An integer, the index of a numeric variable. This is the value used to select the column that contains weight score values for pairwise relationships, and it is used to fill the elements of the incidence matrix. These values are also utilized for investigating missing data and predicting edges via imputation.
#' @param palette A string or number. Color palette used for the heat map. By default, it sets to "Blues". (Find more option in the manual of [scale_fill_distiller()]).
#' @param verbose A logical value, If it is set to \code{TRUE}, the plot is saved as the .png file in the working directory. By default, it is set to \code{FALSE}.
#' @param plot_weight A logical value, If it is set to \code{TRUE}, the plot is displayed with the corresponding colors based on weight scores, otherwise the binary matrix image is displayed. By default, it is set to \code{FALSE}.
#' @param print_skim A logical value, If \code{TRUE}, then the function prints \code{\link[skimr]{skim}} information in console.
#'
#' @return An incidence matrix object and image with the dimension and missing value proportion.
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
#' # visualize the input dataset
#' beatAML_incidence_matrix <- plotIncMatrix(beatAML_data,
#' index_numeric= 3)
plotIncMatrix <- function(x,
                      index_nominal = c(1, 2),
                      index_numeric = NULL,
                      palette = "Blues",
                      verbose = FALSE,
                      plot_weight = FALSE,
                      print_skim = FALSE) {
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
  inc_mat <- nominalAsBinet(el = x, index_nominal = index_nominal, index_numeric = index_numeric, print_skim = print_skim)

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
