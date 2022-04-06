#' Extract the non-missing submatrices from a given matrix.
#'
#' @description This function arranges the input matrix and extracts the submatrices with non-missing values or with a specific proportion of missing values (except for the elements-max submatrix). The result is also shown as \code{plotly} figure.
#' @details This function performs row- and column-wise preprocessing in order to extract the largest submtrices. The distinction is that the first employs the original input matrix (row-wise), whereas the second employs the transposed matrix (column-wise). Following that, this function performs a "three-step arrangement" on the matrix, the first step being row-by-row arrangement, the second step being column-by-column arrangement, and the third step being total rearranging. Then, using four strategies, namely "Square", "Rectangular row", "Rectangular col", and "Rectangular element max", this function finds the largest possible submatrix (with no missing values), outputs the result, and prints the visualization. "Square" denotes the square submatric with the same number of rows and columns. "Rectangular_row" indicates the submatrices with the most rows. "Rectangular_col" denotes the submatrices with the most columns. "Rectangular_element_max" indicates the submatrices with the most elements which is typically a rectangular submatrix.
#'
#' @seealso \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{arrange_if}}
#'
#' @param x A matrix.
#' @param shape A string array indicating the shape of the submatrix, by default is "All", other options are "Square", "Rectangular_row", "Rectangular_col", "Rectangular_element_max".
#' @param verbose A logical value, If \code{TRUE}, the plot is saved as the .png file in the working directory. By default, it is \code{FALSE}.
#' @param palette A string or number. Color palette used for the visualization. By default, it is 'Blues'.
#' @param row.vars A string, the name for the row variable.
#' @param col.vars A string, the name for the column variable.
#' @param bar A numeric value. The cut-off percentage, i.e., the proportion of non-missing values. By default, it is set to 1, indicating that no missing values are permitted in the submatrices. This argument is not applicable to the elements-max sub-matrix.
#' @param plot_weight A logical value, If \code{TRUE}, then the function prints submatrices with weights, otherwise it prints the submatrices with all weights as 1.
#' @param print_skim A logical value, If \code{TRUE}, then the function prints \code{\link[skimr]{skim}} information in console. By default, it is \code{FALSE}.
#'
#' @return A matrix or a list of matrices with non-missing (bar = 1) or a few missing values inside. Also, a specific heat map plot is generated to visualize the topology of missing values and the submatrix sub-setting from the original incidence matrix. Additionally, the nestedness temperature is included to indicate whether the original incidence matrix should be divided into several incidence matrices beforehand.
#' @importFrom  bipartite nested
#' @import ggplot2
#' @importFrom  dplyr arrange arrange_if
#' @importFrom purrr is_double
#' @importFrom utils askYesNo
#' @importFrom crayon red green
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:10000,]
#'
#' # convert to incidence matrix
#' beatAML_incidence_matrix <- nominalAsBinet(beatAML_data)
#'
#' # extract submatrices with non-missing values
#' sub_matrices <- extractSubMatrix(beatAML_incidence_matrix, col.vars = "patient_id",
#'  row.vars = "inhibitor")
extractSubMatrix <- function(x,
                             shape = "All",
                             verbose = FALSE,
                             palette = "Greys",
                             row.vars = NULL,
                             col.vars = NULL,
                             bar = 1,
                             plot_weight = FALSE,
                             print_skim = FALSE) {
  input_type <- c("matrix", "data.frame")
  if (!(class(x)[1] %in% input_type)) {
    stop("The input should be matrix or data frame")
  }

  req_shape <- c(
    "Square", "Rectangular_row",
    "Rectangular_col", "Rectangular_element_max",
    "All"
  )

  if (FALSE %in% (shape %in% req_shape)) {
    stop("Please specify the desire shape of the output")
  }

  # making new col and row names in order using mutate properly and replace back
  # the original names at the end.
  R.names <- rownames(x)
  C.names <- colnames(x)

  if (is.null(R.names) | is.null(C.names)) {
    warning("There is no row names or column names for given input")
  }

  if (is.null(row.vars) | is.null(col.vars)) {
    warning("There is no name specified for samples on x-axis lable or y-axis lable")
  }

  # giving the xlab and ylab if they are given by user
  if (is.null(row.vars) | is.null(col.vars)) {
    c_lable <- "columns"
    r_lable <- "rows"
  } else {
    c_lable <- as.character(col.vars)
    r_lable <- as.character(row.vars)
  }

  # x_with0: replace all Na in x with 0
  x_with0 <- x
  x_with0[is.na(x_with0)] <- 0

  # print nestedness
  binmatnest2.temp <- bipartite::nested(x_with0)
  print(binmatnest2.temp)
  if (binmatnest2.temp < 1) {
    cat(crayon::red$bold("The nestedness temperature is less than 1, highly nested!\nWe suggest that divide the data into different parts."))
    if (!utils::askYesNo("Do you still want to continue?")) {
      stop("Stop the funtion by user.")
    }
  }

  # convert to dataframe
  x <- x %>%
    as.data.frame()
  t_x <- t(x) %>%
    as.data.frame()

  # 3 steps arrange
  x_after_arrange_without_weight <- threeStepArrange(x)
  x_after_arrange_with_weight <- x[rownames(x_after_arrange_without_weight), colnames(x_after_arrange_without_weight)]
  ## transpose
  t_x_after_arrange_without_weight <- threeStepArrange(t_x)
  t_x_after_arrange_with_weight <- t_x[rownames(t_x_after_arrange_without_weight), colnames(t_x_after_arrange_without_weight)]

  # get all sub matrices that needed
  result <- list()
  rect_data <- list()
  rect_data_t <- list()
  ## Square
  if ("Square" %in% shape || "All" %in% shape) {
    max_sq_nomiss <- findSquareCutoffSubmatrix(x_after_arrange_without_weight, bar)
    max_sq_nomiss_t <- findSquareCutoffSubmatrix(t_x_after_arrange_without_weight, bar)

    rect_data$max_sq_nomiss <- max_sq_nomiss # for plotting
    rect_data_t$max_sq_nomiss_t <- max_sq_nomiss_t # for plotting

    x[rownames(max_sq_nomiss), colnames(max_sq_nomiss), drop = FALSE]

    if (prod(dim(max_sq_nomiss)) < prod(dim(max_sq_nomiss_t))) {
      max_sq_nomiss_better <- t(max_sq_nomiss_t)
      # cat('Square is from row-wise-arrangement matrix\n')
    } else {
      max_sq_nomiss_better <- max_sq_nomiss
      # cat('Square is from column-wise-arrangement matrix\n')
    }
    cat(crayon::green$bold(
      "Size of Square: \t", dim(max_sq_nomiss_better)[1], "rows x ", dim(max_sq_nomiss_better)[2], "columns", "\n"
    ))

    max_sq_nomiss_better <- x[rownames(max_sq_nomiss_better), colnames(max_sq_nomiss_better), drop = FALSE]
    result$Square <- max_sq_nomiss_better
  }
  ## Rectangular_row
  if ("Rectangular_row" %in% shape || "All" %in% shape) {
    row_max_rec <- findRowwiseCutoffSubmatrix(x_after_arrange_without_weight, bar)
    row_max_rec_t <- findColwiseCutoffSubmatrix(t_x_after_arrange_without_weight, bar)

    rect_data$row_max_rec <- row_max_rec # for plotting
    rect_data_t$row_max_rec_t <- row_max_rec_t # for plotting

    if (prod(dim(row_max_rec)) < prod(dim(row_max_rec_t))) {
      row_max_rec_better <- t(row_max_rec_t)
      # cat('Rectangular_row is from row-wise-arrangement matrix\n')
    } else {
      row_max_rec_better <- row_max_rec
      # cat('Rectangular_row is from column-wise-arrangement matrix\n')
    }
    cat(crayon::green$bold(
      "Size of Rectangular_row: \t", dim(row_max_rec_better)[1], "rows x ", dim(row_max_rec_better)[2], "columns", "\n"
    ))
    row_max_rec_better <- x[rownames(row_max_rec_better), colnames(row_max_rec_better), drop = FALSE]
    result$Rectangular_row <- row_max_rec_better
  }
  ## Rectangular_col
  if ("Rectangular_col" %in% shape || "All" %in% shape) {
    col_max_rec <- findColwiseCutoffSubmatrix(x_after_arrange_without_weight, bar)
    col_max_rec_t <- findRowwiseCutoffSubmatrix(t_x_after_arrange_without_weight, bar)

    rect_data$col_max_rec <- col_max_rec # for plotting
    rect_data_t$col_max_rec_t <- col_max_rec_t # for plotting

    if (prod(dim(col_max_rec)) < prod(dim(col_max_rec_t))) {
      col_max_rec_better <- t(col_max_rec_t)
      # cat('Rectangular_col is from row-wise-arrangement matrix\n')
    } else {
      col_max_rec_better <- col_max_rec
      # cat('Rectangular_col is from column-wise-arrangement matrix\n')
    }
    cat(crayon::green$bold(
      "Size of Rectangular_col: \t", dim(col_max_rec_better)[1], "rows x ", dim(col_max_rec_better)[2], "columns", "\n"
    ))
    col_max_rec_better <- x[rownames(col_max_rec_better), colnames(col_max_rec_better), drop = FALSE]
    result$Rectangular_col <- col_max_rec_better
  }
  ## Elements max
  if ("Rectangular_element_max" %in% shape || "All" %in% shape) {
    max_rec <- findElementMaxSubmatrix(x_after_arrange_without_weight)
    max_rec_t <- findElementMaxSubmatrix(t_x_after_arrange_without_weight)

    rect_data$max_rec <- max_rec # for plotting
    rect_data_t$max_rec <- max_rec_t # for plotting

    if (prod(dim(max_rec)) < prod(dim(max_rec_t))) {
      max_rec_better <- t(max_rec_t)
      # cat('Rectangular_element_max is from row-wise-arrangement matrix\n')
    } else {
      max_rec_better <- max_rec
      # cat('Rectangular_element_max is from column-wise-arrangement matrix\n')
    }
    cat(crayon::green$bold(
      "Size of Rectangular_element_max: \t", dim(max_rec_better)[1], "rows x ", dim(max_rec_better)[2], "columns", "\n"
    ))
    max_rec_better <- x[rownames(max_rec_better), colnames(max_rec_better), drop = FALSE]
    result$Rectangular_element_max <- max_rec_better
  }


  # create a data frame (table) for plotting
  if (plot_weight) {
    x_after_arrange <- x_after_arrange_with_weight
    t_x_after_arrange <- t_x_after_arrange_with_weight
  } else {
    x_after_arrange <- x_after_arrange_without_weight
    t_x_after_arrange <- t_x_after_arrange_without_weight
  }

  print_dataframe <- x_after_arrange %>%
    as.matrix() %>%
    as.data.frame.table()

  print_dataframe_t <- t_x_after_arrange %>%
    as.matrix() %>%
    as.data.frame.table()

  plotSubmatrix(
    x = x, print_dataframe = print_dataframe, rect_data = rect_data,
    verbose = verbose, palette = palette,
    c_lable = c_lable, r_lable = r_lable,
    figure_name = "Row_wise_arrangement",
    title = "Row-wise-arrangement"
  )

  plotSubmatrix(
    x = t_x, print_dataframe = print_dataframe_t, rect_data = rect_data_t,
    verbose = verbose, palette = palette,
    c_lable = r_lable, r_lable = c_lable,
    figure_name = "Column_wise_arrangement",
    title = "Column-wise-arrangement"
  )
  if (print_skim) {
    for (mat in names(result)) {
      switch(mat,
        Rectangular_row = {
          Rectangular_row <- result$Rectangular_row
          print(skimr::skim(Rectangular_row))
        },
        Rectangular_element_max = {
          Rectangular_element_max <- result$Rectangular_element_max
          print(skimr::skim(Rectangular_element_max))
        },
        Square = {
          Square <- result$Square
          print(skimr::skim(Square))
        },
        Rectangular_col = {
          Rectangular_col <- result$Rectangular_col
          print(skimr::skim(Rectangular_col))
        }
      )
    }
  }
  return(result)
}

threeStepArrange <- function(x) {

  # replace original colnames and rownames with col1,col2.../row1, row2...
  col_name <- colnames(x)
  row_name <- rownames(x)

  colnames(x) <- paste0("col", 1:ncol(x))
  names(col_name) <- paste0("col", 1:ncol(x))
  rownames(x) <- paste0("row", 1:nrow(x))
  names(row_name) <- paste0("row", 1:nrow(x))

  ## 1 row arranging
  x_arrange1 <- dplyr::arrange(x, by = rowSums(is.na(x)))

  ## 2 col arranging
  ### Transpose, and using the same method in last step
  x_arrange1 <- t(x_arrange1) %>% as.data.frame()
  x_arrange2 <- dplyr::arrange(x_arrange1, by = rowSums(is.na(x_arrange1)))
  ### replace all valid weights with 1
  x_arrange2[!is.na(x_arrange2)] <- 1
  x_arrange2 <- t(x_arrange2)

  ## 3 total rearranging after replacing back NAs as the NAs would be ignor and result in a new arrangment
  x_arrange3 <- x_arrange2 %>%
    as.data.frame() %>%
    dplyr::arrange_if(purrr::is_double)

  # put back original col/row names
  colnames(x_arrange3) <- col_name[colnames(x_arrange3)]
  rownames(x_arrange3) <- row_name[rownames(x_arrange3)]

  return(x_arrange3)
}

findElementMaxSubmatrix <- function(x) {
  # add a NA column and a NA row at bargin
  x <- rbind(x, rep(NA))
  x <- cbind(x, rep(NA))
  # Get the index of first NA value in each row
  pos_first_na <- apply(
    X = x,
    MARGIN = 1,
    function(row) {
      min(which(is.na(row)))
    }
  )

  row_pivot <- which.max(c(1:nrow(x)) * (pos_first_na - 1))
  col_pivot <- pos_first_na[row_pivot] - 1
  max_rec <- x[1:row_pivot, 1:col_pivot, drop = FALSE]

  return(max_rec)
}

findSquareCutoffSubmatrix <- function(x, bar) {
  i <- 1
  while (sum(x[1:i, 1:i], na.rm = TRUE) >= i * i * bar) {
    i <- i + 1
  }
  max_sq <- x[1:i - 1, 1:i - 1, drop = FALSE]

  return(max_sq)
}

findColwiseCutoffSubmatrix <- function(x, bar) {
  if (TRUE %in% (is.na(x[, 1]))) {
    col_boundary <- min(which(is.na(x[, 1]))) - 1
  } else {
    col_boundary <- length(x[, 1])
  }
  i <- 1

  while (sum(x[1:col_boundary, 1:i], na.rm = TRUE) >= i * col_boundary * bar) {
    i <- i + 1
  }
  col_max_rec <- x[1:col_boundary, 1:i - 1, drop = FALSE]

  return(col_max_rec)
}

findRowwiseCutoffSubmatrix <- function(x, bar) {
  if (TRUE %in% (is.na(x[1, ]))) {
    row_boundary <- min(which(is.na(x[1, ]))) - 1
  } else {
    row_boundary <- length(x[1, ])
  }
  i <- 1

  while (sum(x[1:i, 1:row_boundary], na.rm = TRUE) >= i * row_boundary * bar) {
    i <- i + 1
  }
  row_max_rec <- x[1:i - 1, 1:row_boundary, drop = FALSE]

  return(row_max_rec)
}

plotSubmatrix <- function(x, print_dataframe, rect_data,
                          figure_name = "after_arranging",
                          verbose, palette,
                          c_lable, r_lable,
                          title = "After arranging") {
  # to avoid devtools::check() notes... meaningless
  y1 <- y2 <- x1 <- x2 <- size <- NULL
  # create a data frame for 4 rectangles in plot
  sub_matrix_size <- sapply(rect_data, dim)
  rect_plot <- data.frame(
    x1 = rep(1, length(rect_data)), x2 = sub_matrix_size[1, ],
    y1 = rep(1, length(rect_data)), y2 = sub_matrix_size[2, ],
    size = paste0("\t", sub_matrix_size[1, ], "x", sub_matrix_size[2, ], "\n", names(rect_data))
  )
  p <- ggplot2::ggplot(
    data = print_dataframe,
    aes_string(x = colnames(print_dataframe)[1], y = colnames(print_dataframe)[2])
  ) +
    geom_raster(aes_string(fill = colnames(print_dataframe)[3])) +
    ggtitle(title) +
    xlab(c_lable) +
    ylab(r_lable) +
    scale_fill_distiller(direction = 1, palette = palette, na.value = "white") +
    geom_rect(
      data = rect_plot, aes(ymin = y1, ymax = y2, xmin = x1, xmax = x2),
      fill = c("red", "blue", "green", "orange")[1:length(rect_data)],
      colour = "black", size = 0.5, alpha = 0.1, inherit.aes = FALSE
    ) +
    geom_text(
      data = rect_plot,
      aes(x = x2 + 3, y = y2 + 4, label = size),
      size = 3,
      check_overlap = TRUE
    ) +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.title = element_text(color = "black", size = 14, face = "bold"),
      axis.title.x = element_text(color = "blue", size = 14, face = "bold"),
      axis.title.y = element_text(color = "blue", size = 14, face = "bold"),
      panel.background = element_blank()
    )
  if (verbose) {
    ggsave(paste0(figure_name, ".png"), width = nrow(x), height = ncol(x), dpi = 200, units = "mm")
  }

  print(plotly::ggplotly(p))
}
