#' Convert Edge list to Incidence Matrix
#' @description This function will convert edge-list-format data (usually a
#'   dataframe concluding 2/3 columns stand for two parts and possible numerical
#'   relationship) to a matrix (columns and rows stand for two parts, values in
#'   the matrix are the 'relationship')
#' @param el A dataframe or matrix, edgelist data
#' @param index_nominal  A vector with two values. The indexes for the nominal
#'   columns (the first value indicating the rows objects and the second value
#'   indicating the column object).
#' @param index_numeric An integer, the index for numeric values. (this is value
#'   for selecting the column which contains our numeric values and we change it
#'   to the matrix for missing value investigation and imputation).
#' @param print_skim A Boolean value, If \code{TRUE}, then the funtion will
#'   print \code{\link[skimr]{skim}} information in console.
#' @return The incidence matrix
#' @seealso \code{\link[tidyr]{pivot_wider}},
#'   \code{\link[tibble]{column_to_rownames}}
#' @importFrom tidyr pivot_wider
#' @importFrom  tibble column_to_rownames
#' @import skimr
#' @export
#'
#' @examples
#' # generate a edgelist
#' el <- data.frame(
#'   A = c(1, 2, 3, 4, 5, 6),
#'   B = c("a", "b", "c", "e", "f", "g"),
#'   m = c(1, 2, 1, 2, 1, 2)
#' )
#'
#' # run el2IncMatrix() to convert edgelist to incidence matrix
#' inc_mat <- el2IncMatrix(el, print_skim = FALSE)
el2IncMatrix <- function(el, index_nominal = c(1, 2), # el = edgelist; 2 = to; IncMatrix = Incidence Matrix.
                         index_numeric = 3,
                         print_skim = TRUE) {
  cn <- colnames(el)

  inc_mat <- el %>%
    tidyr::pivot_wider(names_from = cn[index_nominal[1]], values_from = cn[index_numeric]) %>%
    tibble::column_to_rownames(var = cn[index_nominal[2]]) %>%
    as.matrix()
  # inc_mat[is.na(inc_mat)] <- 0
  if (print_skim) {
    print(skimr::skim(inc_mat))
  }
  return(inc_mat)
}
