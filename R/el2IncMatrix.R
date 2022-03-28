#' Convert Edge list to Incidence Matrix
#' @description This function converts data in the edge list format to an incidence matrix. An edge list is typically a data frame with two (or three) columns representing starting and ending nodes (or with a possible numerical relationship). The elements in the incidence matrix are the binary or numeric values of the pairwise relationships.
#' @param el A data frame or matrix object as an edge list.
#' @param index_nominal  A vector with two values represents the indices for the nominal columns. The first value indicates the row objects and the second value indicates the column objects in the incidence matrix output.
#' @param index_numeric An integer, the index for numeric values. This is value for selecting the column which contains the numeric values of pairwise relationship. This column is used for missing value investigation and imputation steps.
#' @param print_skim A logical value, If \code{TRUE}, then the function prints \code{\link[skimr]{skim}} information in console.
#' @return The incidence matrix.
#' @seealso \code{\link[tidyr]{pivot_wider}},
#'   \code{\link[tibble]{column_to_rownames}}
#' @importFrom tidyr pivot_wider
#' @importFrom  tibble column_to_rownames
#' @import skimr
#' @export
#'
#' @examples
#' # generate an edge list without numeric value
#' el1 <- data.frame(
#'   Part1 = c("d", "e", "e", "b", "a", "a"),
#'   Part2 = c("J", "N", "O", "R", "R", "L")
#' )
#'
#' # generate an edge list with numeric value
#' el2 <- data.frame(
#'   Part1 = c("d", "e", "e", "b", "a", "a"),
#'   Part2 = c("J", "N", "O", "R", "R", "L"),
#'   Value = c(4, 5, 5, 8, 7, 7)
#' )
#'
#' # run el2IncMatrix() to convert the edge list to the incidence matrix
#' inc_mat1 <- el2IncMatrix(el1)
#' inc_mat2 <- el2IncMatrix(el2)
el2IncMatrix <- function(el, index_nominal = c(1, 2), # el = edgelist; 2 = to; IncMatrix = Incidence Matrix.
                         index_numeric = 3,
                         print_skim = FALSE) {
  cn <- colnames(el)
  if (ncol(el) == 2){
    el[,'link'] <- rep(1,nrow(el))
    cn[index_numeric] <- 'link'
  }

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
