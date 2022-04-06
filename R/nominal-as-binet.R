#' Convert nominal data to a bipartite network
#' @description This function converts nominal data, which is represented in a data frame format as an edge list, to a bipartite network in the incidence matrix format. Nominal data is typically a data frame with two (or three) columns representing two nominal variables labels that NIMAA considers as starting and ending nodes (labels) in an edge list (or with a numeric value for each pairwise relationship of labels). The elements in the incidence matrix are the binary or numeric values of the pairwise relationships.
#' @param el A data frame or matrix object as an edge list.
#' @param index_nominal  A vector with two values represents the indices for the columns containing nominal variables. The first value indicates the row objects and the second value indicates the column objects in the incidence matrix output.
#' @param index_numeric An integer, the index for numeric values. This is the value used to pick the column containing the numeric values corresponding to the pairwise relationship of nominal variable labels. This column is used for missing value investigation and imputation steps.
#' @param print_skim A logical value, If \code{TRUE}, then the function prints \code{\link[skimr]{skim}} information in console.
#' @return The incidence matrix representing the corresponding bipartite network. If `print_skim` set to `TRUE`, a summary of the matrix is also provided.
#' @seealso \code{\link[tidyr]{pivot_wider}},
#'   \code{\link[tibble]{column_to_rownames}}
#' @importFrom tidyr pivot_wider
#' @importFrom  tibble column_to_rownames
#' @import skimr
#' @export
#'
#' @examples
#' # generate a data frame with two nominal variables without numeric values
#' el1 <- data.frame(
#'   nominal_var_1 = c("d", "e", "e", "b", "a", "a"),
#'   nominal_var_2 = c("J", "N", "O", "R", "R", "L")
#' )
#'
#' # generate a data frame with two nominal variables with numeric values
#' el2 <- data.frame(
#'   nominal_var_1 = c("d", "e", "e", "b", "a", "a"),
#'   nominal_var_2 = c("J", "N", "O", "R", "R", "L"),
#'   numeric_val = c(4, 5, 5, 8, 7, 7)
#' )
#'
#' # run nominalAsBinet() to convert the edge list to the incidence matrix
#' inc_mat1 <- nominalAsBinet(el1)
#' inc_mat2 <- nominalAsBinet(el2)
#'
nominalAsBinet <- function(el, index_nominal = c(1, 2), # nominal = nominal data; Binet = bipartite network.
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
