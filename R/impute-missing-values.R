#' Impute the missing value in the given matrix.
#' @description This function will call several data imputation methods, where
#'   the columns of the matrix are different objects, and the rows represent
#'   multiple observations.
#'
#' @details First, this function will convert the column name and row name to
#'   avoid possible interpolation failures caused by the special characters of
#'   the column name and row name. Then it will perform a variety of numerical
#'   imputation according to the user's input, and return all the data that does
#'   not contain any missing data, a list of matrices. 'median' will replace the
#'   missing values with the median of each rows(observations), 'knn' is the
#'   method in package \code{bnstruct}, 'als' and 'svd' are methods from
#'   package \code{softImpute}, 'CA', 'PCA' and 'FAMD' are from package
#'   \code{missMDA}, others are from the famous \code{mice}.
#'
#' @seealso \code{\link[bnstruct]{knn.impute}},
#'   \code{\link[softImpute]{softImpute}}, \code{\link[missMDA]{imputeCA}},
#'   \code{\link[missMDA]{imputeFAMD}}, \code{\link[missMDA]{imputePCA}},
#'   \code{\link[mice]{mice}}.
#'
#' @param inc_mat A matrix containing missing values, represented by NAs.
#' @param method A string or list of string, can be one of them, or belong to
#'   the interpolation method suitable for numerical data in the MICE
#'   package.Default is a list, c('svd','median','als','CA'), other options
#'   could be in MICE or 'knn', 'FAMD', 'PCA', 'pmm'.
#'
#' @return A list of matrices without missing values
#'
#' @importFrom  bnstruct knn.impute
#' @importFrom  stats median
#' @importFrom  softImpute softImpute complete
#' @import missMDA
#' @importFrom mice mice
#' @export
#'
#' @examples
#' # load part of beatAML data
#' data <- NIMAA::beatAML[1:10000,]
#'
#' # convert to incidence matrix
#' inc_mat <- el2IncMatrix(data, print_skim = FALSE)
#'
#' # impute
#' imputeMissingValue(inc_mat)
imputeMissingValue <- function(inc_mat,
                               method = c("svd", "median", "als", "CA")) {
  if (TRUE %in% rowSums(is.na(inc_mat)) == ncol(inc_mat)) {
    warning("Some observation row(s) are all Na, removed")
    inc_mat <- inc_mat[rowSums(is.na(inc_mat)) != ncol(inc_mat), ]
  }

  x <- inc_mat

  col_name <- colnames(x)
  row_name <- rownames(x)



  colnames(x) <- paste0("col", 1:ncol(x))
  names(col_name) <- paste0("col", 1:ncol(x))
  rownames(x) <- paste0("row", 1:nrow(x))
  names(row_name) <- paste0("row", 1:nrow(x))

  result <- list()

  if ("knn" %in% method) {
    method <- method[method != "knn"]
    knn_imputation <- bnstruct::knn.impute(x)
    colnames(knn_imputation) <- col_name[colnames(knn_imputation)]
    rownames(knn_imputation) <- row_name[rownames(knn_imputation)]
    result$knn <- base::as.matrix(knn_imputation)
  }

  if ("median" %in% method) {
    method <- method[method != "median"]
    median_imputation <- t(apply(x, 1, FUN = function(x) {
      x[is.na(x)] <- stats::median(x, na.rm = T)
      x
    }))
    colnames(median_imputation) <- col_name[colnames(median_imputation)]
    rownames(median_imputation) <- row_name[rownames(median_imputation)]
    result$median <- base::as.matrix(median_imputation)
  }

  if ("svd" %in% method) {
    method <- method[method != "svd"]
    fits_svd <- softImpute::softImpute(x, type = "svd", trace.it = F)
    svd_imputation <- softImpute::complete(x, fits_svd)
    colnames(svd_imputation) <- col_name[colnames(svd_imputation)]
    rownames(svd_imputation) <- row_name[rownames(svd_imputation)]
    result$svd <- base::as.matrix(svd_imputation)
  }

  if ("als" %in% method) {
    method <- method[method != "als"]
    fits_als <- softImpute::softImpute(x, type = "als", trace.it = F)
    als_imputation <- softImpute::complete(x, fits_als)
    colnames(als_imputation) <- col_name[colnames(als_imputation)]
    rownames(als_imputation) <- row_name[rownames(als_imputation)]
    result$als <- base::as.matrix(als_imputation)
  }

  if ("CA" %in% method) {
    method <- method[method != "CA"]
    CA_imputation <- missMDA::imputeCA(x)
    colnames(CA_imputation) <- col_name[colnames(CA_imputation)]
    rownames(CA_imputation) <- row_name[rownames(CA_imputation)]
    result$CA <- base::as.matrix(CA_imputation)
  }

  if ("FAMD" %in% method) {
    method <- method[method != "FAMD"]
    FAMD_imputation <- missMDA::imputeFAMD(x)$completeObs
    colnames(FAMD_imputation) <- col_name[colnames(FAMD_imputation)]
    rownames(FAMD_imputation) <- row_name[rownames(FAMD_imputation)]
    result$FAMD <- base::as.matrix(FAMD_imputation)
  }

  if ("PCA" %in% method) {
    method <- method[method != "PCA"]
    PCA_imputation <- missMDA::imputePCA(x)$completeObs
    colnames(PCA_imputation) <- col_name[colnames(PCA_imputation)]
    rownames(PCA_imputation) <- row_name[rownames(PCA_imputation)]
    result$PCA <- base::as.matrix(PCA_imputation)
  }

  if (!is.null(method)) {
    for (mice_method in method) {
      imp <- mice::mice(x, method = mice_method, m = 1, maxit = 1, printFlag = F)
      inc_mat_imp <- mice::complete(imp)
      colnames(inc_mat_imp) <- col_name[colnames(inc_mat_imp)]
      rownames(inc_mat_imp) <- row_name[rownames(inc_mat_imp)]
      result[[mice_method]] <- base::as.matrix(inc_mat_imp)
    }
  }
  return(result)
}
