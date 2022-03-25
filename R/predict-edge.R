#' Edge prediction of weighted bipartite network.
#'
#' @description This function utilizes several data imputation methods in order to predict the existence of a link between two nodes by imputing the edges' weight in a weighted bipartite network of nominal data.
#'
#' @details This function performs a variety of numerical imputation according to the user's input, and returns a list of imputed data matrices based on each method separately, such as `median` which replaces the missing values with the median of each rows (observations), `knn` is the method in package \code{bnstruct}, `als` and `svd` are methods from \code{softImpute} package, `CA`, `PCA` and `FAMD` are from \code{missMDA} package, others are from the \code{mice} package.
#'
#' @seealso \code{\link[bnstruct]{knn.impute}},
#'   \code{\link[softImpute]{softImpute}}, \code{\link[missMDA]{imputeCA}},
#'   \code{\link[missMDA]{imputeFAMD}}, \code{\link[missMDA]{imputePCA}},
#'   \code{\link[mice]{mice}}.
#'
#' @param inc_mat An incidence matrix containing missing values (edge weights), represented by NAs.
#' @param method A string or list of string. By default, it is set to this list: `c("svd", "median", "als", "CA")`. Other available methods in `MICE`, `knn`, `FAMD`, `PCA`, and `pmm`, can be called to perform at a single step.
#'
#' @return A list of matrices with original and imputed values by different methods.
#'
#' @importFrom  bnstruct knn.impute
#' @importFrom  stats median
#' @importFrom  softImpute softImpute complete
#' @import missMDA
#' @importFrom mice mice
#' @export
#'
#' @examples
#' # load part of the beatAML data
#' beatAML_data <- NIMAA::beatAML[1:10000,]
#'
#' # convert to incidence matrix
#' beatAML_incidence_matrix <- el2IncMatrix(beatAML_data)
#'
#' # predict the edges by imputation the wights
#' predictEdge(beatAML_incidence_matrix)
predictEdge <- function(inc_mat,
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
