#' beatAML
#'
#' The Beat AML program produced a comprehensive dataset on acute myeloid leukemia (AML) that included genomic data (i.e., whole-exome sequencing and RNA sequencing), ex vivo drug response, and clinical data. We have included only the drug response data from this program in this package.
#'
#' @format A tibble with three variables:
#' \describe{
#' \item{\code{inhibitor}}{names of inhibitors}
#' \item{\code{patient_id}}{anonymous patient ID}
#' \item{\code{median}}{the median of drug response at corresponding \code{inhibitor} and \code{patient_id}}
#' }
#' @source Tyner, Jeffrey W., et al.(2018), Functional Genomic Landscape of Acute Myeloid Leukaemia. Nature 562 (7728): 526–31. \url{http://vizome.org/aml/}
"beatAML"

#' herbIngredient
#'
#' The second version of the TCMID database was used as one of the large datasets in the TCM herb field. TCMID includes additional ingredient-specific experimental data based on herbal mass spectrometry spectra. The herb and ingredient associations from this database are provided here.
#'
#'
#' @format A tibble with two variables:
#' \describe{
#' \item{\code{herb}}{Scientific Latin names of available herbs in TCM database}
#' \item{\code{pubchem_id}}{unique compound id (CID) of ingredients based on PubChem database for available \code{herb} in TCMID database}
#' }
#' @source Huang, L., Xie, D., Yu, Y., Liu, H., Shi, Y., Shi, T., & Wen, C. (2018). TCMID 2.0: a comprehensive resource for TCM. Nucleic acids research, 46(D1), D1117–D1120. \url{http://www.megabionet.org/tcmid/}
"herbIngredient"

#' drugComb
#'
#' DrugComb is a web-based portal for storing and analyzing drug combination screening datasets, containing 739,964 drug combination experiments across 2320 cancer cell lines and 8397 drugs. Single drug screening was extracted and provided here as a subset of drug combination experiments.
#'
#' @format A tibble with three variables:
#' \describe{
#' \item{\code{inhibitor}}{names of inhibitors}
#' \item{\code{cell_line}}{cell_line for each \code{inhibitor}}
#' \item{\code{median}}{the median of drug response at corresponding \code{inhibitor} and \code{cell_line}}
#' }
#'
#' @source Zheng, S., Aldahdooh, J., Shadbahr, T., Wang, Y., Aldahdooh, D., Bao, J., Wang, W., Tang, J. (2021). DrugComb update: a more comprehensive drug sensitivity data repository and analysis portal, Nucleic Acids Research, 49(W1), W174–W184.  \url{https://academic.oup.com/nar/article/49/W1/W174/6290546}
#'
"drugComb"

#' robertson
#'
#' The author listed 1429 animal species visiting flowers of 456 plant species that grew in a small area in soutWhwestern Illinois, USA. Marlin and LaBerge (2001) describe Robertson’s methods.
#'
#' @format A tibble with two variables:
#' \describe{
#' \item{\code{plant}}{names of plants}
#' \item{\code{pollinator}}{names of pollinators for \code{plant}}
#' }
#' @source \url{http://www.ecologia.ib.usp.br/iwdb/html/robertson_1929.html}
"robertson"
