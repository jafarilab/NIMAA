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
#' @source Huang, L., Xie, D., Yu, Y., Liu, H., Shi, Y., Shi, T., & Wen, C. (2018). TCMID 2.0: a comprehensive resource for TCM. Nucleic acids research, 46(D1), D1117–D1120. \url{https://academic.oup.com/nar/article/46/D1/D1117/4584630}
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
#' Charles Robertson's detailed collections provide as an amazing resource for long-term comparisons of numerous terrestrial plant and insect species collected in a single location. From 1884 through 1916, Robertson observed and collected 1429 insect species that visited 456 flower of plant species. He gathered this dataset in Macoupin County, Illinois, USA, within a 16-kilometer radius of Carlinville.
#'
#' @format A tibble with two variables:
#' \describe{
#' \item{\code{plant}}{Scientific Latin names of plants}
#' \item{\code{pollinator}}{Scientific Latin names of pollinators for the \code{plant}}
#' }
#' @source Marlin, J. C., & LaBerge, W. E. (2001). The native bee fauna of Carlinville, Illinois, revisited after 75 years: a case for persistence. Conservation Ecology, 5(1). \url{http://www.ecologia.ib.usp.br/iwdb/html/robertson_1929.html}
"robertson"
