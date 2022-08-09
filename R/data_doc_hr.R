#' HR data
#'
#' Human resource ranking data.
#'
#' @details Applications for a position are vetted and ranked by Human Resource
#' (HR) professionals. The top five are interviewed by a selection committee of
#' ten. Each member of the committee gives an initial ranking of the applicants
#' and no one on the committee sees either the ranking by the HR professionals
#' or the initial rankings of the other committee members. Ties are not
#' permitted in any ranking. It is of interest to know if the rankings of the HR
#' professionals and the initial rankings of the selection committee are
#' correlated.
#'
#' @docType data
#'
#' @usage data(hr)
#'
#' @format A data frame with 50 rows and three columns.
#' \describe{
#'   \item{applicant}{the applicant to which ratings are applied}
#'   \item{applicant_ranking}{the ranking from made by the committee member}
#'   \item{committee_member}{member of the selection panel that ranks five candidates}
#' }
#'
#' @keywords datasets hr CMH
#'
#' @references
#' Rayner, J.C.W and Livingston, G. C. (2022). An Introduction to Cochran-Mantel-Haenszel Testing and Nonparametric ANOVA. Wiley.
#'
#'
#' @examples
#' attach(hr)
#' a_ij = matrix(1:5,ncol=10,nrow=5)
#' b_hj = matrix(1:5,ncol=10,nrow=5)
#' CMH(treatment = applicant, response = applicant_ranking,
#'     strata = committee_member, a_ij = a_ij, b_hj = b_hj,
#'     test_OPA = FALSE, test_GA = FALSE, test_MS = FALSE)
"hr"
