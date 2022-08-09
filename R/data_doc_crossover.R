#' Cross-over data
#'
#' A success or fail response variable with three treatments repeated over 11
#' patients.
#'
#' @details
#' In StatXact (2003), three treatment, three period cross-over clinical data
#' for 11 patients are given. This is a CMH design with t = 3, c = 2, and
#' b = 11.
#'
#' @docType data
#'
#' @usage data(crossover)
#'
#' @format A data frame with 33 rows and three columns.
#' \describe{
#'   \item{treatment}{the type of treatment applied. The three treatments are
#'   placebo, aspirin and a new drug}
#'   \item{success}{whether the treatment was a success or not}
#'   \item{patient}{the patient the treatment was applied to}
#' }
#'
#' @keywords datasets crossover
#'
#' @references
#' StatXact (2003). User Manual Volume 2. CYTEL Software.
#'
#' @source StatXact (2003)
#'
#'
#' @examples
#' attach(crossover)
#' CMH(treatment = treatment,response = success,strata = patient)
"crossover"
