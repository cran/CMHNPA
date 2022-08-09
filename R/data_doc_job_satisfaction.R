#' Job Satisfaction data
#'
#' Job satisfaction data based on income and gender.
#'
#' @details The data relate job satisfaction and income in males and females.
#' Gender induces two strata; the treatments are income with categories scored
#' 3, 10, 20 and 35 while the response is job satisfaction with categories
#' scored 1, 3, 4, and 5.
#'
#' @docType data
#'
#' @usage data(job_satisfaction)
#'
#' @format A data frame with 104 rows and three columns.
#' \describe{
#'   \item{income}{income level categorised}
#'   \item{satisfaction}{the level of job satisfaction for the respondent}
#'   \item{gender}{gender of the respondent}
#' }
#'
#' @keywords datasets job_satisfaction
#'
#' @references Agresti, A. (2003). Categorical Data Analysis. Hoboken: John Wiley & Sons.
#'
#'
#' @source Agresti (2003)
#'
#'
#' @examples
#' attach(job_satisfaction)
#' a_ij = matrix(rep(c(3,10,20,35),2),nrow=4)
#' b_hj = matrix(rep(c(1,3,4,5),2),nrow=4)
#' unconditional_CMH(treatment = income, response = satisfaction,
#' strata = gender, U = 2, V = 2, a_ij = a_ij,
#' b_hj = b_hj)
"job_satisfaction"
