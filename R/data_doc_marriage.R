#' Marriage data
#'
#' Scores of 1, 2 and 3 are assigned to the responses agree, neutral and
#' disagree respectively to the proposition "Homosexuals should be able to
#' marry" and scores of 1, 2 and 3 are assigned to the religious categories
#' fundamentalist, moderate and liberal respectively.
#'
#' @docType data
#'
#' @usage data(marriage)
#'
#' @format A data frame with 133 rows and three columns.
#' \describe{
#'   \item{education}{highest level of eduction for the respondent}
#'   \item{religion}{level of religiosity. Scores of 1, 2 and 3 are assigned to the religious categories fundamentalist, moderate and liberal respectively}
#'   \item{opinion}{the response to the proposition: "Homosexuals should be able to marry". Scores of 1, 2 and 3 are assigned to the responses agree, neutral and
#' disagree respectively}
#' }
#'
#' @keywords datasets marriage
#'
#' @source Agresti (2002)
#'
#' @references
#' Agresti, A. (2002). Categorical Data Analysis, 2nd ed; Wiley: New York.
#'
#' @examples
#' attach(marriage)
#' lm(as.numeric(opinion)~religion+education)
"marriage"
