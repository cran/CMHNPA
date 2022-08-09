#' Traffic data
#'
#' Traffic light data for different light sequences, intersections, and times of
#' day.
#'
#' @details A traffic engineer conducted a study to compare the total unused red-light
#' time for five different traffic light signal sequences. The experiment was
#' conducted with a Latin square design in which blocking factors were (1) five
#' intersections and (2) five time of day periods.
#'
#'
#' @docType data
#'
#' @usage data(traffic)
#'
#' @format A data frame with 25 rows and four columns.
#' \describe{
#'   \item{minutes}{the amount of unused red-light time in minutes}
#'   \item{treatment}{the traffic light sequence}
#'   \item{intersection}{the intersection the \code{treatment} was applied}
#'   \item{time_of_day}{the time of the day the \code{treatment} was applied}
#' }
#'
#' @keywords datasets traffic LSD
#'
#' @source Kuehl, R. (2000)
#'
#' @references
#' Kuehl, R. (2000). Design of Experiments: Statistical Principles of Research Design and Analysis. Belmont, California: Duxbury Press.
#'
#' Best, D. J. and Rayner, J. C. W. (2011). Nonparametric tests for Latin squares. NIASRA Statistics Working Paper Series, 11-11.
#'
#' @examples
#' attach(traffic)
#' np_anova(ordered_vars = rank(minutes),
#' predictor_vars = data.frame(treatment,intersection,time_of_day), uvw = 1)
"traffic"
