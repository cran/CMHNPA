#' Asbestos data
#'
#' A data set showing individuals' grade of asbestosis together with their
#' length of expposure to asbestos.
#'
#'
#' @details
#' Irving J. Selikoff (1915–1992) was a chest physician and researcher who had
#' often been described as America’s foremost medical expert on asbestos related
#' diseases between the 1960s and the early 1990s.
#'
#' Through a lung clinic that he operated in New Jersey, Selikoff collected data
#' from a sample of around 1200 insulation workers in metropolitan New York in
#' 1963.
#'
#' @docType data
#'
#' @usage data(asbestos)
#'
#' @format A data frame with 1117 rows and two columns.
#' \describe{
#'   \item{exposure}{a factor representing the number of years of exposure to asbestos}
#'   \item{grade}{a factor representing the grade of asbestosis. Grade 0 represents none present}
#' }
#'
#' @keywords datasets asbestos asbestosis
#'
#' @source Selikoff IJ. Household risks with inorganic fibers. Bull N Y Acad Med. 1981 Dec;57(10):947-61.
#'
#' @references Selikoff IJ. Household risks with inorganic fibers. Bull N Y Acad Med. 1981 Dec;57(10):947-61.
#'
#' @examples
#' attach(asbestos)
#' KW(treatment = exposure, response = grade)
"asbestos"
