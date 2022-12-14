% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_doc_whiskey.R
\docType{data}
\name{whiskey}
\alias{whiskey}
\title{Whiskey data}
\format{
A data frame with 8 rows and two columns.
\describe{
\item{maturity}{the length of time the whiskey has matured for}
\item{grade}{grade applied to the whiskey. A lower score indicates a better
whiskey}
}
}
\source{
O'Mahony (1986, p.363)
}
\usage{
data(whiskey)
}
\description{
Ratings of different whiskeys of differing maturity.
}
\examples{
attach(whiskey)
CMH(treatment = maturity, response = grade, a_ij = c(1,5,7),
b_hj = 1:3, test_OPA = FALSE, test_GA = FALSE, test_MS = FALSE)
}
\references{
Rayner, J.C.W. and Best, D.J. (2001). A Contingency Table Approach to Nonparametric Testing. Chapman & Hall/CRC: Boca Raton FL.

O'Mahony, M. (1986). Sensory Evaluation of Food - Statistical Methods and Procedures. Marcel Dekker: New York.
}
\keyword{datasets}
\keyword{whiskey}
