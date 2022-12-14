% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_doc_strawberry.R
\docType{data}
\name{strawberry}
\alias{strawberry}
\title{Strawberry data}
\format{
A data frame with 28 rows and four columns.
\describe{
\item{block}{the experimental block}
\item{pesticide}{the pesticide applied to the plant. Pesticide O is a control.}
\item{response}{a measure of the growth rate of strawberry plants with \code{pesticide} applied.}
\item{rank}{rank of the response}
}
}
\source{
Pearce (1960)
}
\usage{
data(strawberry)
}
\description{
Strawberry growth rates for different pesticides.
}
\details{
Pesticides are applied to strawberry plants to inhibit the growth of
weeds. The question is, do they also inhibit the growth of the strawberries?
The design is a supplemented balanced design.
}
\examples{
attach(strawberry)
lm_strawberry = lm(rank~pesticide+block)
anova(lm_strawberry)
}
\references{
Pearce, S.C. (1960). Supplemented balanced. Biometrika, 47, 263-271.
}
\keyword{datasets}
\keyword{strawberry}
