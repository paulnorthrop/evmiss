% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{BrestSurgeMissing}
\alias{BrestSurgeMissing}
\title{Missing values in sea surge heights at Brest, France}
\format{
\code{BrestSurgeMissing} is a data frame with 162 rows (years 1846 to
2007) and the 12 variables (one for each month of the year). Each value
in the data frame gives the number of days for which the surge height data
were missing in the month in question.

The row names of \code{BrestSurgeMaxima} are the years \code{1946:2007} and the column
names are the abbreviated names of the months.
}
\source{
The dataset \code{Brest} in the \code{Renext} R package, specifically
\code{Brest$OTmissing}. Originally, the source was
\url{https://data.shom.fr/}.
}
\usage{
BrestSurgeMissing
}
\description{
Numbers of missing values in each month of the Brest sea surge heights data
\code{\link{BrestSurgeMaxima}}.
}
\examples{
head(BrestSurgeMissing)

# Proportion of missing values by year
propn_year <- rowSums(BrestSurgeMissing) /
  days_in_year(rownames(BrestSurgeMissing))
plot(rownames(BrestSurgeMissing), propn_year,
     ylab = "proportion of missing values", xlab = "year", pch = 16)

# Proportion of missing values by year and month
propn_year_month <- BrestSurgeMissing / BrestSurgeDays

# Proportion of missing values by month
plot(1:12, colMeans(propn_year_month), axes = FALSE,
        ylab = "proportion of missing values", xlab = "month", pch = 16)
axis(1, at = 1:12, labels = 1:12)
axis(2)
box()
}
\references{
Deville Y. and Bardet L. (2023). Renext: Renewal Method for
Extreme Values Extrapolation. R package version 3.1-4.
\doi{https://doi.org/10.32614/CRAN.package.Renext}
}
\seealso{
\itemize{
\item \code{\link{BrestSurgeMaxima}}: Annual maxima surge heights at Brest, France.
\item \code{\link{BrestSurgeDays}}: Number of days per month in 1846-2007.
}
}
\keyword{datasets}
