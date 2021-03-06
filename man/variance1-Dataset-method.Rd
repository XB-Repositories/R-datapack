% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{variance1,Dataset-method}
\alias{variance1,Dataset-method}
\title{Function to compute the variance of a numerical attribute}
\usage{
\S4method{variance1}{Dataset}(object, att)
}
\arguments{
\item{att}{Name of the numerical attribute}
}
\value{
The computed variance
}
\description{
This function computes the variance of a numerical attribute
}
\examples{
MyDataset <- dataset(list("Weight"=c(1,3,2,4,5,2)))
var <- variance1(MyDataset,"Weight")
}
