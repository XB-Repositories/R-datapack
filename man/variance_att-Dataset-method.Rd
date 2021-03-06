% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{variance_att,Dataset-method}
\alias{variance_att,Dataset-method}
\title{Function to compute the variance of all the numerical attributes}
\usage{
\S4method{variance_att}{Dataset}(object)
}
\value{
A list containing the computed variances
}
\description{
This function computes the variance of all the numerical attributes
}
\examples{
MyDataset <- dataset(list("Weight"=c(1,3,2,4,5),"Quantity"=c(34,1,23,2,1)))
vars <- variance_att(MyDataset)
}
