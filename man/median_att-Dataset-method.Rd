% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{median_att,Dataset-method}
\alias{median_att,Dataset-method}
\title{Function to compute the median of all the numerical attributes}
\usage{
\S4method{median_att}{Dataset}(object)
}
\value{
A list containing the computed medians
}
\description{
This function computes the median of all the numerical attributes
}
\examples{
MyDataset <- dataset(list("Weight"=c(1,3,2,4,5),"Quantity"=c(34,1,23,2,1)))
medians <- median_att(MyDataset)
}
