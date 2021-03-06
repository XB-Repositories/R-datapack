% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{get_attribute,Dataset-method}
\alias{get_attribute,Dataset-method}
\title{Function to get an attribute from the dataset}
\usage{
\S4method{get_attribute}{Dataset}(object, att)
}
\arguments{
\item{att}{Name of the attribute to be returned}
}
\value{
An object of class \code{\linkS4class{Attribute}}
}
\description{
This function returns the specified attribute from the dataset
}
\examples{
MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple")))
Fruits_attribute <- get_attribute(MyDataset,"Fruit")

}
