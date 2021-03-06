% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_cor}
\alias{plot_cor}
\title{Function to plot the correlation matrix of the dataset}
\usage{
plot_cor(object, method = "pearson")
}
\arguments{
\item{method}{Correlation measure to be used: pearson, spearman, kendall (\code{"pearson"} by default)}
}
\description{
This function plots the correlation matrix of the dataset
}
\details{
This function requires two additional packages, ggplot2 and reshape2
}
\examples{
MyDataset <- dataset(list("Code"=c(1,11,2,43,12),"Weight"=c(1,23,2,1,4),"Quantity"=c(1,2,1,1,3)))
plot_cor(MyDataset,method="pearson")
plot_cor(MyDataset,method="spearman")
plot_cor(MyDataset,method="kendall")
}
