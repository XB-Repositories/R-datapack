% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{filter_by,Dataset-method}
\alias{filter_by,Dataset-method}
\title{Function to filter the attributes in the dataset}
\usage{
\S4method{filter_by}{Dataset}(object, metric, comparator, threshold)
}
\arguments{
\item{metric}{Metric to be used during the filtering. Allowed options are: roc_auc, variance, mean and median
in the case of numerical attributes, and entropy in the case of non-numerical attributes}

\item{comparator}{Comparator operator to be used: lt, gt, le, ge, eq, neq}

\item{threshold}{Threshold value between accepted and not accepted attributes}
}
\value{
An object of class \code{\linkS4class{Dataset}}
}
\description{
This function filters all the attributes in the dataset according to the specified metric,
threshold value and comparation operator. The attributes that don't meet the condition are removed
}
\examples{
MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Weight"=c(1,3,2,12),"Quantity"=c(10,23,2,1),"Expensive"=c(TRUE,FALSE,FALSE,FALSE)))
MyDataset1 <- filter_by(MyDataset,metric="entropy",comparator="le",threshold=1)
MyDataset2 <- filter_by(MyDataset,metric="mean",comparator="gt",threshold=6)
}
