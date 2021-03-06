% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset.R
\name{norm_mutual_info_att,Dataset-method}
\alias{norm_mutual_info_att,Dataset-method}
\title{Function to compute the normalized mutual information between all the non-numerical attributes}
\usage{
\S4method{norm_mutual_info_att}{Dataset}(object)
}
\value{
Matrix containing the computed normalized mutual informations
}
\description{
This function computes the normalized mutual information between all the non-numerical attributes
}
\examples{
MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Shop"=c("A","B","A","C"),"Expensive"=c(TRUE,TRUE,FALSE,FALSE)))
nmis <- norm_mutual_info_att(MyDataset)
}
