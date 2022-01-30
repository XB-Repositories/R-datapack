#'Function to plot the ROC curve of a numerical attribute (when the class is boolean)
#'
#'@description This function plots the ROC curve of a numerical attribute (when the class is boolean)
#'@param att Name of the numerical attribute
#'@details This function requires the additional package ggplot2
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,23,2,1,4),"Expensive"=c(TRUE,FALSE,TRUE,FALSE,FALSE)),c="Expensive")
#'plot_roc(MyDataset,"Weight")
plot_roc <- function(object,att){
  if(!requireNamespace("ggplot2")){
    stop("ggplot2 not found. Please install the required packages and try again.")
  }
  values <- fpr_tpr(object,att)
  FPR <- values$FPR
  TPR <- values$TPR
  auc <- sum(sapply(1:(length(TPR)-1), FUN= function(i,tpr,fpr){
    return((fpr[i]-fpr[i+1])*((tpr[i]-tpr[i+1])/2+tpr[i+1]))
  }, tpr=TPR, fpr=FPR))
  df_roc <- data.frame(FPR,TPR)
  ggplot2::ggplot(data=df_roc, mapping=ggplot2::aes(x=FPR, y=TPR)) + ggplot2::geom_path(col="blue",size=2) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = 1, y = 0, yend = 1), colour = "red",lty = "dashed") +
    ggplot2::annotate(geom = "text", size=8, x = 0.8, y = 0.2, label = paste("AUC: ", round(auc,2),sep="")) +
    ggplot2::ggtitle("ROC curve")
}

#'Function to plot the correlation matrix of the dataset
#'
#'@description This function plots the correlation matrix of the dataset
#'@param method Correlation measure to be used: pearson, spearman, kendall (\code{"pearson"} by default)
#'@details This function requires two additional packages, ggplot2 and reshape2
#'@examples
#'MyDataset <- dataset(list("Code"=c(1,11,2,43,12),"Weight"=c(1,23,2,1,4),"Quantity"=c(1,2,1,1,3)))
#'plot_cor(MyDataset,method="pearson")
#'plot_cor(MyDataset,method="spearman")
#'plot_cor(MyDataset,method="kendall")
plot_cor <- function(object,method="pearson"){
  if(!requireNamespace("ggplot2") | !requireNamespace("reshape2")){
    stop("ggplot2 or reshape2 not found. Please install the required packages and try again.")
  }
  object_cor <- correlation_att(object,method=method)
  df_cor <- reshape2::melt(object_cor)
  ggplot2::ggplot(df_cor, ggplot2::aes(x = Var1, y = Var2, fill = value)) + ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = round(value, 2))) +
    ggplot2::ggtitle(paste("Correlation matrix (",method,")"))
}

#'Function to plot the normalized mutual information matrix of the dataset
#'
#'@description This function plots the normalized mutual information matrix of the dataset
#'@details This function requires two additional packages, ggplot2 and reshape2
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Shop"=c("A","B","A","C"),"Expensive"=c(TRUE,TRUE,FALSE,FALSE)))
#'plot_norm_mutual_info(MyDataset)
plot_norm_mutual_info <- function(object){
  if(!requireNamespace("ggplot2") | !requireNamespace("reshape2")){
    stop("ggplot2 or reshape2 not found. Please install the required packages and try again.")
  }
  object_info <- norm_mutual_info_att(object)
  df_info <- reshape2::melt(object_info)
  ggplot2::ggplot(df_info, ggplot2::aes(x = Var1, y = Var2, fill = value)) + ggplot2::geom_tile() + ggplot2::geom_text(ggplot2::aes(label = round(value, 2))) +
    ggplot2::ggtitle("Normalized mutual information matrix")
}