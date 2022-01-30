## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6,
  fig.height=4
)

## -----------------------------------------------------------------------------
library(datapack)

## -----------------------------------------------------------------------------
path <- system.file("data", package="datapack")
MyDataset <- from_csv(paste(path,"/fruits.csv",sep=""),cat=6,c="Expensive")

## -----------------------------------------------------------------------------
t <- read.table(paste(path,"/fruits.csv",sep=""),sep=",",header=TRUE)
MyDataset2 <- dataset(t,c="Expensive")

## -----------------------------------------------------------------------------
print_dataset(MyDataset)

## -----------------------------------------------------------------------------
t <- to_dataframe(MyDataset)
print(t)
class(t)

## -----------------------------------------------------------------------------
print("Code:"); print_data(get_attribute(MyDataset,"Code"))
print("Fruit:"); print_data(get_attribute(MyDataset,"Fruit"))
print("Quantity:"); print_data(get_attribute(MyDataset,"Quantity"))
print("Weight:"); print_data(get_attribute(MyDataset,"Weight"))
print("Color:"); print_data(get_attribute(MyDataset,"Color"))
print("Expensive:"); print_data(get_attribute(MyDataset,"Expensive"))

## -----------------------------------------------------------------------------
MyDataset <- add_attribute(MyDataset,"Shop",c("Tienda Marlo","Tienda Marlo","Tienda Pepe","Tienda Pepe","Tienda Marlo","Tienda Marlo","Tienda Marlo","Tienda Pepe","Tienda Marlo","Tienda Marlo"))
MyDataset <- to_factor(MyDataset,"Shop",levels=c("Tienda Pepe","Tienda Marlo"))
print_data(get_attribute(MyDataset,"Shop"))

## -----------------------------------------------------------------------------
to_csv(MyDataset,paste(path,"/fruits_new.csv",sep=""),header=TRUE,sep=";")

## -----------------------------------------------------------------------------
print("Mean of Weight:"); print(mean1(MyDataset,"Weight"))
print("Median of Weight:"); print(median1(MyDataset,"Weight"))
print("Variance of Weight:"); print(variance1(MyDataset,"Weight"))
print("Mean of all the numerical attributes:"); print(mean_att(MyDataset))
print("Median of all the numerical attributes:"); print(median_att(MyDataset))
print("Variance of all the numerical attributes:"); print(variance_att(MyDataset))

## -----------------------------------------------------------------------------
print("Mode of Color:"); print(mode1(MyDataset,"Color"))
print("Entropy of Color:"); print(entropy1(MyDataset,"Color"))
print("Mode of all the non-numerical attributes:"); print(mode_att(MyDataset))
print("Entropy of all the non-numerical attributes:"); print(entropy_att(MyDataset))

## -----------------------------------------------------------------------------
print("Pearson correlation between Weight and Quantity:"); print(correlation1(MyDataset,"Weight","Quantity",method="pearson"))
print("Spearman correlation between Weight and Quantity:"); print(correlation1(MyDataset,"Weight","Quantity",method="spearman"))
print("Kendall correlation between Weight and Quantity:"); print(correlation1(MyDataset,"Weight","Quantity",method="kendall"))
print("Pearson correlation matrix:"); print(correlation_att(MyDataset,method="pearson"))

## -----------------------------------------------------------------------------
plot_cor(MyDataset,method="pearson")

## -----------------------------------------------------------------------------
print("NMI Fruit and Color:"); print(norm_mutual_info1(MyDataset,"Fruit","Color"))
print("NMI matrix:"); print(norm_mutual_info_att(MyDataset))

## -----------------------------------------------------------------------------
plot_norm_mutual_info(MyDataset)

## -----------------------------------------------------------------------------
print("AUC for the Weight (Class: Expensive):"); print(roc_auc1(MyDataset,"Weight"))
print("AUC for all the numerical attributes (Class: Expensive):"); print(roc_auc_att(MyDataset))

## -----------------------------------------------------------------------------
plot_roc(MyDataset,"Weight")

## -----------------------------------------------------------------------------
print("Weight before normalizing:"); print_data(get_attribute(MyDataset, "Weight"))
MyDataset <- normalize1(MyDataset,"Weight")
print("Weight after normalizing:"); print_data(get_attribute(MyDataset, "Weight"))

## -----------------------------------------------------------------------------
print("Quantity before normalizing:"); print_data(get_attribute(MyDataset, "Quantity"))
MyDataset <- standarize1(MyDataset,"Quantity")
print("Weight after standarizing:"); print_data(get_attribute(MyDataset, "Quantity"))

## -----------------------------------------------------------------------------
print("Dataset before discretizing:"); print_dataset(MyDataset)
MyDataset <- discretize1(MyDataset,"Code",method="frequency",num_bins=5)
MyDataset <- discretize1(MyDataset,"Quantity",method="custom",cut_points=c(-1,0,1))
MyDataset <- discretize1(MyDataset,"Weight",method="width",num_bins=4)
print("Dataset after discretizing:"); print_dataset(MyDataset)

## -----------------------------------------------------------------------------
print("Entropy of the non-numerical attributes:"); print(entropy_att(MyDataset))
MyDataset <- filter_by(MyDataset,metric="entropy",threshold=2,comparator="gt")
print("Dataset after applying the filter:"); print_dataset(MyDataset)

