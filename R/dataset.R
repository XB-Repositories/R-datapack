checkValidity_dt <- function(object){
  if(length(object@data)>0){
    lg1 <- unlist(lapply(object@data,function(x){return(class(x)=="Attribute")}))
    lg2 <- unlist(lapply(object@data,function(x){return(length(x@value)==object@length)}))
    if(!all(lg1)){
      stop("All elements in the dataset must be 'Attributes'.")
    }
    if(!all(lg2)){
      stop("All attributes in the dataset must be the same length.")
    }
  }
  if(object@class_att!="" && !(object@class_att %in% names(object@data))){
    stop("Class attribute must be part of the dataset.")
  }
  return(TRUE)
}

#'An S4 class to represent a Dataset
#'
#'@slot data List containing the attributes of the dataset (class \code{\linkS4class{Attribute}})
#'@slot class_att Name of the class variable
#'@slot length Number of instances in the dataset
#'
setClass(Class="Dataset", 
         slots=c("data"="list", "class_att"="character","length"="numeric"),
         prototype=list(data=list(), class_att="", length=0), validity = checkValidity_dt)

#'Basic constructor of the Dataset class
#'
#'@description This function creates an object of class \code{\linkS4class{Dataset}}
#'
#'@param data List or dataframe containing the data
#'@param c Name of the class variable (\code{""} by default)
#'@return An object of class \code{\linkS4class{Dataset}} that represents the dataset
#'
dataset <- function(data,c=""){
  l <- as.list(data)
  if(length(l)>0){length=length(l[[1]])}else{length=0}
  d <- lapply(l, function(x){return(attribute(x))})
  object <- new("Dataset",data=d,class_att=c,length=length)
  return(object)
}

setGeneric("get_attribute", function(object,att) standardGeneric("get_attribute"))

#'Function to get an attribute from the dataset
#'
#'@description This function returns the specified attribute from the dataset
#'@param att Name of the attribute to be returned
#'@return An object of class \code{\linkS4class{Attribute}}
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple")))
#'Fruits_attribute <- get_attribute(MyDataset,"Fruit")
#'
setMethod(f="get_attribute", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            return(object@data[[att]])
          })

setGeneric("add_attribute", function(object,key,att) standardGeneric("add_attribute"))

#'Function to add a new attribute to the dataset
#'
#'@description This function adds the specified attribute to the dataset
#'@param key Name of the attribute to be added
#'@param att Vector containing the attribute data
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple")))
#'Weight_data <- c(1,4,2)
#'MyDataset <- add_attribute(MyDataset,"Weight",Weight_data)
setMethod(f="add_attribute", signature="Dataset",
          definition= function(object,key,att){
            if((key %in% names(object@data))){
              stop("The attribute already exists.")
            }
            if(length(att)!=object@length){
              stop("The length of the new attribute must be the same as the number of instances in the dataset.")
            }
            new_object <- dataset(list())
            new_object@data <- object@data
            new_object@data[[key]] <- attribute(att)
            new_object@class_att <- object@class_att
            new_object@length <- object@length
            return(new_object)
          })

setGeneric("remove_attribute", function(object,att) standardGeneric("remove_attribute"))

#'Function to remove an attribute from the dataset
#'
#'@description This function removes the specified attribute from the dataset
#'@param att Name of the attribute to be removed
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple")))
#'MyDataset <- remove_attribute(MyDataset,"Fruit")
setMethod(f="remove_attribute", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            new_object <- dataset(list())
            new_object@data <- object@data
            new_object@data[[att]] <- NULL
            if(att!=object@class_att){
              new_object@class_att <- object@class_att
            }else{
              new_object@class_att <- ""
            }
            if(length(new_object@data)>0){
              new_object@length <- object@length
            }else{
              new_object@length <- 0
            }
            return(new_object)
          })

setGeneric("to_factor", function(object,att,levels=NULL) standardGeneric("to_factor"))

#'Function to convert a non-numerical attribute to factor
#'
#'@description This function converts the specified non-numerical attribute to factor
#'@param key Name of the non-numerical attribute to be converted
#'@param levels Vector containing the levels of the factor attribute (\code{NULL} by default)
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple")))
#'MyDataset <- to_factor(MyDataset,"Fruit",levels=c("Apple","Orange"))
setMethod(f="to_factor", signature="Dataset",
          definition= function(object,att,levels=NULL){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            factorized_object <- dataset(list())
            factorized_object@data <- object@data
            factorized_object@data[[att]] <- to_factor_(factorized_object@data[[att]],levels=levels)
            factorized_object@class_att <- object@class_att
            factorized_object@length <- object@length
            return(factorized_object)
          })

setGeneric("to_csv", function(object,f,header=TRUE,sep=",") standardGeneric("to_csv"))

#'Function to write the dataset to a csv file
#'
#'@description This function writes the dataset to a csv file
#'@param f Path of the csv file
#'@param header Boolean that indicates whether the header should be included or not (\code{TRUE} by default)
#'@param sep Character that is used to separate the data in the csv format (\code{","} by default)
setMethod(f = "to_csv", signature = "Dataset",
          definition = function(object,f,header=TRUE,sep=","){
            t <- do.call(cbind, lapply(object@data, function(x) { return(as.data.frame(x@value))}))
            colnames(t) <- names(object@data)
            tryCatch( {write.table(t,f,sep=sep,col.names=header,row.names=FALSE,quote=FALSE)},
                      error = function(e){
                        stop("Csv file can't be written with the given parameters. Format: dataset, output file (string), header (none or logical), separator (none or string).")
                      })
          })

setGeneric("to_dataframe", function(object) standardGeneric("to_dataframe"))

#'Function to convert the dataset to a dataframe
#'
#'@description This function converts the dataset to a dataframe object
#'@return A dataframe object containing the dataset data
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple")))
#'MyDataframe <- to_dataframe(MyDataset)
setMethod(f = "to_dataframe", signature = "Dataset",
          definition = function(object){
            if(length(object@data)==0) return(data.frame())
            t <- do.call(cbind, lapply(object@data, function(x) { return(as.data.frame(x@value))}))
            colnames(t) <- names(object@data)
            return(t)
          })

setGeneric("print_dataset", function(object) standardGeneric("print_dataset"))

#'Function to print the dataset data
#'
#'@description This function prints the dataset data
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple")))
#'print_dataset(MyDataset)
setMethod(f = "print_dataset", signature = "Dataset",
          definition= function(object){
            t <- to_dataframe(object)
            print(t)
            print(paste0("Class: ", object@class_att))
          })

setGeneric("variance1", function(object,att) standardGeneric("variance1"))

#'Function to compute the variance of a numerical attribute
#'
#'@description This function computes the variance of a numerical attribute
#'@param att Name of the numerical attribute
#'@return The computed variance
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,4,5,2)))
#'var <- variance1(MyDataset,"Weight")
setMethod(f= "variance1", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            return(variance_(object@data[[att]]))
          })

setGeneric("variance_att", function(object) standardGeneric("variance_att"))

#'Function to compute the variance of all the numerical attributes
#'
#'@description This function computes the variance of all the numerical attributes
#'@return A list containing the computed variances
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,4,5),"Quantity"=c(34,1,23,2,1)))
#'vars <- variance_att(MyDataset)
setMethod(f= "variance_att", signature="Dataset",
          definition= function(object){
            return(lapply(object@data, function(x) { if(!is.vector(x@value,mode="numeric")) {return(NaN)} else {return(variance_(x))}}))
          })

setGeneric("mean1", function(object,att) standardGeneric("mean1"))

#'Function to compute the mean of a numerical attribute
#'
#'@description This function computes the mean of a numerical attribute
#'@param att Name of the numerical attribute
#'@return The computed mean
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,4,5,2)))
#'mean <- mean1(MyDataset,"Weight")
setMethod(f= "mean1", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            return(mean_(object@data[[att]]))
          })

setGeneric("mean_att", function(object) standardGeneric("mean_att"))

#'Function to compute the mean of all the numerical attributes
#'
#'@description This function computes the mean of all the numerical attributes
#'@return A list containing the computed means
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,4,5),"Quantity"=c(34,1,23,2,1)))
#'means <- mean_att(MyDataset)
setMethod(f= "mean_att", signature="Dataset",
          definition= function(object){
            return(lapply(object@data, function(x) { if(!is.vector(x@value,mode="numeric")) {return(NaN)} else {return(mean_(x))}}))
          })

setGeneric("median1", function(object,att) standardGeneric("median1"))

#'Function to compute the median of a numerical attribute
#'
#'@description This function computes the median of a numerical attribute
#'@param att Name of the numerical attribute
#'@return The computed median
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,4,5,2)))
#'median <- median1(MyDataset,"Weight")
setMethod(f= "median1", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            return(median_(object@data[[att]]))
          })

setGeneric("median_att", function(object) standardGeneric("median_att"))

#'Function to compute the median of all the numerical attributes
#'
#'@description This function computes the median of all the numerical attributes
#'@return A list containing the computed medians
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,4,5),"Quantity"=c(34,1,23,2,1)))
#'medians <- median_att(MyDataset)
setMethod(f= "median_att", signature="Dataset",
          definition= function(object){
            return(lapply(object@data, function(x) { if(!is.vector(x@value,mode="numeric")) {return(NaN)} else {return(median_(x))}}))
          })

setGeneric("entropy1", function(object,att) standardGeneric("entropy1"))

#'Function to compute the entropy of a non-numerical attribute
#'
#'@description This function computes the entropy of a non-numerical attribute
#'@param att Name of the non-numerical attribute
#'@return The computed entropy
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon")))
#'entropy <- entropy1(MyDataset,"Fruit")
setMethod(f= "entropy1", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            return(entropy_(object@data[[att]]))
          })

setGeneric("entropy_att", function(object) standardGeneric("entropy_att"))

#'Function to compute the entropy of all the non-numerical attributes
#'
#'@description This function computes the entropy of all the non-numerical attributes
#'@return A list containing the computed entropies
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Expensive"=c(TRUE,TRUE,TRUE,FALSE)))
#'entropies <- entropy_att(MyDataset)
setMethod(f= "entropy_att", signature="Dataset",
          definition= function(object){
            return(lapply(object@data, function(x) { if(!is.vector(x@value,mode="logical") && !is.vector(x@value,mode="character") && !class(x@value) == "factor") {return(NaN)} else {entropy_(x)}}))
          })

setGeneric("mode1", function(object,att) standardGeneric("mode1"))

#'Function to compute the mode of a non-numerical attribute
#'
#'@description This function computes the mode of a non-numerical attribute.
#'If there is a tie, the value that appears first is returned
#'@param att Name of the non-numerical attribute
#'@return The computed mode
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon")))
#'mode <- mode1(MyDataset,"Fruit")
setMethod(f= "mode1", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            return(mode_(object@data[[att]]))
          })

setGeneric("mode_att", function(object) standardGeneric("mode_att"))

#'Function to compute the mode of all the non-numerical attributes
#'
#'@description This function computes the mode of all the non-numerical attributes.
#'If there is a tie, the value that appears first is returned
#'@return A list containing the computed modes
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Expensive"=c(TRUE,TRUE,TRUE,FALSE)))
#'modes <- mode_att(MyDataset)
setMethod(f= "mode_att", signature="Dataset",
          definition= function(object){
            return(lapply(object@data, function(x) { if(!is.vector(x@value,mode="logical") && !is.vector(x@value,mode="character") && !class(x@value) == "factor") {return(NaN)} else {return(mode_(x))}}))
          })

setGeneric("fpr_tpr", function(object,att) standardGeneric("fpr_tpr"))

#Function that is used to calculate the FPR and TPR scores during the computation of the ROC curve.
setMethod(f="fpr_tpr", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data)) || !is.vector(object@data[[att]]@value,mode="numeric")){
              stop("Only existing numerical attributes can be used as predictor variables.")
            }
            if(object@class_att == ""){
              stop("Class attribute not specified.")
            }else if(!is.vector(object@data[[object@class_att]]@value,mode="logical")){
              stop("Class must be boolean.")
            }else if(length(unique(object@data[[object@class_att]]@value))==1){
              stop("The ROC curve is not defined when the class attribute contains only one class.")
            }
            df <- data.frame(object@data[[att]]@value, object@data[[object@class_att]]@value)
            df_ordered <- df[order(df[,1]),]
            length_df <- length(df[,1])
            cut_points <- c(findInterval(unique(df_ordered[,1]),df_ordered[,1],left.open=TRUE)+1,length_df+1)
            TP <- sapply(cut_points, FUN= function(i,data){
              data_s <- tail(data,length_df-i+1)
              return(length(data_s[data_s[,2]==TRUE,1]))
            }, data=df_ordered)
            TN <- sapply(cut_points, FUN= function(i,data){
              data_s <- head(data,i-1)
              return(length(data_s[data_s[,2]==FALSE,1]))
            }, data=df_ordered)
            TPR <- TP/(TP+(cut_points-1-TN))
            FPR <- ((length_df-cut_points+1)-TP)/(((length_df-cut_points+1)-TP)+TN)
            return(list("FPR"= FPR, "TPR"= TPR))
          })

setGeneric("roc_auc1", function(object,att) standardGeneric("roc_auc1"))

#'Function to compute the AUC of a numerical attribute (when the class is boolean)
#'
#'@description This function computes the AUC of a numerical attribute (when the class is boolean)
#'@param att Name of the numerical attribute
#'@return The computed AUC
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,23,2,1,4),"Expensive"=c(TRUE,FALSE,TRUE,FALSE,FALSE)),c="Expensive")
#'auc <- roc_auc1(MyDataset,"Weight")
setMethod(f="roc_auc1", signature="Dataset",
          definition= function(object,att){
            values <- fpr_tpr(object,att)
            FPR <- values$FPR
            TPR <- values$TPR
            return(sum(sapply(1:(length(TPR)-1), FUN= function(i,tpr,fpr){
              return((fpr[i]-fpr[i+1])*((tpr[i]-tpr[i+1])/2+tpr[i+1]))
            }, tpr=TPR, fpr=FPR)))
          })

setGeneric("roc_auc_att", function(object) standardGeneric("roc_auc_att"))

#'Function to compute the AUC of all the numerical attributes (when the class is boolean)
#'
#'@description This function computes the AUC of all the numerical attributes (when the class is boolean)
#'@return A list containing the computed AUCs
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,23,2,1,4),"Quantity"=c(1,2,1,1,3),"Expensive"=c(TRUE,FALSE,TRUE,FALSE,FALSE)),c="Expensive")
#'aucs <- roc_auc_att(MyDataset)
setMethod(f="roc_auc_att", signature="Dataset",
          definition= function(object){
            if(object@class_att == ""){
              stop("Class attribute not specified.")
            }else if(!is.vector(object@data[[object@class_att]]@value,mode="logical")){
              stop("Class must be boolean.")
            }else if(length(unique(object@data[[object@class_att]]@value))==1){
              stop("The ROC curve is not defined when the class attribute contains only one class.")
            }
            return(lapply(names(object@data), function(i) { if(!is.vector(object@data[[i]]@value,mode="numeric")) {return(NaN)} else {return(roc_auc1(object,i))}}))
          })

setGeneric("correlation1", function(object,att_A,att_B,method="pearson") standardGeneric("correlation1"))

#'Function to compute the correlation between two numerical attributes
#'
#'@description This function computes the correlation between two numerical attributes
#'@param att_A Name of the first numerical attribute
#'@param att_B Name of the second numerical attribute
#'@param method Correlation measure to be used: pearson, spearman, kendall (\code{"pearson"} by default)
#'@return The computed correlation
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,23,2,1,4),"Quantity"=c(1,2,1,1,3)))
#'cor_p <- correlation1(MyDataset,"Weight","Quantity",method="pearson")
#'cor_s <- correlation1(MyDataset,"Weight","Quantity",method="spearman")
#'cor_k <- correlation1(MyDataset,"Weight","Quantity",method="kendall")
setMethod(f="correlation1", signature="Dataset", 
          definition= function(object,att_A,att_B,method="pearson"){
            if(!(att_A %in% names(object@data)) || !is.vector(object@data[[att_A]]@value,mode="numeric") || !(att_B %in% names(object@data)) || !is.vector(object@data[[att_B]]@value,mode="numeric")){
              stop("Correlation can only be computed between existing numerical attributes.")
            }
            if(method!="pearson" && method!="spearman" && method!="kendall"){
              stop("Invalid correlation measure. Allowed methods are: pearson, spearman, kendall.")
            }
            return(cor(object@data[[att_A]]@value, object@data[[att_B]]@value,method=method))
          })

setGeneric("correlation_att", function(object,method="pearson") standardGeneric("correlation_att"))

#'Function to compute the correlation between all the numerical attributes
#'
#'@description This function computes the correlation between all the numerical attributes
#'@param method Correlation measure to be used: pearson, spearman, kendall (\code{"pearson"} by default)
#'@return A matrix containing the computed correlations
#'@examples
#'MyDataset <- dataset(list("Code"=c(1,11,2,43,12),"Weight"=c(1,23,2,1,4),"Quantity"=c(1,2,1,1,3)))
#'cors_p <- correlation_att(MyDataset,method="pearson")
#'cors_s <- correlation_att(MyDataset,method="spearman")
#'cors_k <- correlation_att(MyDataset,method="kendall")
setMethod(f="correlation_att", signature="Dataset",
          definition= function(object,method="pearson"){
            if(method!="pearson" && method!="spearman" && method!="kendall"){
              stop("Invalid correlation measure. Allowed methods are: pearson, spearman, kendall.")
            }
            grid <- expand.grid(names(object@data), names(object@data))
            m <- matrix(nrow=length(object@data), byrow=TRUE, apply(grid, 1, function(x){ if(!is.vector(object@data[[x["Var1"]]]@value,mode="numeric") || !is.vector(object@data[[x["Var2"]]]@value,mode="numeric")) {return(NaN)} else {return(correlation1(object,x["Var1"],x["Var2"],method=method))}}))
            rownames(m) <- names(object@data)
            colnames(m) <- names(object@data)
            return(m)
          })

setGeneric("norm_mutual_info1", function(object,att_A,att_B) standardGeneric("norm_mutual_info1"))

#'Function to compute the normalized mutual information between two non-numerical attributes
#'
#'@description This function computes the normalized mutual information between two non-numerical attributes
#'@param att_A Name of the first non-numerical attribute
#'@param att_B Name of the second non-numerical attribute
#'@return The computed normalized mutual information
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Expensive"=c(TRUE,TRUE,FALSE,FALSE)))
#'nmi <- norm_mutual_info1(MyDataset,"Fruit","Expensive")
setMethod(f="norm_mutual_info1", signature="Dataset",
          definition= function(object,att_A,att_B){
            if(!(att_A %in% names(object@data)) || !(class(object@data[[att_A]]@value) == "character" || class(object@data[[att_A]]@value) == "logical" || class(object@data[[att_A]]@value) == "factor") || !(att_B %in% names(object@data)) || !(class(object@data[[att_B]]@value) == "character" || class(object@data[[att_B]]@value) == "logical" || class(object@data[[att_B]]@value) == "factor")){
              stop("Normalized mutual information can only be computed between existing logical, string and factor attributes.")
            }
            H_x <- entropy_(object@data[[att_A]])
            H_y <- entropy_(object@data[[att_B]])
            H_xy <- 0
            tot <- length(object@data[[att_B]]@value)
            col_vals <- table(object@data[[att_B]]@value)
            for(value in unique(object@data[[att_B]]@value)){
              H_xy <- H_xy + (col_vals[[as.character(value)]]/tot)*entropy_(attribute(object@data[[att_A]]@value[ifelse(object@data[[att_B]]@value==value,TRUE,FALSE)]))
            }
            return((2*(H_x - H_xy))/(H_x + H_y))
          })

setGeneric("norm_mutual_info_att", function(object) standardGeneric("norm_mutual_info_att"))

#'Function to compute the normalized mutual information between all the non-numerical attributes
#'
#'@description This function computes the normalized mutual information between all the non-numerical attributes
#'@return Matrix containing the computed normalized mutual informations
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Shop"=c("A","B","A","C"),"Expensive"=c(TRUE,TRUE,FALSE,FALSE)))
#'nmis <- norm_mutual_info_att(MyDataset)
setMethod(f="norm_mutual_info_att", signature="Dataset",
          definition= function(object){
            grid <- expand.grid(names(object@data), names(object@data))
            m <- matrix(nrow=length(object@data), byrow=TRUE, apply(grid, 1, function(x){ 
              if(!(class(object@data[[x["Var1"]]]@value) == "character" || class(object@data[[x["Var1"]]]@value) == "logical" || class(object@data[[x["Var1"]]]@value) == "factor") 
                 || !(class(object@data[[x["Var2"]]]@value) == "character" || class(object@data[[x["Var2"]]]@value) == "logical" || class(object@data[[x["Var2"]]]@value) == "factor")) {
                return(NaN)
              } else {
                return(norm_mutual_info1(object,x["Var1"],x["Var2"]))}
              }))
            rownames(m) <- names(object@data)
            colnames(m) <- names(object@data)
            return(m)
          })

setGeneric("discretize1", function(object, att, method, num_bins=NULL, cut_points=NULL) standardGeneric("discretize1"))

#'Function to discretize a numerical attribute
#'
#'@description This function discretizes the specified numerical attribute
#'@param att Name of the numerical attribute to be discretized
#'@param method Discretization method to be used: frequency, width, custom
#'@param num_bins Number of intervals (only for frequency and width methods, \code{NULL} by default)
#'@param cut_points Vector containing the interval cut points (only for custom method, \code{NULL} by default)
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,12,10,5)))
#'MyDataset1 <- discretize1(MyDataset,"Weight",method="frequency",num_bins=2)
#'MyDataset2 <- discretize1(MyDataset,"Weight",method="width",num_bins=2)
#'MyDataset3 <- discretize1(MyDataset,"Weight",method="custom",cut_points=c(4,11))
setMethod(f="discretize1", signature="Dataset",
          definition= function(object, att, method, num_bins=NULL, cut_points=NULL){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            if(method=="frequency"){
              param <- num_bins
              f <- getFunction("discretizeEF_")
            }else if(method=="width"){
              param <- num_bins
              f <- getFunction("discretizeEW_")
            }else if(method=="custom"){
              param <- cut_points
              f <- getFunction("discretize_")
            }else{
              stop("Invalid discretization method. Accepted methods are: frequency, width, custom.")
            }
            discretized_object <- dataset(list())
            discretized_object@data <- object@data
            discretized_object@data[[att]] <- f(discretized_object@data[[att]],param)[[1]]
            discretized_object@class_att <- object@class_att
            discretized_object@length <- object@length
            return(discretized_object)
          })

setGeneric("discretize_att", function(object, method, num_bins) standardGeneric("discretize_att"))

#'Function to discretize all the numerical attributes
#'
#'@description This function discretizes all the numerical attributes
#'@param method Discretization method to be used: frequency, width
#'@param num_bins Number of intervals
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,12,10,5),"Quantity"=c(10,4,5,2,7,56)))
#'MyDataset1 <- discretize_att(MyDataset,method="frequency",num_bins=2)
#'MyDataset2 <- discretize_att(MyDataset,method="width",num_bins=2)
setMethod(f="discretize_att", signature="Dataset",
          definition= function(object, method, num_bins){
            if(method=="frequency"){
              f <- getFunction("discretizeEF_")
            }else if(method=="width"){
              f <- getFunction("discretizeEW_")
            }else{
              stop("Invalid discretization method. Accepted methods are: frequency, width.")
            }
            if(class(num_bins) != "numeric" || num_bins != as.integer(num_bins)){
              stop("Number of intervals must be an integer.")
            }
            if(num_bins < 2){
              stop("Number of intervals must be equal to or greater than 2.")
            }
            discretized_object <- dataset(list())
            discretized_object@data <- append(discretized_object@data, lapply(object@data, function(x) { if(!is.vector(x@value,mode="numeric")) {return(x)} else {return(f(x,num_bins)[[1]])}}))
            discretized_object@class_att <- object@class_att
            return(discretized_object)
          })

setGeneric("standarize1", function(object,att) standardGeneric("standarize1"))

#'Function to standarize a numerical attribute
#'
#'@description This function standarizes the specified numerical attribute
#'@param att Name of the numerical attribute to be standarized
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,12,10,5)))
#'MyDataset <- standarize1(MyDataset,"Weight")
setMethod(f="standarize1", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            standarized_object <- dataset(list())
            standarized_object@data <- object@data
            standarized_object@data[[att]] <- standarize_(standarized_object@data[[att]])
            standarized_object@class_att <- object@class_att
            standarized_object@length <- object@length
            return(standarized_object)
          })

setGeneric("standarize_att", function(object) standardGeneric("standarize_att"))

#'Function to standarize all the numerical attributes
#'
#'@description This function standarizes all the numerical attributes
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,12,10,5),"Quantity"=c(10,23,2,1,0,2)))
#'MyDataset <- standarize_att(MyDataset)
setMethod(f="standarize_att", signature="Dataset",
          definition= function(object){
            standarized_object <- dataset(list())
            standarized_object@data <- append(standarized_object@data, lapply(object@data, function(x) { if(!is.vector(x@value,mode="numeric")) {return(x)} else {return(standarize_(x))}}))
            standarized_object@class_att <- object@class_att
            return(standarized_object)
          })

setGeneric("normalize1", function(object,att) standardGeneric("normalize1"))

#'Function to normalize a numerical attribute
#'
#'@description This function normalizes the specified numerical attribute
#'@param att Name of the numerical attribute to be normalized
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,12,10,5)))
#'MyDataset <- normalize1(MyDataset,"Weight")
setMethod(f="normalize1", signature="Dataset",
          definition= function(object,att){
            if(!(att %in% names(object@data))){
              stop("Attribute not found.")
            }
            normalized_object <- dataset(list())
            normalized_object@data <- object@data
            normalized_object@data[[att]] <- normalize_(normalized_object@data[[att]])
            normalized_object@class_att <- object@class_att
            normalized_object@length <- object@length
            return(normalized_object)
          })

setGeneric("normalize_att", function(object) standardGeneric("normalize_att"))

#'Function to normalize all the numerical attributes
#'
#'@description This function normalizes all the numerical attributes
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Weight"=c(1,3,2,12,10,5),"Quantity"=c(10,23,2,1,0,2)))
#'MyDataset <- normalize_att(MyDataset)
setMethod(f="normalize_att", signature="Dataset",
          definition= function(object){
            normalized_object <- dataset(list())
            normalized_object@data <- append(normalized_object@data, lapply(object@data, function(x) { if(!is.vector(x@value,mode="numeric")) {return(x)} else {return(normalize_(x))}}))
            normalized_object@class_att <- object@class_att
            return(normalized_object)
          })

setGeneric("filter_by", function(object,metric,comparator,threshold) standardGeneric("filter_by"))

#'Function to filter the attributes in the dataset
#'
#'@description This function filters all the attributes in the dataset according to the specified metric,
#'threshold value and comparation operator. The attributes that don't meet the condition are removed
#'@param metric Metric to be used during the filtering. Allowed options are: roc_auc, variance, mean and median
#'in the case of numerical attributes, and entropy in the case of non-numerical attributes
#'@param comparator Comparator operator to be used: lt, gt, le, ge, eq, neq
#'@param threshold Threshold value between accepted and not accepted attributes
#'@return An object of class \code{\linkS4class{Dataset}}
#'@examples
#'MyDataset <- dataset(list("Fruit"=c("Apple","Orange","Apple","Watermelon"),"Weight"=c(1,3,2,12),"Quantity"=c(10,23,2,1),"Expensive"=c(TRUE,FALSE,FALSE,FALSE)))
#'MyDataset1 <- filter_by(MyDataset,metric="entropy",comparator="le",threshold=1)
#'MyDataset2 <- filter_by(MyDataset,metric="mean",comparator="gt",threshold=6)
setMethod(f="filter_by",signature="Dataset",
          definition= function(object,metric,comparator,threshold){
            lt <- function(x,y){return(x<y)}
            gt <- function(x,y){return(x>y)}
            le <- function(x,y){return(x<=y)}
            ge <- function(x,y){return(x>=y)}
            eq <- function(x,y){return(x==y)}
            neq <- function(x,y){return(x!=y)}
            if(class(threshold) != "numeric"){
              stop("The threshold value must be integer or float.")
            }
            if(metric=="entropy"){
              values <- entropy_att(object)
            }else if(metric=="roc_auc"){
              values <- roc_auc_att(object)
            }else if(metric=="variance"){
              values <- variance_att(object)
            }else if(metric=="mean"){
              values <- mean_att(object)
            }else if(metric=="median"){
              values <- median_att(object)
            }else{
              stop("Invalid metric. Accepted metrics: entropy, roc_auc, variance, mean, median.")
            }
            if(comparator!="lt" && comparator!="gt" && comparator!="le" && comparator!="ge" && comparator!="eq" && comparator!="neq"){
              stop("Invalid comparator. Accepted comparators: lt, gt, le, ge, eq, neq.")
            }
            object@data <- object@data[unlist(lapply(names(object@data), function(x){return(object@class_att==x || is.nan(values[[x]]) || get(comparator)(values[[x]],threshold))}))]
            return(object)
          })

#'Function to read the dataset from a csv file
#'
#'@description This function reads the dataset from a csv file
#'@param f Path of the csv file
#'@param cat Minimum number of possible values for a character attribute. Any character attribute with less than
#'cat possible values is converted to factor (\code{5} by default)
#'@param header Boolean that indicates whether the csv file contains a header or not (\code{TRUE} by default)
#'@param sep Character that is used to separate the data in the csv file (\code{","} by default)
#'@param c Name of the class variable (\code{""} by default)
#'@return An object of class \code{\linkS4class{Dataset}}
from_csv <- function(f,cat=5,header=TRUE,sep=",",c=""){
  if(class(cat)!="numeric" || cat != as.integer(cat)){
    stop("Factor threshold must be an integer.")
  }
  tryCatch({t <- read.table(f,sep=sep,header=header)
  t[] <- lapply(t, function(x){if(class(x)=="character" && length(unique(x))<cat){return(as.factor(x))}else{return(x)}})
  },
  error= function(e){ 
    stop("Csv file can't be read with the given parameters. Format: input file (string), categorical threshold (none or integer), header (none or logical), separator (none or string), class attribute name (none or string).")
  })
  return(dataset(t,c))
}