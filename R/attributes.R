checkValidity_att <- function(object){
  if(!is.vector(object@value,mode="numeric") && !is.vector(object@value,mode="logical") && !is.vector(object@value,mode="character") && !class(object@value) == "factor"){
    stop("Attribute must be a numerical vector, a logical vector, a string vector or a factor.")
  }
  if(class(object@value) != object@type){
    stop("The class of the attribute must be the same as 'object@type'.")
  }
  return(TRUE)
}

#'An S4 class to represent an Attribute
#'
#'@slot value Vector containing the attribute data
#'@slot type Type of the attribute
#'
setClass(Class="Attribute", 
         slots=c("value"="vector", "type"="character"),
         prototype=list(value=c(), type=""), validity = checkValidity_att)

#'Basic constructor of the Attribute class
#'
#'@description This function creates an object of class \code{\linkS4class{Attribute}}
#'
#'@param data Vector containing the attribute data
#'@return An object of class \code{\linkS4class{Attribute}} that represents the attribute
#'
attribute <- function(data){
  type = class(data)
  object <- new("Attribute",value=data,type=type)
  return(object)
}

setGeneric("to_factor_", function(object,levels=NULL) standardGeneric("to_factor_"))

#Function that converts the attribute to factor type
setMethod(f="to_factor_", signature="Attribute",
          definition= function(object,levels=NULL){
            if(!is.vector(object@value,mode="logical") && !is.vector(object@value,mode="character")){
              stop("Only logical and string attributes can be converted to factor.")
            }
            if(is.null(levels)){
              return(attribute(factor(object@value)))
            }else{
              return(attribute(factor(object@value,levels=levels)))
            }
          })

setGeneric("print_data", function(object) standardGeneric("print_data"))

#'Function to print the attribute data
#'
#'@description This function prints the attribute data
#'@examples
#'MyAttribute <- attribute(c(1,2,3,1,2))
#'print_data(MyAttribute)
setMethod(f="print_data", signature="Attribute",
          definition= function(object){
            print(paste("Attribute type: ",object@type,sep=""))
            print("Attribute values:")
            cat(paste(object@value,collapse="\n"))
            cat("\n")
          })

setGeneric("normalize_", function(object) standardGeneric("normalize_"))

#Function that normalizes the attribute data
setMethod(f="normalize_", signature="Attribute",
          definition= function(object){
            if(!is.vector(object@value,mode="numeric")){
              stop("Only numerical attributes can be normalized.")
            }
            x <- object@value
            object@value <- (x - min(x)) / (max(x) - min(x))
            return(object)
          })

setGeneric("standarize_", function(object) standardGeneric("standarize_"))

#Function that standarizes the attribute data
setMethod(f="standarize_", signature="Attribute",
          definition= function(object){
            if(!is.vector(object@value,mode="numeric")){
              stop("Only numerical attributes can be standarized.")
            }
            x <- object@value
            object@value <- (x-mean(x)) / sd(x)
            return(object)
          })

setGeneric("discretizeEW_", function(object,num.bins) standardGeneric("discretizeEW_"))

#Function that discretizes the attribute data using an equal-width approach
setMethod(f="discretizeEW_", signature="Attribute",
          definition= function (object, num.bins) {
            x <- object@value
            if(!is.vector(object@value,mode="numeric")){
              stop("Only numerical attributes can be discretized.")
            }
            if(class(num.bins) != "numeric" || num.bins != as.integer(num.bins)){
              stop("Number of intervals must be an integer.")
            }
            if(num.bins < 2){
              stop("Number of intervals must be equal to or greater than 2.")
            }
            min.val <- min(x)
            size.cut <- (max(x)-min.val)/num.bins
            cut.points <- min.val+size.cut*c(1:(num.bins-1))
            if(length(cut.points)>1){
              cat.values <- c(paste("( -infinity, ", cut.points[1], "]",sep=""),paste("(",cut.points[1:(length(cut.points)-1)],",",cut.points[2:length(cut.points)],"]",sep=""), paste("(",cut.points[length(cut.points)],", infinity)", sep=""))
            }else{
              cat.values <- c(paste("( -infinity, ", cut.points[1], "]",sep=""),paste("(",cut.points[length(cut.points)],", infinity)", sep=""))
            }
            x.discretized <- factor(cat.values[findInterval(x,cut.points,left.open=TRUE)+1],levels=cat.values)
            return(list(attribute(x.discretized), cut.points))
          })

setGeneric("discretizeEF_", function(object,num.bins) standardGeneric("discretizeEF_"))

#Function that discretizes the attribute data using an equal-frequency approach
setMethod(f="discretizeEF_", signature="Attribute",
          definition= function (object, num.bins) {
            x <- object@value
            if(!is.vector(object@value,mode="numeric")){
              stop("Only numerical attributes can be discretized.")
            }
            if(class(num.bins) != "numeric" || num.bins != as.integer(num.bins)){
              stop("Number of intervals must be an integer.")
            }
            if(num.bins < 2){
              stop("Number of intervals must be equal to or greater than 2.")
            }
            num.bins <- min(c(num.bins,length(x)))
            cut.size <- trunc(length(x)/num.bins)
            cut.mod <- length(x)%%num.bins
            x.sorted <- sort(x)
            cut.points <- c(x.sorted[((cut.size+1)*seq(0,cut.mod-1,length.out=cut.mod))],x.sorted[((cut.size)*seq(cut.mod,num.bins-1))+cut.mod])
            if(length(cut.points)>1){
              cat.values <- c(paste("( -infinity, ", cut.points[1], "]",sep=""),paste("(",cut.points[1:(length(cut.points)-1)],",",cut.points[2:length(cut.points)],"]",sep=""), paste("(",cut.points[length(cut.points)],", infinity)", sep=""))
            }else{
              cat.values <- c(paste("( -infinity, ", cut.points[1], "]",sep=""),paste("(",cut.points[length(cut.points)],", infinity)", sep=""))
            }
            x.discretized <- factor(cat.values[findInterval(x,cut.points,left.open=TRUE)+1],levels=cat.values)
            return(list(attribute(x.discretized), cut.points))
          })

setGeneric("discretize_", function(object,cut.points) standardGeneric("discretize_"))

#Function that discretizes the attribute data using a custom approach
setMethod(f="discretize_", signature="Attribute",
          definition= function (object, cut.points) {
            x <- object@value
            if(!is.vector(object@value,mode="numeric")){
              stop("Only numerical attributes can be discretized.")
            }
            if(!is.vector(cut.points,mode="numeric")){
              stop("Cut points must be a numerical vector.")
            }
            if(length(cut.points)<1){
              stop("Cut point list must contain at least one cut point.")
            }
            if(length(cut.points)>1){
              cat.values <- c(paste("( -infinity, ", cut.points[1], "]",sep=""),paste("(",cut.points[1:(length(cut.points)-1)],",",cut.points[2:length(cut.points)],"]",sep=""), paste("(",cut.points[length(cut.points)],", infinity)", sep=""))
            }else{
              cat.values <- c(paste("( -infinity, ", cut.points[1], "]",sep=""),paste("(",cut.points[length(cut.points)],", infinity)", sep=""))
            }
            x.discretized <- factor(cat.values[findInterval(x,cut.points,left.open=TRUE)+1],levels=cat.values)
            return(list(attribute(x.discretized), cut.points))
          })

setGeneric("variance_", function(object) standardGeneric("variance_"))

#Function that computes the variance of the attribute data
setMethod(f="variance_", signature="Attribute",
          definition= function(object){
            if(!is.vector(object@value,mode="numeric")){
              stop("The variance can only be computed for numerical attributes.")
            }
            return(var(object@value))
          })

setGeneric("mean_", function(object) standardGeneric("mean_"))

#Function that computes the mean of the attribute data
setMethod(f="mean_", signature="Attribute",
          definition= function(object){
            if(!is.vector(object@value,mode="numeric")){
              stop("The mean can only be computed for numerical attributes.")
            }
            return(mean(object@value))
          })

setGeneric("median_", function(object) standardGeneric("median_"))

#Function that computes the median of the attribute data
setMethod(f="median_", signature="Attribute",
          definition= function(object){
            if(!is.vector(object@value,mode="numeric")){
              stop("The median can only be computed for numerical attributes.")
            }
            return(median(object@value))
          })

setGeneric("entropy_", function(object) standardGeneric("entropy_"))

#Function that computes the entropy of the attribute data
setMethod(f="entropy_", signature="Attribute",
          definition= function(object){
            if(!is.vector(object@value,mode="logical") && !is.vector(object@value,mode="character") && !class(object@value) == "factor"){
              stop("The entropy can only be computed for logical, string and factor attributes.")
            }
            x <- object@value
            tot <- length(x)
            count <- table(x)
            count <- count/tot
            count <- count[count>0]
            return(sum(-count*log(count,base=2)))
          })

setGeneric("mode_", function(object) standardGeneric("mode_"))

#Function that computes the mode of the attribute data
setMethod(f="mode_", signature="Attribute",
          definition= function(object){
            if(!is.vector(object@value,mode="logical") && !is.vector(object@value,mode="character") && !class(object@value) == "factor"){
              stop("The mode can only be computed for logical, string and factor attributes.")
            }
            uniqv <- unique(object@value)
            mod <- uniqv[which.max(tabulate(match(object@value, uniqv)))]
            return(mod)
          })