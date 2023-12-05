#' @title Confusion matrix
#' @description Using the confusion matrix, various indices are calculated.
#' @param values Confusion matrix
#' @param ID Identifier. By default ID is a date in YYYYMMDD format
#' @param Date Date provided by the user. By default the date provided by the system will be taken.
#' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
#' @return Object of class MatCon.
#' @note  Error Messages
#'
#' List of possible errors:
#' \itemize{
#'  \item \code{Error type 1}: Non-square matrix.
#'  \item \code{Error type 2}: Single element matrix.
#'  \item \code{Error type 3}: negative values.
#'  \item \code{Error type 4}: Sum of elements 0.
#'  \item \code{Error type 5}: Sum of rows 0.
#'  \item \code{Error type 6}: Sum of columns 0.
#'  \item \code{Error type 7}: It is not a matrix.
#'}
#' @export MatCon
#' @importFrom R6 R6Class
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{congalton2008}{PaolaR6Nuevo}
#'
#' \insertRef{liu2007}{PaolaR6Nuevo}
#'
#' \insertRef{koukoulas2001}{PaolaR6Nuevo}
#'
#' \insertRef{turk2002}{PaolaR6Nuevo}
#'
#' \insertRef{hellden1980}{PaolaR6Nuevo}
#'
#' \insertRef{rosenfield1986}{PaolaR6Nuevo}
#'
#' \insertRef{short1982}{PaolaR6Nuevo}
#'
#' \insertRef{finn1993}{PaolaR6Nuevo}
#'
#' \insertRef{tung1988}{PaolaR6Nuevo}
#'
#' \insertRef{cohen1960}{PaolaR6Nuevo}
#'
#' \insertRef{strehl2002}{PaolaR6Nuevo}
#'
#' \insertRef{ghosh2002}{PaolaR6Nuevo}
#'
#' \insertRef{strehl2002relationship}{PaolaR6Nuevo}
#'
#' \insertRef{book}{PaolaR6Nuevo}
#'
#' \insertRef{pontius2014}{PaolaR6Nuevo}
#'
#' \insertRef{ariza2011}{PaolaR6Nuevo}
#'
#' \insertRef{fienberg1970}{PaolaR6Nuevo}
#'
#' \insertRef{munoz2016}{PaolaR6Nuevo}
#'
#' \insertRef{foody1992}{PaolaR6Nuevo}
#'
#' \insertRef{garcia2018}{PaolaR6Nuevo}
#'
#' \insertRef{ma1995Tau}{PaolaR6Nuevo}
#'
#' \insertRef{alba2020}{PaolaR6Nuevo}
#' @examples
#' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
#' mc <- MatCon$new (A,ID=5,Date="27-10-2023",Source="Congalton and Green, 2008")
#'
#' @aliases


MatCon <- R6Class("MatCon",
  public = list(
    #initialize the confusion matrix. An array must be added
    values = NULL,
    #initialize name
    ID = NULL,
    #initialize range
    nk = NULL,
    #initialize date
    Date = NULL,
    #Source Matrix
    Source=NULL,
    #initialize sumfil
    sumfil=NULL,
    #initialize sumcol
    sumcol=NULL,

    #' @description Public method to create an instance of the MatCon class. When creating it, values must be given to the matrix. The optional possibility of adding metadata to the matrix is offered.
    #' The creation includes a series of checks on the data that, if not met, give coded error messages. The values of the matrix must be organized in such a way that the columns represent the categories in the reference and the rows represent the categories in the product being evaluated.
    #' @param values Confusion matrix
    #' @param ID Identifier. By default, the date in YYYYMMDD format will be taken as the ID.
    #' @param Date Date provided by the user. By default the date provided by the system will be taken.
    #' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
    #' @return Object of class MatCon or an error if a matrix isn't entered.
    #' @examples
    #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
    #' mc <- MatCon$new (A,ID=5,Date="27-10-2023",Source="Congalton and Green, 2008")
    #'
    #' @aliases

#All parameters are entered
initialize = function(values,ID=NULL,Date=NULL,Source=NULL) {


# Initializing values -----------------------------------------------------



  self$values<-values
  #It is optional that you identify your parent.
  #If you add this value, a custom ID is given to the MC
  #otherwise you will be given today's date as ID
  #ID="Name" or ID=YYYYMMDD
  if(is.null(ID)){
    secuencia <- sprintf("%s-%03d", format(Sys.Date(),"%Y%m%d"), 1:999)
    self$ID <- secuencia[1]
    secuencia <- setdiff(secuencia, secuencia[1])
  }else{
    self$ID<-ID
  }
  #If no date is added (Date=2710, Date="27-10", Date="27/10")
  #In that case the system date will be taken
  if(!is.null(Date)){
    self$Date<-Date
  }else{self$Date <- Sys.Date()}

  if(!is.null(Source)){
    self$Source <- Source
  }else{self$Source<-NULL}

  #Values to check the MC
  #array rank
  nk<-nrow(self$values)
  nfilas <- nrow(self$values)
  ncolumnas <- ncol(self$values)
  #sum of row elements
  self$sumfil<-apply(self$values,1,sum)
  #sum of col elements
  self$sumcol<-apply(self$values,2,sum)



# Matrix check ------------------------------------------------------------



  error1<- FALSE
  error2<- FALSE
  error3<- FALSE
  error4<- FALSE
  error5<- FALSE
  error6<- FALSE
  error7<- FALSE
   if((nfilas != ncolumnas)) {
     error1<- TRUE
     print("Error type 1: Non-square matrix")
   }

   if((nk==1)){
     error2<-TRUE
     print("Error type 2: Single element matrix")
   }

   for (i in 1:nfilas){
    for (j in 1:ncolumnas){
      if(self$values[i,j]<0){
     error3<-TRUE
     print("Error type 3: negative values")
      }
     }
    }
   if(sum(self$values)==0 ){
     error4<-TRUE
     print("Error type 4: Sum of elements 0")
   }
   if(sum(apply(self$values,1,sum))==0 ){
     error5<-TRUE
     print("Error type 5: Sum of rows 0")
   }
   if(sum(apply(self$values,2,sum))==0 ){
     error6<-TRUE
     print("Error type 6: Sum of columns 0")
   }
   if(is.matrix(self$values) == FALSE){
     error7<-TRUE
     print("Error type 7: It is not a matrix")
   }
if ((error1 == TRUE) || (error2==TRUE) || (error3 == TRUE) || (error4 == TRUE) || (error5==TRUE) || (error6 == TRUE) || (error7 == TRUE)) {
   warning("Type errors 1, 2, 3, 4, 5, 6 or 7")
   stop()
   }
  },



# Functions that return indices and variances -----------------------------



      #' @description Public method to calculate the global index called Overall accuracy. The Overall accuracy for a particular classified image/map is then calculated by dividing the sum of the entries that form the major diagonal (i.e., the number of correct classifications) by the total number of samples taken. The method also offers the variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @description
      #' The mathematical expression is:
      #'
      #' \deqn{
      #' OverallAcc = \dfrac{\sum_{i=1}^{n} x_{ii}}{\sum_{i, j=1}^{n} x_{ij}}
      #' }
      #'
      #' \deqn{
      #' \sigma^2_{OverallAcc}=\dfrac{OverallAcc \cdot (1-OverallAcc)}{N}
      #' }
      #' Where:
      #' \enumerate{
      #'   \item \eqn{OverallAcc}: overall accuracy.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{x_{ij}}: element of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #'
      #' @return A list of the overall accuracy, its variance, and its confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A)
      #' p$OverallAcc()
      #'
      #' @aliases

     OverallAcc = function(a=NULL) {
     index <- sum(diag(self$values))/sum(self$values)
     VarIndex<-abs((index*(1-index))/sum(self$values))
     ConfInt<-private$ConfInt(index,VarIndex,a)
     return(list(OverallAcc=index,VarOverallAcc=VarIndex,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method for deriving a class index called user's accuracy. The user's accuracy for the class i of thematic map is calculated by dividing the value in the diagonal of class i by the sum of all values in the row of the class i. The method also offers the variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @description
      #' The mathematical expression is:
      #' \deqn{
      #' UserAcc=\dfrac{x_{ii}}{\sum_{j=1}^n x_{ij}}
      #' }
      #'  \deqn{
      #' \sigma^2_{UserAcc}=\dfrac{UserAcc \cdot (1-UserAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{UserAcc}: user accuracy.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{x_{ij}}: element of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with a vector of values for the user's accuracy rate for all classes, another vector with their variances and confidence intervals for each class.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$UserAcc()
      #'
      #' @aliases

     UserAcc = function(a=NULL){
     #matrix range
     n <- sqrt(length(self$values))
     UserAcc <- rep(0,n)
     VarUserAcc<-rep(0,n)
     ConfInt<-list()
       for (i in 1:n){
         UserAcc[i] <- self$values[i,i] / self$sumfil[i]
         VarUserAcc[i]<-abs((UserAcc[i]*(1-UserAcc[i]))/self$sumfil[i])
         ConfInt[[i]]<-c(private$ConfInt(UserAcc[i],VarUserAcc[i],a)$ConfInt_inf,private$ConfInt(UserAcc[i],VarUserAcc[i])$ConfInt_sup,a)
       }
     return(list(UserAcc=UserAcc,VarUserAcc=VarUserAcc,Conf_Int=ConfInt))
     },




      #' @description Public method where the user's accuracy index is defined for a specific class i. The user precision for class i of the thematic map is calculated by dividing the value on the diagonal of class i by the sum of all values in the row of class i. The method also offers variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' UserAcc_{i}=\dfrac{x_{ii}}{\sum_{j=1}^n x_{ij}}
      #' }
      #' \deqn{
      #' \sigma^2_{UserAcc_i}=\dfrac{UserAcc_i \cdot (1-UserAcc_i)}{N}
      #' }
      #'
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{UserAcc_i}: user accuracy index for class i.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{x_{ij}}: element of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @param i User class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @return A list of the user's accuracy index values for class i, its variance and its confidence interval.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$UserAcc_i(2)
      #'
      #' @aliases

     UserAcc_i=function(i,a=NULL){
      UserAcc_i <- self$values[i,i] / self$sumfil[i]
      VarUserAcc_i <- abs((UserAcc_i*(1-UserAcc_i))/self$sumfil[i])
      ConfInt <- private$ConfInt(UserAcc_i,VarUserAcc_i,a)
     return(list(UserAcc_i=UserAcc_i,VarUserAcc_i=VarUserAcc_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method for deriving a class index called producer's accuracy. The producer's accuracy for the class i of thematic map is calculated by dividing the value in the diagonal of class i by the sum of all values in the column of the class i. The method also offers the variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} if followed for the computations.
      #' @description
      #'  \deqn{
      #' ProdAcc=\dfrac{x_{jj}}{\sum_{j=1}^n x_{ij}}
      #' }
      #'\deqn{
      #' \sigma^2_{ProdAcc}=\dfrac{ProdAcc \cdot (1-ProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{ProdAcc}: producer accuracy.
      #'   \item \eqn{x_{jj}}: diagonal element of the matrix.
      #'   \item \eqn{x_{ij}}: element of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with a vector of values for the producer's accuracy index of all classes, another vector with their variances and confidence intervals for each class.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$ProdAcc()
      #'
      #' @aliases

     ProdAcc = function (a=NULL){
      n <- sqrt(length(self$values))
      ProdAcc <- rep(0,n)
      VarProdAcc<-rep(0,n)
      ConfInt<-list()
        for(i in 1:n){
          ProdAcc[i] <- self$values[i,i] / self$sumcol[i]
          VarProdAcc[i]<-abs((ProdAcc[i]*(1-ProdAcc[i]))/self$sumcol[i])
          ConfInt[[i]]<-c(private$ConfInt(ProdAcc[i],VarProdAcc[i],a)$ConfInt_inf,private$ConfInt(ProdAcc[i],VarProdAcc[i])$ConfInt_sup,a)
        }
     return(list(ProdAcc=ProdAcc,VarProdAcc=VarProdAcc,Conf_Int=ConfInt))
     },




      #' @description Public method where the producer's accuracy index is defined for a specific class i. The user precision for class i of the thematic map is calculated by dividing the value on the diagonal of class i by the sum of all values in the column of class i. The method also offers variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ProdAcc_{i}=\dfrac{x_{jj}}{\sum_{j=1}^n x_{ij}}
      #' }
      #'\deqn{
      #' \sigma^2_{ProdAcc_i}=\dfrac{ProdAcc_i \cdot (1-ProdAcc_i)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{ProdAcc_i}: producer accuracy index for class i.
      #'   \item \eqn{x_{jj}}: diagonal element of the matrix.
      #'   \item \eqn{x_{ij}}: element of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @param i Producer class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @return A list of the producer's accuracy index values for class i, its variance and its confidence interval.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$ProdAcc_i(1)
      #'
      #' @aliases

     ProdAcc_i = function(i,a=NULL){
      ProdAcc_i <- self$values[i,i] / self$sumcol[i]
      VarProdAcc_i <- abs((ProdAcc_i*(1-ProdAcc_i))/self$sumcol[i])
      ConfInt <- private$ConfInt(ProdAcc_i,VarProdAcc_i,a)
     return(list(ProdAcc_i=ProdAcc_i,VarProdAcc_i=VarProdAcc_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },



      #' @description Public method that provides the average of the accuracy rates of the user and producer of a specific class. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #' The mathematical expression is:
      #'  \deqn{
      #' AvUserProdAcc_i=\dfrac{UserAcc_i+ProdAcc_i}{2}
      #' }
      #'\deqn{
      #' \sigma^2_{AvUserProdAcc_i}=\dfrac{AvUserProdAcc_i \cdot (1-AvUserProdAcc_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvUserProdAcc_i}: average of user's and producer's accuracy.
      #'   \item \eqn{UserAcc_i}: user accuracy index for class i.
      #'   \item \eqn{ProdAcc_i}: producer accuracy index for class i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with average of user's and producer's accuracy, its variance for class i and its confidence interval.
      #' @param i Class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$AvUserProdAcc_i(2)
      #'
      #' @aliases

     AvUserProdAcc_i = function(i,a=NULL){
      AvUserProdAcc_i <- (self$UserAcc_i(i)[[1]] + self$ProdAcc_i(i)[[1]])/2
      VarAvUserProdAcc_i <- abs((AvUserProdAcc_i*(1-AvUserProdAcc_i))/(self$sumcol[i]+self$sumfil[i]))
      ConfInt <- private$ConfInt(AvUserProdAcc_i,VarAvUserProdAcc_i,a)
     return(list(AvUserProdAcc_i=AvUserProdAcc_i,VarAvUserProdAcc_i=VarAvUserProdAcc_i,ConfInt=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method that provides the Classification Success Index (CSI) applies to all class and gives an overall estimation of classification effectiveness. The references \insertCite{koukoulas2001,turk2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description The mathematical expression is:
      #'  \deqn{
      #' Sucess=1-(1-AvUserAcc+1-AvProdAcc)=AvUserAcc+AvProdAcc-1
      #' }
      #'  \deqn{
      #' VarSucess=\dfrac{Sucess \cdot (1-Sucess)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{Sucess}: classification succes index.
      #'   \item \eqn{AvUserAcc}: average accuracy from user's perspective.
      #'   \item \eqn{AvProdAcc}: average accuracy from producer's perspective.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the classification success index, its variance and its confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(0.3,0.02,0.01,0.12,0.19,0.03,0.02,0.01,0.3),nrow=3,ncol=3)
      #' p<-MatCon$new(A,Source="Labatut and Cherifi 2011")
      #' p$Sucess()
      #'
      #' @aliases

     Sucess = function(a=NULL){
      Sucess <- self$AvUserAcc()[[1]] + self$AvProdAcc()[[1]] - 1
       VarSucess <- abs((Sucess*(1-Sucess))/sum(self$values))
       ConfInt <- private$ConfInt(Sucess,VarSucess,a)
     return(list(Sucess=Sucess,VarSucess=VarSucess,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method that provides the Individual Classification Success Index (ICSI) applies to the classification effectiveness for one particular class of interest. The references \insertCite{koukoulas2001,turk2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #' The mathematical expression is:
      #'  \deqn{
      #' Sucess_i=1-(1-UserAcc_i+1-ProdAcc_i)=UserAcc_i+ProdAcc_i-1
      #' }
      #'
      #' \deqn{
      #' \sigma^2_{Sucess_i}=\dfrac{Sucess_i \cdot (1-Sucess_i)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{Sucess_i}: individual classification success index.
      #'   \item \eqn{UserAcc_i}: user accuracy index for class i.
      #'   \item \eqn{ProdAcc_i}: producer accuracy index for class i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the individual classification success index, its variance and its confidence interval.
      #' @param i Class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(0.3,0.02,0.01,0.12,0.19,0.03,0.02,0.01,0.3),nrow=3,ncol=3)
      #' p<-MatCon$new(A,Source="Labatut and Cherifi 2011")
      #' p$Sucess_i(2)
      #'
      #' @aliases

     Sucess_i = function(i,a=NULL){
      Sucess_i <- self$UserAcc_i(i)[[1]] + self$ProdAcc_i(i)[[1]] - 1
      VarSucess_i <- abs((Sucess_i*(1-Sucess_i))/(self$sumcol[i]+self$sumfil[i]))
      ConfInt <- private$ConfInt(Sucess_i,VarSucess_i,a)
     return (list(Sucess_i=Sucess_i,VarSucess_i=VarSucess_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },





      #' @description Public method that provides the Hellden' average accuracy, denotes for the probability that a randomly chosen point of a specific class on the map has a correspondence of the same class in the same position in the field and that a randomly chosen point in the field of the same class has a correspondence of the same class in the same position on the map.The method also offers variance. The references \insertCite{hellden1980,rosenfield1986}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvHelldenAcc_i=\dfrac{2}{\dfrac{1}{UserAcc_i}+\dfrac{1}{ProdAcc_i}}
      #' }
      #' \deqn{
      #' \sigma^2_{AvHelldenAcc_i}=\dfrac{AvHelldenAcc_i \cdot (1-AvHelldenAcc_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvHelldenAcc_i}: Hellden's mean accuracy.
      #'   \item \eqn{UserAcc_i}: user accuracy index for class i.
      #'   \item \eqn{ProdAcc_i}: producer accuracy index for class i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @param i Class to evaluate.
      #' @return A list with Hellden's mean accuracy, its variance and its confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A <- matrix(c(148,1,8,2,0,0,50,15,3,0,1,6,39,7,1,1,0,6,25,1,1,0,0,1,6),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Rosenfield and Fitzpatrick 1986")
      #' p$AvHelldenAcc_i(2)
      #'
      #' @aliases

     AvHelldenAcc_i = function(i,a=NULL){

       if (self$UserAcc_i(i)[[1]] == 0 || self$ProdAcc_i(i)[[1]] == 0) {
        stop ("/ by 0")
       }else{
          AvHelldenAcc_i <- 2 / (1/self$UserAcc_i(i)[[1]] + 1/self$ProdAcc_i(i)[[1]])
         VarAvHelldenAcc_i <- abs((AvHelldenAcc_i*(1-AvHelldenAcc_i))/(self$sumcol[i]+self$sumfil[i]))
         ConfInt <- private$ConfInt(AvHelldenAcc_i,VarAvHelldenAcc_i,a)
         }

     return(list(AvHelldenAcc_i=AvHelldenAcc_i,VarAvHelldenAcc_i=VarAvHelldenAcc_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method that provides Short's mapping accuracy for each class is stated as the number of correctly classified pixels (equal to the total in the correctly classified area) in terms of all pixels affected by its classification (equal to this total in the displayed area as well as the pixels involved in errors of commission and omission). The method also offers variance. The references \insertCite{rosenfield1986,short1982}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ShortAcc_i=\dfrac{x_{ii}}{\sum^n_{j=1} x_{+ j}+\sum^n_{i=1} x_{i +}-x_{ii}}
      #' }
      #'\deqn{
      #' \sigma^2_{ShortAcc_i}=\dfrac{ShortAcc_i \cdot (1-ShortAcc_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{ShortAcc_i}: Short's mapping accuracy
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{x_{j+}}: sum of all elements in rows j.
      #'   \item \eqn{x_{+j}}: sum of all elements in column j.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @param i Class to evaluate.
      #' @return A list with Short's mapping accuracy, its variance and its confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A <- matrix(c(148,1,8,2,0,0,50,15,3,0,1,6,39,7,1,1,0,6,25,1,1,0,0,1,6),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Rosenfield and Fitzpatrick-Lins 1986")
      #' p$ShortAcc_i(2)
      #'
      #' @aliases

     ShortAcc_i = function(i,a=NULL){
      if (self$sumfil[i] + self$sumcol[i] - self$values[i,i] == 0) {
      stop ("/ by 0")
      }else{
        ShortAcc_i = self$values[i,i] / (self$sumfil[i] + self$sumcol[i] - self$values[i,i])
        VarShortAcc_i=abs((ShortAcc_i*(1-ShortAcc_i))/(self$sumcol[i]+self$sumfil[i]))
        ConfInt <- private$ConfInt(ShortAcc_i,VarShortAcc_i,a)
        }

     return(list(ShortAcc_i=ShortAcc_i,VarShortAcc_i=VarShortAcc_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },





      #' @description Public method that evaluates the kappa coefficient from the user's perspective, for a specific class i. The method also offers variance. The reference \insertCite{rosenfield1986}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' UserKappa_i=\dfrac{UserAcc_i-\dfrac{\sum^n_{i=1} x_{i + }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}{1-\dfrac{\sum^n_{i=1} x_{i + }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}
      #' }
      #'  \deqn{
      #' \sigma^2_{UserKappa_i}=\dfrac{UserKappa_i \cdot (1-UserKappa_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{UserKappa_i}: coefficient kappa (user's).
      #'   \item \eqn{UserAcc_i}: user accuracy index for class i.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{x_{j+}}: sum of all elements in rows j.
      #'   \item \eqn{x_{+j}}: sum of all elements in column j.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with coefficient kappa (user's), its variance and its confidence interval.
      #' @param i Class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(73,13,5,1,0,21,32,13,3,0,16,39,35,29,13,3,5,7,28,48,1,0,2,3,17),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Næsset 1996")
      #' p$UserKappa_i(2)
      #'
      #' @aliases

     UserKappa_i = function(i,a=NULL){
      if (1 - self$sumcol[i]/sum(self$values) == 0) {
       stop ("/ by 0")
      }else{
        UserKappa_i <- (self$UserAcc_i(i)[[1]] - self$sumcol[i]/sum(self$values)) / (1 - self$sumcol[i]/sum(self$values))
        VarUserKappa_i <- abs((UserKappa_i*(1-UserKappa_i))/self$sumfil[i])
        ConfInt <- private$ConfInt(UserKappa_i,VarUserKappa_i,a)
        }

     return(list(UserKappa_i=UserKappa_i,VarUserKappa_i=VarUserKappa_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method that evaluates the kappa coefficient from the producer's perspective, for a specific class i. The method also offers variance. The reference \insertCite{rosenfield1986}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ProdKappa_i=\dfrac{ProdAcc_i-\dfrac{\sum^n_{j=1} x_{ + j }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}{1-\dfrac{\sum^n_{j=1} x_{+ j }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}
      #' }
      #'  \deqn{
      #' \sigma^2_{ProdKappa_i}=\dfrac{ProdKappa_i \cdot (1- ProdKappa_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{ProdKappa_i}: coefficient kappa (producer's).
      #'   \item \eqn{ProdAcc_i}: producer accuracy index for class i.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{x_{j+}}: sum of all elements in rows j.
      #'   \item \eqn{x_{+j}}: sum of all elements in column j.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with coefficient kappa (producer's), its variance and its confidence interval.
      #' @param i Class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(73,13,5,1,0,21,32,13,3,0,16,39,35,29,13,3,5,7,28,48,1,0,2,3,17),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Næsset 1996")
      #' p$ProdKappa_i(2)
      #'
      #' @aliases

      ProdKappa_i = function(i,a=NULL){
      if (1 - self$sumfil[i]/sum(self$values) == 0) {
       stop ("/ by 0")
      }else{
        ProdKappa_i <- (self$ProdAcc_i(i)[[1]] - self$sumfil[i]/sum(self$values)) / (1 - self$sumfil[i]/sum(self$values))
        VarProdKappa_i <- abs((ProdKappa_i*(1-ProdKappa_i))/self$sumcol[i])
        ConfInt <- private$ConfInt(ProdKappa_i,VarProdKappa_i,a)
        }
     return(list(ProdKappa_i=ProdKappa_i,VarProdKappa_i=VarProdKappa_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },


      #' @description Public method that provides the overall modified kappa coefficient. The method also offers variance. The references \insertCite{stehman1997,foody1992}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ModKappa=\dfrac{OverallAcc-\dfrac{1}{\sqrt{M}}}{1-\dfrac{1}{\sqrt{M}}}
      #' }
      #' \deqn{
      #' \sigma^2_{ModKappa}=\dfrac{ModKappa \cdot (1- ModKappa)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{ModKappa}: modified coefficient kappa.
      #'   \item \eqn{OverallAcc}: overall accuracy.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with modified coefficient kappa, its variance and its confidence interval.
      #' @param i Class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(317,61,2,35,23,120,4,29,0,0,60,0,0,0,0,8),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Foody 1992")
      #' p$ModKappa()
      #'
      #' @aliases

     ModKappa = function(i,a=NULL){
       ModKappa <- (self$OverallAcc()[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
       VarModKappa <- abs((ModKappa*(1-ModKappa))/self$values)
       ConfInt <- private$ConfInt(ModKappa,VarModKappa,a)
     return(list(ModKappa=ModKappa,VarModKappa=VarModKappa,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method, derived from the general modified kappa coefficient, which provides the modified coefficient kappa for the user. The method also offers variance. The references \insertCite{stehman1997,foody1992}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ModKappaUser_i=\dfrac{UserAcc_i-\dfrac{1}{\sqrt{M}}}{1-\dfrac{1}{\sqrt{M}}}
      #' }
      #' \deqn{
      #' \sigma^2_{ModKappaUser_i}=\dfrac{ModKappaUser_i \cdot (1- ModKappaUser_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{ModKappaUser_i}: modified coefficient kappa (user's).
      #'   \item \eqn{UserAcc_i}: user accuracy index for class i.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with modified coefficient kappa (user's), its variance and confidence interval
      #' @param i Class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al. 2007")
      #' p$ModKappaUser_i(2)
      #'
      #' @aliases

     ModKappaUser_i = function(i,a=NULL){
       ModKappaUser_i <- (self$UserAcc_i(i)[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
       VarModKappaUser_i <- abs((ModKappaUser_i*(1-ModKappaUser_i))/self$sumfil[i])
       ConfInt <- private$ConfInt(ModKappaUser_i,VarModKappaUser_i,a)
     return(list(ModKappaUser_i=ModKappaUser_i,VarModKappaUser_i=VarModKappaUser_i, Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },





      #' @description Public method, derived from the general modified kappa coefficient, which provides the modified coefficient kappa for the producer. The method also offers variance. The references \insertCite{stehman1997,foody1992}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ModKappaProd_i=\dfrac{ProdAcc_i-\dfrac{1}{\sqrt{M}}}{1-\dfrac{1}{\sqrt{M}}}
      #' }
      #' \deqn{
      #' \sigma^2_{ModKappaProd_i}=\dfrac{ModKappaProd_i \cdot (1- ModKappaProd_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{ModKappaUser_i}: modified coefficient kappa (producer's).
      #'   \item \eqn{ProdAcc_i}: producer accuracy index for class i.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with modified coefficient kappa (producer's), its variance and confidence interval.
      #' @param i Class to evaluate.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(317,61,2,35,23,120,4,29,0,0,60,0,0,0,0,8),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al. 2007")
      #' p$ModKappaProd_i(2)
      #'
      #' @aliases

     ModKappaProd_i = function(i,a=NULL){
      ModKappaProd_i <- (self$ProdAcc_i(i)[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
      VarModKappaProd_i <- abs((ModKappaProd_i*(1-ModKappaProd_i))/self$sumcol[i])
      ConfInt <- private$ConfInt(ModKappaProd_i,VarModKappaProd_i,a)
     return(list(ModKappaProd_i=ModKappaProd_i,VarModKappaProd_i=VarModKappaProd_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

     #' @description Public method that calculates relative change of entropy given a category on map. That is, the degree of uncertainty of the category. The method also offers variance. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
     #' @description
     #'  \deqn{
     #' Entrop_i(A)=-\sum^n_{j=1}( (\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
     #' }
     #' \deqn{
     #' Entrop_i(A|b_i)=-\sum^n_{j=1}( (\dfrac{ x_{ij}}{\sum^n_{j=1} x_{+ j} }) \cdot \log (\dfrac{x_{ij}}{\sum^n_{j=1} x_{+ j}}) )
     #' }
     #' \deqn{
     #' EntropUser_i= \dfrac{Entrop_i(A)-Entrop_i(A|b_i)}{Entrop_i(A)}
     #' }
     #' \deqn{
     #' \sigma^2_{EntropUser_i}= \dfrac{EntropUser_i \cdot (1-EntropUser_i)}{N}
     #' }
     #'
     #' where:
     #'
     #' \enumerate{
     #'   \item \eqn{EntropUser_i}: relative change of entropy given a category on map.
     #'   \item \eqn{Entrop_i(A)}: Entropy of the map with respect to the category of the map.
     #'   \item \eqn{x_{j+}}: sum of all elements in rows j.
     #'   \item \eqn{x_{+j}}: sum of all elements in column j.
     #'   \item \eqn{Entrop_i(A|b_i)}: Entropy of map A knowing that the location corresponding to map B is in class b_i.
     #'   \item \eqn{N}: number of cases involved in the calculation of the index.
     #' }
     #' @return A list with the relative change of entropy given a category on map, its variance, its confidence interval, map entropy, and entropy of map A knowing that the location corresponding to map B is in class b_i.
     #' @param i Class to evaluate (row).
     #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
     #' @param a Significance level. By default 0.05.
     #' @examples
     #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
     #' p<-MatCon$new(A,Source="Liu et al. 2007")
     #' p$EntropUser_i(1)
     #'
     #' @aliases

        #They can be negative values if the entropy (uncertainty) increases and
        #positive if entropy decreases
        #Classes by rows. If any element is 0 in the row you get an error,
        #due to log

    EntropUser_i = function(i,v=NULL,a=NULL){
      if(!is.null(v)){
       v<-v
      }else{v<-10}

       #log10 ->Hartleys (entropy units)
       #log2->bits
       #LN->nats

     #na.rm=TRUE. In this way it does the sum with the values it has and ignores NA
    Entrop_iA <- - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
    Entrop_iAbi <- - sum ((self$values[i,] / self$sumfil[i]) * log(self$values[i,] / self$sumfil[i],base=v),na.rm=TRUE)

    if (Entrop_iA == 0){
     stop("/by 0")
    }else {
      EntropUser_i <- (Entrop_iA - Entrop_iAbi) / Entrop_iA
      VarEntropUser_i <- abs((EntropUser_i*(1-EntropUser_i))/sum(self$values))
      ConfInt <- private$ConfInt(EntropUser_i,VarEntropUser_i,a)
      }
    return(list(EntropUser_i=EntropUser_i,VarEntropUser_i=VarEntropUser_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup),Entrop_iA=Entrop_iA,Entrop_iAbi=Entrop_iAbi))
    },

     #' @description Public method that calculates relative change of entropy given a category on ground truthing. That is, the degree of uncertainty of the category. The method also offers variance. The reference \insertCite{stehman1997}{PaolaR6Nuevo} is followed for the calculations.
     #' @param i Class to evaluate
     #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
     #' @param a Significance level. By default 0.05.
     #' @description
     #'  \deqn{
     #' Entrop_i(B)=-\sum^n_{i=1}( (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
     #' }
     #' \deqn{
     #' Entrop_i(B|a_j)=-\sum^n_{j=1}( (\dfrac{ x_{ij}}{\sum^n_{i=1} x_{i +} }) \cdot \log (\dfrac{x_{ij}}{\sum^n_{i=1} x_{i +}}) )
     #' }
     #' \deqn{
     #' EntropProd_i= \dfrac{EntropMap(B)-EntropMap(B|a_j)}{EntropMap(B)}
     #' }
     #'\deqn{
     #' \sigma^2_{EntropProd_i}= \dfrac{EntropProd_i \cdot (1-EntropProd_i)}{N}
     #' }
     #' where:
     #'
     #' \enumerate{
     #'   \item \eqn{EntropProd_i}: relative change of entropy given a category on ground truthing.
     #'   \item \eqn{Entrop_i(B)}: Entropy of the map with respect to the category on ground truthing.
     #'   \item \eqn{x_{j+}}: sum of all elements in rows j.
     #'   \item \eqn{x_{+j}}: sum of all elements in column j.
     #'   \item \eqn{Entrop_i(B|a_j)}: Entropy of map B knowing that the location corresponding to map A is in class a_j.
     #'   \item \eqn{N}: number of cases involved in the calculation of the index.
     #' }
     #' @return A list of the relative change of entropy given a category on ground truthing, its variance,its confidence interval, map entropy, and entropy of map B knowing that the location corresponding to map A is in class a_j.
     #' @examples
     #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
     #' p<-MatCon$new(A,Source="Liu et al. 2007")
     #' p$EntropProd_i(2)
     #'
     #' @aliases

    EntropProd_i = function(i,v=NULL,a=NULL){
     if(!is.null(v)){
      v<-v
     }else{v<-10}
    Entrop_iB <- - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base = v)),na.rm=TRUE)
    Entrop_iBaj <- - sum ((self$values[,i] / self$sumcol[i]) * log(self$values[,i] / self$sumcol[i],base=v),na.rm=TRUE)

      if (Entrop_iB == 0){
        stop("/by 0")
      }else {
        EntropProd_i <- (Entrop_iB - Entrop_iBaj) / Entrop_iB
        VarEntropProd_i <- abs((EntropProd_i*(1-EntropProd_i))/sum(self$values))
        ConfInt <- private$ConfInt(EntropProd_i,VarEntropProd_i,a)
        }
    return(list(EntropProd_i=EntropProd_i,VarEntropProd_i=VarEntropProd_i,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup),Entrop_iB=Entrop_iB,Entrop_iBaj=Entrop_iBaj))
    },




      #' @description Public method that provides the user's average accuracy, which is an average of the accuracy of individual categories, in this case the categories will be taken from the user's perspective. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvUserAcc=\dfrac{1}{\sqrt{M}} \sum^n_{i=1} \dfrac{x_{ii}}{\sum_{j=1}^n x_{j+}}
      #' }
      #'\deqn{
      #' \sigma^2_{AvUserAcc}=\dfrac{AvUserAcc \cdot (1-AvUserAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvUserAcc}: average accuracy from user's perspective.
      #'   \item \eqn{x_{j+}}: sum of all elements in rows j.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the average accuracy from user's perspective, its variance and its confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung and LeDrew 1988")
      #' p$AvUserAcc()
      #'
      #' @aliases

     AvUserAcc = function(a=NULL){
       for (i in 1:length(self$sumfil)) {
          if (self$sumfil[i] == 0) {
            stop ("/ by 0")
          }
       }
      AvUserAcc <- 1/sqrt(length(self$values)) * sum (diag(self$values)/self$sumfil)
      VarAvUserAcc <- abs((AvUserAcc*(1-AvUserAcc))/sum(self$values))
      ConfInt <- private$ConfInt(AvUserAcc,VarAvUserAcc,a)
     return(list(AvUserAcc=AvUserAcc,VarAvUserAcc=VarAvUserAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that provides the producer's average accuracy, which is an average of the accuracy of individual categories, in this case the categories will be taken from the producer's perspective. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvProdAcc=\dfrac{1}{\sqrt{N}} \sum^n_{i=1} \dfrac{x_{ii}}{\sum_{j=1}^n x_{+j}}
      #' }
      #'\deqn{
      #' \sigma^2_{AvProdAcc}=\dfrac{AvProdAcc \cdot (1-AvProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvProdAcc}: average accuracy from producer's perspective.
      #'   \item \eqn{x_{+j}}: sum of all elements in column j.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the average accuracy from producer's perspective, its variance and its confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung and LeDrew 1988")
      #' p$AvProdAcc()
      #'
      #' @aliases

     AvProdAcc = function(a=NULL){
      AvProdAcc <- 1/sqrt(length(self$values)) * sum (diag(self$values)/self$sumcol)
      VarAvProdAcc <- abs((AvProdAcc*(1-AvProdAcc))/sum(self$values))
      ConfInt <- private$ConfInt(AvProdAcc,VarAvProdAcc,a)
     return(list(AvProdAcc=AvProdAcc,VarAvProdAcc=VarAvProdAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that offers the average of the average precision from the perspective of the user and the producer. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvUserProdAcc=\dfrac{AvUserAcc+AvProdAcc}{2}
      #' }
      #'  \deqn{
      #' \sigma^2_{AvUserProdAcc}=\dfrac{AvUserProdAcc \cdot (1-AvUserProdAcc)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvUserProdAcc}: average of average of user's and producer's perspective.
      #'   \item \eqn{AvUserAcc}: average accuracy from user's perspective.
      #'   \item \eqn{AvProdAcc}: average accuracy from producer's perspective.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list of the average mean precision values from the user and producer perspective, their variance and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$AvUserProdAcc()
      #'
      #' @aliases

     AvUserProdAcc = function(a=NULL){
      AvUserProdAcc <- (self$AvUserAcc()[[1]] + self$AvProdAcc()[[1]]) / 2
      VarAvUserProdAcc <- abs((AvUserProdAcc*(1-AvUserProdAcc))/sum(self$values))
      ConfInt <- private$ConfInt(AvUserProdAcc,VarAvUserProdAcc,a)
     return(list(AvUserProdAcc=AvUserProdAcc,VarAvUserProdAcc=VarAvUserProdAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },


      #' @description Public method that provides the average value of the Hellden mean precision index. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvHelldenAcc=\dfrac{1}{\sqrt{M}}\sum^n_{i=1} \dfrac{2 x_{ii}}{ x_{+i} + x_{i+}}
      #' }
      #'  \deqn{
      #' \sigma^2_{AvHelldenAcc}=\dfrac{AvHelldenAcc \cdot (1-AvHelldenAcc)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvHelldenAcc}: average of Hellden's mean accuracy index.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with average of Hellden's mean accuracy index, its variance and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$AvHelldenAcc()
      #'
      #' @aliases

     AvHelldenAcc = function(a=NULL){
      AvHelldenAcc <- 1/sqrt(length(self$values)) * sum ((2*diag(self$values)) / (self$sumfil + self$sumcol))
      VarAvHelldenAcc <- abs((AvHelldenAcc*(1-AvHelldenAcc))/sum(self$values))
      ConfInt <- private$ConfInt(AvHelldenAcc,VarAvHelldenAcc,a)
     return(list(AvHelldenAcc=AvHelldenAcc,VarAvHelldenAcc=VarAvHelldenAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },


      #' @description Public method that provides the average of Short's mapping accuracy index. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvShortAcc=\dfrac{1}{\sqrt{M}}\dfrac{\dfrac{\sum^n_{i=1} x_{ii}}{\sum^n_{i,j=1}x_{ij}}}{\sum^n_{j=1} x_{+ j}+\sum^n_{i=1} x_{i +}-x_{ii}}
      #' }
      #'\deqn{
      #' \sigma^2_{AvShortAcc}=\dfrac{AvShortAcc \cdot (1-AvShortAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{x_{ii}}: diagonal element of the matrix.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #'    }
      #' @return A list with average of Short's mapping accuracy index, its variance and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$AvShortAcc()
      #'
      #' @aliases

     AvShortAcc = function(a=NULL){
      sum1 <- self$sumfil+self$sumcol- diag(self$values)
       for (i in 1:length(sum1)) {
          if (sum1[i] == 0) {
           stop ("/ by 0")
          }
       }
      AvShortAcc = 1/sqrt(length(self$values)) * sum (diag(self$values) / (self$sumfil + self$sumcol - diag(self$values)))
      VarAvShortAcc=abs((AvShortAcc*(1-AvShortAcc))/sum(self$values))
      ConfInt <- private$ConfInt(AvShortAcc,VarAvShortAcc,a)

     return(list(AvShortAcc=AvShortAcc,VarAvShortAcc=VarAvShortAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },


      #' @description Public method that provides the combined user accuracy that is the average of the overall accuracy and the average user accuracy. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' CombUserAcc=\dfrac{OverallAcc+AvUserAcc}{2}
      #' }
      #' \deqn{
      #' \sigma^2_{CombUserAcc}=\dfrac{CombUserAcc \cdot (1-CombUserAcc)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{CombUserAcc}: combined accuracy from user's perspective.
      #'   \item \eqn{OverallAcc}: overall accuracy.
      #'   \item \eqn{AvUserAcc}: average accuracy from user's perspective.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #'   }
      #' @return A list of the combined accuracy from the user's perspective, its variation and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung and LeDrew 1988")
      #' p$CombUserAcc()
      #'
      #' @aliases

     CombUserAcc = function(a=NULL){
      CombUserAcc <- (self$OverallAcc()[[1]] + self$AvUserAcc()[[1]]) / 2
      VarCombUserAcc <- abs((CombUserAcc*(1-CombUserAcc))/sum(self$values))
      ConfInt <- private$ConfInt(CombUserAcc,VarCombUserAcc,a)
     return(list(CombUserAcc=CombUserAcc,VarCombUserAcc=VarCombUserAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that provides the combined producer accuracy that is the average of the overall accuracy and the average producer accuracy. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' CombProdAcc=\dfrac{OverallAcc+AvProdAcc}{2}
      #' }
      #'\deqn{
      #' \sigma^2_{CombProdAcc}=\dfrac{CombProdAcc \cdot (1-CombProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{CombProdAcc}: combined accuracy from producer's perspective.
      #'   \item \eqn{OverallAcc}: overall accuracy.
      #'   \item \eqn{AvProdAcc}: average accuracy from producer's perspective.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list of the combined accuracy from producer's perspective, its variance and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung and LeDrew 1988")
      #' p$CombProdAcc()
      #'
      #' @aliases

     CombProdAcc = function(a=NULL){
      CombProdAcc <- (self$OverallAcc()[[1]] + self$AvProdAcc()[[1]]) / 2
      VarCombProdAcc <- abs((CombProdAcc*(1-CombProdAcc))/sum(self$values))
      ConfInt <- private$ConfInt(CombProdAcc,VarCombProdAcc,a)
     return(list(CombProdAcc=CombProdAcc,VarCombProdAcc=VarCombProdAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that provides the combined accuracy which is the average of the overall accuracy and the Hellden average accuracy, which refers to the average user and producer accuracies. The method also offers variation. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' CombUserProdAcc=\dfrac{OverallAcc+AvHelldenAcc}{2}
      #' }
      #'  \deqn{
      #' \sigma^2_{CombUserProdAcc}=\dfrac{CombUserProdAcc \cdot (1-CombUserProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{CombUserProdAcc}: combined accuracy from both user's and producer's perspectives.
      #'   \item \eqn{OverallAcc}: overall accuracy.
      #'   \item \eqn{AvHelldenAcc}: average of Hellden's mean accuracy index.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list of the combined accuracy from both user's and producer's perspectives, its variance and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$CombUserProdAcc()
      #'
      #' @aliases

     CombUserProdAcc = function(a=NULL){
      CombUserProdAcc <- ( self$OverallAcc()[[1]] + self$AvHelldenAcc()[[1]] ) / 2
      VarCombUserProdAcc <- abs((CombUserProdAcc*(1-CombUserProdAcc))/sum(self$values))
      ConfInt <- private$ConfInt(CombUserProdAcc,VarCombUserProdAcc,a)
     return(list(CombUserProdAcc=CombUserProdAcc,VarCombUserProdAcc=VarCombUserProdAcc,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that provides kappa coefficient, which measures the relationship between agreement beyond chance and expected disagreement. The method also offers variation. The reference \insertCite{cohen1960}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ExpAcc=\sum^n_{i=1} (\dfrac{x _{+ i}}{\sum_{j=1}^n x_{ij}} \cdot \dfrac{x _{i +}}{\sum_{j=1}^n x_{ij}})
      #' }
      #' \deqn{
      #' Kappa=\dfrac{OverallAcc-ExpAcc}{1-ExpAcc}
      #' }
      #'  \deqn{
      #' \sigma^2_{Kappa}=\dfrac{OverallAcc-ExpAcc}{(1-ExpAcc) \cdot N}
      #' }
      #'
      #'
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{Kappa}: Kappa coefficient.
      #'   \item \eqn{OverallAcc}: overall accuracy.
      #'   \item \eqn{ExpAcc}: expected accuracy of agreement if agreement were purely random.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with kappa coefficient, its variance and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$Kappa()
      #'
      #' @aliases

     Kappa = function(a=NULL){
      ExpAcc <- (sum (self$sumfil * self$sumcol))/sum(self$values)^2
      if (1-ExpAcc == 0){
       stop ("/ by 0")
      }else{
        kappa <- (self$OverallAcc()[[1]]- ExpAcc) / (1 - ExpAcc)
        VarKappa <- abs((self$OverallAcc()[[1]]*(1-self$OverallAcc()[[1]]))/(sum(self$values)*(1-ExpAcc)^2))
        ConfInt <- private$ConfInt(kappa,VarKappa,a)
      }
     return(list(Kappa=kappa,VarKappa=VarKappa,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




      #' @description Public method for calculating map entropy. Which refers to the degree of uncertainty that the map presents. The method also offers variation. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop=\sum^n_{i,j=1} (\dfrac{x_{ij}}{\sum^n_{i,j=1} x_{ij}} \cdot \log (\dfrac{x_{ij}}{\dfrac{\sum^n_{i=1} x_{i +} \cdot \sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij}}}))
      #' }
      #'\deqn{
      #' \sigma^2_{Entrop}=\dfrac{Entrop \cdot (1-Entrop)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{Entrop}: map entropy.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with map entropy, its variance and confidence interval.
      #' @param a Significance level. By default 0.05.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al. 2007")
      #' p$Entrop()
      #'
      #' @aliases

     Entrop = function(v=NULL,a=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

       Entrop <- sum ((self$values/sum(self$values)) * log(self$values / ((self$sumfil * self$sumcol)/sum(self$values)),base=v),na.rm=TRUE)
       VarEntrop <- abs((Entrop*(1-Entrop))/sum(self$values))
       ConfInt <- private$ConfInt(Entrop,VarEntrop,a)
     return(list(Entrop=Entrop,VarEntrop=VarEntrop,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },


      #' @description Public method that calculates normalized entropy using the map. The method also offers variation. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' NormEntropUser=\dfrac{Entrop}{Entrop_i(B)}
      #' }
      #'\deqn{
      #' \sigma^2_{NormEntropUser}=\dfrac{NormEntropUser \cdot (1-NormEntropUser)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{NormEntropUser}: normalized entropy using map.
      #'   \item \eqn{Entrop_i(B)}: entropy of the map with respect to the category on ground truthing.
      #'   \item \eqn{Entrop}: map entropy.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using map, its variance and confidence interval.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al. 2007")
      #' p$NormEntropUser()
      #'
      #' @aliases

     NormEntropUser = function(v=NULL,a=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

       Entrop_iB <- - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base=v)),na.rm=TRUE)

       if(Entrop_iB == 0){
        stop("/ by 0")
       }

       NormEntropUser <- self$Entrop(v)[[1]]/Entrop_iB
       VarNormEntropUser <- abs((NormEntropUser*(1-NormEntropUser))/sum(self$values))
       ConfInt <- private$ConfInt(NormEntropUser,VarNormEntropUser,a)
     return(list(NormEntropUser=NormEntropUser,VarNormEntropUser=VarNormEntropUser,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that calculates normalized entropy using on ground truthing. The method also offers variation. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(A)=-\sum^n_{j=1}( (\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' NormEntropProd=\dfrac{Entrop}{Entrop_i(A)}
      #' }
      #'\deqn{
      #' \sigma^2_{NormEntropProd}=\dfrac{NormEntropProd \cdot (1-NormEntropProd)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{NormEntropProd}: normalized mutual information using the entropy on ground truthing.
      #'   \item \eqn{Entrop_i(A)}: Entropy of the map with respect to the category of the map.
      #'   \item \eqn{Entrop}: map entropy.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using on ground truthing, its variance and confidence interval.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al. 2007")
      #' p$NormEntropProd()
      #'
      #' @aliases

     NormEntropProd = function(v=NULL,a=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

      Entrop_iA <- - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
       if (Entrop_iA == 0){
        stop ("/ by 0")
       }else{
         NormEntropProd <- self$Entrop()[[1]]/Entrop_iA
         VarNormEntropProd <- abs((NormEntropProd*(1-NormEntropProd))/sum(self$values))
         ConfInt <- private$ConfInt(NormEntropProd,VarNormEntropProd,a)
         }
     return(list(NormEntropProd=NormEntropProd,VarNormEntropProd=VarNormEntropProd,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that calculates normalized entropy using the arithmetic mean of the entropies on the map and on ground truthing. The method also offers variation. The reference \insertCite{strehl2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(A)=-\sum^n_{j=1}( (\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' AvNormEntrop=\dfrac{2Entrop}{Entrop_i(A)+Entrop_i(B)}
      #' }
      #' \deqn{
      #' \sigma^2_{AvNormEntrop}=\dfrac{AvNormEntrop \cdot (1-AvNormEntrop)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvNormEntrop}: normalized entropy using the arithmetic mean of the entropies on the map and on ground truthing.
      #'   \item \eqn{Entrop_i(B)}: entropy of the map with respect to the category on ground truthing.
      #'   \item \eqn{Entrop_i(A)}: Entropy of the map with respect to the category of the map.
      #'   \item \eqn{Entrop}: map entropy.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return normalized entropy using the arithmetic mean of the entropies on the map and on ground truthing, its variance and confidence interval.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$AvNormEntrop()
      #'
      #' @aliases

     AvNormEntrop = function(v=NULL,a=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

      Entrop_iB <- - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base=v)),na.rm=TRUE)
      Entrop_iA <- - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
        if (Entrop_iA + Entrop_iB == 0) {
          stop ("/ by 0")
        }else{
          AvNormEntrop <- 2 * self$Entrop(v)[[1]] / (Entrop_iA + Entrop_iB)
          VarAvNormEntrop <- abs((AvNormEntrop*(1-AvNormEntrop))/sum(self$values))
          ConfInt <- private$ConfInt(AvNormEntrop,VarAvNormEntrop,a)
        }

     return(list(AvNormEntrop=AvNormEntrop,VarAvNormEntrop=VarAvNormEntrop,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public method that calculates normalized entropy using the geometric mean of the entropies on the map and on ground truthing. The method also offers variation. The reference \insertCite{ghosh2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(A)=-\sum^n_{j=1}( (\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' GeomAvNormEntrop=\dfrac{Entrop}{\sqrt{Entrop_i(A) \cdot Entrop_i(B)}}
      #' }
      #'\deqn{
      #' \sigma^2_{GeomAvNormEntrop}=\dfrac{GeomAvNormEntrop \cdot (1-GeomAvNormEntrop)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{GeomAvNormEntrop}: normalized entropy using the geometric mean of the entropies on map and on ground truthing.
      #'   \item \eqn{Entrop_i(B)}: entropy of the map with respect to the category on ground truthing.
      #'   \item \eqn{Entrop_i(A)}: Entropy of the map with respect to the category of the map.
      #'   \item \eqn{Entrop}: map entropy.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using the geometric mean of the entropies on map and on ground truthing, its variance and confidence interval.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$GeomAvNormEntrop()
      #'
      #' @aliases

     GeomAvNormEntrop = function(v=NULL,a=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}
      Entrop_iB <- - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base=v)),na.rm=TRUE)
      Entrop_iA <- - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
       if (Entrop_iA * Entrop_iB == 0) {
        stop ("/ by 0")
       }else{
         GeomAvNormEntrop <- self$Entrop(v)[[1]] / sqrt(Entrop_iA * Entrop_iB)
         VarGeomAvNormEntrop <- abs((GeomAvNormEntrop*(1-GeomAvNormEntrop)))
         ConfInt<-private$ConfInt(GeomAvNormEntrop,VarGeomAvNormEntrop,a)
         }
     return(list(GeomAvNormEntrop=GeomAvNormEntrop,VarGeoAvNormEntrop=VarGeomAvNormEntrop,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },

      #' @description Public mathod that provides normalized entropy using the arithmetic mean of the maximum entropies on map and on ground truthing.The method also offers variation. The reference \insertCite{strehl2002relationship}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #' Entrop_i(A)=-\sum^n_{j=1}( (\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\dfrac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\dfrac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' AvMaxNormEntrop=\dfrac{2 Entrop}{max(Entrop_i(A))+max(Entrop_i(B))}=\dfrac{Entrop}{\log \sqrt{M}}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item \eqn{AvMaxNormEntrop}: normalized entropy using the arithmetic mean of the maximum entropies on map and on ground truthing.
      #'   \item \eqn{Entrop_i(B)}: entropy of the map with respect to the category on ground truthing.
      #'   \item \eqn{Entrop_i(A)}: Entropy of the map with respect to the category of the map.
      #'   \item \eqn{Entrop}: map entropy.
      #'   \item \eqn{x_{+i}}: sum of all elements in column i.
      #'   \item \eqn{x_{i+}}: sum of all elements in row i.
      #'   \item \eqn{M}: number of elements of the matrix.
      #'   \item \eqn{N}: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using the arithmetic mean of the maximum entropies on map and on ground truthing, its variance and confidence interval.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$AvMaxNormEntrop()
      #'
      #' @aliases

     AvMaxNormEntrop = function(v=NULL,a=NULL){
       if(!is.null(v)){
         v<-v
       }else{v<-10}

        AvMaxNormEntrop <- self$Entrop(v)[[1]] / log(sqrt(length(self$values)),base=v)
        VarAvMaxNormEntrop <- abs((AvMaxNormEntrop*(1-AvMaxNormEntrop))/sum(self$values))
        ConfInt <- private$ConfInt(AvMaxNormEntrop,VarAvMaxNormEntrop,a)

     return (list(AvMaxNormEntrop=AvMaxNormEntrop,Var=VarAvMaxNormEntrop,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },



      #' @description Public method that calculates the tau index and its variance. Its value indicates how much the classification has improved compared to a random classification of the N elements into M groups. The method also offers the variance. The reference \insertCite{book}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list with Tau index, its variance and confidence interval.
      #' @description
      #' The mathematical expression is:
      #'
      #' \deqn{
      #' PrAgCoef=\dfrac{1}{M}
      #' }
      #' \deqn{
      #' Tau = \dfrac{OverallAcc-CoefAccPr}{1-PrAgCoef}
      #' }
      #'
      #' \deqn{
      #' \sigma^2_{Tau}=\dfrac{OverallAcc \cdot (1-OverallAcc)}{N \cdot (1-CoefAccPr)^2}
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item \eqn{OverallAcc}: overall accuracy.
      #'   \item \eqn{PrAgCoef}: a priori random agreement coefficient.
      #'   \item \eqn{M}: number of classes.
      #'   \item \eqn{N}: number of elements of the matrix, cardinal of the matrix.
      #' }
      #' @param a Significance level. By default 0.05.
      #' @examples
      #' A<-matrix(c(238051,7,132,0,0,24,9,2,189,1,4086,188,0,4,16,45,1,0,939,5082,
      #' 51817,0,34,500,1867,325,17,0,0,5,11148,1618,78,0,0,0,0,48,4,834,2853,340,
      #' 32,0,197,5,151,119,135,726,6774,75,1,553,0,105,601,110,174,155,8257,8,0,
      #' 29,36,280,0,0,6,5,2993,0,115,2,0,4,124,595,0,0,4374),nrow=9,ncol=9)
      #' p<-MatCon$new(A,Source="Muñoz 2016")
      #' p$Tau()
      #'
      #' @aliases

     Tau = function(a=NULL){
        Ca<-1/nrow(self$values)
        Tau <- ((self$OverallAcc()[[1]]-Ca)/(1-Ca))
        VarTau <- ((self$OverallAcc()[[1]]*(1-self$OverallAcc()[[1]]))/(sum(self$values)*(1-Ca)))
        ConfInt <- private$ConfInt(Tau,VarTau,a)
     return(list(Tau=Tau,VarTau=VarTau,Conf_Int=c(ConfInt$ConfInt_inf,ConfInt$ConfInt_sup)))
     },




# Functions that return multiple indices ----------------------------------






      #' @description Public method that calculates the pressures of the user and the producer jointly. The method also offers the standard desviations. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list containing the producer's and user's accuracies and their standard deviations, respectively.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$UserProdAcc()
      #'
      #' @aliases

     UserProdAcc =function(){
      #  calculation of Class accuracies and standard deviations
      nc <- nrow(self$values)
       for (i in 1:nc){
         pcpa <- self$ProdAcc()[[1]]
         pcua <-self$UserAcc()[[1]]
         pcpasd <- sqrt(self$ProdAcc()[[2]])
         pcuasd <- sqrt(self$UserAcc()[[2]])
       }
     return(list(ProdAcc=pcpa,ProdAccSDeviation=pcpasd,UserAcc= pcua, UserAccSDeviation=pcuasd))
     },


      #' @description  Public method that calculates the general Kappa agreement index, its standard deviation and the test statistic to test its significance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list of the kappa coefficient, its standard deviation, and the value of its test statistic.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$DetailedKappa()
      #'
      #' @aliases

     DetailedKappa=function (){
       nc <- nrow(self$values)
       SumaMatriz <-sum(self$values)

       # The 4 coefficients
       O1 <- sum(diag(self$values))/SumaMatriz #OA
       O2 <- sum((self$sumcol*self$sumfil))/(SumaMatriz*SumaMatriz) #EA
       O3 <- sum(diag(self$values)*(self$sumcol+self$sumfil))/(SumaMatriz*SumaMatriz)
       mintermedia1<- matrix(rep(self$sumcol, nc), nrow =nc, ncol=nc, byrow=TRUE)
       mintermedia2<- matrix(rep(self$sumfil, nc), nrow =nc, ncol=nc, byrow=FALSE)
       mintermedia3 <-(mintermedia1+mintermedia2)^2
       O4 <- sum(self$values*(t(mintermedia3)) )/(SumaMatriz*SumaMatriz*SumaMatriz)
       t1 <- (1-O1) #no oa
       t2<- (1-O2) #no ea
       K <- (O1-O2)/t2 #Kappa
       t3<- 2*O1*O2-O3
       t4<- O4-4*(O2^2)
       t5<- O1*t1/(t2^2)+2*t1*t3/(t2^3)+(t1^2)*t4/(t2^4)
       SdK <- sqrt((1/SumaMatriz)*t5) #standard desviation kappa
       CV <- K/SdK #the test statistic to test significance

     return(list(K=K, SdK=SdK, CV=CV))
     },


      #' @description Public method that calculates the Kappa class agreement index (conditional Kappa) from the perspective of user (i) and producer (j) and its standard desviations. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list with conditional Kappa index of the user and the producer, and its corresponding standard deviation.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$DetailedCondKappa ()
      #'
      #' @aliases


     DetailedCondKappa = function(){
       SumaMatriz <-sum(self$values)
        # In %
        ConfM<- self$values/sum(self$values)

       # UnWeighted marginals (quantities)
        pcol <- apply(ConfM,2,sum)
        prow<- apply(ConfM,1,sum)
       # Initialization of vectors
        nc  <- nrow(ConfM)
        Ki_ <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        K_j <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        Ki_sd <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        K_jsd <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
          for (i in 1:nc){
            Ki_[i] <- ((ConfM[i,i]/prow[i])-pcol[i])/(1-pcol[i])
            K_j[i] <- ((ConfM[i,i]/pcol[i])-prow[i])/(1-prow[i])
            ti1 <- prow[i]-ConfM[i,i]
            ti2<-  ti1/((prow[i]^3)*(1-pcol[i])^3)
            ti3<-  ti1*(pcol[i]*prow[i]-ConfM[i,i])
            ti4<-  ConfM[i,i]*(1-pcol[i]-prow[i]+ConfM[i,i])
            Ki_sd[i] <- sqrt((1/SumaMatriz)*ti2*(ti3+ti4))
            tj1 <- pcol[i]-ConfM[i,i]
            tj2<-  tj1/((pcol[i]^3)*(1-prow[i])^3)
            tj3<-  tj1*(pcol[i]*prow[i]-ConfM[i,i])
            tj4<-  ConfM[i,i]*(1-pcol[i]-prow[i]+ConfM[i,i])
            K_jsd[i] <- sqrt((1/SumaMatriz)*tj2*(tj3+tj4))
          }
     return(list(UserCondKappa=Ki_, SD_UserCondKappa=Ki_sd, ProdCondKappa=K_j, SD_ProdCondKappa=K_jsd))
     },


      #' @description Public method that calculates the values of quantity, change and shift. The reference \insertCite{pontius2014}{PaolaR6Nuevo} is followed for the computations.
      #' @param TI Time interval (default value = 1)
      #' @param SF Scale factor for results (default value = 1)
      #' @return A list of general values for the interval t of difference, quantity, shift, and shift.In addition to the differences for categories, number of components, change of categories and turn of the components.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$QES(TI=1, SF=6)
      #'
      #' @aliases

     QES = function(TI=NULL, SF=1){
      # Overall Quantity, Exchange and Shift values
      # TI Time interval
      # SF Scale factor
       if(!is.null(TI)){
         TI<-TI
       }else{TI<-1}

       if(!is.null(SF)){
         SF<-SF
       }else{SF<-1}

      nc <- nrow(self$values)
      SumaMatriz <-sum(self$values)
      SumaDigonal<-sum(diag(self$values))
      ee<- matrix(rep(0, nc*nc), nrow =nc, ncol=nc, byrow=TRUE)
      d<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
      q<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
      e<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
      s<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        for (j in 1:nc){
          for (i in 1:nc){
            if (i>j){#diagonal 0//triangular superior
            ee[j,i] <-  (min(self$values[i,j], self$values[j,i]))*2
            }else{
              ee[j,i] <- 0
            }
          }
        }

        for (j in 1:nc){
        d[j]<-d[j]+ sum(self$values[,j])+sum(self$values[j,])-2*self$values[j,j]
        q[j]<-q[j]+ abs(sum(self$values[,j])- sum(self$values[j,]))
        e[j]<-e[j]+ sum(ee[,j])+sum(ee[j,])
        s[j]<-d[j]- q[j]-e[j]
        }

      d<-d/((TI)*SumaMatriz)
      q<-q/((TI)*SumaMatriz)
      e<-e/((TI)*SumaMatriz)
      s<-s/((TI)*SumaMatriz)
      D<- SF*sum(d)/2
      Q<- SF*sum(q)/2
      E<- SF*sum(e)/2
      S<- SF*sum(s)/2
     return(list(OverallDiff=D, OverallQuant=Q, OverallExch=E, OverallShift=S, diff=d, quant=q, exch=e, shift=s))
     },




# Functions that return matrices ------------------------------------------

      #' @description Public method that types the values of each cell. The total sum of the original matrix is used for typing. The resulting values can be presented as real (parameter RaR=1) or as a percentage (parameter RaR !=1)
      #' @description
      #'  \deqn{
      #' MTypify=\dfrac{x_{ij}}{\sum^n_{i,j=1} x_{ij}}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item MTyipify: typified matrix.
      #'   \item \eqn{x_{ij}}: matrix element.
      #' }
      #' @param RaR "1" indicates result as real, other values mean percentage as integer. By default RaR=1.
      #' @return A list with original matrix and typified matrix
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A, Source="Congalton and Green 2008")
      #' p$MTypify(RaR=5)

      #Cell Values of a matrix are typified

     MTypify =function(RaR=NULL){
        if(!is.null(RaR)){
          RaR <- RaR
        }else{R<-1}
      # Create a matrix in which all elements are proportions
      # such that the sum of all the elements is 1
      ConfM=self$values

      M <- ConfM/(sum(ConfM))
        if (RaR==1){
        M <- ConfM/(sum(ConfM))
        return(M)
        } else {
      M <- ConfM/(sum(ConfM))
      M[] <- as.integer(100*M)
      return(list(OriginalMatrix=self$values,TypifyMatrix=M))
      }
     },



      #' @description Public method in which multiple parameters are calculated for the given confusion matrix. The references \insertCite{congalton2008,cohen1960,munoz2016}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list containing confusion matrix, dimension, total sum of cell values, overall precision, overall variance precision, global precision kappa index, global kappa simplified variance, producer precision by class, user precision by class, pseudoceros matrix.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$AllParameters()
      #'
      #' @aliases

     AllParameters=function(){
     # Overall Accuracy
     # Kappa
     # User accurary
     # Producer accurary
     # Variances
     # Pseudozero matrix
      OverallAcc<-self$OverallAcc()
      dimension <- nrow(self$values)
      SumaMatriz <-sum(self$values)
      PAcuerdo <- OverallAcc[[1]]
      ExProdu <- self$ProdAcc()[[1]]
      UserAcc <-self$UserAcc()[[1]]
      PAAzar <- sum((self$sumfil*self$sumcol))/(SumaMatriz*SumaMatriz)
      Kappa <- self$Kappa()[[1]]
      VarPAcuerdo <- self$OverallAcc()[[2]]
      VarKappa <-  self$Kappa()[[2]]
      MPseudoceros <- self$MPseudoZeroes()[[2]]
      salida<-list(Matrix=self$values, Dimension =dimension, n=SumaMatriz, OverallAcc=PAcuerdo, VarOverallAcc=VarPAcuerdo, Kappa=Kappa,VarKappa=VarKappa,ProdAcc=ExProdu,UserAcc=UserAcc,MPseudoceros=MPseudoceros)
     return(salida)
     },



      #' @description Public method that provides N resamples of the confusion matrix from a MatCon object. The reference \insertCite{ariza2011}{PaolaR6Nuevo} is followed for the computations.
      #' @param n Number of resamples.
      #' @param pr Probability for resampling. By default, the probability of success for each cell will be taken.
      #' @return A list formed by the original confusion matrix and simulated matrices, from the confusion matrix. The multinomial distribution is applied.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A, Source="Congalton and Green 2008")
      #' p$MBootStrap(2)
      #'
      #' @aliases

     MBootStrap=function(n,pr=NULL){
      #matrix range
      nc<-ncol(self$values)
      #convert to vector
      #M1<-as.vector(self$values)
      M1<-self$values
      #probability
      if(is.null(pr)){
        pr<-M1/sum(M1)
      }else{
        pr<-pr
      }

      #M2: matrix list
      M2<-list()
      #resampling with multinomial
      boots<-rmultinom(n,sum(M1),pr)

      #save simulated matrix
        for(i in 1:ncol(boots)){
          M2[[i]]<-matrix(boots[,i],ncol=nc,nrow=nc)
        }

     return(list(OriginalMatrix=self$values,BootStrap=M2))
     },



      #' @param n Number of iteration. By default n=100.
      #' @return A list formed by the original confusion matrix and the normalized matrix.
      #' @description Public method that carries out an iterative process is carried out where each element is divided by the total of the sum of its row, thus obtaining new values. In the next iteration, all the elements are added by columns and each element is divided by the total of its column and they obtain new values, and so on. The references \insertCite{fienberg1970,munoz2016}{PaolaR6Nuevo} is followed for the computations.
      #' @examples
      #' A<-matrix(c(238051,7,132,0,0,24,9,2,189,1,4086,188,0,4,16,45,1,0,939,5082,
      #' 51817,0,34,500,1867,325,17,0,0,5,11148,1618,78,0,0,0,0,48,4,834,2853,340,
      #' 32,0,197,5,151,119,135,726,6774,75,1,553,0,105,601,110,174,155,8257,8,0,
      #' 29,36,280,0,0,6,5,2993,0,115,2,0,4,124,595,0,0,4374),nrow=9,ncol=9)
      #' p<-MatCon$new(A,Source="Muñoz 2016")
      #' p$MNormalize()$values
      #'
      #' @aliases

     MNormalize=function(n=NULL){

      if(!is.null(n)){
        n <- n
      }else{n<-100}

      rg<-nrow(self$values)
      x1<-self$values
        for (k in 1:n) {
        sumfilas=apply(x1,1,sum)
          for (i in 1:rg) {
          x1[i,]=x1[i,]/sumfilas[i]
          }
        sumcolumnas=apply(x1,2,sum)
          for (j in 1:rg) {
          x1[,j]=x1[,j]/sumcolumnas[j]
          }
        }
      NormMatrix<-x1
     return(list(OriginalMatrix=self$values,NormalizeMatrix=NormMatrix))
     },



      #' @description Public method that small values are calculated for empty cells of the matrix. All non-empty cells of the matrix change their values. This function will not be applied if all the elements of the matrix are different from 0. The reference \insertCite{munoz2016}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list formed by the original confusion matrix and the Pseudozeroes matrix.
      #' @examples
      #' A<-matrix(c(238051,7,132,0,0,24,9,2,189,1,4086,188,0,4,16,45,1,0,939,5082,
      #' 51817,0,34,500,1867,325,17,0,0,5,11148,1618,78,0,0,0,0,48,4,834,2853,340,
      #' 32,0,197,5,151,119,135,726,6774,75,1,553,0,105,601,110,174,155,8257,8,0,
      #' 29,36,280,0,0,6,5,2993,0,115,2,0,4,124,595,0,0,4374),nrow=9,ncol=9)
      #' p<-MatCon$new(A,Source="Muñoz 2016")
      #' p$MPseudoZeroes()
      #'
      #' @aliases

     MPseudoZeroes = function(){

       #It checks if any element is 0
       k=0
       rg<-nrow(self$values)
        for (i in 1:rg) {
         for (j in 1:rg) {
          if((self$values[i,j]!=0)==TRUE){
            k=k+1
             if(k==length(self$values)){
               stop("The Pseudoceros Matrix removes the zeros from the matrix. Your matrix does not have any zeros to remove.")
             }}
        }
       }

       ConfM=self$values
       SumaMatriz <-sum(ConfM)
       MLandas <- (self$sumfil %*% t(self$sumcol))/(SumaMatriz*SumaMatriz)
       K <- (SumaMatriz*SumaMatriz - sum(ConfM*ConfM))/sum((SumaMatriz*MLandas - ConfM )^2)
       MPseudoceros <- (SumaMatriz/(K+SumaMatriz))*(ConfM + K*MLandas)

     return(list(OriginalMatrix=self$values,PseudoZeroesMatrix=MPseudoceros))
     },



# Functions that use weight matrices --------------------------------------




      #' @description Public method that calculates the general Tau concordance index and its standard deviation.
      #' @param WV Weights vector (as matrix)
      #' @return  A list with the weight matrix, the Tau index, its standard deviation and its statistics.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' WV <-matrix(c(0.4, 0.1, 0.4, 0.1), ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' p$DetailedWTau(WV)
      #'
      #' @aliases

     DetailedWTau = function(WV){
       nc <- nrow(self$values)
       SumaMatriz <-sum(self$values)
      # In %
       ConfM<- self$values/SumaMatriz
      # UnWeighted marginals (prob)
       pcol <- apply(ConfM,2,sum)
       prow<- apply(ConfM,1,sum)
       O1 <- sum(diag(ConfM)  )
       O2 <- sum(WV*pcol)
       O3 <- sum(diag(ConfM)*(WV+pcol))
       mintermedia1<- matrix(rep(pcol, nc), nrow =nc, ncol=nc, byrow=FALSE)
       mintermedia2<- matrix(rep(WV, nc), nrow =nc, ncol=nc, byrow=TRUE)
       mintermedia3 <-(mintermedia1+mintermedia2)^2
       O4 <- sum(ConfM*mintermedia3)
       t1<- (1-O1) #probabilidad error general porporcional
       t2<- (1-O2) #probabilidad error productor proporcional
       t3<- O1*t1/(t2^2)
       t4<- 2*t1*(2*O1*O2-O3)/(t2^3)
       t5<- (t1^2)*(O4-4*O2^2)/(t2^4)
       Tau <- (O1-O2)/t2
       SdT <- sqrt((t3+t4+t5)/SumaMatriz)
       CV<- Tau/SdT
     return(list(WeightsVector=WV, Tau=Tau, SdT=SdT, CV=CV))
     },

      #' @description Public method that calculates the general Kappa agreement index (weighted) and its standard deviation. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @param WM  Weight matrix
      #' @return A list with the weight matrix, kappa index obtained from the original matrix and the weight matrix, its standard desviations and the value of its test statistic.
      #' @examples
      #' A <- A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' WM<- t(matrix(c(1,0,0.67,1,0,1,0,0,1,0,1,1,0.91,0,0.61,1), nrow = 4, ncol=4))
      #' p<-MatCon$new(A)
      #' p$DetailedWKappa(WM)
      #'
      #' @aliases

     DetailedWKappa = function(WM){
       nc <- nrow(self$values)
       SumaMatriz <-sum(self$values)
      # In %
       ConfM<- self$values/SumaMatriz
      # UnWeighted marginals (prob)
       pcol <- apply(ConfM,2,sum)
       prow<- apply(ConfM,1,sum)
      # Weighted matrix
       WConfM<-ConfM*WM

      # The 4 coefficients
       Ow1 <- sum(WM*ConfM)
       Ow2 <- sum(t(WM*prow)*pcol)
       c1<- (1-Ow1)
       c2<- (1-Ow2)
       wi_ <- WM %*% pcol
       w_j <- WM %*% prow
       mintermedia1<- matrix(rep(wi_, nc), nrow =nc, ncol=nc, byrow=FALSE)
       mintermedia2<- matrix(rep(w_j, nc), nrow =nc, ncol=nc, byrow=TRUE)
       mintermedia3 <-(mintermedia1+mintermedia2)*c1
       mintermedia4 <- (WM*c2-mintermedia3)^2
       Ow4 <- sum(ConfM*mintermedia4)
       K <- (Ow1-Ow2)/c2
       SdK <- sqrt((Ow4-(Ow1*Ow2-2*Ow2+Ow1)^2)/(SumaMatriz*(c2^4)))
       CV <- K/SdK
     return(list(WeightMatrix=WM, K=K, SdK=SdK, CV=CV))
     },


      #' @description Public method that calculates the weighted accuracies and standard deviations of the user and the producer. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @param WM Weight matrix
      #' @return A list with weight matrix, Matrix formed with its original elements and their corresponding weights, general accuracy of the weight matrix obtained, accuracy of the producer and user and their standard deviations,
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' WM<- t(matrix(c(1,0,0.67,1,0,1,0,0,1,0,1,1,0.91,0,0.61,1), nrow = 4, ncol=4))
      #' p$UserProdAcc_W(WM)
      #'
      #' @aliases

     UserProdAcc_W =function(WM){
     #  Calculation of weighted Class accuracies
     #  The error matrix and the weight matrix are required
     #  The weights for diagonal cells are 1
     #  The values of the weights for the off-diagonal cells  must be in the range [0,1]


     # UnWeighted marginals (quantities)
        ncol <- self$sumcol
        nrow<- self$sumfil

        # In %
        ConfM<- self$values/sum(self$values)
        # Weighted matrix
        WConfM<-ConfM*WM

        # Weighted OA
        WOverallAcc <- sum(diag(WConfM))/sum(WConfM)
        # Weighted marginals
        mcol<- apply(WConfM,2,sum)
        mrow<- apply(WConfM,1,sum)
        # UnWeighted marginals (proportions)
        pj <- apply(ConfM,2,sum)
        pi <- apply(ConfM,1,sum)

        # Initialization of vectors
        nc <- nrow(ConfM)
        wi<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        wj<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        # Weighted per class user's and producer's accuracies
        wpcua<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        wpcpa<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        pcpasd <-matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE) # Per class producer's accuracy standard deviation
        pcuasd <-matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE) # Per class user's accuracy standard deviation
            for (i in 1:nc){
                wi[i]    <- sum(pj*WM[i,])
                wj[i]    <- sum(pi*WM[,i])
                wpcua[i]  <- sum(WConfM[i,])/sum(ConfM[i,])
                wpcpa[i]  <- sum(WConfM[,i])/sum(ConfM[,i])
                pcpasd[i] <- sqrt(wpcpa[i]*(1-wpcpa[i])/ncol[i])
                pcuasd[i] <- sqrt(wpcua[i]*(1-wpcua[i])/nrow[i])
            }

     return(list(OriginalWeightMatrix=WM,WMatrix=WConfM, WOverallAcc=WOverallAcc, WPrAcc=wpcpa,WPrAccSDeviation=pcpasd,WUserAcc= wpcua, WUserAccSDeviation=pcuasd))
     },



# test function -----------------------------------------------------------


      #' @description Public method that provides the Hellinger distance between two elements of the MatCon class.
      #' The reference \insertCite{garcia2018}{PaolaR6Nuevo} is followed for the computations.
      #' The mathematical expression is:
      #'
      #' \deqn{
      #' HD = \dfrac{4nm}{n+m} \sum^{M}_{i=1} (\sqrt{p_i}-\sqrt{q_i})^2
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item \eqn{HD}: Hellinger Distance
      #'   \item \eqn{n}: number of elements in the matrix A.
      #'   \item \eqn{p_i}: element i of the probability vector of matrix A.
      #'   \item \eqn{q_i}: element i of the probability vector of matrix B.
      #' }
      #' @return The statistic value of the statistical test based on the Hellinger distance.
      #' @param f f Element of the MatCon.
      #' @param p matrix probability vector. By default, the probability of success for each cell is taken.
      #' @param q matrix probability vector. By default, the probability of success for each cell is taken.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' r<-MatCon$new(A,Source="Congalton and Green 2008")
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f <- MatCon$new(B,Source="Congalton and Green 2008")
      #' p$StHell(f)
      #'
      #' @aliases

    StHell = function(f,p=NULL,q=NULL){

      if(class(f)[1]!="MatCon"){
       warning("A MatCon element is not being introduced")
        stop(" ")
      }
      A<-self$values
      B<-f$values
      if(is.null(p)){
      p<-A/sum(A)
      }else{p<-p}
      if(is.null(q)){
        q<-B/sum(B)
      }else{q<-q}

      if(length(q)!=length(p)){
        stop("Probabilities with different sizes.")
      }else{
        p_orig <- 4*((sum(self$values)*sum(B)/(sum(self$values)+sum(B))) * sum((sqrt(p) - sqrt(q))^2))

      return(StHell=p_orig)
      }
    },



      #' @description Public method that tests whether two independent confusion matrices of the MatCon class are significantly different using their kappa index.
      #' The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' The mathematical expression to calculate its statistic is:
      #'
      #' \deqn{
      #' Z = \dfrac{|k1-k2|}{\sqrt(var(K1)+var(K2))}
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item \eqn{k1}: kappa index of matrix A
      #'   \item \eqn{k2}: kappa index of matrix B
      #'   \item \eqn{var(k1)}: variance of k1.
      #'   \item \eqn{var(k2)}: variance of k2.
      #' }
      #' @return A list with the value of the statistic between kappa values and its z score for a given alpha significance level.
      #' @param alpha significance level. By default alpha=0.05.
      #' @param f Element of the MatCon class.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f <- MatCon$new(B,Source="Congalton and Green 2008")
      #' p$Kappa.test(f)
      #'
      #' @aliases

      Kappa.test=function(f,alpha=NULL){
        if(class(f)[1]!="MatCon"){
          warning("A MatCon element is not being introduced")
          stop(" ")
        }
        if(is.null(alpha)){
          alpha<-0.05
        }else{alpha<-alpha}

        k1<-self$Kappa()[[1]]
        k2<-f$Kappa()[[1]]
        v1<-self$Kappa()[[2]]
        v2<-f$Kappa()[[2]]

        Z<-abs(k1-k2)/(sqrt(v1+v2))
        cl<-qnorm(1-alpha/2)

        if(Z>-cl & Z<cl){
          cat("The null hypothesis is not rejected. Therefore, the kappa values and the confusion matrices do not present significant differences.\n")
        }else{cat("The null hypothesis is rejected. Therefore, their kappa values and confusion matrices are significantly different.\n")}

        return(list(St=Z,Z=cl))
      },



      #' @description Public method that tests whether two independent confusion matrices of the MatCon class are significantly different using their overall accuracy index.
      #' The reference \insertCite{book,ma1995Tau}{PaolaR6Nuevo} is followed for the computations.
      #' The mathematical expression to calculate its statistic is:
      #'
      #' \deqn{
      #' Z = \dfrac{|k1-k2|}{\sqrt{var(k1)+var(k2)}}
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item \eqn{k1}: overall index of matrix A
      #'   \item \eqn{k2}: overall index of matrix B
      #'   \item \eqn{var(K1)}: variance of k1.
      #'   \item \eqn{var(K2)}: variance of k2.
      #' }
      #' @return A list of the statistic's value between the overall accuracies and its z-score for a given alpha significance level.
      #' @param alpha significance level. By default alpha=0.05.
      #' @param f Element of the MatCon class.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f<-MatCon$new(B,Source="Congalton and Green 2008")
      #' p$OverallAcc.test(f)
      #'
      #' @aliases

    OverallAcc.test=function(f,alpha=NULL){
      if(class(f)[1]!="MatCon"){
        warning("A MatCon element is not being introduced")
        stop(" ")
      }
      if(is.null(alpha)){
        alpha<-0.05
      }else{alpha<-alpha}

      k1<-self$OverallAcc()[[1]]
      k2<-f$OverallAcc()[[1]]
      v1<-self$OverallAcc()[[2]]
      v2<-f$OverallAcc()[[2]]

      #Ma-Tau
      Z<-abs(k1-k2)/sqrt(v1+v2)

      cl<-qnorm(1-alpha/2)

      if(Z>-cl & Z<cl){
        cat("The null hypothesis is not rejected. Therefore, the kappa values and the confusion matrices do not present significant differences.\n")
      }else{cat("The null hypothesis is rejected. Therefore, their kappa values and confusion matrices are significantly different.\n")}

    return(list(St=Z,Z=cl))
    },


      #' @description Public method that tests whether two independent confusion matrices of the MatCon class are significantly different using their Tau index.
      #' The reference \insertCite{book,ma1995Tau}{PaolaR6Nuevo} is followed for the computations.
      #' The mathematical expression to calculate its statistic is:
      #'
      #' \deqn{
      #' Z = \dfrac{|k1-k2|}{\sqrt{var(k1)+var(k2)}}
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item \eqn{k1}: Tau index of matrix A
      #'   \item \eqn{k2}: Tau index of matrix B
      #'   \item \eqn{var(k1)}: variance of k1.
      #'   \item \eqn{var(k2)}: variance of k2.
      #' }
      #' @return A list of the statistic's value between the Tau index and its z-score for a given alpha significance level.
      #' @param alpha significance level. By default alpha=0.05.
      #' @param f Element of the MatCon class.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f<-MatCon$new(B,Source="Congalton and Green 2008")
      #' p$Tau.test(f)
      #'
      #' @aliases

    Tau.test=function(f,alpha=NULL){
      if(class(f)[1]!="MatCon"){
        warning("A MatCon element is not being introduced")
        stop(" ")
      }
      if(is.null(alpha)){
        alpha<-0.05
      }else{alpha<-alpha}

      k1<-self$Tau()[[1]]
      k2<-f$Tau()[[1]]
      v1<-self$Tau()[[2]]
      v2<-f$Tau()[[2]]


      #Ma-Tau
      Z<-abs(k1-k2)/sqrt(v1+v2)

      cl<-qnorm(1-alpha/2)

      if(Z>-cl & Z<cl){
        cat("The null hypothesis is not rejected. Therefore, the kappa values and the confusion matrices do not present significant differences.\n")
      }else{cat("The null hypothesis is rejected. Therefore, their kappa values and confusion matrices are significantly different.\n")}

    return(list(St=Z,Z=cl))
    },

      #' @description Public method that performs a homogeneity test between two matrices of the MatCon class based on the Hellinger distance.
      #' The test considers the individual cell values in the matrices.
      #' The reference \insertCite{garcia2018}{PaolaR6Nuevo} is followed for the computations.
      #' @return p value and decision to make.
      #' @param n1 Number of bootstraps that you want to generate. By default n=10000.
      #' @param alpha significance level. By default alpha=0.05.
      #' @param f Element of the MatCon class.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton and Green 2008")
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f<-MatCon$new(B,Source="Congalton and Green 2008")
      #' p$TSCM.test(f)
      #'
      #' @aliases

    TSCM.test=function(f,n1=NULL,alpha=NULL){
      if(class(f)[1]!="MatCon"){
        warning("A MatCon element is not being introduced")
        stop(" ")
      }
      if(is.null(n1)){
        n1<-10000
      }else{n1<-n1}

      if(is.null(alpha)){
        alpha<-0.05
      }else{alpha<-alpha}

      A<-self$values
      B<-f$values
      n<-length(A)
      m<-length(B)
      p2<-A/sum(A)
      q2<-B/sum(B)

      p_orig<-self$StHell(f)

      #Probability defined between p and q
      p_0<-c()
      for(i in 1:length(p2)){
        p_01<-(n*p2[i]+m*q2[i])/(n+m)
        p_0<-c(p_0,p_01)
      }
      #bootstrap
      q1<-list()
      p1<-list()
      p1<-self$MBootStrap(n1,p_0)[[2]]
      q1<-f$MBootStrap(n1,p_0)[[2]]
      #calculation of hellinger statistics by pairs

      Tn<-c()

      for (i in 1:length(p1)) {
        Tn1<-self$StHell(f,p=(p1[[i]]/sum(p1[[i]])),q=(q1[[i]]/sum(q1[[i]])))
        Tn<-c(Tn,Tn1)
      }
      #Those that meet the condition of being greater than the original statistic are saved.
      Tn_boot<-c()
      for (i in 1:length(Tn)) {
        if((Tn[i]>=p_orig)==TRUE){
          Tn_boot<-c(Tn_boot,Tn[i])
        }
      }

      #mean p
      pvalue<-length(Tn_boot)/length(Tn)

     if(pvalue>alpha){
       cat("The null hypothesis is not rejected, both confusion matrices exhibit a similar level of accuracy.\n")
     }else{
       cat("The hypothesis that both distributions are is still rejected and the confusion matrices, therefore, are not similar.\n")
     }
    return(pvalue=pvalue)
    }




   ),
   private = list(

     #' @description Private method to calculate the confidence interval from the value of a given index.
     #' @param a Significance level. By default 0.05
     #' @param p Index value
     #' @param var Index variance
     #' @return Confidence interval

     ConfInt=function(p,var,a=NULL){

       if(is.null(a)){
         a<-0.05
       }else{a<-a}

       z<-qnorm(1-a/2)
       ConfInt_inf<-p-z*sqrt(var)
       ConfInt_sup<-p+z*sqrt(var)
       return(list(ConfInt_inf=ConfInt_inf,ConfInt_sup=ConfInt_sup))
     }
   ),
   active = list(
   )
)


