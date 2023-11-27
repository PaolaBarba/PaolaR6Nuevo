#' @title Confusion matrix
#' @description Using the confusion matrix, various indices are calculated.
#' @param values Confusion matrix
#' @param ID Identifier. By default ID is a date in YYYYMMDD format
#' @param Date Date provided by the user. By default the date provided by the system will be taken.
#' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
#' @return Object of class MatCon or an error if a matrix is not entered.
#' @details
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
#'
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
#' @examples
#' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
#' mc <- MatCon$new (A,ID=5,Date="27-10-2023",Source="Congalton&Green, 2008")
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

    #' @description
    #' Public method to create an instance of the MatCon class. When creating it, values must be given to the matrix. The optional possibility of adding metadata to the matrix is offered.
    #' The creation includes a series of checks on the data that, if not met, give coded error messages. The values of the matrix must be organized in such a way that the columns represent the categories in the reference and the rows represent the categories in the product being evaluated.
    #'
    #' @param values Confusion matrix
    #' @param ID Identifier. By default, the date in YYYYMMDD format will be taken as the ID.
    #' @param Date Date provided by the user. By default the date provided by the system will be taken.
    #' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
    #' @return Object of class MatCon or an error if a matrix isn't entered.
    #' @examples
    #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
    #' mc <- MatCon$new (A,ID=5,Date="27-10-2023",Source="Congalton&Green, 2008")
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
      #' OverallAcc = \frac{\sum_{i=1}^{n} x_{ii}}{\sum_{i, j=1}^{n} x_{ij}}
      #' }
      #'
      #' \deqn{
      #' \sigma^2_{OverallAcc}=\frac{OverallAcc \cdot (1-OverallAcc)}{N}
      #' }
      #' Where:
      #' \enumerate{
      #'   \item OverallAcc: overall accuracy.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item x_ij: element of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #'
      #' @return Overall accuracy and variance as a list.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A)
      #' p$OverallAcc()
      #'
      #' @aliases

     OverallAcc = function() {
     indice <- sum(diag(self$values))/sum(self$values)
     VarIndic<-abs((indice*(1-indice))/sum(self$values))
     return(list(OverallAcc=indice,VarOverallAcc=VarIndic))
     },




      #' @description Public method for deriving a class index called user's accuracy. The user's accuracy for the class i of thematic map is calculated by dividing the value in the diagonal of class i by the sum of all values in the row of the class i. The method also offers the variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @description
      #' The mathematical expression is:
      #' \deqn{
      #' UserAcc=\frac{x_{ii}}{\sum_{j=1}^n x_{ij}}
      #' }
      #'  \deqn{
      #' \sigma^2_{UserAcc}=\frac{UserAcc \cdot (1-UserAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item UserAcc: user accuracy.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item x_ij: element of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with a vector of values for the user's accuracy index of all classes and another vector with their variances.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$UserAcc()
      #'
      #' @aliases

     UserAcc = function(){
     #matrix range
     n <- sqrt(length(self$values))
     UserAcc <- rep(0,n)
     VarUserAcc<-rep(0,n)
       for (i in 1:n){
         UserAcc[i] <- self$values[i,i] / self$sumfil[i]
         VarUserAcc[i]<-abs((UserAcc[i]*(1-UserAcc[i]))/self$sumfil[i])
       }
     return(list(UserAcc=UserAcc,VarUserAcc=VarUserAcc))
     },




      #' @description Public method where the user's accuracy index is defined for a specific class i. The user precision for class i of the thematic map is calculated by dividing the value on the diagonal of class i by the sum of all values in the row of class i. The method also offers variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' UserAcc_{i}=\frac{x_{ii}}{\sum_{j=1}^n x_{ij}}
      #' }
      #' \deqn{
      #' \sigma^2_{UserAcc_i}=\frac{UserAcc_i \cdot (1-UserAcc_i)}{N}
      #' }
      #'
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item UserAcc_i: user accuracy index for class i.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item x_ij: element of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @param i User class to evaluate
      #' @return A list of the user's accuracy index values for class i and its variance.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$UserAcc_i(2)
      #'
      #' @aliases

     UserAcc_i=function(i){
      UserAcc_i = self$values[i,i] / self$sumfil[i]
      VarUserAcc_i=abs((UserAcc_i*(1-UserAcc_i))/self$sumfil[i])
     return(list(UserAcc_i=UserAcc_i,VarUserAcc_i=VarUserAcc_i))
     },




      #' @description  Public method for deriving a class index called producer's accuracy. The producer's accuracy for the class i of thematic map is calculated by dividing the value in the diagonal of class i by the sum of all values in the column of the class i. The method also offers the variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} if followed for the computations.
      #' @description
      #'  \deqn{
      #' ProdAcc=\frac{x_{jj}}{\sum_{j=1}^n x_{ij}}
      #' }
      #'\deqn{
      #' \sigma^2_{ProdAcc}=\frac{ProdAcc \cdot (1-ProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item ProdAcc: producer accuracy.
      #'   \item x_jj: diagonal element of the matrix.
      #'   \item x_ij: element of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with a vector of values for the producer's accuracy index of all classes and another vector with their variances.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$ProdAcc()
      #'
      #' @aliases

     ProdAcc = function (){
      n <- sqrt(length(self$values))
      ProdAcc <- rep(0,n)
      VarProdAcc<-rep(0,n)
        for(i in 1:n){
          ProdAcc[i] <- self$values[i,i] / self$sumcol[i]
          VarProdAcc[i]<-abs((ProdAcc[i]*(1-ProdAcc[i]))/self$sumcol[i])
        }
     return(list(ProdAcc=ProdAcc,VarProdAcc=VarProdAcc))
     },




      #' @description  Public method where the producer's accuracy index is defined for a specific class i. The user precision for class i of the thematic map is calculated by dividing the value on the diagonal of class i by the sum of all values in the column of class i. The method also offers variance. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ProdAcc_{i}=\frac{x_{jj}}{\sum_{j=1}^n x_{ij}}
      #' }
      #'\deqn{
      #' \sigma^2_{ProdAcc_i}=\frac{ProdAcc_i \cdot (1-ProdAcc_i)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item ProdAcc_i: producer accuracy index for class i.
      #'   \item x_jj: diagonal element of the matrix.
      #'   \item x_ij: element of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @param i Producer class to evaluate.
      #' @return A list of the producer's accuracy index values for class i and its variance
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$ProdAcc_i(1)
      #'
      #' @aliases

     ProdAcc_i = function(i){
      ProdAcc_i = self$values[i,i] / self$sumcol[i]
      VarProdAcc_i=abs((ProdAcc_i*(1-ProdAcc_i))/self$sumcol[i])
     return(list(ProdAcc_i=ProdAcc_i,VarProdAcc_i=VarProdAcc_i))
     },



      #' @description Public method that provides the average of the accuracy rates of the user and producer of a specific class. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #' The mathematical expression is:
      #'  \deqn{
      #' AvUserProdAcc_i=\frac{UserAcc_i+ProdAcc_i}{2}
      #' }
      #'\deqn{
      #' \sigma^2_{AvUserProdAcc_i}=\frac{AvUserProdAcc_i \cdot (1-AvUserProdAcc_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item AvUserProdAcc_i: average of user's and producer's accuracy.
      #'   \item UserAcc_i: user accuracy index for class i.
      #'   \item ProdAcc_i: producer accuracy index for class i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with average of user's and producer's accuracy and its variance for class i.
      #' @param i Class to evaluate.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$AvUserProdAcc_i(2)
      #'
      #' @aliases

     AvUserProdAcc_i = function(i){
      AvUserProdAcc_i = (self$UserAcc_i(i)[[1]] + self$ProdAcc_i(i)[[1]])/2
      VarAvUserProdAcc_i=abs((AvUserProdAcc_i*(1-AvUserProdAcc_i))/(self$sumcol[i]+self$sumfil[i]))
     return(list(AvUserProdAcc_i=AvUserProdAcc_i,VarAvUserProdAcc_i=VarAvUserProdAcc_i))
     },




      #' @description Public method that provides the Classification Success Index (CSI) applies to all class and gives an overall estimation of classification effectiveness. The reference \insertCite{koukoulas2001,turk2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description The mathematical expression is:
      #'  \deqn{
      #' Sucess=1-(1-AvUserAcc+1-AvProdAcc)=AvUserAcc+AvProdAcc-1
      #' }
      #'  \deqn{
      #' VarSucess=\frac{Sucess \cdot (1-Sucess)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item Sucess: classification succes index.
      #'   \item AvUserAcc: average accuracy from user's perspective.
      #'   \item AvProdAcc: average accuracy from producer's perspective.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the classification success index and its variance.
      #' @examples
      #' A<-matrix(c(0.3,0.02,0.01,0.12,0.19,0.03,0.02,0.01,0.3),nrow=3,ncol=3)
      #' p<-MatCon$new(A,Source="Labatut&Cherifi, 2011")
      #' p$Sucess()
      #'
      #' @aliases

     Sucess = function(){
      Sucess = self$AvUserAcc()[[1]] + self$AvProdAcc()[[1]] - 1
       #NO ESTOY SEGURA DE ESE 2*
       VarSucess=abs((Sucess*(1-Sucess))/2*sum(self$values))
     return(list(Sucess=Sucess,VarSucess=VarSucess))
     },




      #' @description  Public method that provides the Individual Classification Success Index (ICSI) applies to the classification effectiveness for one particular class of interest. The reference \insertCite{koukoulas2001,turk2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #' The mathematical expression is:
      #'  \deqn{
      #' Sucess_i=1-(1-UserAcc_i+1-ProdAcc_i)=UserAcc_i+ProdAcc_i-1
      #' }
      #'
      #' \deqn{
      #' \sigma^2_{Sucess_i}=\frac{Sucess_i \cdot (1-Sucess_i)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item Sucess_i: individual classification success index.
      #'   \item UserAcc_i: user accuracy index for class i.
      #'   \item ProdAcc_i: producer accuracy index for class i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the individual classification success index and its variance.
      #' @param i Class to evaluate.
      #' @examples
      #' A<-matrix(c(0.3,0.02,0.01,0.12,0.19,0.03,0.02,0.01,0.3),nrow=3,ncol=3)
      #' p<-MatCon$new(A,Source="Labatut&Cherifi, 2011")
      #' p$Sucess_i(2)
      #'
      #' @aliases

     Sucess_i = function(i){
      Sucess_i = self$UserAcc_i(i)[[1]] + self$ProdAcc_i(i)[[1]] - 1
      VarSucess_i=abs((Sucess_i*(1-Sucess_i))/(self$sumcol[i]+self$sumfil[i]))
     return (list(Sucess_i=Sucess_i,VarSucess_i=VarSucess_i))
     },





      #' @description  Public method that provides the Hellden' average accuracy, denotes for the probability that a randomly chosen point of a specific class on the map has a correspondence of the same class in the same position in the field and that a randomly chosen point in the field of the same class has a correspondence of the same class in the same position on the map.The method also offers variance. The reference \insertCite{hellden1980,rosenfield1986}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvHelldenAcc_i=\frac{2}{\frac{1}{UserAcc_i}+\frac{1}{ProdAcc_i}}
      #' }
      #' \deqn{
      #' \sigma^2_{AvHelldenAcc_i}=\frac{AvHelldenAcc_i \cdot (1-AvHelldenAcc_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item AvHelldenAcc_i: Hellden's mean accuracy.
      #'   \item UserAcc_i: user accuracy index for class i.
      #'   \item ProdAcc_i: producer accuracy index for class i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @param i Class to evaluate.
      #' @return A list with Hellden's mean accuracy and its variance.
      #' @examples
      #' A <- matrix(c(148,1,8,2,0,0,50,15,3,0,1,6,39,7,1,1,0,6,25,1,1,0,0,1,6),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Rosenfield&Fitzpatrick, 1986")
      #' p$AvHelldenAcc_i(2)
      #'
      #' @aliases

     AvHelldenAcc_i = function(i){

       #esta condicion se daria cuando el elemento de la diagonal
       #sea 0, ¿poco probable? se puede eliminar esta condicion?
       if (self$UserAcc_i(i)[[1]] == 0 || self$ProdAcc_i(i)[[1]] == 0) {
        stop ("/ by 0")
       }else{
          AvHelldenAcc_i= 2 / (1/self$UserAcc_i(i)[[1]] + 1/self$ProdAcc_i(i)[[1]])
         VarAvHelldenAcc_i=abs((AvHelldenAcc_i*(1-AvHelldenAcc_i))/(self$sumcol[i]+self$sumfil[i]))
         }

     return(list(AvHelldenAcc_i=AvHelldenAcc_i,VarAvHelldenAcc_i=VarAvHelldenAcc_i))
     },




      #' @description Public method that provides Short's mapping accuracy for each class is stated as the number of correctly classified pixels (equal to the total in the correctly classified area) in terms of all pixels affected by its classification (equal to this total in the displayed area as well as the pixels involved in errors of commission and omission). The method also offers variance. The reference \insertCite{rosenfield1986,short1982}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ShortAcc_i=\frac{x_{ii}}{\sum^n_{j=1} x_{+ j}+\sum^n_{i=1} x_{i +}-x_{ii}}
      #' }
      #'\deqn{
      #' \sigma^2_{ShortAcc_i}=\frac{ShortAcc_i \cdot (1-ShortAcc_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item ShortAcc_i: Short's mapping accuracy
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item x_j+: sum of all elements in rows j.
      #'   \item x_+j: sum of all elements in column j.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @param i Class to evaluate.
      #' @return A list with Short's mapping accuracy and its variance.
      #' @examples
      #' A <- matrix(c(148,1,8,2,0,0,50,15,3,0,1,6,39,7,1,1,0,6,25,1,1,0,0,1,6),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Rosenfield&Fitzpatrick-Lins, 1986")
      #' p$ShortAcc_i(2)
      #'
      #' @aliases

     ShortAcc_i = function(i){
      if (self$sumfil[i] + self$sumcol[i] - self$values[i,i] == 0) {
      stop ("/ by 0")
      }else{
        ShortAcc_i = self$values[i,i] / (self$sumfil[i] + self$sumcol[i] - self$values[i,i])
        VarShortAcc_i=abs((ShortAcc_i*(1-ShortAcc_i))/(self$sumcol[i]+self$sumfil[i]))
        }

     return(list(ShortAcc_i=ShortAcc_i,VarShortAcc_i=VarShortAcc_i))
     },





      #' @description Public method that evaluates the kappa coefficient from the user's perspective, for a specific class i. The method also offers variance. The reference \insertCite{rosenfield1986}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' UserKappa_i=\frac{UserAcc_i-\frac{\sum^n_{i=1} x_{i + }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}{1-\frac{\sum^n_{i=1} x_{i + }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}
      #' }
      #'#'  \deqn{
      #' \sigma^2_{UserKappa_i}=\frac{UserKappa_i \cdot (1-UserKappa_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item UserKappa_i: coefficient kappa (user's).
      #'   \item UserAcc_i: user accuracy index for class i.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item x_j+: sum of all elements in rows j.
      #'   \item x_+j: sum of all elements in column j.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with coefficient kappa (user's) and its variance.
      #' @param i Class to evaluate.
      #' @examples
      #' A<-matrix(c(73,13,5,1,0,21,32,13,3,0,16,39,35,29,13,3,5,7,28,48,1,0,2,3,17),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Næsset, 1996")
      #' p$UserKappa_i(2)
      #'
      #' @aliases

     UserKappa_i = function(i){
      if (1 - self$sumcol[i]/sum(self$values) == 0) {
       stop ("/ by 0")
      }else{
        UserKappa_i = (self$UserAcc_i(i)[[1]] - self$sumcol[i]/sum(self$values)) / (1 - self$sumcol[i]/sum(self$values))
        VarUserKappa_i=abs((UserKappa_i*(1-UserKappa_i))/self$sumfil[i])
        }

     return(list(UserKappa_i=UserKappa_i,VarUserKappa_i=VarUserKappa_i))
     },




      #' @description Public method that evaluates the kappa coefficient from the producer's perspective, for a specific class i. The method also offers variance. The reference \insertCite{rosenfield1986}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ProdKappa_i=\frac{ProdAcc_i-\frac{\sum^n_{j=1} x_{ + j }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}{1-\frac{\sum^n_{j=1} x_{+ j }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}
      #' }
      #'  \deqn{
      #' \sigma^2_{ProdKappa_i}=\frac{ProdKappa_i \cdot (1- ProdKappa_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item ProdKappa_i: coefficient kappa (producer's).
      #'   \item ProdAcc_i: producer accuracy index for class i.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item x_j+: sum of all elements in rows j.
      #'   \item x_+j: sum of all elements in column j.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with coefficient kappa (producer's) and its variance.
      #' @param i Class to evaluate.
      #' @examples
      #' A<-matrix(c(73,13,5,1,0,21,32,13,3,0,16,39,35,29,13,3,5,7,28,48,1,0,2,3,17),
      #' nrow=5,ncol=5)
      #' p<-MatCon$new(A,Source="Næsset, 1996")
      #' p$ProdKappa_i(2)
      #'
      #' @aliases

      ProdKappa_i = function(i){
      if (1 - self$sumfil[i]/sum(self$values) == 0) {
       stop ("/ by 0")
      }else{
        ProdKappa_i = (self$ProdAcc_i(i)[[1]] - self$sumfil[i]/sum(self$values)) / (1 - self$sumfil[i]/sum(self$values))
        VarProdKappa_i=abs((ProdKappa_i*(1-ProdKappa_i))/self$sumcol[i])
        }
     return(list(ProdKappa_i=ProdKappa_i,VarProdKappa_i=VarProdKappa_i))
     },


      #' @description Public method that provides the overall modified kappa coefficient. The method also offers variance. The reference \insertCite{stehman1997,foody1992}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ModKappa=\frac{OverallAcc-\frac{1}{\sqrt{M}}}{1-\frac{1}{\sqrt{M}}}
      #' }
      #' \deqn{
      #' \sigma^2_{ModKappa}=\frac{ModKappa \cdot (1- ModKappa)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item ModKappa: modified coefficient kappa.
      #'   \item OverallAcc: overall accuracy.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with modified coefficient kappa and its variance.
      #' @param i Class to evaluate.
      #' @examples
      #' A<-matrix(c(317,61,2,35,23,120,4,29,0,0,60,0,0,0,0,8),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Foody, 1992")
      #' p$ModKappa()
      #'
      #' @aliases

     ModKappa = function(i){
       ModKappa= (self$OverallAcc()[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
       VarModKappa=abs((ModKappa*(1-ModKappa))/self$values)
     return(list(ModKappa=ModKappa,VarModKappa=VarModKappa))
     },




      #' @description Public method, derived from the general modified kappa coefficient, which provides the modified coefficient kappa for the user. The method also offers variance. The reference \insertCite{stehman1997,foody1992}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ModKappaUser_i=\frac{UserAcc_i-\frac{1}{\sqrt{M}}}{1-\frac{1}{\sqrt{M}}}
      #' }
      #' \deqn{
      #' \sigma^2_{ModKappaUser_i}=\frac{ModKappaUser_i \cdot (1- ModKappaUser_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item ModKappaUser_i: modified coefficient kappa (user's).
      #'   \item UserAcc_i: user accuracy index for class i.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with modified coefficient kappa (user's) and its variance.
      #' @param i Class to evaluate.
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al., 2007")
      #' p$ModKappaUser_i(2)
      #'
      #' @aliases

     ModKappaUser_i = function(i){
       ModKappaUser_i = (self$UserAcc_i(i)[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
       VarModKappaUser_i=abs((ModKappaUser_i*(1-ModKappaUser_i))/self$sumfil[i])
     return(list(ModKappaUser_i=ModKappaUser_i,VarModKappaUser_i=VarModKappaUser_i))
     },





      #' @description Public method, derived from the general modified kappa coefficient, which provides the modified coefficient kappa for the producer. The method also offers variance. The reference \insertCite{stehman1997,foody1992}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ModKappaProd_i=\frac{ProdAcc_i-\frac{1}{\sqrt{M}}}{1-\frac{1}{\sqrt{M}}}
      #' }
      #' \deqn{
      #' \sigma^2_{ModKappaProd_i}=\frac{ModKappaProd_i \cdot (1- ModKappaProd_i)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item ModKappaUser_i: modified coefficient kappa (producer's).
      #'   \item ProdAcc_i: producer accuracy index for class i.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with modified coefficient kappa (producer's) and its variance.
      #' @param i Class to evaluate.
      #' @examples
      #' A<-matrix(c(317,61,2,35,23,120,4,29,0,0,60,0,0,0,0,8),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al., 2007")
      #' p$ModKappaProd_i(2)
      #'
      #' @aliases

     ModKappaProd_i = function(i){
      ModKappaProd_i = (self$ProdAcc_i(i)[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
      VarModKappaProd_i=abs((ModKappaProd_i*(1-ModKappaProd_i))/self$sumcol[i])
     return(list(ModKappaProd_i=ModKappaProd_i,VarModKappaProd_i=VarModKappaProd_i))
     },

     #' @description Public method that calculates relative change of entropy given a category on map. That is, the degree of uncertainty of the category. The method also offers variance. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
     #' @description
     #'  \deqn{
     #' Entrop_i(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
     #' }
     #' \deqn{
     #' Entrop_i(A|b_i)=-\sum^n_{j=1}( (\frac{ x_{ij}}{\sum^n_{j=1} x_{+ j} }) \cdot \log (\frac{x_{ij}}{\sum^n_{j=1} x_{+ j}}) )
     #' }
     #' \deqn{
     #' EntropUser_i= \frac{Entrop_i(A)-Entrop_i(A|b_i)}{Entrop_i(A)}
     #' }
     #' \deqn{
     #' \sigma^2_{EntropUser_i}= \frac{EntropUser_i \cdot (1-EntropUser_i)}{N}
     #' }
     #'
     #' where:
     #'
     #' \enumerate{
     #'   \item EntropUser_i: relative change of entropy given a category on map.
     #'   \item Entrop_i(A): Entropy of the map with respect to the category of the map.
     #'   \item x_j+: sum of all elements in rows j.
     #'   \item x_+j: sum of all elements in column j.
     #'   \item Entrop_i(A|b_i): Entropy of map A knowing that the location corresponding to map B is in class b_i.
     #'   \item N: number of cases involved in the calculation of the index.
     #' }
     #' @return A list with the relative change of entropy given a category on map, its variance, map entropy, and entropy of map A knowing that the location corresponding to map B is in class b_i.
     #' @param i Class to evaluate (row).
     #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
     #' @examples
     #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
     #' p<-MatCon$new(A,Source="Liu et al., 2007")
     #' p$EntropUser_i(1)
     #'
     #' @aliases

        #They can be negative values if the entropy (uncertainty) increases and
        #positive if entropy decreases
        #Classes by rows. If any element is 0 in the row you get an error,
        #due to log

    EntropUser_i = function(i,v=NULL){
      if(!is.null(v)){
       v<-v
      }else{v<-10}

       #log10 ->Hartleys (entropy units)
       #log2->bits
       #LN->nats

     #na.rm=TRUE. In this way it does the sum with the values it has and ignores NA
    Entrop_iA = - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
    Entrop_iAbi = - sum ((self$values[i,] / self$sumfil[i]) * log(self$values[i,] / self$sumfil[i],base=v),na.rm=TRUE)

    if (Entrop_iA == 0){
     stop("/by 0")
    }else {
      EntropUser_i = (Entrop_iA - Entrop_iAbi) / Entrop_iA
      VarEntropUser_i=abs((EntropUser_i*(1-EntropUser_i))/sum(self$values)) #I'm not very sure, but all the values in the matrix are involved.
      }
    return(list(EntropUser_i=EntropUser_i,VarEntropUser_i=VarEntropUser_i,Entrop_iA=Entrop_iA,Entrop_iAbi=Entrop_iAbi))
    },

     #' @description Public method that calculates relative change of entropy given a category on ground truthing. That is, the degree of uncertainty of the category. The method also offers variance. The reference \insertCite{stehman1997}{PaolaR6Nuevo} is followed for the calculations.
     #' @param i Class to evaluate
     #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
     #' @description
     #'  \deqn{
     #' Entrop_i(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
     #' }
     #' \deqn{
     #' Entrop_i(B|a_j)=-\sum^n_{j=1}( (\frac{ x_{ij}}{\sum^n_{i=1} x_{i +} }) \cdot \log (\frac{x_{ij}}{\sum^n_{i=1} x_{i +}}) )
     #' }
     #' \deqn{
     #' EntropProd_i= \frac{EntropMap(B)-EntropMap(B|a_j)}{EntropMap(B)}
     #' }
     #'\deqn{
     #' \sigma^2_{EntropProd_i}= \frac{EntropProd_i \cdot (1-EntropProd_i)}{N}
     #' }
     #' where:
     #'
     #' \enumerate{
     #'   \item EntropProd_i: relative change of entropy given a category on ground truthing.
     #'   \item Entrop_i(B): Entropy of the map with respect to the category on ground truthing.
     #'   \item x_j+: sum of all elements in rows j.
     #'   \item x_+j: sum of all elements in column j.
     #'   \item Entrop_i(B|a_j): Entropy of map B knowing that the location corresponding to map A is in class a_j.
     #'   \item N: number of cases involved in the calculation of the index.
     #' }
     #' @return A list of the relative change of entropy given a category on ground truthing, its variance, map entropy, and entropy of map B knowing that the location corresponding to map A is in class a_j.
     #' @examples
     #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
     #' p<-MatCon$new(A,Source="Liu et al., 2007")
     #' p$EntropProd_i(2)
     #'
     #' @aliases

    EntropProd_i = function(i,v=NULL){
     if(!is.null(v)){
      v<-v
     }else{v<-10}
    Entrop_iB = - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base = v)),na.rm=TRUE)
    Entrop_iBaj = - sum ((self$values[,i] / self$sumcol[i]) * log(self$values[,i] / self$sumcol[i],base=v),na.rm=TRUE)

      if (Entrop_iB == 0){
        stop("/by 0")
      }else {
        EntropProd_i = (Entrop_iB - Entrop_iBaj) / Entrop_iB
        VarEntropProd_i=abs((EntropProd_i*(1-EntropProd_i))/sum(self$values))
        }
    return(list(EntropProd_i=EntropProd_i,VarEntropProd_i=VarEntropProd_i,Entrop_iB=Entrop_iB,Entrop_iBaj=Entrop_iBaj))
    },




      #' @description Public method that provides the user's average accuracy, which is an average of the accuracy of individual categories, in this case the categories will be taken from the user's perspective. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvUserAcc=\frac{1}{\sqrt{M}} \sum^n_{i=1} \frac{x_{ii}}{\sum_{j=1}^n x_{j+}}
      #' }
      #'\deqn{
      #' \sigma^2_{AvUserAcc}=\frac{AvUserAcc \cdot (1-AvUserAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item AvUserAcc: average accuracy from user's perspective.
      #'   \item x_j.: sum of all elements in rows j.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the average accuracy from user's perspective and its variance.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung&LeDrew, 1988")
      #' p$AvUserAcc()
      #'
      #' @aliases

     AvUserAcc = function(){
       for (i in 1:length(self$sumfil)) {
          if (self$sumfil[i] == 0) {
            stop ("/ by 0")
          }
       }
      AvUserAcc = 1/sqrt(length(self$values)) * sum (diag(self$values)/self$sumfil)
      VarAvUserAcc=abs((AvUserAcc*(1-AvUserAcc))/sum(self$values)) #sum de todos los sumfil..self$values intervienen
     return (list(AvUserAcc=AvUserAcc,VarAvUserAcc=VarAvUserAcc))
     },

      #' @description Public method that provides the producer's average accuracy, which is an average of the accuracy of individual categories, in this case the categories will be taken from the producer's perspective. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvProdAcc=\frac{1}{\sqrt{N}} \sum^n_{i=1} \frac{x_{ii}}{\sum_{j=1}^n x_{+j}}
      #' }
      #'\deqn{
      #' \sigma^2_{AvProdAcc}=\frac{AvProdAcc \cdot (1-AvProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item AvProdAcc: average accuracy from producer's perspective.
      #'   \item x_+j: sum of all elements in column j.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the average accuracy from producer's perspective and its variance.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung&LeDrew, 1988")
      #' p$AvProdAcc()
      #'
      #' @aliases

     AvProdAcc = function(){
      AvProdAcc = 1/sqrt(length(self$values)) * sum (diag(self$values)/self$sumcol)
      VarAvProdAcc=abs((AvProdAcc*(1-AvProdAcc))/sum(self$values))
     return(list(AvProdAcc=AvProdAcc,VarAvProdAcc=VarAvProdAcc))
     },

      #' @description Public method that offers the average of the average precision from the perspective of the user and the producer. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvUserProdAcc=\frac{AvUserAcc+AvProdAcc}{2}
      #' }
      #'  \deqn{
      #' \sigma^2_{AvUserProdAcc}=\frac{AvUserProdAcc \cdot (1-AvUserProdAcc}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item AvUserProdAcc: average of average of user's and producer's perspective.
      #'   \item AvUserAcc: average accuracy from user's perspective.
      #'   \item AvProdAcc: average accuracy from producer's perspective.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with the values of the average of the average precision from the perspective of the user and the producer the user and producer perspective and their variance.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$AvUserProdAcc()
      #'
      #' @aliases

     AvUserProdAcc = function(){
      AvUserProdAcc = (self$AvUserAcc()[[1]] + self$AvProdAcc()[[1]]) / 2
      VarAvUserProdAcc=abs((AvUserProdAcc*(1-AvUserProdAcc))/sum(self$values))
     return(list(AvUserProdAcc=AvUserProdAcc,VarAvUserProdAcc=VarAvUserProdAcc))
     },


      #' @description Public method that provides the average value of the Hellden mean precision index. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvHelldenAcc=\frac{1}{\sqrt{M}}\sum^n_{i=1} \frac{2 x_{ii}}{ x_{+i} + x_{i+}}
      #' }
      #'  \deqn{
      #' \sigma^2_{AvHelldenAcc}=\frac{AvHelldenAcc \cdot (1-AvHelldenAcc)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item AvHelldenAcc: average of Hellden's mean accuracy index.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with average of Hellden's mean accuracy index and its variance.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$AvHelldenAcc()
      #'
      #' @aliases

     AvHelldenAcc = function(){
      AvHelldenAcc = 1/sqrt(length(self$values)) * sum ((2*diag(self$values)) / (self$sumfil + self$sumcol))
      VarAvHelldenAcc=abs((AvHelldenAcc*(1-AvHelldenAcc))/sum(self$values))
     return(list(AvHelldenAcc=AvHelldenAcc,VarAvHelldenAcc=VarAvHelldenAcc))
     },


      #' @description Public method that provides the average of Short's mapping accuracy index. The method also offers variance. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' AvShortAcc=\frac{1}{\sqrt{M}}\frac{\frac{\sum^n_{i=1} x_{ii}}{\sum^n_{i,j=1}x_{ij}}}{\sum^n_{j=1} x_{+ j}+\sum^n_{i=1} x_{i +}-x_{ii}}
      #' }
      #'\deqn{
      #' \sigma^2_{AvShortAcc}=\frac{AvShortAcc \cdot (1-AvShortAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item x_ii: diagonal element of the matrix.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.#' }
      #' @return A list with average of Short's mapping accuracy index and its variance.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$AvShortAcc()
      #'
      #' @aliases

     AvShortAcc = function(){
      AvShortAcc = 1/sqrt(length(self$values)) * sum (diag(self$values) / (self$sumfil + self$sumcol - diag(self$values)))
      sum1=self$sumfil+self$sumcol- diag(self$values)
      VarAvShortAcc=abs((AvShortAcc*(1-AvShortAcc))/sum(self$values))
       for (i in 1:length(sum1)) {
          if (sum1[i] == 0) {
           stop ("/ by 0")
          }
       }
     return(list(AvShortAcc=AvShortAcc,VarAvShortAcc=VarAvShortAcc))
     },


      #' @description Public method that provides the combined user accuracy that is the average of the overall accuracy and the average user accuracy. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' CombUserAcc=\frac{OverallAcc+AvUserAcc}{2}
      #' }
      #' \deqn{
      #' \sigma^2_{CombUserAcc}=\frac{CombUserAcc \cdot (1-CombUserAcc)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item CombUserAcc: combined accuracy from user's perspective.
      #'   \item OverallAcc: overall accuracy.
      #'   \item AvUserAcc: average accuracy from user's perspective.
      #'   \item N: number of cases involved in the calculation of the index.
      #'   }
      #' @return A list of the combined accuracy from the user's perspective and its variation.
      #' @details
      #' Example matrix taken from Tung, F., & LeDrew, E. (1988). The determination of optimal threshold levels for change detection using various accuracy indexes. Photogrammetric Engineering and Remote Sensing, 54(10), 1449-1454.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung&LeDrew, 1988")
      #' p$CombUserAcc()
      #'
      #' @aliases

     CombUserAcc = function(){
      CombUserAcc = (self$OverallAcc()[[1]] + self$AvUserAcc()[[1]]) / 2
      VarCombUserAcc=abs((CombUserAcc*(1-CombUserAcc))/sum(self$values))
     return(list(CombUserAcc=CombUserAcc,VarCombUserAcc=VarCombUserAcc))
     },

      #' @description Public method that provides the combined producer accuracy that is the average of the overall accuracy and the average producer accuracy. The method also offers variance. The reference \insertCite{tung1988}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' CombProdAcc=\frac{OverallAcc+AvProdAcc}{2}
      #' }
      #'\deqn{
      #' \sigma^2_{CombProdAcc}=\frac{CombProdAcc \cdot (1-CombProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item CombProdAcc: combined accuracy from producer's perspective.
      #'   \item OverallAcc: overall accuracy.
      #'   \item AvProdAcc: average accuracy from producer's perspective.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list of the combined accuracy from producer's perspective and its variance.
      #' @details
      #' Example matrix taken from Tung, F., & LeDrew, E. (1988). The determination of optimal threshold levels for change detection using various accuracy indexes. Photogrammetric Engineering and Remote Sensing, 54(10), 1449-1454.
      #' @examples
      #' A<-matrix(c(352,43,89,203),nrow=2,ncol=2)
      #' p<-MatCon$new(A,Source="Tung&LeDrew, 1988")
      #' p$CombProdAcc()
      #'
      #' @aliases

     CombProdAcc = function(){
      CombProdAcc = (self$OverallAcc()[[1]] + self$AvProdAcc()[[1]]) / 2
      VarCombProdAcc=abs((CombProdAcc*(1-CombProdAcc))/sum(self$values))
     return(list(CombProdAcc=CombProdAcc,VarCombProdAcc=VarCombProdAcc))
     },

      #' @description Public method that provides the combined accuracy which is the average of the overall accuracy and the Hellden average accuracy, which refers to the average user and producer accuracies. The method also offers variation. The reference \insertCite{liu2007}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' CombUserProdAcc=\frac{OverallAcc+AvHelldenAcc}{2}
      #' }
      #'  \deqn{
      #' \sigma^2_{CombUserProdAcc}=\frac{CombUserProdAcc \cdot (1-CombUserProdAcc)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item CombUserProdAcc: combined accuracy from both user's and producer's perspectives.
      #'   \item OverallAcc: overall accuracy.
      #'   \item AvHelldenAcc: average of Hellden's mean accuracy index.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list of the combined accuracy from both user's and producer's perspectives and its variance.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$CombUserProdAcc()
      #'
      #' @aliases

     CombUserProdAcc = function(){
      CombUserProdAcc= ( self$OverallAcc()[[1]] + self$AvHelldenAcc()[[1]] ) / 2
      VarCombUserProdAcc=abs((CombUserProdAcc*(1-CombUserProdAcc))/sum(self$values))
     return(list(CombUserProdAcc=CombUserProdAcc,VarCombUserProdAcc=VarCombUserProdAcc))
     },

      #' @description Public method that provides kappa coefficient, which measures the relationship between agreement beyond chance and expected disagreement. The method also offers variation. The reference \insertCite{cohen1960}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' ExpAcc=\sum^n_{i=1} (\frac{x _{+ i}}{\sum_{j=1}^n x_{ij}} \cdot \frac{x _{i +}}{\sum_{j=1}^n x_{ij}})
      #' }
      #' \deqn{
      #' Kappa=\frac{OverallAcc-ExpAcc}{1-ExpAcc}
      #' }
      #' #' \deqn{
      #' \sigma^2_{Kappa}=\frac{OverallAcc-ExpAcc}{(1-ExpAcc) \cdot N}
      #' }
      #'
      #'
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item Kappa: Kappa coefficient.
      #'   \item OverallAcc: overall accuracy.
      #'   \item ExpAcc: expected accuracy of agreement if agreement were purely random.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with kappa coefficient and its variance.
      #' @details
      #' Example matrix taken from Congalton, R.G., & Green, K. (2008). Assessing the Accuracy of Remotely Sensed Data: Principles and Practices, Second Edition (2nd ed.). CRC press
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$Kappa()
      #'
      #' @aliases

     Kappa = function(){
      ExpAcc = (sum (self$sumfil * self$sumcol))/sum(self$values)^2
      if (1-ExpAcc == 0){
       stop ("/ by 0")
      }else{
        kappa = (self$OverallAcc()[[1]]- ExpAcc) / (1 - ExpAcc)
        VarKappa=abs((self$OverallAcc()[[1]]*(1-self$OverallAcc()[[1]]))/(sum(self$values)*(1-ExpAcc)^2))
      }
     return(list(Kappa=kappa,VarKappa=VarKappa))
     },




      #' @description  Public method for calculating map entropy. Which refers to the degree of uncertainty that the map presents. The method also offers variation. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop=\sum^n_{i,j=1} (\frac{x_{ij}}{\sum^n_{i,j=1} x_{ij}} \cdot \log (\frac{x_{ij}}{\frac{\sum^n_{i=1} x_{i +} \cdot \sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij}}}))
      #' }
      #'\deqn{
      #' \sigma^2_{Entrop}=\frac{Entrop \cdot (1-Entrop)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item Entrop: map entropy.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with map entropy and its variance.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al.,2007")
      #' p$Entrop()
      #'
      #' @aliases

     Entrop = function(v=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

       Entrop = sum ((self$values/sum(self$values)) * log(self$values / ((self$sumfil * self$sumcol)/sum(self$values)),base=v),na.rm=TRUE)
       VarEntrop=abs((Entrop*(1-Entrop))/sum(self$values))
     return(list(Entrop=Entrop,VarEntrop=VarEntrop))
     },


      #' @description Public method that calculates normalized entropy using the map. The method also offers variation. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' NormEntropUser=\frac{Entrop}{Entrop_i(B)}
      #' }
      #'\deqn{
      #' \sigma^2_{NormEntropUser}=\frac{NormEntropUser \cdot (1-NormEntropUser)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item NormEntropUser: normalized entropy using map.
      #'   \item Entrop_i(B): entropy of the map with respect to the category on ground truthing.
      #'   \item Entrop: map entropy.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using map and its variance.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al., 2007")
      #' p$NormEntropUser()
      #'
      #' @aliases

     NormEntropUser = function(v=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

       Entrop_iB = - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base=v)),na.rm=TRUE)

       if(Entrop_iB == 0){
        stop("/ by 0")
       }

       NormEntropUser = self$Entrop(v)[[1]]/Entrop_iB
       VarNormEntropUser=abs((NormEntropUser*(1-NormEntropUser))/sum(self$values))
     return(list(NormEntropUser=NormEntropUser,VarNormEntropUser=VarNormEntropUser))
     },

      #' @description Public method that calculates normalized entropy using on ground truthing. The method also offers variation. The reference \insertCite{finn1993}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' NormEntropProd=\frac{Entrop}{Entrop_i(A)}
      #' }
      #'\deqn{
      #' \sigma^2_{NormEntropProd}=\frac{NormEntropProd \cdot (1-NormEntropProd)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item NormEntropProd: normalized mutual information using the entropy on ground truthing.
      #'   \item Entrop_i(A): Entropy of the map with respect to the category of the map.
      #'   \item Entrop: map entropy.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using on ground truthing and its variance.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @examples
      #' A<-matrix(c(0,12,0,0,12,0,0,0,0,0,0,12,0,0,12,0),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Liu et al., 2007")
      #' p$NormEntropProd()
      #'
      #' @aliases

     NormEntropProd = function(v=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

      Entrop_iA = - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
       if (Entrop_iA == 0){
        stop ("/ by 0")
       }else{
         NormEntropProd = self$Entrop()[[1]]/Entrop_iA
         VarNormEntropProd=abs((NormEntropProd*(1-NormEntropProd))/sum(self$values))
         }
     return(list(NormEntropProd=NormEntropProd,VarNormEntropProd=VarNormEntropProd))
     },

      #' @description Public method that calculates normalized entropy using the arithmetic mean of the entropies on the map and on ground truthing. The method also offers variation. The reference \insertCite{strehl2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' AvNormEntrop=\frac{2Entrop}{Entrop_i(A)+Entrop_i(B)}
      #' }
      #' \deqn{
      #' \sigma^2_{AvNormEntrop}=\frac{AvNormEntrop \cdot (1-AvNormEntrop)}{N}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item AvNormEntrop: normalized entropy using the arithmetic mean of the entropies on the map and on ground truthing.
      #'   \item Entrop_i(B): entropy of the map with respect to the category on ground truthing.
      #'   \item Entrop_i(A): Entropy of the map with respect to the category of the map.
      #'   \item Entrop: map entropy.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return normalized entropy using the arithmetic mean of the entropies on the map and on ground truthing and its variance.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$AvNormEntrop()
      #'
      #' @aliases

     AvNormEntrop = function(v=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}

      Entrop_iB = - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base=v)),na.rm=TRUE)
      Entrop_iA = - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
        if (Entrop_iA + Entrop_iB == 0) {
          stop ("/ by 0")
        }else{
          AvNormEntrop = 2 * self$Entrop(v)[[1]] / (Entrop_iA + Entrop_iB)
          VarAvNormEntrop= abs((AvNormEntrop*(1-AvNormEntrop))/sum(self$values))
        }

     return(list(AvNormEntrop=AvNormEntrop,VarAvNormEntrop=VarAvNormEntrop))
     },

      #' @description Public method that calculates normalized entropy using the geometric mean of the entropies on the map and on ground truthing. The method also offers variation. The reference \insertCite{ghosh2002}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #'  \deqn{
      #' Entrop_i(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' GeomAvNormEntrop=\frac{Entrop}{\sqrt{Entrop_i(A) \cdot Entrop_i(B)}}
      #' }
      #'\deqn{
      #' \sigma^2_{GeomAvNormEntrop}=\frac{GeomAvNormEntrop \cdot (1-GeomAvNormEntrop)}{N}
      #' }
      #' where:
      #'
      #' \enumerate{
      #'   \item GeomAvNormEntrop: normalized entropy using the geometric mean of the entropies on map and on ground truthing.
      #'   \item Entrop_i(B): entropy of the map with respect to the category on ground truthing.
      #'   \item Entrop_i(A): Entropy of the map with respect to the category of the map.
      #'   \item Entrop: map entropy.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using the geometric mean of the entropies on map and on ground truthing and its variance.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$GeomAvNormEntrop()
      #'
      #' @aliases

     GeomAvNormEntrop = function(v=NULL){
      if(!is.null(v)){
        v<-v
      }else{v<-10}
      Entrop_iB = - sum ((self$sumfil/sum(self$values)) * (log(self$sumfil/sum(self$values),base=v)),na.rm=TRUE)
      Entrop_iA = - sum ((self$sumcol/sum(self$values)) * (log(self$sumcol/sum(self$values),base=v)),na.rm=TRUE)
       if (Entrop_iA * Entrop_iB == 0) {
        stop ("/ by 0")
       }else{
         GeomAvNormEntrop = self$Entrop(v)[[1]] / sqrt(Entrop_iA * Entrop_iB)
         VarGeomAvNormEntrop=abs((GeomAvNormEntrop*(1-GeomAvNormEntrop)))
         }
     return(list(GeomAvNormEntrop=GeomAvNormEntrop,VarGeoAvNormEntrop=VarGeomAvNormEntrop))
     },

      #' @description Public mathod that provides normalized entropy using the arithmetic mean of the maximum entropies on map and on ground truthing.The method also offers variation. The reference \insertCite{strehl2002relationship}{PaolaR6Nuevo} is followed for the calculations.
      #' @description
      #' Entrop_i(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i +}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' Entrop_i(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{+ j}}{\sum^n_{i,j=1} x_{ij} }) )
      #' }
      #' \deqn{
      #' AvMaxNormEntrop=\frac{2 Entrop}{max(Entrop_i(A))+max(Entrop_i(B))}=\frac{Entrop}{\log \sqrt{M}}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item AvMaxNormEntrop: normalized entropy using the arithmetic mean of the maximum entropies on map and on ground truthing.
      #'   \item Entrop_i(B): entropy of the map with respect to the category on ground truthing.
      #'   \item Entrop_i(A): Entropy of the map with respect to the category of the map.
      #'   \item Entrop: map entropy.
      #'   \item x_+i: sum of all elements in column i.
      #'   \item x_i+: sum of all elements in row i.
      #'   \item M: number of elements of the matrix.
      #'   \item N: number of cases involved in the calculation of the index.
      #' }
      #' @return A list with normalized entropy using the arithmetic mean of the maximum entropies on map and on ground truthing and its variance.
      #' @param v Base of the logarithm. By default v=10. This value is used for the entropy units, v=10(Hartleys), v=2(bits), v=e(nats).
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
      #' p$AvMaxNormEntrop()
      #'
      #' @aliases

     AvMaxNormEntrop = function(v=NULL){
       if(!is.null(v)){
         v<-v
       }else{v<-10}

        AvMaxNormEntrop = self$Entrop(v)[[1]] / log(sqrt(length(self$values)),base=v)
        VarAvMaxNormEntrop=abs((AvMaxNormEntrop*(1-AvMaxNormEntrop))/sum(self$values))

     return (list(AvMaxNormEntrop=AvMaxNormEntrop,Var=VarAvMaxNormEntrop))
     },



      #' @description Public method that calculates the tau index and its variance. Its value indicates how much the classification has improved compared to a random classification of the N elements into M groups. The method also offers the variance. The reference \insertCite{book}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list with Tau index and its variance.
      #' @description
      #' The mathematical expression is:
      #'
      #' \deqn{
      #' PrAgCoef=\frac{1}{M}
      #' }
      #' \deqn{
      #' Tau = \frac{OverallAcc-CoefAccPr}{1-PrAgCoef}
      #' }
      #'
      #' \deqn{
      #' \sigma^2_{Tau}=\frac{OverallAcc \cdot (1-OverallAcc)}{N \cdot (1-CoefAccPr)^2}
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item OverallAcc: overall accuracy.
      #'   \item PrAgCoef: a priori random agreement coefficient.
      #'   \item M: number of classes.
      #'   \item N: number of elements of the matrix, cardinal of the matrix.
      #' }
      #' @details Example matrix taken from Muñoz, J. M. S. (2016). Análisis de Calidad Cartográfica mediante el estudio de la Matriz de Confusión. Pensamiento matemático, 6(2), 9-26.
      #' @examples
      #' A<-matrix(c(238051,7,132,0,0,24,9,2,189,1,4086,188,0,4,16,45,1,0,939,5082,
      #' 51817,0,34,500,1867,325,17,0,0,5,11148,1618,78,0,0,0,0,48,4,834,2853,340,
      #' 32,0,197,5,151,119,135,726,6774,75,1,553,0,105,601,110,174,155,8257,8,0,
      #' 29,36,280,0,0,6,5,2993,0,115,2,0,4,124,595,0,0,4374),nrow=9,ncol=9)
      #' p<-MatCon$new(A,Source="Muñoz, 2016")
      #' p$Tau()
      #'
      #' @aliases

     Tau = function(){
        Ca<-1/nrow(self$values)
        Tau<-((self$OverallAcc()[[1]]-Ca)/(1-Ca))
        VarTau=((self$OverallAcc()[[1]]*(1-self$OverallAcc()[[1]]))/(sum(self$values)*(1-Ca)))
     return(list(Tau=Tau,VarTau=VarTau))
     },




# Functions that return multiple indices ----------------------------------






      #' @description  Public method that calculates the pressures of the user and the producer jointly. The method also offers the standard desviations. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list containing the producer's and user's accuracies and their standard deviations, respectively.
      #' @details
      #' Example matrix taken from Congalton, R.G., & Green, K. (2008). Assessing the Accuracy of Remotely Sensed Data: Principles and Practices, Second Edition (2nd ed.). CRC press
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green,2008")
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
      #' p<-MatCon$new(A,Source="Congalton&Green,2008")
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
      #' p<-MatCon$new(A,Source="Congalton&Green,2008")
      #' p$DetailedCondKappa ()
      #'
      #' @aliases


     DetailedCondKappa = function(){
       SumaMatriz <-sum(self$values)
        # In %
        MERROR<- self$values/sum(self$values)

       # UnWeighted marginals (quantities)
        pcol <- apply(MERROR,2,sum)
        prow<- apply(MERROR,1,sum)
       # Initialization of vectors
        nc  <- nrow(MERROR)
        Ki_ <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        K_j <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        Ki_sd <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        K_jsd <- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
          for (i in 1:nc){
            Ki_[i] <- ((MERROR[i,i]/prow[i])-pcol[i])/(1-pcol[i])
            K_j[i] <- ((MERROR[i,i]/pcol[i])-prow[i])/(1-prow[i])
            ti1 <- prow[i]-MERROR[i,i]
            ti2<-  ti1/((prow[i]^3)*(1-pcol[i])^3)
            ti3<-  ti1*(pcol[i]*prow[i]-MERROR[i,i])
            ti4<-  MERROR[i,i]*(1-pcol[i]-prow[i]+MERROR[i,i])
            Ki_sd[i] <- sqrt((1/SumaMatriz)*ti2*(ti3+ti4))
            tj1 <- pcol[i]-MERROR[i,i]
            tj2<-  tj1/((pcol[i]^3)*(1-prow[i])^3)
            tj3<-  tj1*(pcol[i]*prow[i]-MERROR[i,i])
            tj4<-  MERROR[i,i]*(1-pcol[i]-prow[i]+MERROR[i,i])
            K_jsd[i] <- sqrt((1/SumaMatriz)*tj2*(tj3+tj4))
          }
     return(list(UserCondKappa=Ki_, SD_UserCondKappa=Ki_sd, ProdCondKappa=K_j, SD_ProdCondKappa=K_jsd))
     },


      #' @description  Public method that calculates the values of quantity, change and shift. The reference \insertCite{pontius2014}{PaolaR6Nuevo} is followed for the computations.
      #' @param TI Time interval (default value = 1)
      #' @param SF Scale factor for results (default value = 1)
      #' @return A list of general values for the interval t of difference, quantity, shift, and shift.In addition to the differences for categories, number of components, change of categories and turn of the components.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
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

      #' @description  Public method that types the values of each cell. The total sum of the original matrix is used for typing. The resulting values can be presented as real (parameter RaR=1) or as a percentage (parameter RaR !=1)
      #' @description
      #'  \deqn{
      #' MTypify=\frac{x_{ij}}{\sum^n_{i,j=1} x_{ij}}
      #' }
      #'
      #' where:
      #'
      #' \enumerate{
      #'   \item MTyipify: typified matrix.
      #'   \item x_ij: matrix element.
      #' }
      #' @param RaR "1" indicates result as real, other values mean percentage as integer. By default RaR=1.
      #' @return A list with original matrix and typified matrix
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A, Source="Congalton&Green, 2008")
      #' p$MTypify(RaR=5)

      #Cell Values of a matrix are typified

     MTypify =function(RaR=NULL){
        if(!is.null(RaR)){
          RaR <- RaR
        }else{R<-1}
      # Create a matrix in which all elements are proportions
      # such that the sum of all the elements is 1
      MERROR=self$values

      MatrizSalida <- MERROR/(sum(MERROR))
        if (RaR==1){
        MatrizSalida <- MERROR/(sum(MERROR))
        return(MatrizSalida)
        } else {
      MatrizSalida <- MERROR/(sum(MERROR))
      MatrizSalida[] <- as.integer(100*MatrizSalida)
      return(list(OriginalMatrix=self$values,TypifyMatrix=MatrizSalida))
      }
     },



      #' @description  Public method in which multiple parameters are calculated for the given confusion matrix. The reference [1,11,16,19] is followed for the computations.
      #' @return A list containing Confusion Matrix, Dimension, Total Sum of Cell Values, Overall Precision, Overall Variance Precision, Global Precision Kappa Index, Global Kappa Simplified Variance, Producer Precision by Class, User Precision by Class, k value for the calculation of pseudozeroes, Pseudoceros Matrix, L Matrix for the calculation of pseudozeroes.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
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
      VarPAcuerdo <- PAcuerdo *(1-PAcuerdo)/SumaMatriz
      VarKappa <-  VarPAcuerdo / ((1-PAAzar)*(1-PAAzar))
      MLandas <- (self$sumfil %*% t(self$sumcol))/(SumaMatriz*SumaMatriz)
      K <- (SumaMatriz*SumaMatriz - sum(self$values*self$values))/sum((SumaMatriz*MLandas - self$values)^2)
      MPseudoceros <- (SumaMatriz/(K+SumaMatriz))*(self$values + K*MLandas)
      salida<-list(Matrix=self$values, Dimension =dimension, n=SumaMatriz, OverallAcc=PAcuerdo, VarOverallAcc=VarPAcuerdo, Kappa=Kappa,VarKappa=VarKappa,ProdAcc=ExProdu,UserAcc=UserAcc,Kpseudo=K, MPseudoceros=MPseudoceros,MLandas=MLandas)
     return(salida)
     },



      #' @description Public method that provides N resamples of the confusion matrix from a MatCon object. The reference \insertCite{ariza2011}{PaolaR6Nuevo} is followed for the computations.
      #' @param n Number of resamples.
      #' @return A list formed by the original confusion matrix and simulated matrices, from the confusion matrix. The multinomial distribution is applied.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A, Source="Congalton&Green, 2008")
      #' p$MBootStrap(2)
      #'
      #' @aliases

     MBootStrap=function(n){
      #matrix range
      nc<-ncol(self$values)
      #convert to vector
      M1<-as.vector(self$values)
      #probability
      prob<-M1/sum(M1)
      #M2: matrix list
      M2<-list()
      #resampling with multinomial
      boots<-rmultinom(n,sum(M1),prob)
      #save simulated matrix
        for(i in 1:ncol(boots)){
          M2[[i]]<-matrix(boots[,i],ncol=nc,nrow=nc)
        }

     return(list(OriginalMatrix=self$values,BootStrap=M2))
     },



      #' @param n Number of iteration. By default n=100.
      #' @return A list formed by the original confusion matrix and the normalized matrix.
      #' @description Public method that carries out an iterative process is carried out where each element is divided by the total of the sum of its row, thus obtaining new values. In the next iteration, all the elements are added by columns and each element is divided by the total of its column and they obtain new values, and so on. The reference \insertCite{fienberg1970,munoz2016}{PaolaR6Nuevo} is followed for the computations.
      #' @examples
      #' A<-matrix(c(238051,7,132,0,0,24,9,2,189,1,4086,188,0,4,16,45,1,0,939,5082,
      #' 51817,0,34,500,1867,325,17,0,0,5,11148,1618,78,0,0,0,0,48,4,834,2853,340,
      #' 32,0,197,5,151,119,135,726,6774,75,1,553,0,105,601,110,174,155,8257,8,0,
      #' 29,36,280,0,0,6,5,2993,0,115,2,0,4,124,595,0,0,4374),nrow=9,ncol=9)
      #' p<-MatCon$new(A,Source="Muñoz, 2016")
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



      #' @description  Public method that small values are calculated for empty cells of the matrix. All non-empty cells of the matrix change their values. This function will not be applied if all the elements of the matrix are different from 0. The reference \insertCite{munoz2016}{PaolaR6Nuevo} is followed for the computations.
      #' @return A list formed by the original confusion matrix and the Pseudozeroes matrix.
      #' @examples
      #' A<-matrix(c(238051,7,132,0,0,24,9,2,189,1,4086,188,0,4,16,45,1,0,939,5082,
      #' 51817,0,34,500,1867,325,17,0,0,5,11148,1618,78,0,0,0,0,48,4,834,2853,340,
      #' 32,0,197,5,151,119,135,726,6774,75,1,553,0,105,601,110,174,155,8257,8,0,
      #' 29,36,280,0,0,6,5,2993,0,115,2,0,4,124,595,0,0,4374),nrow=9,ncol=9)
      #' p<-MatCon$new(A,Source="Muñoz, 2016")
      #' p$MPseudoZeroes()$values
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

       MERROR=self$values
       SumaMatriz <-sum(MERROR)
       MLandas <- (self$sumfil %*% t(self$sumcol))/(SumaMatriz*SumaMatriz)
       K <- (SumaMatriz*SumaMatriz - sum(MERROR*MERROR))/sum((SumaMatriz*MLandas - MERROR )^2)
       MPseudoceros <- (SumaMatriz/(K+SumaMatriz))*(MERROR + K*MLandas)

     return(list(OriginalMatrix=self$values,PseudoZeroesMatrix=MPseudoceros))
     },



# Functions that use weight matrices --------------------------------------




      #' @description  Public method that calculates the general Tau concordance index and its standard deviation.
      #' @param WV Weights vector (as matrix)
      #' @return  Overall accuracy index, producer accurancy index, O3,O4, Tau index?(mirar definicion en funcion) y its standard desviation.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' WV <-matrix(c(0.4, 0.1, 0.4, 0.1), ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green,2008")
      #' p$DetailedWTau(WV)
      #'
      #' @aliases

     DetailedWTau = function(WV){
       nc <- nrow(self$values)
       SumaMatriz <-sum(self$values)
      # In %
       MERROR<- self$values/SumaMatriz
      # UnWeighted marginals (prob)
       pcol <- apply(MERROR,2,sum)
       prow<- apply(MERROR,1,sum)
       O1 <- sum(diag(MERROR)  )
       O2 <- sum(WV*pcol)
       O3 <- sum(diag(MERROR)*(WV+pcol))
       mintermedia1<- matrix(rep(pcol, nc), nrow =nc, ncol=nc, byrow=FALSE)
       mintermedia2<- matrix(rep(WV, nc), nrow =nc, ncol=nc, byrow=TRUE)
       mintermedia3 <-(mintermedia1+mintermedia2)^2
       O4 <- sum(MERROR*mintermedia3)
       t1<- (1-O1) #probabilidad error general porporcional
       t2<- (1-O2) #probabilidad error productor proporcional
       t3<- O1*t1/(t2^2)
       t4<- 2*t1*(2*O1*O2-O3)/(t2^3)
       t5<- (t1^2)*(O4-4*O2^2)/(t2^4)
       Tau <- (O1-O2)/t2
       SdT <- sqrt((t3+t4+t5)/SumaMatriz)
       CV<- Tau/SdT
     return(list(WeightsVector=WV, O1=O1, O2=O2, O3=O3,O4=O4, Tau=Tau, SdT=SdT, CV=CV))
     },

      #' @description  Public method that calculates the general Kappa agreement index (weighted) and its standard deviation. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
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
       MERROR<- self$values/SumaMatriz
      # UnWeighted marginals (prob)
       pcol <- apply(MERROR,2,sum)
       prow<- apply(MERROR,1,sum)
      # Weighted matrix
       WMERROR<-MERROR*WM

      # The 4 coefficients
       Ow1 <- sum(WM*MERROR)
       Ow2 <- sum(t(WM*prow)*pcol)
       c1<- (1-Ow1)
       c2<- (1-Ow2)
       wi_ <- WM %*% pcol
       w_j <- WM %*% prow
       mintermedia1<- matrix(rep(wi_, nc), nrow =nc, ncol=nc, byrow=FALSE)
       mintermedia2<- matrix(rep(w_j, nc), nrow =nc, ncol=nc, byrow=TRUE)
       mintermedia3 <-(mintermedia1+mintermedia2)*c1
       mintermedia4 <- (WM*c2-mintermedia3)^2
       Ow4 <- sum(MERROR*mintermedia4)
       K <- (Ow1-Ow2)/c2
       SdK <- sqrt((Ow4-(Ow1*Ow2-2*Ow2+Ow1)^2)/(SumaMatriz*(c2^4)))
       CV <- K/SdK
     return(list(WeightMatrix=WM, K=K, SdK=SdK, CV=CV))
     },


      #' @description  Public method that calculates the weighted accuracies and standard deviations of the user and the producer. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' @param WM Weight matrix
      #' @return A list with weight matrix, Matrix formed with its original elements and their corresponding weights, general accuracy of the weight matrix obtained, accuracy of the producer and user and their standard deviations,
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' p<-MatCon$new(A,Source="Congalton&Green, 2008")
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
        MV<- self$values/sum(self$values)
        # Weighted matrix
        WMERROR<-MV*WM

        # Weighted OA
        WOverallAcc <- sum(diag(WMERROR))/sum(WMERROR)
        # Weighted marginals
        mcol<- apply(WMERROR,2,sum)
        mrow<- apply(WMERROR,1,sum)
        # UnWeighted marginals (proportions)
        p_j <- apply(MV,2,sum)
        pi_ <- apply(MV,1,sum)

        # Initialization of vectors
        nc <- nrow(MV)
        wi_<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        w_j<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        # Weighted per class user's and producer's accuracies
        wpcua<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        wpcpa<- matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE)
        pcpasd <-matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE) # Per class producer's accuracy standard deviation
        pcuasd <-matrix(rep(0, nc), nrow =1, ncol=nc, byrow=TRUE) # Per class user's accuracy standard deviation
            for (i in 1:nc){
                wi_[i]    <- sum(p_j*WM[i,])
                w_j[i]    <- sum(pi_*WM[,i])
                wpcua[i]  <- sum(WMERROR[i,])/sum(MV[i,])
                wpcpa[i]  <- sum(WMERROR[,i])/sum(MV[,i])
                pcpasd[i] <- sqrt(wpcpa[i]*(1-wpcpa[i])/ncol[i])
                pcuasd[i] <- sqrt(wpcua[i]*(1-wpcua[i])/nrow[i])
            }

     return(list(WeightMatrix=WM,WMERROR=WMERROR, WOverallAcc=WOverallAcc, WPrAcc=wpcpa,WPrAccSDeviation=pcpasd,WUserAcc= wpcua, WUserAccSDeviation=pcuasd))
     }



   ),
   private = list(
     nUsos = NULL
   ),
   active = list(
   )
)

