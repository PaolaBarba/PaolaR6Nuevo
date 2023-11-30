#' @title test
#' @description
#' Creates a new instance of this [R6][R6::R6Class] class.
#' @param A Matrix
#' @param B Matrix
#' @param ID Identifier. By default ID is a random number between 1 and 1000.
#' @param Date Date provided by the user. By default the date provided by the system will be taken.
#' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
#' @return Object of class test
#' @export test
#' @references
#' \insertRef{garcia2018}{PaolaR6Nuevo}
#' @importFrom R6 R6Class
#' @importFrom Rdpack reprompt
#'
#'
#' @aliases


test <- R6Class("test",
  #inherit=MatCon,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param A Matrix
    #' @param B Matrix
    #' @param ID Identifier. By default ID is a date in YYYYMMDD format
    #' @param Date Date provided by the user. By default the date provided by the system will be taken.
    #' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
    #' @examples
    #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
    #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
    #' f <- test$new(A,B,Source="Congalton and Green 2008")
    #'
    #' @aliases


    A=NULL,
    B=NULL,
    ID=NULL,
    Date=NULL,
    Source=NULL,
    n=NULL,
    m=NULL,
  initialize = function(A=NULL,B=NULL, ID = NULL, Date=NULL, Source=NULL){
   # self$a1<-MatCon$new(A)
   # self$b1<-MatCon$new(B)

# Optional values ---------------------------------------------------------
    if(is.null(ID)){
      secuencia <- sprintf("%s-%03d", format(Sys.Date(),"%Y%m%d"), 1:999)
      self$ID <- secuencia[1]
      secuencia <- setdiff(secuencia, secuencia[1])
      }else{
      self$ID<-ID
    }
    if(!is.null(Date)){
      self$Date<-Date
    }else{self$Date <- Sys.Date()}

    if(!is.null(Source)){
      self$Source <- Source
    }else{self$Source<-NULL}

# Initializing values and checking if they are correct values -----------

  self$A <- A
  self$B <- B
  self$n <- length(self$A)
  self$m <- length(self$B)
# Check -------------------------------------------------------------------

    if (self$n != self$m) {
      print("the matrices are not the same size")
    }
    },


      #' @description Public method that provides the Hellinger distance. The reference \insertCite{garcia2018}{PaolaR6Nuevo} is followed for the computations.
      #' The mathematical expression is:
      #'
      #' \deqn{
      #' HD = \dfrac{4nm}{n+m} \sum^{M}_{i=1} (\sqrt{p_i}-\sqrt{q_i})^2
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item HD: Hellinger Distance
      #'   \item n: number of elements in the matrix A.
      #'   \item \code{p_i}: element i of the probability vector of matrix A.
      #'   \item \code{q_i}: element i of the probability vector of matrix B.
      #' }
      #' @return The statistical value of the statistical test based on the Hellinger distance.
      #' @param A matrix. By default, the defined matrix is taken to create the object of the test class.
      #' @param B matrix. By default, the defined matrix is taken to create the object of the test class.
      #' @param p matrix probability vector. By default, the probability of success for each cell is taken.
      #' @param q matrix probability vector. By default, the probability of success for each cell is taken.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f <- test$new(A,B,Source="Congalton and Green 2008")
      #' f$StHell()
      #'
      #' @aliases

    StHell = function(A=NULL,B=NULL,p=NULL,q=NULL){
      if(is.null(A)){
        A<-self$A
      }else{A<-A}
      if(is.null(B)){
        B<-self$B
      }else{B<-B}
      if(is.null(p)){
        p<-A/sum(A)
      }else{p<-p}
      if(is.null(q)){
        q<-B/sum(B)
      }else{q<-q}

      if(length(q)!=length(p)){
       stop("Probabilities with different sizes.")
      }else{
        p_orig <- 4*((sum(A)*sum(B)/(sum(A)+sum(B))) * sum((sqrt(p) - sqrt(q))^2))

    return(StHell=p_orig)
    }
    },

      #' @description Public method that tests whether two independent confusion matrices are significantly different. The reference \insertCite{congalton2008}{PaolaR6Nuevo} is followed for the computations.
      #' The mathematical expression to calculate its statistic is:
      #'
      #' \deqn{
      #' Z = \dfrac{\abs{K1-K2}}{\sqrt(var(K1)+var(K2))}
      #' }
      #'
      #' Where:
      #' \enumerate{
      #'   \item K1: Kappa index of matrix A
      #'   \item K2: Kappa index of matrix B
      #'   \item var(K1): variance of K1.
      #'   \item var(K2): variance of K2.
      #' }
      #' @return A list with the value of the statistic between kappa values and its z score for a given alpha significance level.
      #' @param alpha significance level. By default alpha=0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f <- test$new(A,B,Source="Congalton and Green 2008")
      #' f$kappa.test()
      #'
      #' @aliases

    kappa.test=function(alpha=NULL){
      if(is.null(alpha)){
        alpha<-0.05
      }else{alpha<-alpha}

      k1<-MatCon$new(self$A)$Kappa()[[1]]
      k2<-MatCon$new(self$B)$Kappa()[[1]]
      v1<-MatCon$new(self$A)$Kappa()[[2]]
      v2<-MatCon$new(self$B)$Kappa()[[2]]

      Z<-abs(k1-k2)/(sqrt(v1+v2))
      cl<-qnorm(alpha/2)

      if(Z>=cl){
        cat(sprintf("The null hypothesis is rejected. Therefore, their kappa values and confusion matrices are significantly different.\n"))
      }else{cat(sprint("The null hypothesis is not rejected. Therefore, the kappa values and the confusion matrices do not present significant differences.\n"))}

    return(list(St=Z,Z=cl))
    },

# test public function. ---------------------------------------------------


      #' @description Public method that performs a homogeneity test between two matrices based on the Hellinger distance. The reference \insertCite{garcia2018}{PaolaR6Nuevo} is followed for the computations.
      #' @return p value and decision to make.
      #' @param n1 Number of bootstraps that you want to generate. By default n=10000.
      #' @param alpha significance level. By default alpha=0.05.
      #' @examples
      #' A<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),nrow=4,ncol=4)
      #' B<-matrix(c(45,6,0,4,4,91,8,7,12,5,55,3,24,8,9,55),nrow=4,ncol=4)
      #' f <- test$new(A,B,Source="Garcia-Balboa el al. 2018")
      #' f$TSCM.test()
      #'
      #' @aliases

    TSCM.test=function(n1=NULL,alpha=NULL){
      if(is.null(n1)){
        n1<-10000
      }else{n1<-n1}

      if(is.null(alpha)){
        alpha<-0.05
      }else{alpha<-alpha}

      p2<-self$A/sum(self$A)
      q2<-self$B/sum(self$B)

      p_orig<-self$StHell()
      #Probability defined between p and q
      p_0<-c()
      for(i in 1:length(p2)){
        p_01<-(self$n*p2[i]+self$m*q2[i])/(self$n+self$m)
        p_0<-c(p_0,p_01)
      }
      #bootstrap
      q1<-list()
      p1<-list()
      p1<-MatCon$new(self$A)$MBootStrap(n1,p_0)[[2]]
      q1<-MatCon$new(self$B)$MBootStrap(n1,p_0)[[2]]
      #calculation of hellinger statistics by pairs
      Tn<-c()

      for (i in 1:length(p1)) {
        Tn1<-self$StHell(p1[[i]],q1[[i]])
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
       cat(sprintf("The null hypothesis is not rejected, both confusion matrices exhibit a similar level of accuracy.\n"))
     }else{
       cat(sprintf("The hypothesis that both distributions are is still rejected and the confusion matrices, therefore, are not similar.\n"))
     }
    return(pvalue)
    }

),


# Private functions -------------------------------------------------------

  private = list(

  )
)

