#' @title Quality Control Columns Set
#' @description The p value is calculated using the multinomial distribution from vectors and their corresponding probabilities.
#' @param vectors vector list
#' @param prob probabilities list
#' @param ID Identifier. By default ID is a random number between 1 and 1000.
#' @param Date Date provided by the user. By default the date provided by the system will be taken.
#' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
#' @return Object of class QCCS
#' @export QCCS
#' @references
#' \insertRef{QCCS}{PaolaR6Nuevo}
#' @importFrom R6 R6Class
#' @importFrom stats dmultinom
#' @importFrom Rdpack reprompt
#'
#'
#' @aliases


QCCS <- R6Class("QCCS",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param vectors vector list.
    #' @param prob probabilities list.
    #' @param ID Identifier. By default ID is a date in YYYYMMDD format
    #' @param Date Date provided by the user. By default the date provided by the system will be taken.
    #' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
    #' @examples
    #' vectors<-list(c(47,4,0),c(40,5,3),c(45,6,2),c(48,0))
    #' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02),c(0.9,0.08,0.02),c(0.99,0.01))
    #' A <- QCCS$new(vectors,prob)
    #'
    #' @aliases


    vectors = NULL,
    prob = NULL,
    ID=NULL,
    Date=NULL,
    Source=NULL,
  initialize = function(vectors, prob, ID = NULL, Date=NULL, Source=NULL) {


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



    self$vectors <- vectors
    self$prob <- prob
    n <- length(self$vectors)
    m <- length(self$prob)
      if (n != m) {
        print("There must be the same number of data columns as probability columns")
      }
      for (i in 1:n) {
        vi <- self$vectors[[i]]
        pi <- self$prob[[i]]
        ni <- length(vi)
        mi <- length(pi)

      # check
        if ((ni != mi) == TRUE) {
            print("The vectors and their corresponding probabilities must have the same size")
        }
      }

  },



    #   #' @description Public method that applies the Bonferroni method to make decisions in contrasting hypotheses. The reference \insertCite{alba2020}{PaolaR6Nuevo} is followed for the computations.
    #   #' The mathematical expression is:
    #   #'
    #   #' \deqn{
    #   #' BonfMeth = \left\{ \begin{array}{lcc} H_0 \text{is rejected}   &  if & p value \geq \dfrac{\alpha}{4} \\ \\ H_0 \text{is rejected}   &  if & p value<\dfrac{\alpha}{4} \end{array} \right.
    #   #' }
    #   #'
    #   #' Where:
    #   #' \enumerate{
    #   #'   \item BonfMeth: Bonferroni method.
    #   #'   \item alpha: significance level.
    #   #' }
    #   #' @return The statistic value of the statistical test based on the Hellinger distance.
    #   #' @param pvalue vector with the p values obtained.
    #   #' @param alpha significance level. By default alpha=0.05.
    #   #' @examples
    #   #' vectors<-list(c(47,4,0),c(40,5,3),c(45,6,2),c(48,0))
    #   #' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02),c(0.9,0.08,0.02),c(0.99,0.01))
    #   #' p <- QCCS$new(vectors,prob,Source="")
    #   #' pval<-p$QCCS()[[2]]
    #   #' p$MethBonf(pval)
    #   #'
    #   #' @aliases
    #
    # MethBonf = function(pvalue,alpha=NULL){
    #   if(is.null(alpha)){
    #     alpha<-0.05
    #   }else{alpha<-alpha}
    #   n<-length(pvalue)
    #
    #   for (i in 1:n) {
    #     if(pvalue[i]>alpha/n){
    #       cat(sprintf("The null hypothesis is not rejected.\n",p[i],">=",alpha/n))
    #     }else{cat(sprintf("The null hypothesis is rejected.\n",p[i],"<",alpha/n))}
    #   }
    #   return(list(p_value=pvalue,BF=alpha/4))
    # },


# QCCS public function. To calculate p-value, use test-ntol ---------------



      #' @description Public method that, using a list of vectors and their corresponding probabilities, through a multinomial distribution, calculates the p value using each of the vectors. The reference \insertCite{QCCS}{PaolaR6Nuevo} is followed for the computations.
      #' @return The p value is obtained for each vector, and using the Bonferroni criterion it is decided whether the elements are well classified or not.
      #' @examples
      #' vectors<-list(c(47,4,0),c(40,5,3),c(45,6,2),c(48,0))
      #' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02),c(0.9,0.08,0.02),c(0.99,0.01))
      #' A <- QCCS$new(vectors,prob,Source="Ariza et al.,2019")
      #' A$QCCS()
      #'
      #' @aliases

    QCCS = function() {
      n <- length(self$vectors)
      m <- length(self$prob)
      sol <- c()
      p_value<-c()
      p_value1<-0
        for (i in 1:n) {
          vi <- self$vectors[[i]]
          pi <- self$prob[[i]]
          ni <- length(vi)
          mi <- length(pi)
          s <- sum(vi)

          if(ni==2){
            p_value1<-private$test.2tol(vi,pi)[1]
          } else
          if(ni!=2){
            p_value1<-private$test.ntol(vi,pi)$p.valor
          }
          p_value<-c(p_value,p_value1)
        }

      sol <- c(sol, p_value)
    return(p_value=sol)
    }

  ),



# Private functions -------------------------------------------------------



  private = list(

     MethBonf = function(pvalue,alpha=NULL){
       if(is.null(alpha)){
         alpha<-0.05
       }else{alpha<-alpha}
       n<-length(pvalue)

       for (i in 1:n) {
         if(pvalue[i]>alpha/n){
           cat(sprintf("The null hypothesis is not rejected.\n",p[i],">=",alpha/n))
         }else{cat(sprintf("The null hypothesis is rejected.\n",p[i],"<",alpha/n))}
       }
       return(list(p_value=pvalue,BF=alpha/4))
     },



    combina = function(n, N) {
      result <- NULL
      stack <- list(list(comb = NULL, remaining = N, index = 1))
      #define progress bar
      pb <- txtProgressBar(min = 0,      # Minimum value of progress bar
                           max = choose(N+1+n-1,n),    # Maximum value of progress bar
                           style = 3,    # Style
                           width = 50,   # Broad
                           char = "=")   # Character used to create the bar
      k<-0
      while (length(stack) > 0) {
        current <- stack[[length(stack)]]
        stack <- stack[-length(stack)]

        comb <- current$comb
        remaining <- current$remaining
        index <- current$index
          if (length(comb) == n) {
            k=k+1
            setTxtProgressBar(pb, k)
            result <- rbind(result, comb)
          } else {
            for (i in 0:remaining) {
            stack <- c(stack, list(list(comb = c(comb, i), remaining = remaining - i, index = index + 1)))
          }
        }
      }
      close(pb)
      return(result)
    },

# Functions depending on the number of tolerances -------------------------

    test.ntol = function(M, p){
      n<-length(M)
      N <- sum(M)

      # Calculate the sample space
      Sucesos1<-NULL
      Sucesos<-private$combina(n,N)
      Sucesos1<- matrix(ncol = n,nrow=0)

        for (i in 1:dim(Sucesos)[1]) {
          if (sum(Sucesos[i,]) == N) {
          Sucesos1<-rbind(Sucesos1,Sucesos[i,])
          }
        }

      #probabilities
      Q <- matrix(ncol = n+2, nrow = dim(Sucesos1)[1])

      for (i in 1:dim(Sucesos1)[1]) {
        Q[,1:n]<-Sucesos1[,1:n]
        Q[i,n+1]<-dmultinom(Sucesos1[i,1:n],prob=p)
        Q[1,n+2]<-Q[1,n+1]
        if(i>1){
          Q[i,n+2]<-Q[i, n+1] + Q[(i - 1), n+2]
        }
      }
      A <- matrix(nrow = 0, ncol = n+2)

      #Character string with the conditions to evaluate
      cond<-"Q[j,1]==M[1]"
      cond1<-paste(" Q[j,", 2:n, "] == M[", 2:n, "]", sep = "")
      cond<-c(cond,cond1)
      condicion<-noquote(cond)

      for(j in 1:dim(Q)[1]) {
        if ( (Q[j, 1] < M[1])){
          A <- rbind(A, Q[j, ])
        }

        for(ni in 1:n-1){
        #conditions of equality to be evaluated in each case
        condicion1<-paste(cond[1:ni], collapse = " & ")

          if(( eval(parse(text=condicion1)))){
            if(Q[j, ni+1] < M[ni+1]){
              A<-rbind(A,Q[j,])
            }
          }
        }
        if(Q[j,1]==M[1] & Q[j,2]==M[2] & Q[j,3]==M[3] ){
              A<-rbind(A,Q[j,])
        }
      }

    #  p_valor <- max(A[, n+2])+dmultinom(M,prob=p)
    #  p_valor1<-c(sum(A[,n+1]))+dmultinom(M,prob=p) #así creo que es para QCCS 2019
      p_valor<-sum(A[,n+1])

      resultados <- list(A = A, p.valor = p_valor)
    return(resultados)
    },


# Function for 2 tolerances -----------------------------------------------



    test.2tol=function(M, p){
      N<-sum(M)
      p<-pbinom(M,N,p)
      p_value<-p[1]
     return(p_value)
    }




 )
)

