#' @title Quality Control Columns Set
#' @description The p-value is calculated from,the given data and probability
#' vectors, which follow a multinomial or binomial distribution. Hypothesis
#' contrasts are applied and it is decided whether the classification of
#' elements is optimal or not.
#' @param vectors vector list
#' @param prob probabilities list
#' @param ID Identifier. By default ID is a random number between 1 and 1000.
#' @param Date Date provided by the user. By default the date provided by the
#' system will be taken.
#' @param Source Indicates where the matrix comes from (article, project, etc.).
#'  By default is NULL.
#' @return Object of class QCCS.
#' @export QCCS
#' @note  Error Messages
#'
#' List of possible errors:
#' \itemize{
#'  \item \code{Error type 1}: Different number of data vectors and probability.
#'  \item \code{Error type 2}: Different number of elements in the pair of data
#'  vectors and probabilities.
#'  \item \code{Error type 3}: The sum of the elements of the data vectors is 0.
#'  \item \code{Error type 4}: The sum of the elements of the probability
#'  vectors is 0.
#'  \item \code{Error type 5}: Some element of the data vector is negative.
#'  \item \code{Error type 6}: Some element of the probability vector is negative.
#'}
#' @references
#' \insertRef{QCCS}{PaolaR6Nuevo}
#'
#' \insertRef{alba2020}{PaolaR6Nuevo}
#' @importFrom R6 R6Class
#' @importFrom stats dmultinom pchisq
#' @importFrom Rdpack reprompt
#'
#'
#' @aliases


QCCS <- R6Class("QCCS",
  public = list(
    #' @description Public method to create an instance of the QCCS class.
    #' At the time of creation, a list of vectors with data and a list
    #' of probability vectors that correspond to the data must be
    #' defined. The same number of data vectors as probability vectors
    #' must be entered, the pairs of data-probability vectors must have
    #' the same size, otherwise an error will be displayed.
    #' The optional possibility of adding metadata to the matrix is offered.
    #' The values of the data vectors represent the reference categories that
    #' will be taken into account.
    #'
    #' @param vectors vector list.
    #' @param prob probabilities list.
    #' @param ID Identifier. By default ID is a date in YYYYMMDD format
    #' @param Date Date provided by the user. By default the date provided
    #' by the system will be taken.
    #' @param Source Indicates where the matrix comes from
    #' (article, project, etc.). By default is NULL.
    #' @examples
    #' vectors<-list(c(18,0,3,0),c(27,19))
    #' prob<-list(c(0.85,0.1,0.03,0.02),c(0.8,0.2))
    #' A<-QCCS$new(vectors,prob,
    #' Source="Alba-Fernández et al. 2020")
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
    error1<- FALSE
    error2<- FALSE
    error3<- FALSE
    error4<- FALSE
    error5<- FALSE
    error6<- FALSE


    self$vectors <- vectors
    self$prob <- prob
    n <- length(self$vectors)
    m <- length(self$prob)
      if (n != m) {
        error1<-TRUE
        cat("Error type 1: There must be the same number of\ndata columns as probability columns")
      }

      for (i in 1:n) {
        vi <- self$vectors[[i]]
        pi <- self$prob[[i]]
        ni <- length(vi)
        mi <- length(pi)

      # check
        if ((ni != mi) == TRUE) {
          error2<-TRUE
          cat("Error type 2: The vectors and their corresponding\nprobabilities must have the same size")
        }
        if(sum(vi)==0){
          error3<-TRUE
          cat("Error type 3: The sum of the elements of the\ndata vectors is 0")
        }
        if(sum(pi)==0){
          error4<-TRUE
          cat("Error type 4: The sum of the elements of the\nprobability vectors is 0")
        }
        for (i in 1:ni) {

        if(vi[i]<0){
          error5<-TRUE
          cat("Error type 5: Some element of the data\nvector is negative")
        }
        if(pi[i]<0){
          error6<-TRUE
          cat("Error type 6: Some element of the probability\nvector is negative.")
        }
        }
      }
    if ((error1 == TRUE) || (error2==TRUE) || (error3 == TRUE)
        || (error4 == TRUE) || (error5==TRUE) || (error6==TRUE)) {
      warning("Type errors 1, 2, 3, 4, 5 or 6")
      stop()
    }
  },


# To calculate p-value, use test-ntol -------------------------------------
# Multinomial Exact Tests -------------------------------------------------


      #' @description Public method that, using a list of vectors and their
      #' corresponding probabilities, through a multinomial or binomial
      #' distribution depending on the size of the vector, calculates the
      #' p value using each pair of vector-probability data. The null
      #' hypothesis shows that for each category the data set is either
      #' well classified or not. The Bonferroni method is used.
      #' The references \insertCite{QCCS,alba2020}{PaolaR6Nuevo} is followed
      #' for the computations.
      #' @return The p value is obtained for each vector, and using
      #' the Bonferroni criterion it is decided whether the elements
      #' are well classified or not.
      #' @param alpha significance level. By default alpha=0.05.
      #' @examples
      #' vectors<-list(c(18,0,3,0),c(27,19))
      #' prob<-list(c(0.85,0.1,0.03,0.02),c(0.8,0.2))
      #' A<-QCCS$new(vectors,prob,
      #' Source="Alba-Fernández et al. 2020")
      #' A$Exact.test()
      #'
      #' @aliases

    Exact.test = function(alpha=NULL) {
      if(is.null(alpha)){
        alpha<-0.05
      }else{alpha<-alpha}

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

      a<-private$MethBonf(sol,alpha)

    return(a)
    },



# ji global multinomial test ----------------------------------------------

      #' @description Public method that, using a list of vectors and their
      #' corresponding probabilities that follow a binomial or multinomial
      #' distribution, calculates the p-value for compliance with all
      #' defined specifications. The chi square test is used.
      #' The null hypothesis verifies that the probabilities are met and
      #' therefore that the set of elements are well defined. If one of
      #' the defined probabilities is not met, the null hypothesis would
      #' be rejected. The references \insertCite{QCCS,alba2020}{PaolaR6Nuevo}
      #' is followed for the computations.
      #' @return The p value of the entire data set is obtained, through
      #' the chi-square, and it is decided whether the elements are well
      #' classified or not.
      #' @param alpha significance level. By default alpha=0.05.
      #' @examples
      #' vectors<-list(c(18,0,3,0),c(27,19))
      #' prob<-list(c(0.85,0.1,0.03,0.02),c(0.8,0.2))
      #' A <- QCCS$new(vectors,prob,
      #' Source="Alba-Fernández et al. 2020")
      #' A$JiGlobal.test()
      #'
      #' @aliases

    JiGlobal.test=function(alpha=NULL){
      if(is.null(alpha)){
        alpha<-0.05
      }else{alpha<-alpha}

    #number of vectors and prob vectors
    n <- length(self$vectors)
    m <- length(self$prob)
    p_value<-c()
    Suma<-0
    S<-list()
    k<-0
    #for each vector
      for (j in 1:n) {
      #elements of each vector
      vi <- self$vectors[[j]]
      pi <- self$prob[[j]]
      ni <- length(vi)
      mi <- length(pi)
      k<-k+(ni-1)
      pj<-c()
        for (i in 1:ni){
          pj<-c(pj,(vi[i]-sum(vi)*pi[i])/sqrt(sum(vi)*pi[i]))
        }
      S[[j]]<-pj

      ST<-pj^2

      Suma<-Suma+sum(ST)
      }


    p_value<-pchisq(Suma, k, lower.tail=FALSE)

    if(p_value>alpha){
      cat("The null hypothesis is not rejected.\n",p_value,">=",alpha)
    }else{cat("The null hypothesis is rejected.\n",p_value,"<",alpha)}

    return(p_value)
    },


# ji multinomial test ----------------------------------------------

      #' @description Public method that, using a list of vectors and
      #' their corresponding probabilities that follow a multinomial
      #' or binomial distribution, calculates the p value using each
      #' vector-probability data pair. The chi square test is used. The
      #' null hypothesis shows that for each category the data set is either
      #' well classified or not. The Bonferroni method is used.
      #' The references \insertCite{QCCS,alba2020}{PaolaR6Nuevo} is
      #' followed for the computations.
      #' @return The p value is obtained for each vector, and using
      #' the Bonferroni criterion it is decided whether the elements are
      #' well classified or not.
      #' @param alpha significance level. By default alpha=0.05.
      #' @examples
      #' vectors<-list(c(18,0,3,0),c(27,19))
      #' prob<-list(c(0.85,0.1,0.03,0.02),c(0.8,0.2))
      #' A <- QCCS$new(vectors,prob,
      #' Source="Alba-Fernández et al. 2020")
      #' A$Ji.test()
      #'
      #' @aliases

    Ji.test=function(alpha=NULL){
      if(is.null(alpha)){
      alpha<-0.05
      }else{alpha<-alpha}

      #number of vectors and prob vectors
      n <- length(self$vectors)
      m <- length(self$prob)
      p_value<-c()
      k<-0
      #for each vector
        for (j in 1:n) {
        #elements of each vector
        vi <- self$vectors[[j]]
        pi <- self$prob[[j]]
        ni <- length(vi)
        mi <- length(pi)
        k<-ni-1
        pj<-c()
        for (i in 1:ni){
          pj<-c(pj,(vi[i]-sum(vi)*pi[i])/sqrt(sum(vi)*pi[i]))
        }
        ST<-sum(pj^2)

        pvalue<-pchisq(ST, k, lower.tail=FALSE)
        p_value<-c(p_value,pvalue)
        }

        a<-private$MethBonf(p_value,alpha)

    return(a)
    }


  ),



# Private functions -------------------------------------------------------



  private = list(

       #' @description Public method that applies the Bonferroni method
       #' to make decisions in contrasting hypotheses. The reference
       #' \insertCite{alba2020}{PaolaR6Nuevo} is followed for the
       #' computations.
       #' The mathematical expression is:
       #'
       #' \deqn{
       #' BonfMeth = \left\{
       #' \begin{array}{lcc} H_0 \text{is rejected}
       #' &  if & p value \geq \dfrac{\alpha}{4} \\
       #' \\ H_0 \text{is rejected}   &  if & p value<\dfrac{\alpha}{4}
       #' \end{array}
       #' \right.
       #' }
       #'
       #' Where:
       #' \enumerate{
       #'   \item BonfMeth: Bonferroni method.
       #'   \item alpha: significance level.
       #' }
       #' @return The statistic value of the statistical test based
       #' on the Hellinger distance.
       #' @param pvalue vector with the p values obtained.
       #' @param alpha significance level. By default alpha=0.05.
       #' @examples
       #' vectors<-list(c(47,4,0),c(40,5,3),c(45,6,2),c(48,0))
       #' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02),
       #' c(0.9,0.08,0.02),c(0.99,0.01))
       #' p <- QCCS$new(vectors,prob,Source="")
       #' pval<-p$QCCS()[[2]]
       #' p$MethBonf(pval)
       #'
       #' @aliases

     MethBonf = function(pvalue,alpha=NULL){
       if(is.null(alpha)){
         alpha<-0.05
       }else{alpha<-alpha}
       n<-length(pvalue)
        v<-alpha/n
       for (i in 1:n) {
         if(pvalue[i]>v){
           cat(" The null hypothesis is not rejected. ",pvalue[i],">=",v,"\n")
         }else{cat(" The null hypothesis is rejected. ",pvalue[i],"<",v,"\n")}
       }
       return(list(p_value=pvalue,Bonferroni=v))
     },



    combina = function(n, N) {
      result <- NULL
      stack <- list(list(comb = NULL, remaining = N, index = 1))
      #define progress bar
      pb <- txtProgressBar(min = 0,# Minimum value of progress bar
                           max = choose(N+1+n-1,n),# Maximum value of progress bar
                           style = 3,# Style
                           width = 50,# Broad
                           char = "=")# Character used to create the bar
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
            stack <- c(stack, list(list(comb = c(comb, i),
                     remaining = remaining - i, index = index + 1)))
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

