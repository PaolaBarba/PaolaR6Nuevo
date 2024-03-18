#' @title Quality Control Columns Set
#' @description The difference between a QCCS and a confusion matrix is
#' that while forming a confusion matrix requires that the reference and
#' the product be more or less equivalent, for the QCCS it is required
#' that the reference be actually of higher quality than the product.
#' This forces us to leave the marginals corresponding to the reference
#' fixed. That is why we work in columns. In this way, the QCCS class
#' works with a confusion matrix expressed as a set of column vectors
#' and it will be analyzed by columns. A QCCS is constructed by comparing
#' a sample of a set of common positions in the product and the ground
#' truth. Appropriate sampling methods must be applied to generate the
#' QCCS. It is considered that the classes of the ground truth
#' correspond to the columns and that the classes of the
#' product to be valued correspond to the rows. On the other hand, the
#' concept of QCCS is directly linked to quality control, so the
#' specifications of this control must be indicated. Specifications are
#' stated as percentages. E.g. for class "A" under consideration, a
#' minimum quality value is established (e.g. better than 90%), and
#' maximum values of confusion with other categories (e.g. confusion
#' between A and B less than 5%). The specifications are proportions of
#' a multinomial. First, an object of this class of object must be
#' created (instantiated) and then the methods that offer the index
#' calculations will be invoked.
#' @export QCCS
#' @note  Error Messages: List of possible errors:
#' \itemize{
#'  \item \code{Error type 1}: Different number of data vectors and probability.
#'  \item \code{Error type 2}: Different number of elements in the pair of data
#'  vectors and probabilities.
#'  \item \code{Error type 3}: The sum of the elements of the data vectors is 0.
#'  \item \code{Error type 4}: The sum of each probability vectors must be 1.
#'  \item \code{Error type 5}: Some element of the data vector is negative.
#'  \item \code{Error type 6}: Some element of the probability vector is negative.
#'}
#' @references
#' \insertRef{QCCS}{ConfMatrix}
#'
#' \insertRef{alba2020}{ConfMatrix}
#' @importFrom R6 R6Class
#' @importFrom stats dmultinom pchisq
#' @importFrom Rdpack reprompt
#'
#'
#' @aliases QCCS


QCCS <- R6Class("QCCS",
  cloneable=FALSE,
   public = list(
    #' @field vectors
    #'\verb{
     #'List of integer data vectors.
     #'}
    vectors = NULL,
    #' @field prob
    #'\verb{
    #'List of probability vectors.
    #'}
    prob = NULL,
    #' @field ID
    #'\verb{
    #' Identifier. It is a character string with a maximum length of 50
    #' characters. By default,} \eqn{QCCS_i} \verb{will be taken as identification.
    #' Where} \eqn{i \in (1,999)} \verb{will be the number of QCCS instances already defined.
    #'}
    ID=NULL,
    #' @field Date
    #'\verb{
    #'Date provided by the user in format DDMMYYYY, "DD-MM-YYYY", "DD/MM/YYYY".
    #'By default the date provided by the system will be taken.
    #'}
    Date=NULL,
    #' @field ClassName
    #'\verb{
    #' Name of the classes. It is given by a character strings vector whose
    #' elements are the name of the classes. Each element of the vector is
    #' a string of maximum 20 characters. By default for the column elements
    #' they will be} \eqn{Ref_i'}.
    #'
    ClassName=NULL,
    #' @field Source
    #'\verb{
    #'Indicates where the "vectors" and "prob" parameters come from (article,
    #'project, etc.). It is suggested to enter a reference or a DOI. A character
    #'string with a maximum length of 80 characters can be entered. By default,
    #'is NULL.
    #'}
    Source=NULL,


    #' @description Public method to create an instance of the QCCS class.
    #' At the time of creation, column set data and specification values
    #' must be provided. The same number of data and as specification values
    #' must be entered, and the pairs of data-specifications vectors must
    #' have the same size, otherwise an error will be provided.
    #' The optional possibility of adding metadata to the matrix is offered.
    #' The values of the data vectors represent the classes of ground truth.
    #' @param vectors
    #' \verb{
    #' List of integer data vectors.
    #' }
    #' @param prob
    #' \verb{
    #' List of probability vectors.
    #' }
    #' @param ID
    #'\verb{
    #' Identifier. It is a character string with a maximum length of 50
    #' characters. By default,} \eqn{QCCS_i} \verb{will be taken as identification.
    #' Where} \eqn{i \in (1,999)} \verb{will be the number of QCCS instances already defined.
    #'}
    #' @param Date
    #'\verb{
    #' Date provided by the user in format DDMMYYYY, "DD-MM-YYYY", "DD/MM/YYYY".
    #' By default the date provided by the system will be taken.
    #'
    #'}
    #' @param ClassName
    #' \verb{
    #' Name of the classes. It is given by a character strings vector whose
    #' elements are the name of the classes. Each element of the vector is
    #' a string of maximum 20 characters. By default for the column elements
    #' they will be} \eqn{Ref_i'}.
    #'
    #' @param Source
    #' \verb{
    #' Indicates where the "vectors" and "prob" parameters come from (article,
    #' project, etc.). It is suggested to enter a reference or a DOI. A character
    #' string with a maximum length of 80 characters can be entered. By default,
    #' is NULL.
    #' }
    #' @examples
    #' vectors<-list(c(47,4,0),c(44,5,3))
    #' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02))
    #' A<-QCCS$new(vectors,prob,
    #' Source="Ariza-Lopez et al. 2019")
    #'
    #' @aliases NULL

  initialize = function(vectors,prob,ID=NULL,Date=NULL,ClassName=NULL,Source=NULL) {


# Optional values ---------------------------------------------------------


    self$vectors <- vectors
    self$prob <- prob

    if(is.null(ID)){
      secuencia <- paste("QCCS_",seq(1:999),sep="")
      self$ID <- secuencia[1]
      secuencia <- setdiff(secuencia, secuencia[1])
    }else{
      self$ID<-substr(ID,1,50)
    }
    if(!is.null(Date)){
      self$Date<-Date
    }else{self$Date <- Sys.Date()}

    colname<-c()
    if (!is.null(ClassName)) {
      self$ClassName <- ClassName
      for (i in 1:length(self$vectors)) {
        colname <- c(colname, sprintf("Ref_%.20s", self$ClassName[i]))
      }
      names(self$vectors) <- colname

    } else {
      self$ClassName <- ClassName
      for (i in 1:length(self$vectors)) {
        colname <- c(colname, sprintf("Ref_%d", i))
      }
      names(self$vectors) <- colname
    }
    if(!is.null(Source)){
      self$Source <- substr(Source,1,80)
    }else{self$Source<-NULL}

# Initializing values and checking if they are correct values -----------
    error1<- FALSE
    error2<- FALSE
    error3<- FALSE
    error4<- FALSE
    error5<- FALSE
    error6<- FALSE


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
          cat("Error type 2: The vectors and their corresponding\nprobabilities must have the same size\n")
        }
        if(sum(vi)==0){
          error3<-TRUE
          cat("Error type 3: The sum of the elements of the\ndata vectors is 0\n")
        }
        if(sum(pi)!=1){
          error4<-TRUE
          cat("Error type 4: The sum of each probability\nvectors must be 1\n")
        }

        for (i in 1:ni) {

        if(vi[i]<0){
          error5<-TRUE
          cat("Error type 5: Some element of the data\nvector is negative\n",vi,"\n")
        }
        if(pi[i]<0){
          error6<-TRUE
          cat("Error type 6: Some element of the probability\nvector is negative.\n",pi,"\n")
        }
        }
      }
    if ((error1 == TRUE) || (error2==TRUE) || (error3 == TRUE)
        || (error4 == TRUE) || (error5==TRUE) || (error6==TRUE)) {
      warning("Type errors 1, 2, 3, 4, 5 or 6\n")
      stop()
    }
  },






# To calculate p-value, use test-ntol (method private) --------------------
# Multinomial or binomial Exact Tests -------------------------------------


      #' @description Public method that using a QCCS object
      #' instance calculates whether the data meets specifications.
      #' An exact test is applied to each of the multinomials
      #' that are defined for each column.
      #' The Bonferroni method is used.
      #' The references \insertCite{QCCS,alba2020}{ConfMatrix} are followed
      #' for the computations.
      #' @param a significance level. By default a=0.05.
      #' @return The p value of the exact test using Bonferroni.
      #' @examples
      #' vectors<-list(c(47,4,0),c(44,5,3))
      #' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02))
      #' A<-QCCS$new(vectors,prob,
      #' Source="Ariza-Lopez et al. 2019")
      #' A$Exact.test()
      #'
      #' @aliases NULL


    Exact.test = function(a=NULL) {
      if(is.null(a)){
        a<-0.05
      }else{a<-a}

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

      ap<-private$MethBonf(sol,a)

    return(list(ap,OriginalVectors=self$vectors,OriginalProb=self$prob))
    },



# ji global multinomial or binomial test ----------------------------------------------

      #' @description Public method that using a QCCS
      #' object instance calculates whether the data meets
      #' specifications. The chi square test is used.
      #' The references
      #' \insertCite{QCCS,alba2020}{ConfMatrix}
      #' are followed for the computations.
      #' @param a significance level. By default a=0.05.
      #' @return The p value derived from the chi square test.
      #' @examples
      #' vectors<-list(c(18,0,3,0),c(27,19))
      #' prob<-list(c(0.85,0.1,0.03,0.02),c(0.8,0.2))
      #' A <- QCCS$new(vectors,prob,
      #' Source="Alba-Fernández et al. 2020")
      #' A$JiGlobal.test()
      #'
      #' @aliases NULL

    JiGlobal.test=function(a=NULL){
      if(is.null(a)){
        a<-0.05
      }else{a<-a}

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

    if(p_value>a){
      cat("The null hypothesis is not rejected.\n",p_value,">=",a)
      cat("\nThe set of elements are well defined")
    }else{
      cat("The null hypothesis is rejected.\n",p_value,"<",a)
      cat("\nThe set of elements are not well defined")

    }

    return(p_value)
    },


# ji multinomial or binomial test ----------------------------------------------

      #' @description Public method that using a QCCS object instance
      #' calculates whether the data meets specifications. The chi
      #' square test is used. The Bonferroni method is used.
      #' The references \insertCite{QCCS,alba2020}{ConfMatrix} are
      #' followed for the computations.
      #' @param a significance level. By default a=0.05.
      #' @return The p value from the chi square test.
      #' @examples
      #' vectors<-list(c(18,0,3,0),c(27,19))
      #' prob<-list(c(0.85,0.1,0.03,0.02),c(0.8,0.2))
      #' A <- QCCS$new(vectors,prob,
      #' Source="Alba-Fernández et al. 2020")
      #' A$Ji.test()
      #'
      #' @aliases NULL

    Ji.test=function(a=NULL){
      if(is.null(a)){
      a<-0.05
      }else{a<-a}

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

        ap<-private$MethBonf(p_value,a)

    return(list(ap,OriginalVectors=self$vectors,OriginalProb=self$prob))
    },

# print function ----------------------------------------------------------

      #' @description Public method that shows all the data entered
      #' by the user.
      #' @return QCCS object identifier, Date, name of classes, source
      #' of data and data vectors and probability.
      #' @examples
      #' vectors<-list(c(18,0,3,0),c(27,19))
      #' prob<-list(c(0.85,0.1,0.03,0.02),c(0.8,0.2))
      #' A<-QCCS$new(vectors,prob,
      #' Source="Alba-Fernández et al. 2020")
      #' A$print()
      #'
      #' @aliases NULL

    print=function(){
      cat("Identifier (ID)\n", self$ID, "\n")
      cat("-------------------------------------\n")
      cat(sprintf("Date\n %s \n", self$Date))
      cat("-------------------------------------\n")
      cat("Source\n", self$Source, "\n")
      cat("-------------------------------------\n")
      for(i in 1:length(self$vectors)){
        cat("Name of Class|",names(self$vectors)[i], "\n")
        cat("Vector       |",self$vectors[[i]],"\n")
        cat("Probability  |",self$prob[[i]],"\n")
        cat("-------------------------------------\n")

      }

    }


  ),



# Private functions -------------------------------------------------------



  private = list(

     MethBonf = function(pvalue,a=NULL){
       if(is.null(a)){
         a<-0.05
       }else{a<-a}
       n<-length(pvalue)
        v<-a/n
       for (i in 1:n) {
         if(pvalue[i]>v){
           cat(" The null hypothesis is not rejected.\n ",pvalue[i],">=",v,"\n")
         }else{cat(" The null hypothesis is rejected.\n ",pvalue[i],"<",v,"\n")}
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

