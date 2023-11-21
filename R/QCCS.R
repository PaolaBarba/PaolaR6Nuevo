#' @title Quality Control Columns Set
#' @description The p value is calculated using the multinomial distribution from vectors and their corresponding probabilities.
#' @param vectors vector list
#' @param prob probabilities list
#' @param ID Identifier. By default ID is a random number between 1 and 1000.
#' @param Date Date provided by the user. By default the date provided by the system will be taken.
#' @param Source Indicates where the matrix comes from (article, project, etc.). By default is NULL.
#' @return Object of class QCCS
#' @export QCCS
#' @references [1] Ariza-López, F. J., Rodríguez-Avi, J., Alba-Fernández, M. V., & García-Balboa, J. L. (2019). Thematic accuracy quality control by means of a set of multinomials. Applied Sciences, 9(20), 4240.
#' @importFrom R6 R6Class
#' @importFrom stats dmultinom
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
    #Si no se añade fecha (Date=2710, Date="27-10", Date="27/10")
    #En ese caso se tomara la fecha del sistema
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
        print("Debe haber el mismo n\xFAmero de columnas de datos que de probabilidades")
      }
      for (i in 1:n) {
        vi <- self$vectors[[i]]
        pi <- self$prob[[i]]
        ni <- length(vi)
        mi <- length(pi)

      # Comprobacion
        if ((ni != mi) == TRUE) {
            print("Deben tener el mismo tama\xF1o los vectores y sus correspondientes probabilidades")
        }
      }

  },


# QCCS public function. To calculate p-value, use test-*tol ---------------



      #' @description Using a list of vectors and their corresponding probabilities, through a multinomial distribution, the p value is calculated using each of the vectors. See reference [1].
      #' @return The p value is obtained for each vector, and using the Bonferroni criterion it is decided whether the elements are well classified or not.
      #' @examples
      #' vectors<-list(c(47,4,0),c(40,5,3),c(45,6,2),c(48,0))
      #' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02),c(0.9,0.08,0.02),c(0.99,0.01))
      #' A <- QCCS$new(vectors,prob)
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
    return(list(p_value=sol))
    }

  ),

  private = list(

    combina = function(n, N) {
      result <- NULL
      stack <- list(list(comb = NULL, remaining = N, index = 1))
      #
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

      # Calcular el espacio muestral
      Sucesos1<-NULL
      Sucesos<-private$combina(n,N)
      Sucesos1<- matrix(ncol = n,nrow=0)

        for (i in 1:dim(Sucesos)[1]) {
          if (sum(Sucesos[i,]) == N) {
          Sucesos1<-rbind(Sucesos1,Sucesos[i,])
          }
        }

      #Se calculan las probabilidades del espacion muestral
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

      #Cadena de caracteres con las condiciones a evaluar
      cond<-"Q[j,1]==M[1]"
      cond1<-paste(" Q[j,", 2:n, "] == M[", 2:n, "]", sep = "")
      cond<-c(cond,cond1)
      condicion<-noquote(cond)

      for(j in dim(Q)[1]:1) {
        if ( (Q[j, 1] < M[1])){
          A <- rbind(A, Q[j, ])
        }

        for(ni in 1:n-1){
        #condiciones de igualdad a evaluar en cada caso
        condicion1<-paste(cond[1:ni], collapse = " & ")

          if(( eval(parse(text=condicion1)))){
            if(Q[j, ni+1] > M[ni+1]){
              A<-rbind(A,Q[j,])
            }
          }
        }
      }

      p_valor <- max(A[, n+2])+dmultinom(M,prob=p)
      p_valor1<-c(sum(A[,n+1])+dmultinom(M,prob=p))

      resultados <- list(A = A, p.valor = p_valor1)
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
