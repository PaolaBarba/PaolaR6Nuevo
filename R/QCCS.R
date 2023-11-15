#' @title Quality Control Columns Set
#' @description The p value is calculated using the multinomial distribution from vectors and their corresponding probabilities.
# #' @param vectors vector list
# #' @param prob probabilities list
#' @return Object of class QCCS
#' @export QCCS
#' @references [1] Ariza-López, F. J., Rodríguez-Avi, J., Alba-Fernández, M. V., & García-Balboa, J. L. (2019). Thematic accuracy quality control by means of a set of multinomials. Applied Sciences, 9(20), 4240.
#' @importFrom R6 R6Class
#' @importFrom stats dmultinom
#' @examples
#' vectors<-list(c(47,4,0),c(40,5,3),c(45,6,2),c(48,0))
#' prob<-list(c(0.95,0.04,0.01),c(0.88,0.1,0.02),c(0.9,0.08,0.02),c(0.99,0.01))
#' A <- QCCS$new(vectors,prob)
#'
#' @aliases


QCCS <- R6Class("QCCS",
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param vectors vector list.
    #' @param p probabilities list.
    #'
    #' @concept


    vectors = NULL,
    prob = NULL,
  initialize = function(vectors, prob) {
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

  #Funcion QCCS publica. Para calcular p-valor se usan test-*tol

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
          }
          if(ni==3){
            p_value1<-private$test.3tol(vi,pi)$p.valor
          }
          p_value<-c(p_value,p_value1)
        }

      sol <- c(sol, p_value)
    return(list(p_value=sol))
    }

  ),

  private = list(
  nUsos = NULL,

      #test 3 tolerancias
    test.3tol = function(M, p){
      N <- sum(M)
    # Calcular el espacio muestral
      Sucesos <- NULL
      for (i in 0:N) {
        for (j in 0:N) {
          if (i + j < (N + 0.1)) {
            Sucesos <- rbind(Sucesos, c(i, j, N - i - j))
          }
        }
      }

      # Calcular la fd y la Fd
      Q <- matrix(ncol = 5, nrow = dim(Sucesos)[1])
      Q[, 1:3] <- Sucesos
        for (i in 1:dim(Sucesos)[1]) Q[i, 4] <- dmultinom(Sucesos[i, ], prob = p)
        Q[1, 5] <- Q[1, 4]
        for (i in 2:dim(Sucesos)[1]) Q[i, 5] <- Q[i, 4] + Q[(i - 1), 5]

      # Calcular el p-valor
      A <- matrix(nrow = 0, ncol = 5)
        for (j in 1:dim(Q)[1]) {
     if ( (Q[j, 1] < M[1]) | ((Q[j, 1] == M[1]) & (Q[j, 2] > M[2])) | ((Q[j, 1] == M[1]) & (Q[j, 2] == M[2]) & (Q[j, 3] > M[3]))) {
      A <- rbind(A, Q[j, ])
     }
        }

      p_valor <- A[nrow(A), 5]+dmultinom(M,prob=p)

      resultados <- list(A = A, p.valor = p_valor)
    return(resultados)
    },

      #test para 4 tolerancias
    test.4tol=function(M,p){
     #M es el vector muestral
     #p son las probabilidades bajo H0
     #se calcula el espacio muestral
      N<-sum(M)
      Sucesos <- NULL
       for (i in 0:N) {
        for (j in 0:N) {
         for(k in 0:N){
          if (i + j + k < (N + 0.1)) {
           Sucesos <- rbind(Sucesos, c(i, j, k, N - i - j - k))
          }
         }
        }
       }

       #Se calcula la fd y una Fd con el orden establecido y la prob real

       Q<-matrix(ncol=6, nrow=dim(Sucesos)[1])
       Q[,1:4]<-Sucesos
       for (i in 1:dim(Sucesos)[1]) Q[i,5]<-dmultinom(Sucesos[i,],prob=p)
       Q[1,6]<-Q[1,5]
       for(i in 2:dim(Sucesos)[1]) Q[i,6]<-Q[i,5]+Q[(i-1),6]

       #TOMO EL ESTAD?STICO MUESTRAL M Y CALCULO EL P-VALOR E imprimo los valores que rechazo

       A<-NULL
        for (j in 1:dim(Q)[1]){
         if((Q[j,1]<M[1])|((Q[j,1]==M[1])&(Q[j,2])>M[2])|((Q[j,1]==M[1])& (Q[j,2]==M[2])& (Q[j,3]>M[3])))
          {A<-rbind(A, Q[j,])}
          }
         #p_valor<-sum(A[,5])
         p_valor <- A[nrow(A), 6]+dmultinom(M,prob=p)

                    #Essor<-length(Al[Al[,5]<alfa,5])/M

         #Se devuelve la matriz de A vectores muestrales
      resultados<-list(A=A, p.valor=p_valor)
    return(resultados)
    },



      #TEST 2 TOLERANCIAS
      #Se usa la distribucion Binomial

    test.2tol=function(M, p){
      N<-sum(M)
      p<-pbinom(M,N,p)
      p_value<-p[1]
     return(p_value)
    }

 ),
 active = list(

 )
)
