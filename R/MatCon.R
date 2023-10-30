#' @title Confusion matrix
#' @description Using the confusion matrix, various indices are calculated.
#' @param values Confusion matrix
#' @param ID Identifier (optional)
#' @param Date System or user-provided date (optional)
#' @return Object of class MatCon #y mas
#' @export MatCon
#' @importFrom R6 R6Class
#' @examples
#' C = matrix( c(5, 0, 1, 0,4,0,0,0,3), nrow=3,  ncol=3)
#' mc2 <- MatCon$new (C,ID=5,Date="27-10-2023")


# Ahora con R6
MatCon <- R6Class("MatCon",
                             public = list(
                               #inicializa la matriz de confución. debe de añadirse una matriz
                               values = NULL,
                               #inicializa nombre
                               ID = NULL,
                               #inicializa rango
                               nk = NULL,
                               #inicializando Fecha
                               Date = NULL,
                               #inicializa suma filas
                               sumfil=NULL,
                               #suma columnas
                               sumcol=NULL,


                               #aqui añado todos los parametros

                               initialize = function(values,ID=NULL,Date=NULL) {
                                 #El usuario debe dar la matriz de confusion
                                 self$values<-values

                                 #Es opcional que identifique su matriz.
                                 #Si añade este valor pues se le da un ID personalizado a la MC
                                 #sino se le dará un número aleatorio que puede ir de 1 a 1000
                                 #ID="Paola" o ID=5
                                 if(!is.null(ID)){
                                   self$ID <- ID
                                 }else{self$ID<-sample(c(1:1000),1,replace=FALSE)}
                                 #Si no se añade fecha (Date=2710, Date="27-10", Date="27/10")
                                 #En ese caso se tomara la fecha del sistema
                                 if(!is.null(Date)){
                                   self$Date<-Date
                                 }else{self$Date <- Sys.Date()}

                                 #Valores para chequear la MC
                                 #rango de la matriz
                                 nk<-nrow(self$valores)
                                 nfilas <- nrow(self$values)
                                 ncolumnas <- ncol(self$values)
                                 #suma de los elementos de la filas
                                 self$sumfil<-apply(self$values,1,sum) #definido para algunas funciones
                                 #suma de los elementos de la columna
                                 self$sumcol<-apply(self$values,2,sum) #definido para algunas funciones

                                 #cat(paste0("filas ", nfilas, ".\n"))  PARA INFORME
                                 #cat(paste0("columnas ", ncolumnas, ".\n"))
                                 error1<- FALSE
                                 error2<- FALSE
                                 error3<- FALSE
                                 error4<- FALSE
                                 error5<- FALSE
                                 error6<- FALSE

                                 if((nfilas != ncolumnas)) {
                                   error1<- TRUE
                                   print("Error tipo 1: Matriz no cuadrada")#error matriz no cuadra
                                 }
                                 #Con la comprobacion del error1 ya se ha comprobado el error2
                             #    if((nfilas != nk)) {
                            #       error2<- TRUE #rg distinto a nfilas
                             #    }
                                 for (i in 1:nfilas){
                                   for (j in 1:ncolumnas){if(self$values[i,j]<0) {
                                     error3<-TRUE
                                     print("Error tipo 3: valores negativos")
                                     }
                                   } #error elementos menores que 0
                                 }
                                 if(sum(self$values)==0 ){
                                   error4<-TRUE
                                   print("Error tipo 4: Suma de elementos 0 ")
                                   #error si la suma de los valores de la matriz es 0
                                 }
                                 if(sum(apply(self$values,1,sum))==0 ){
                                   error5<-TRUE
                                   print("Error tipo 5: Suma filas 0")
                                   #suma filas es 0
                                 }

                                 if(sum(apply(self$values,2,sum))==0 ){
                                   error6<-TRUE
                                   print("Error tipo 5: Suma columnas 0")
                                   #suma columnas es 0
                                 }
                                 if ((error1 == TRUE) || (error2==TRUE) || (error3 == TRUE) || (error4 == TRUE) || (error5==TRUE) || (error6 == TRUE)) {
                                   #self$er=TRUE
                                   warning("Errores de tipo 1, 2, 3,4,5 o 6") #chequeo hecho
                                   stop()
                                 }
                               },



                               #' @description Overall accuracy for a particular classified image/map is then calculated by dividing the sum of the entries that form the major diagonal (i.e., the number of correct classifications) by the total number of samples taken.
                               #' @param ... (ignored).
                               #' @description
                               #' The mathematical expression is:
                               #'
                               #' \deqn{
                               #' oa = \frac{\sum_{i=1}^{n} x_{ii}}{\sum_{i, j=1}^{n} x_{ij}}
                               #' }
                               #'
                               #' Where:
                               #' \enumerate{
                               #'   \item `oa`: overall accuracy.
                               #'   \item `x_ii`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' This represents a mathematical expression with a fraction.
                               #'
                               #' @references Story, M., & Congalton, R. G. (1986). Accuracy assessment: a user’s perspective. Photogrammetric Engineering and remote sensing, 52(3), 397-399.
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$oa()

                               oa = function(...) {
                                 indice <- sum(diag(self$values))/sum(self$values)
                                 return(indice)
                               },

                               #' @description Determines whether a value is decimal or not.
                               #' @param ... (ignored).
                               #' @return TRUE or FALSE
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$dec()

                               dec = function(...){
                                 for (i in 1:length(self$values)){
                                   dec = if (round(self$values[i],0) == self$values[i]) {FALSE} else {TRUE}
                                 }
                                 return(dec)
                               },


                               #' @description  The accuracy from the point of view of a map user, not the map maker.
                               #' @description
                               #' The mathematical expression is:
                               #' \deqn{
                               #' ua=\frac{x_{ii}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `ua`: user accuracy.
                               #'   \item `x_ii`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param ... (ignored).
                               #' @return vector of values with the user's accuracy indexes of all classes
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A,ID=1,Date="30/10/2023")
                               #' p$ua()

                               ua = function(...){
                                 n <- sqrt(length(self$values))
                                 ua <- rep(0,n)

                                 for (i in 1:n){

                                   ua[i] <- self$values[i,i] / self$sumfil[i] #Ind Exactitud Usuario para todas las clases a la vez
                                 }

                                 return(ua)
                               },

                               #' @description The accuracy from the point of view of a map user, not the map maker.
                               #' @description
                               #'  \deqn{
                               #' ua_{i}=\frac{x_{ii}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `ua_i`: user accuracy.
                               #'   \item `x_ii`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param i User class to evaluate
                               #' @return Class i user accuracy index
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$uai(2)

                               uai=function(i){

                                 uai = self$values[i,i] / self$sumfil[i]

                                 return(uai)
                               },

                               #' @description  The map accuracy from the point of view of the map maker (the producer).
                               #' @description
                               #'  \deqn{
                               #' pa_{i}=\frac{x_{jj}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `pa_i`: producer accuracy.
                               #'   \item `x_jj`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param i Producer class to evaluate
                               #' @return Class i producer accuracy index
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$pai(1)

                               pai = function(i){

                                 pai = self$values[i,i] / self$sumcol[i]

                                 return(pai)
                               },


                               #' @description  The map accuracy from the point of view of the map maker (the producer).
                               #' @description
                               #'  \deqn{
                               #' pa=\frac{x_{jj}}{\sum_{j=1}^n x_{ij}}
                               #' }
                               #'
                               #' where:
                               #'
                               #' \enumerate{
                               #'   \item `pa`: producer accuracy.
                               #'   \item `x_jj`: diagonal element of the matrix.
                               #'   \item `x_ij`: element of the matrix.
                               #' }
                               #' @param ... (ignored).
                               #' @return Vector of values with the producer's accuracy indexes of all classes
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$pa()

                               pa = function (...){
                                 n <- sqrt(length(self$values))
                                 pa <- rep(0,n)
                                 for(i in 1:n){
                                   pa[i] <- self$values[i,i] / self$sumcol[i]
                                 }
                                 return(pa)
                               },

                            #' @description  Average of the accuracy from the point of view of a map user, not the map maker and the map accuracy from the point of view of the map maker (the producer).
                            #' @description
                            #'  \deqn{
                            #' aup=\frac{ua_i+pa_i}{2}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `aup`: Average of user's and producer's accuracy.
                            #'   \item `ua_i`: user accuracy
                            #'   \item `pa_i`: producer accuracy.
                            #' }
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$aup(2)

                            #Average of user's and producer's accuracy
                            aup=function(i){

                              aup = (self$uai(i) + self$pai(i))/2

                              return(aup)
                            },

                            #' @description  The Individual Classification Success Index (ICSI) applies to the classification effectiveness for one particular class of interest.
                            #' @description
                            #'  \deqn{
                            #' ICSI=ua_i+pa_i-1
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `ICSI`: Individual classification success index.
                            #'   \item `ua_i`: user accuracy.
                            #'   \item `pa_i`: producer accuracy.
                            #' }
                            #' @references Koukoulas, S., & Blackburn, G. A. (2001). Introducing new indices for accuracy evaluation of classified images representing semi-natural woodland environments. Photogrammetric Engineering and Remote Sensing, 67(4), 499-510.
                            #' @references Turk, G. (2002). Map evaluation and" chance correction". Photogrammetric Engineering and Remote Sensing, 68(2), 123-129.
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$ICSI(2)
                            ICSI = function(i){

                              ICSI = self$uai(i) + self$pai(i) - 1

                              return (ICSI)
                            },

                            #' @description  The probability that a randomly chosen point of a specific class on the map has a correspondence of the same class in the same position in the field and that a randomly chosen point in the field of the same class has a correspondence of the same class in the same position on the map.
                            #' @description
                            #'  \deqn{
                            #' mah=\frac{2}{\frac{1}{ua_i}+\frac{1}{pa_i}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `mah`: Hellden's mean accuracy.
                            #'   \item `ua_i`: user accuracy.
                            #'   \item `pa_i`: producer accuracy.
                            #' }
                            #' @param i Class to evaluate.
                            #' @references Helldén, U. (1980). A test of landsat-2 imagery and digital data for thematic mapping illustrated by an environmental study in northern Kenya, Lund University. Natural Geography Institute Report No. 47.
                            #' @references Rosenfield, G. H., & Fitzpatrick-Lins, K. (1986). A coefficient of agreement as a measure of thematic classification accuracy. Photogrammetric engineering and remote sensing, 52(2), 223-227.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$mah(2)

                            mah = function(i){
                            #esta condicion se daria cuando el elemento de la diagonal
                              #sea 0, ¿poco probable? se puede eliminar esta condicion?
                              if (self$uai(i) == 0 || self$pai(i) == 0) {
                                stop ("/ by 0")
                              }
                              else
                              {
                                mah = 2 / (1/self$uai(i) + 1/self$pai(i))
                              }

                              return(mah)
                            },

                            #' @description  Mapping accuracy for each class is stated as the number of correctly classified pixels (equal to the total in the correctly classified area) in terms of all pixels affected by its classification (equal to this total in the displayed area as well as the pixels involved in errors of commission and omission).
                            #' @description
                            #'  \deqn{
                            #' mas=\frac{x_{ii}}{\sum^n_{j=1} x_{\cdot j}+\sum^n_{i=1} x_{i \cdot }-x_{ii}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `mas`: Short's mapping accuracy
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #' }
                            #' @references Short, N. M. (1982). The Landsat tutorial workbook: Basics of satellite remote sensing (Vol. 1078). National Aeronautics and Space Administration, Scientific and Technical Information Branch.
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$mas(2)

                            #Short's mapping accuracy
                            #Reference: Short(1982), Rosenfield and Fitzpatrick-Lins(1986)
                            mas = function(i){
                            if (self$sumfil[i] + self$sumcol[i] - self$values[i,i] == 0) {
                                stop ("/ by 0")
                              }
                              else
                              {mas = self$values[i,i] / (self$sumfil[i] + self$sumcol[i] - self$values[i,i])
                              }

                              return(mas)
                            },

                            #' @description Conditional Kappa will identify the degree of agreement between the two raters for each possible category.
                            #' @description
                            #'  \deqn{
                            #' cku=\frac{ua_i-\frac{\sum^n_{i=1} x_{i \cdot }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}{1-\frac{\sum^n_{i=1} x_{i \cdot }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `cku`: Conditional kappa (user's).
                            #'   \item `ua_i`: user accuracy.
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #' }
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$cku(2)

                            cku = function(i){

                              if (1 - self$sumcol[i]/sum(self$values) == 0) {
                                stop ("/ by 0")
                              }
                              else
                              {cku = (self$uai(i) - self$sumcol[i]/sum(self$values)) / (1 - self$sumcol[i]/sum(self$values))
                              }

                              return(cku)
                            },

                            #' @description Conditional Kappa will identify the degree of agreement between the two raters for each possible category.
                            #' @description
                            #'  \deqn{
                            #' ckp=\frac{pa_i-\frac{\sum^n_{j=1} x_{ \cdot j }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}{1-\frac{\sum^n_{j=1} x_{\cdot j }}{\sum^n_{i=1}\sum^n_{j=1} x_{ij}}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `ckp`: Conditional kappa (producer's).
                            #'   \item `pa_i`: producer accuracy.
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #' }
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$ckp(2)

                            #Conditional kappa (producer's)
                            ckp = function(i){

                              if (1 - self$sumfil[i]/sum(self$values) == 0) {
                                stop ("/ by 0")
                              }
                              else
                              {ckp = (self$pai(i) - self$sumfil[i]/sum(self$values)) / (1 - self$sumfil[i]/sum(self$values))
                              }

                              return(ckp)
                            },

                            #' @description Modified kappa index for the user
                            #' @description
                            #'  \deqn{
                            #' mcku=\frac{ua_i-\frac{1}{\sqrt{card(p)}}}{1-\frac{1}{\sqrt{card(p)}}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `mcku`: Modified conditional kappa (user's).
                            #'   \item `ua_i`: user accuracy.
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #' }
                            #' @param i Class to evaluate.
                            #' @references Stehman, S. V. (1997). Selecting and interpreting measures of thematic classification accuracy. Remote sensing of Environment, 62(1), 77-89.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$mcku(2)

                            #Modified conditional kappa (user's)
                            #Reference: Stehman(1997)
                            mcku = function(i){

                               #ESTA CONDICION SOLO SE VERIFICA SI LA MATRIZ TIENE
                               #UN ELEMENTO.. normalmento las mmatcon tienen más de un elemento
                              #eliminamos esta condicion?

                              if (1 - 1/sqrt(length(self$valores)) == 0) {
                                stop ("/ by 0")
                              }
                              else
                              {
                                mcku = (self$uai(i) - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
                              }

                              return(mcku)
                            },

                            #' @description Modified kappa index for the producer
                            #' @param i Class to evaluate.
                            #' @description
                            #'  \deqn{
                            #' mckp=\frac{pa_i-\frac{1}{\sqrt{card(p)}}}{1-\frac{1}{\sqrt{card(p)}}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `mckp`: Modified conditional kappa (producer's).
                            #'   \item `pa_i`: producer accuracy.
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #' }
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$mckp(2)

                            mckp = function(i){

                              #ESTA CONDICION SOLO SE VERIFICA SI LA MATRIZ TIENE
                              #UN ELEMENTO.. COMPENSA TENERLA?
                              if (1 - 1/sqrt(length(self$values)) == 0) {
                                stop ("/ by 0")
                              }
                              else
                              {mckp = (self$pai(i) - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
                              }

                              return(mckp)
                            },

                            #' @description Relative entropy is a quantity that measures the difference between two maps.
                            #' @references Finn, J. T. (1993). Use of the average mutual information index in evaluating classification error and consistency. International Journal of Geographical Information Science, 7(4), 349-366.
                            #' @description
                            #'  \deqn{
                            #' H(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' H(A|b_i)=-\sum^n_{j=1}( (\frac{ x_{ij}}{\sum^n_{j=1} x_{\cdot j} }) \cdot \log (\frac{x_{ij}}{\sum^n_{j=1} x_{\cdot j}}) ) \\
                            #' ecnu= \frac{H(A)-H(A|b_i)}{H(A)}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `ecnu`: Relative change of entropy given a category on map.
                            #'   \item `H(A)`: the entropy of the map.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `H(A|b_i)`: Entropy of map A knowing that the location corresponding to map B is in class b_i.
                            #' }
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$ecnu(2)

                            ecnu = function(i){
                                #log10 ->Hartleys (unidades de entropia)
                                #log2->bits
                                #LN->nats
                                HA = - sum ((self$sumcol/sum(self$values)) * (log10(self$sumcol/sum(self$values))))
                                HAbi = - sum ((self$valores[i,] / self$sumfil[i]) * log10(self$valores[i,] / self$sumfil[i]))

                              if (HA == 0){
                                stop("/by 0")
                              }
                              else {
                                ecnu = (HA - HAbi) / HA
                              }

                              return(ecnu)
                            },

                            #' @description Relative entropy is a quantity that measures the difference between two ground truthing.
                            #' @param i Class to evaluate
                            #' @description
                            #'  \deqn{
                            #' H(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' H(B|a_j)=-\sum^n_{j=1}( (\frac{ x_{ij}}{\sum^n_{i=1} x_{i \cdot} }) \cdot \log (\frac{x_{ij}}{\sum^n_{i=1} x_{i \cdot}}) ) \\
                            #' ecnp= \frac{H(B)-H(B|a_j)}{H(B)}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `ecnp`: Relative change of entropy given a category on ground truthing.
                            #'   \item `H(B)`: the entropy of the map.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `H(B|a_j)`: Entropy of map B knowing that the location corresponding to map A is in class a_j.
                            #' }
                            #' @examples
                            #' A<-matrix(c(36,5,4,7,2,2,6,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$ecnp(2)
                            #Relative change of entropy given a category on ground truthing
                            #Reference: Finn(1993)
                            ecnp = function(i){

                                HB = - sum ((self$sumfil/sum(self$values)) * (log10(self$sumfil/sum(self$values))))
                                HBaj = - sum ((self$values[,i] / self$sumcol[i]) * log10(self$values[,i] / self$sumcol[i]))

                              if (HB == 0){
                                stop("/by 0")
                              }
                              else {
                                ecnp = (HB - HBaj) / HB
                              }

                              return(ecnp)
                            },

                            #' @description The average accuracy is an average of the accuracy of individual categories. Because the individual categories can be the user's or the producer's accuracy, it can be computed in both ways accordingly.
                            #' @references Tung, F., & LeDrew, E. (1988). The determination of optimal threshold levels for change detection using various accuracy indexes. Photogrammetric Engineering and Remote Sensing, 54(10), 1449-1454.
                            #' @description
                            #'  \deqn{
                            #' aau=\frac{1}{\sqrt{card(p)}} \sum^n_{i=1} \frac{x_{ii}}{\sum_{j=1}^n x_{ij}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `aau`: Average accuracy from user's perspective
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #' }
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$aau()

                            #Average accuracy from user's perspective
                            #Reference: Fung and LeDrew (1988)
                            aau = function(){
                              for (i in 1:length(self$sumfil)) {
                                if (self$sumfil[i] == 0) {
                                  stop ("/ by 0")
                                }
                              }
                              aau = 1/sqrt(length(self$values)) * sum (diag(self$values)/self$sumfil)

                              return (aau)
                            },

                            #' @references Fienberg, S. E. (1970). An iterative procedure for estimation in contingency tables. The Annals of Mathematical Statistics, 41(3), 907-917.
                            #' @param n Iteration
                            #' @return Normalized matrix (Class MatCon)
                            #' @description An iterative process is carried out where each element is divided by the total of the sum of its row, thus obtaining new values. In the next iteration, all the elements are added by columns and each element is divided by the total of its column and they obtain new values, and so on.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$Normalize()
                            #' p$Normalize(1000)

                            Normalize=function(n=NULL){
                              #Da la opcion de añadir un numero concreto de iteraciones
                              #o si no se da un numero concreto, se realizan 100 iteraciones
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
                                  #nose si esto puede hacer que cambie el valores de la matriz inicial
                                }
                              }

                              NormMatrix<-MatCon$new(x1,ID=sample(1:30,1,replace = TRUE))
                              return(NormMatrix)
                            },

                            #' @description The average accuracy is an average of the accuracy of individual categories. Because the individual categories can be the user's or the producer's accuracy, it can be computed in both ways accordingly.
                            #' @description
                            #'  \deqn{
                            #' aap=\frac{1}{\sqrt{card(p)}} \sum^n_{i=1} \frac{x_{ii}}{\sum_{j=1}^n x_{ji}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `aap`: Average accuracy from producer's perspective
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #' }
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$aap()

                            #Average accuracy from producer's perspective
                            #Reference: Fung and LeDrew (1988)
                            aap = function(){

                              aap = 1/sqrt(length(self$values)) * sum (diag(self$values)/self$sumcol)

                              return(aap)
                            }




                             ),

                             private = list(
                               nUsos = NULL
                             ),
                             active = list(

                             )

)

