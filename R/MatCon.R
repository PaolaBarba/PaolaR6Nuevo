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
                                 nk<-nrow(self$values)
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

                                 if((nk==1)){
                                   error2<-TRUE
                                   print("Error tipo 2: Matriz de un solo elemento")
                                   #La matriz debe de tener más de un elemento

                                 }

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
                               #' @return Overall accuracy and variance.
                               #' @references Story, M., & Congalton, R. G. (1986). Accuracy assessment: a user’s perspective. Photogrammetric Engineering and remote sensing, 52(3), 397-399.
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$oa()

                               oa = function(...) {
                                 indice <- sum(diag(self$values))/sum(self$values)
                                 VarIndic<-abs((indice*(1-indice))/sum(self$values))
                                 return(list(oa=indice,Var=VarIndic))
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
                               #' @return vector of values with the user's accuracy indexes of all classes and their variances.
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A,ID=1,Date="30/10/2023")
                               #' p$ua()

                               ua = function(...){
                                 n <- sqrt(length(self$values))
                                 ua <- rep(0,n)
                                 VarUa<-rep(0,n)

                                 for (i in 1:n){

                                   ua[i] <- self$values[i,i] / self$sumfil[i] #Ind Exactitud Usuario para todas las clases a la vez
                                   VarUa[i]<-abs((ua[i]*(1-ua[i]))/self$sumfil[i]) #es sumfil porque son las mediciones que estoy analizando.
                                 }

                                 return(list(ua=ua,Var=VarUa))
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
                               #' @return Class i user accuracy index and their variance.
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$uai(2)

                               uai=function(i){

                                 uai = self$values[i,i] / self$sumfil[i]
                                 VarUai=abs((uai*(1-uai))/self$sumfil[i]) #o suma fila?????
                                 return(list(ua=uai,Var=VarUai))
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
                               #' @return Class i producer accuracy index and their variance.
                               #' @examples
                               #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                               #' p<-MatCon$new(A)
                               #' p$pai(1)

                               pai = function(i){

                                 pai = self$values[i,i] / self$sumcol[i]
                                 VarPai=abs((pai*(1-pai))/self$sumcol[i])

                                 return(list(pa=pai,Var=VarPai))
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
                                 VarPai<-rep(0,n)
                                 for(i in 1:n){
                                   pa[i] <- self$values[i,i] / self$sumcol[i]
                                   VarPai[i]<-abs((pa[i]*(1-pa[i]))/self$sumcol[i])
                                 }
                                 return(list(pa=pa,Var=VarPai))
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
                            #' @return Average of user's and producer's accuracy and its variance.
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$aup(2)

                            #Average of user's and producer's accuracy
                            aup=function(i){

                              aup = (self$uai(i)[[1]] + self$pai(i)[[1]])/2
                              VarAup=abs((aup*(1-aup))/(self$sumcol[i]+self$sumfil[i]))

                              return(list(aup=aup,Var=VarAup))
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
                            #' @return Individual Classification Success Index and its variance.
                            #' @references Koukoulas, S., & Blackburn, G. A. (2001). Introducing new indices for accuracy evaluation of classified images representing semi-natural woodland environments. Photogrammetric Engineering and Remote Sensing, 67(4), 499-510.
                            #' @references Turk, G. (2002). Map evaluation and" chance correction". Photogrammetric Engineering and Remote Sensing, 68(2), 123-129.
                            #' @param i Class to evaluate.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$ICSI(2)
                            ICSI = function(i){

                              ICSI = self$uai(i)[[1]] + self$pai(i)[[1]] - 1
                              VarICSI=abs((ICSI*(1-ICSI))/(self$sumcol[i]+self$sumfil[i]))

                              return (list(ICSI=ICSI,Var=VarICSI))
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
                              if (self$uai(i)[[1]] == 0 || self$pai(i)[[1]] == 0) {
                                stop ("/ by 0")
                              }
                              else
                              {
                                mah = 2 / (1/self$uai(i)[[1]] + 1/self$pai(i)[[1]])
                                VarMah=abs((mah*(1-mah))/(self$sumcol[i]+self$sumfil[i]))
                              }

                              return(list(mah=mah,Var=VarMah))
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
                            #' @return Short's mapping accuracy and its variance.
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
                              VarMas=abs((mas*(1-mas))/(self$sumcol[i]+self$sumfil[i])) #creo que es la suma todos los elementos
                              }

                              return(list(mas=mas,Var=VarMas))
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
                            #' @return Conditional kappa (user's) and its variance.
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
                              {cku = (self$uai(i)[[1]] - self$sumcol[i]/sum(self$values)) / (1 - self$sumcol[i]/sum(self$values))
                              VarCku=abs((cku*(1-cku))/self$sumfil[i])
                              }

                              return(list(cku=cku,Var=VarCku))
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
                            #' @return Conditional kappa (producer's) and its variance.
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
                              {ckp = (self$pai(i)[[1]] - self$sumfil[i]/sum(self$values)) / (1 - self$sumfil[i]/sum(self$values))
                              VarCkp=abs((ckp*(1-ckp))/self$sumcol[i])
                              }

                              return(list(ckp=ckp,Var=VarCkp))
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
                            #' @return Modified conditional kappa (user's) and its variance.
                            #' @param i Class to evaluate.
                            #' @references Stehman, S. V. (1997). Selecting and interpreting measures of thematic classification accuracy. Remote sensing of Environment, 62(1), 77-89.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$mcku(2)

                            #Modified conditional kappa (user's)
                            #Reference: Stehman(1997)
                            mcku = function(i){

                               mcku = (self$uai(i)[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
                               VarMcku=abs((mcku*(1-mcku))/self$sumfil[i])

                              return(list(mcku=mcku,Var=VarMcku))
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
                            #' @return Modified conditional kappa (producer's) and variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$mckp(2)

                            mckp = function(i){

                              mckp = (self$pai(i)[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
                              VarMckp=abs((mckp*(1-mckp))/self$sumcol[i])

                              return(list(mckp=mckp,Var=VarMckp))
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
                            #' @return Relative change of entropy given a category on map and its variance.
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
                                HAbi = - sum ((self$values[i,] / self$sumfil[i]) * log10(self$values[i,] / self$sumfil[i]))
                      #puede ser HA=0?
                              if (HA == 0){
                                stop("/by 0")
                              }
                              else {
                                ecnu = (HA - HAbi) / HA
                                VarEcnu=abs((ecnu*(1-ecnu))/sum(self$values)) #no estoy muy segura, pero intervienen todos los valores de la matriz.
                              }

                              return(list(ecnu=ecnu,Var=VarEcnu))
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
                            #' @return Relative change of entropy given a category on ground truthing and its variance.
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
                                VarEcnp=abs((ecnp*(1-ecnp))/sum(self$values))
                              }

                              return(list(ecnp=ecnp,Var=VarEcnp))
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
                            #'   \item `aau`: Average accuracy from user's perspective.
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #' }
                            #' @return Average accuracy from user's perspective and its variance.
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
                              VarAau=abs((aau*(1-aau))/sum(self$values)) #sum de todos los sumfil..self$values intervienen

                              return (list(aau=aau,Var=VarAau))
                            },

                            #' @references Fienberg, S. E. (1970). An iterative procedure for estimation in contingency tables. The Annals of Mathematical Statistics, 41(3), 907-917.
                            #' @param n Iteration
                            #' @return Normalized matrix (Class MatCon) and its variance.
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
                              VarNorm<-matrix()

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
                              #no estoy nada segura de esta forma de sacar la varianza aqui

                              #for (i in 1:rg) {
                               # for (j in 1:rg) {
                                #  VarNorm[i,j]=(x1[i,j]*(1-x1[i,j])/sum(self$values))
                                #}

                              #}

                              NormMatrix<-MatCon$new(x1,ID=sample(1:3000,1,replace = TRUE))
                              return(list(Norm=NormMatrix))
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
                            #'   \item `aap`: Average accuracy from producer's perspective.
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #' }
                            #' @return Average accuracy from producer's perspective and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$aap()

                            #Average accuracy from producer's perspective
                            #Reference: Fung and LeDrew (1988)
                            aap = function(){

                              aap = 1/sqrt(length(self$values)) * sum (diag(self$values)/self$sumcol)
                              VarAap=abs((aap*(1-aap))/sum(self$values))

                              return(list(aap=aap,Var=VarAap))
                            },

                            #' @description It is the average of the average accuracy from user's and producer's perspective.
                            #' @references Liu, C., Frazier, P., & Kumar, L. (2007). Comparative assessment of the measures of thematic classification accuracy. Remote sensing of environment, 107(4), 606-616.
                            #' @description
                            #'  \deqn{
                            #' daup=\frac{aau+aap}{2}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `daup`: Double average of user's and producer's perspective.
                            #'   \item `aau`: Average accuracy from user's perspective.
                            #'   \item `aap`: Average accuracy from producer's perspective.
                            #' }
                            #' @return Double average of user's and producer's perspective and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$daup()

                            #Double average of user's and producer's perspective
                            daup = function(){

                              daup = (self$aau()[[1]] + self$aap()[[1]]) / 2
                              VarDaup=abs((daup*(1-daup))/sum(self$values))

                              return(list(daup=daup,Var=VarDaup))
                            },


                            #' @description The Classification Success Index (CSI) applies to all classes and gives an overall estimation of classification effectiveness.
                            #' @description
                            #'  \deqn{
                            #' CSI=aau+aap-1
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `CSI`: Classification succes index.
                            #'   \item `aau`: Average accuracy from user's perspective.
                            #'   \item `aap`: Average accuracy from producer's perspective.
                            #' }
                            #' @return Classification sucess index and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$CSI()

                            #Classification succes index
                            #Reference: Koukoulas and Blackburn(2001)
                            CSI = function(){

                              CSI = self$aau()[[1]] + self$aap()[[1]] - 1
                              VarCSI=abs((CSI*(1-CSI))/sum(self$values))

                              return(list(CSI=CSI,Var=VarCSI))
                            },


                            #' @description This function provides the average value of the Hellden mean precision index
                            #' @description
                            #'  \deqn{
                            #' amah=\frac{1}{\sqrt{card(p)}}\sum^n_{i=1} \frac{2}{\frac{1}{ua_i}+\frac{1}{pa_i}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `amah`: Average of Hellden's mean accuracy index.
                            #'   \item `ua_i`: user accuracy.
                            #'   \item `pa_i`: producer accuracy.
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #' }
                            #' @return Average of Hellden's mean accuracy index and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$amah()

                            #Average of Hellden's mean accuracy index
                            amah = function(){

                              amah = 1/sqrt(length(self$values)) * sum ((2*diag(self$values)) / (self$sumfil + self$sumcol))
                              VarAmah=abs((amah*(1-amah))/sum(self$values))

                              return(list(amah=amah,Var=VarAmah))
                            },


                            #' @description This function provides the average of Short's mapping accuracy index.
                            #' @description
                            #'  \deqn{
                            #' amas=\frac{1}{\sqrt{card(p)}}\frac{\frac{\sum^n_{i=1} x_{ii}}{\sum^n_{i,j=1}x_{ij}}}{\sum^n_{j=1} x_{\cdot j}+\sum^n_{i=1} x_{i \cdot }-x_{ii}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `amas`: Average of Short's mapping accuracy index.
                            #'   \item `x_ii`: diagonal element of the matrix.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #' }
                            #' @return Average of Short's mapping accuracy index and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$amas()

                            #Average of Short's mapping accuracy index
                            amas = function(){

                              amas = 1/sqrt(length(self$values)) * sum (diag(self$values) / (self$sumfil + self$sumcol - diag(self$values)))
                              sum1=self$sumfil+self$sumcol- diag(self$values)
                              VarAmas=abs((amas*(1-amas))/sum(self$values))
                              for (i in 1:length(sum1)) {
                                if (sum1[i] == 0) {
                                  stop ("/ by 0")
                                }
                              }
                              return(list(amas=amas,Var=VarAmas))
                            },


                            #' @description The combined accuracy is the average of the overall accuracy and average accuracy.
                            #' @description
                            #'  \deqn{
                            #' cau=\frac{oa+aau}{2}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `cau`: Combined accuracy from user's perspective.
                            #'   \item `oa`: Overall accuracy.
                            #'   \item `aau`: Average accuracy from user's perspective.
                            #' }
                            #' @return Combined accuracy from user's perspective and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$cau()

                            #Combined accuracy from user's perspective
                            #Reference: Fung and LeDrew (1988)
                            cau = function(){
                              cau = (self$oa()[[1]] + self$aau()[[1]]) / 2
                              VarCau=abs((cau*(1-cau))/sum(self$values))
                              #esta bien la varianza así? o deberia de ser cada una por un lado?(que ya esta calculada)

                              return(list(cau=cau,Var=VarCau))
                            },

                            #' @description The combined accuracy is the average of the overall accuracy and average accuracy.
                            #' @description
                            #'  \deqn{
                            #' cap=\frac{oa+aap}{2}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `cap`: Combined accuracy from producer's perspective.
                            #'   \item `oa`: overall accuracy.
                            #'   \item `aap`: Average accuracy from producer's perspective.
                            #' }
                            #' @return Combined accuracy from producer's perspective and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$cap()

                            #Combined accuracy from producer's perspective
                            #Reference: Fung and LeDrew (1988)
                            cap = function(){

                              cap = (self$oa()[[1]] + self$aap()[[1]]) / 2
                              VarCap=abs((cap*(1-cap))/sum(self$values))

                              return(list(cap=cap,Var=VarCap))
                            },


                            #' @description The combined accuracy is the average of the overall accuracy and average accuracy.
                            #' @description
                            #'  \deqn{
                            #' caup=\frac{oa+amah}{2}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `caup`: Combined accuracy from both user's and producer's perspectives.
                            #'   \item `oa`: Overall accuracy.
                            #'   \item `amah`: Average of Hellden's mean accuracy index.
                            #' }
                            #' @return Combined accuracy from both user's and producer's perspectives and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$caup()

                            #Combined accuracy from both user's and producer's perspectives
                            caup = function(){

                              caup= ( self$oa()[[1]] + self$amah()[[1]] ) / 2
                              VarCaup=abs((caup*(1-caup))/sum(self$values))

                              return(list(caup=caup,Var=VarCaup))
                            },


                            #' @description It measures the relationship of beyond chance agreement to expected disagreement.
                            #' @description
                            #'  \deqn{
                            #'  ea=\sum^n_{i=1} (\frac{x _{\cdot i}}{\sum_{j=1}^n x_{ij}} \cdot \frac{x _{i \cdot}}{\sum_{j=1}^n x_{ij}}) \\
                            #' KappaValue=\frac{oa-ea}{1-ea}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `KappaValue`: Kappa coefficient.
                            #'   \item `oa`: Overall accuracy.
                            #'   \item `ea`: Expected accuracy of agreement if agreement were purely random.
                            #' }
                            #' @return Kappa coefficient and its variance.
                            #' @references Cohen, J. (1960). A coefficient of agreement for nominal scales. Educational and psychological measurement, 20(1), 37-46.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$KappaValue()

                            #KappaValue->nombre cambiado, ya existia base::kappa
                            #Reference: Cohen(1960), Rosenfield and Fitzpatrick-Lins(1986)
                            KappaValue = function(){

                              ea = (sum (self$sumfil * self$sumcol))/sum(self$values)^2

                              if (1-ea == 0){
                                stop ("/ by 0")
                              }
                              else{
                                kappa = (self$oa()[[1]]- ea) / (1 - ea)
                                VarKappa=abs((kappa*(1-kappa))/sum(self$values))
                              }

                              return(list(Kappa=kappa,Var=VarKappa))
                            },


                            #' @description It is the proportion of agreement after chance agreement is removed from consideration.
                            #' @description
                            #'  \deqn{
                            #' mkp=\frac{oa-\frac{1}{\sqrt{card(p)}}}{1-\frac{1}{\sqrt{card(p)}}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `mkp`: Modified kappa
                            #'   \item `oa`: Overall accuracy.
                            #'   \item `card(p)`: number of elements of the matrix, cardinal of the matrix.
                            #' }
                            #' @return Modified kappa and its variance.
                            #' @examples
                            #' A<-matrix(c(36,1,0,0,2,0,0,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$mkp()

                            #Modified kappa
                            #Reference: Aickin(1990)
                            mkp = function(){

                              mkp = (self$oa()[[1]] - 1/sqrt(length(self$values))) / (1 - 1/sqrt(length(self$values)))
                              VarMkp=abs((mkp*(1-mkp))/sum(self$values))

                              return(list(mkp=mkp,Var=VarMkp))
                            },

                            #' @description Average mutual information (AMI), is applied to the comparison of thematic maps.
                            #' @description
                            #'  \deqn{
                            #' ami=\sum^n_{i,j=1} (\frac{x_{ij}}{\sum^n_{i,j=1} x_{ij}} \cdot \log_{10} (\frac{x_{ij}}{\frac{\sum^n_{i=1} x_{i \cdot} \cdot \sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij}}}))
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `ami`: Average mutual information.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #' }
                            #' @return Average mutual information and its variance.
                            #' @examples
                            #' A<-matrix(c(36,5,4,7,2,2,6,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$ami()

                            #Average mutual information
                            #Reference: Finn(1993)
                            ami = function(){

                              for (i in 1:length(self$sumcol)) {

                                for(j in 1:length(self$sumcol)){
                                  #si un elemento es 0 para
                                  if(self$values[i,j]==0){
                                    stop(" by 0")
                                  }

                                }
                              }

                              ami = sum ((self$values/sum(self$values)) * log10(self$values / ((self$sumfil * self$sumcol)/sum(self$values))))
                              VarAmi=abs((ami*(1-ami))/sum(self$values))

                              return(list(ami=ami,Var=VarAmi))
                            },


                            #' @description
                            #'  \deqn{
                            #' H(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' nmiu=\frac{ami}{H(B)}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `nmiu`: Normalized mutual information using the entropy on map.
                            #'   \item `H(B)`: The entropy of the map.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `ami`: Average mutual information.
                            #' }
                            #' @return Normalized mutual information using the entropy on map and its variance.
                            #' @examples
                            #' A<-matrix(c(36,5,4,7,2,2,6,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$nmiu()

                            #Normalized mutual information using the entropy on map
                            #Reference: Finn(1993)
                            nmiu = function(){

                            HB = - sum ((self$sumfil/sum(self$values)) * (log10(self$sumfil/sum(self$values))))

                              if(HB == 0){
                                stop("/ by 0")
                              }

                              nmiu = self$ami()[[1]]/HB
                              VarNmiu=abs((nmiu*(1-nmiu))/sum(self$values))

                              return(list(nmiu=nmiu,Var=VarNmiu))
                            },

                            #' @description
                            #'  \deqn{
                            #' H(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' nmip=\frac{ami}{H(A)}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `nmip`: Normalized mutual information using the entropy on ground truthing.
                            #'   \item `H(A)`: the entropy of the map.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `ami`: Average mutual information.
                            #' }
                            #' @return Normalized mutual information using the entropy on ground truthing and its variance.
                            #' @examples
                            #' A<-matrix(c(36,5,4,7,2,2,6,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$nmip()


                            #Normalized mutual information using the entropy on ground truthing
                            #Reference: Finn(1993)
                            nmip = function(){


                              HA = - sum ((self$sumcol/sum(self$values)) * (log10(self$sumcol/sum(self$values))))


                              if (HA == 0){
                                stop ("/ by 0")
                              }
                              else{
                                nmip = self$ami()[[1]]/HA
                                VarNmip=abs((nmip*(1-nmip))/sum(self$values))
                              }

                              return(list(nmip=nmip,Var=VarNmip))
                            },

                            #' @description
                            #'  \deqn{
                            #' H(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' H(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' nmiam=\frac{2ami}{HA+HB}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `nmiam`: Normalized mutual information using the arithmetic mean of the entropies on map and on ground truthing.
                            #'   \item `H(A)`: the entropy of the map.
                            #'   \item `H(B)`: The entropy of the map.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `ami`: Average mutual information.
                            #' }
                            #' @return Normalized mutual information using the arithmetic mean of the entropies on map and on ground truthing and its variance.
                            #' @examples
                            #' A<-matrix(c(36,5,4,7,2,2,6,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$nmiam()

                            #Normalized mutual information using the arithmetic mean of the entropies on map and on ground truthing
                            #Reference: Strehl and Ghosh(2002)
                            nmiam = function(){

                                HB = - sum ((self$sumfil/sum(self$values)) * (log10(self$sumfil/sum(self$values))))
                                HA = - sum ((self$sumcol/sum(self$values)) * (log10(self$sumcol/sum(self$values))))


                              if (HA + HB == 0) {
                                stop ("/ by 0")
                              }
                              else{
                                nmiam = 2 * self$ami()[[1]] / (HA + HB)
                                VarNmiam= abs((nmiam*(1-nmiam))/sum(self$values))
                              }

                              return(list(nmiam=nmiam,Var=VarNmiam))
                            },

                            #' @description
                            #'  \deqn{
                            #' H(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' H(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' nmigm=\frac{ami}{\sqrt{H(A) \cdot H(B)}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `nmigm`: Normalized mutual information using the geometric mean of the entropies on map and on ground truthing.
                            #'   \item `H(A)`: the entropy of the map.
                            #'   \item `H(B)`: The entropy of the map.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `ami`: Average mutual information.
                            #' }
                            #' @return Normalized mutual information using the geometric mean of the entropies on map and on ground truthing and its variance.
                            #' @references Ghosh, J., Strehl, A., & Merugu, S. (2002, November). A consensus framework for integrating distributed clusterings under limited knowledge sharing. In Proc. NSF Workshop on Next Generation Data Mining (pp. 99-108).
                            #' @examples
                            #' A<-matrix(c(36,5,4,7,2,2,6,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$nmigm()


                            #Normalized mutual information using the geometric mean of the entropies on map and on ground truthing
                            #Reference: Ghosh et al.(2002)
                            nmigm = function(){

                              HB = - sum ((self$sumfil/sum(self$values)) * (log10(self$sumfil/sum(self$values))))
                              HA = - sum ((self$sumcol/sum(self$values)) * (log10(self$sumcol/sum(self$values))))


                              if (HA * HB == 0) {
                                stop ("/ by 0")
                              }
                              else{

                                nmigm = self$ami()[[1]] / sqrt(HA * HB)
                                VarNmigm=abs((nmigm*(1-nmigm)))

                              }

                              return(list(nmigm=nmigm,Var=VarNmigm))
                            },

                            #' @description
                            #'  \deqn{
                            #' H(A)=-\sum^n_{j=1}( (\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log(\frac{\sum^n_{i=1} x_{i \cdot}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' H(B)=-\sum^n_{i=1}( (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) \cdot \log (\frac{\sum^n_{j=1} x_{\cdot j}}{\sum^n_{i,j=1} x_{ij} }) ) \\
                            #' nmimx=\frac{2 ami}{max(H(A))+max(H(B))}=\frac{ami}{\log \sqrt{card(p)}}
                            #' }
                            #'
                            #' where:
                            #'
                            #' \enumerate{
                            #'   \item `nmimx`: Normalized mutual information using the arithmetic mean of the maximum entropies on map and on ground truthing
                            #'   \item `H(A)`: the entropy of the map.
                            #'   \item `H(B)`: The entropy of the map.
                            #'   \item `x_.j`: sum with respect to j (rows).
                            #'   \item `x_i.`: sum with respect to i (columns).
                            #'   \item `ami`: Average mutual information.
                            #' }
                            #' @return Normalized mutual information using the arithmetic mean of the maximum entropies on map and on ground truthing and its variance.
                            #' @references Strehl, A. (2002). Relationship-based clustering and cluster ensembles for high-dimensional data mining. The University of Texas at Austin.
                            #' @examples
                            #' A<-matrix(c(36,5,4,7,2,2,6,1,20),nrow=3,ncol=3)
                            #' p<-MatCon$new(A)
                            #' p$nmimx()

                            #Normalized mutual information using the arithmetic mean of the maximum entropies on map and on ground truthing
                            #Reference: Strehl(2002)
                            nmimx = function(){
                              nmimx = self$ami()[[1]] / log10 (sqrt(length(self$values)))
                              VarNmimx=abs((nmimx*(1-nmimx))/sum(self$values))

                              return (list(nmimx=nmimx,Var=VarNmimx))
                            },

                      #' @description  Small values are calculated for empty cells of the matrix. All non-empty cells of the matrix change their values. This function will not be applied if all the elements of the matrix are different from 0.
                      #' @references Muñoz, J. M. S. (2016). Análisis de Calidad Cartográfica mediante el estudio de la Matriz de Confusión. Pensamiento matemático, 6(2), 9-26.
                      #' @return An object of class MatCon with the matrix of Pseudoceros.
                      #' @examples
                      #' A <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                      #' p<-MatCon$new(A)
                      #' p$MPseudozeroes()



                      MPseudozeroes = function(){

                        # Modifica la matriz a?adiendo Pseudoceros
                        # OJO: Esto no se debe hacer si todas las celdas tienen alg?n contenido
                        #      Este caso se deber?a controlar

                        #vemos si algun elemento es 0
                        k=0
                        rg<-nrow(self$values)
                        for (i in 1:rg) {
                            for (j in 1:rg) {
                              if((self$values[i,j]!=0)==TRUE){
                                k=k+1
                                if(k==length(self$values)){
                                stop("La Matriz de Pseudoceros elimina los ceros de la matriz. Su matriz no tiene ningun cero que eliminar.")
                                }}
                              }
                            }


                        MERROR=self$values


                          SumaMatriz <-sum(MERROR)
                          SumaColumnas <- apply (MERROR, 1, sum)
                          SumaFilas <- apply (MERROR, 2, sum)

                          MLandas <- (SumaFilas %*% t(SumaColumnas))/(SumaMatriz*SumaMatriz)
                          K <- (SumaMatriz*SumaMatriz - sum(MERROR*MERROR))/sum((SumaMatriz*MLandas - MERROR )^2)
                          MPseudoceros <- (SumaMatriz/(K+SumaMatriz))*(MERROR + K*MLandas)
                          #Que devuelva la matriz o un objeto de clase MAtCon?
                          MPseudoceros1<-MatCon$new(MPseudoceros,ID=sample(1:3000,1,replace = TRUE))
                          return(MPseudoceros1)

                        },

                      #' @description  Cell values are typified. The total sum of the original matrix is used for the typification. Resulting values can be presented as real (parameter RaR=1) or as percentage (parameter RaR !=1)
                      #' @description
                      #'  \deqn{
                      #' MTypify=\frac{x_{ij}}{\sum^n_{i,j=1} x_{ij}}
                      #' }
                      #'
                      #' where:
                      #'
                      #' \enumerate{
                      #'   \item `MTyipify`: typified matrix.
                      #'   \item `x_ij`: matrix element.
                      #'   \item `sum{x_ij}`: sum of all the elements of the matrix.
                      #' }
                      #' @param RaR "1" indicates result as real, other values mean percentage as integer. By default RaR=1
                      #' @return Typified matrix
                      #' @examples
                      #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                      #' p<-MatCon$new(x)
                      #' p$MTypify(RaR=5)

                      #Cell Values of a matrix are typified
                      MTypify =function(RaR=NULL){
                        if(!is.null(RaR)){
                          RaR <- RaR
                        }else{R<-1}
                        # Crea una matriz en la que todos los elementos son proporciones
                        # tal que la suma de todos los elementos es 1
                        MERROR=self$values

                          MatrizSalida <- MERROR/(sum(MERROR))
                          if (RaR==1){
                            MatrizSalida <- MERROR/(sum(MERROR))
                            return(MatrizSalida)
                          } else {
                            MatrizSalida <- MERROR/(sum(MERROR))
                            MatrizSalida[] <- as.integer(100*MatrizSalida)
                            return(MatrizSalida)
                          }

                      },

                      #' @description  Several parameters are calculated for the given Confusion Matrix. The.
                      #' @return Confusion Matrix, Dimension, Total sum of cell values, Overall Accuracy, Variance overall accuracy, Kappa index of global accuracy, Simplified variance of the global Kappa, per-clas producer's accuracy, per-class user's accuracy, k value for the calculation of pseudozeroes, Pseudozeroes Matrix, L matrix for the calculation of pseudozeroes.
                      #' @examples
                      #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                      #' p<-MatCon$new(x)
                      #' p$MAllParameters()

                      #Several global and per-class parameters are calculated
                      MAllParameters=function(){
                        # ******************************************
                        # Calculo par?metros generales sobre  la matriz de error
                        # Estos par?metros son:
                        # PA = Porcentaje de acuerdo
                        # Kappa = Indice Kappa
                        # Exactitud de usuario
                        # Exactitud de productor
                        # Varianzas
                        # Matriz de pseudoceros

                        oa<-self$oa()
                          dimension <- nrow(self$values)
                          SumaMatriz <-sum(self$values)
                          PAcuerdo <- oa[[1]]
                          ExProdu <- self$pa()[[1]]
                          ExUsuario <-self$ua()[[1]]
                          PAAzar <- sum((self$sumfil*self$sumcol))/(SumaMatriz*SumaMatriz)
                          Kappa <- self$KappaValue()[[1]]
                          VarPAcuerdo <- PAcuerdo *(1-PAcuerdo)/SumaMatriz
                          VarKappa <-  VarPAcuerdo / ((1-PAAzar)*(1-PAAzar))
                          MLandas <- (self$sumfil %*% t(self$sumcol))/(SumaMatriz*SumaMatriz)
                          K <- (SumaMatriz*SumaMatriz - sum(self$values*self$values))/sum((SumaMatriz*MLandas - self$values)^2)
                          MPseudoceros <- (SumaMatriz/(K+SumaMatriz))*(self$values + K*MLandas)

                          salida<-list(Matrix=self$values, Dimension =dimension, n=SumaMatriz, OAccuracy=PAcuerdo, VarOAccuracy=VarPAcuerdo, Kappa=Kappa,VarKappa=VarKappa,ExPro=ExProdu,ExUsu=ExUsuario,Kpseudo=K, MPseudoceros=MPseudoceros,MLandas=MLandas)
                          return(salida)

                      },

                    #' @description  User's and producer's accuracies and standard deviations are computed
                    #' @return A list with the producer's accuracy, its standard deviation, the user's accuracy, and its standard deviation
                    #' @examples
                    #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                    #' p<-MatCon$new(x)
                    #' p$CAccuracies()

                    #User's and producer's accuracies and standard deviations are computed
                    CAccuracies =function(){

                      #  calculation of Class accuracies and standard deviations


                        nc <- nrow(self$values)

                        for (i in 1:nc){
                          pcpa <- self$pa()[[1]]
                          pcua <-self$ua()[[1]]
                          pcpasd <- sqrt(self$pa()[[2]])
                          pcuasd <- sqrt(self$ua()[[2]])

                        }
                        return(list(PrAcc=pcpa,PrAccSDeviation=pcpasd,UAcc= pcua, UAccSDeviation=pcuasd))

                    },


                    #' @description  User's and producer's weighted accuracies and standard deviations are computed.
                    #' @param MP matrix of weights
                    #' @return Matrix formed with its original elements and their corresponding weights, general accuracy of the weight matrix obtained, accuracy of the producer and user and their standard deviations,
                    #' @examples
                    #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                    #' p<-MatCon$new(x)
                    #' MP<- t(matrix(c(1,0,0.67,1,0,1,0,0,1,0,1,1,0.91,0,0.61,1), nrow = 4, ncol=4))
                    #' p$CAccuraciesW(MP)

                    #User's and producer's weighted accuracies and standard deviations are computed
                    CAccuraciesW =function(MP){
                      #  Calculation of weighted Class accuracies
                      #  The error matrix and the weight matrix are required
                      #  The weights for diagonal cells are 1
                      #  The values of the weights for the off-diagonal cells  must be in the range [0,1]
                      #MV<-self$values


                        # UnWeighted marginals (quantities)
                        ncol <- self$sumcol
                        nrow<- self$sumfil

                        # In %
                        MV<- self$values/sum(self$values)

                        # Weighted matrix
                        WMERROR<-MV*MP

                        # Weighted OA
                        WOA <- sum(diag(WMERROR))/sum(WMERROR)

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
                          wi_[i]    <- sum(p_j*MP[i,])
                          w_j[i]    <- sum(pi_*MP[,i])
                          wpcua[i]  <- sum(WMERROR[i,])/sum(MV[i,])
                          wpcpa[i]  <- sum(WMERROR[,i])/sum(MV[,i])
                          pcpasd[i] <- sqrt(wpcpa[i]*(1-wpcpa[i])/ncol[i])
                          pcuasd[i] <- sqrt(wpcua[i]*(1-wpcua[i])/nrow[i])
                        }

                        return(list(WMERROR=WMERROR, WOA=WOA, WPrAcc=wpcpa,WPrAccSDeviation=pcpasd,WUAcc= wpcua, WUAccSDeviation=pcuasd))

                    },


                    #' @description  Overall Kappa agreement index and variance elements are computed
                    #' @return Overall accuracy, Expected accuracy of agreement if agreement were purely random,,, coefficient kappa, standar desviation kappa,
                    #' @examples
                    #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                    #' p$DetailedKappa()

                    #Overall Kappa agreement index and its variance are computed
                    DetailedKappa=function (){
                        nc <- nrow(self$values)
                        SumaMatriz <-sum(self$values)
                        # UnWeighted marginals (quantities)

                        ##########NO ENTIENDO MUY BIEN QUE HACE ESTA FUNCION.
                        ####### QUE SON O3,O4
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

                        K <- (O1-O2)/t2 #KappaValue

                        t3<- 2*O1*O2-O3
                        t4<- O4-4*(O2^2)
                        t5<- O1*t1/(t2^2)+2*t1*t3/(t2^3)+(t1^2)*t4/(t2^4)
                        SdK <- sqrt((1/SumaMatriz)*t5)
                        CV <- SdK/K
                        #NO SE QUE SON O3,O4,CV,SdK,t3,t4,t5

                        return(list(O1=O1, O2=O2, O3=O3,O4=O4, K=K, SdK=SdK, CV=CV))
                    },

                    #' @description Class Kappa agreement index (conditional Kappa) and its variance are computed
                    #' @examples
                    #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                    #' p$DetailedCKappa ()

                    #Class  Kappa agreement index and variance elements  are computed
                    DetailedCKappa = function(){

                        SumaMatriz <-sum(self$values)

                        # In %
                        #MERROR<- self$values/sum(self$values)
                        MERROR<-self$values

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

                        return(list(Ki_=Ki_, Ki_Sd=Ki_sd, K_j=K_j, K_jsd=K_jsd))

                        },

                    #' @description  Overall Kappa agreement index (Weighted) and its variance are computed
                    #' @param MW  matrix of weights
                    #' @examples
                    #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                    #' MW<- t(matrix(c(1,0,0.67,1,0,1,0,0,1,0,1,1,0.91,0,0.61,1), nrow = 4, ncol=4))
                    #' p<-MatCon$new(x)
                    #' p$DetailedWKappa(MW)

                    # Overall Kappa agreement index (Weighted) and variance elements  are computed
                    DetailedWKappa = function(MW){

                        nc <- nrow(self$values)
                        SumaMatriz <-sum(self$values)

                        # In %
                        MERROR<- self$values/SumaMatriz

                        # UnWeighted marginals (prob)
                        pcol <- apply(MERROR,2,sum)
                        prow<- apply(MERROR,1,sum)

                        # Weighted matrix
                        WMERROR<-MERROR*MP

                        # The 4 coefficients
                        Ow1 <- sum(MW*MERROR)
                        Ow2 <- sum(t(MW*prow)*pcol)
                        c1<- (1-Ow1)
                        c2<- (1-Ow2)
                        wi_ <- MW %*% pcol
                        w_j <- MP %*% prow
                        mintermedia1<- matrix(rep(wi_, nc), nrow =nc, ncol=nc, byrow=FALSE)
                        mintermedia2<- matrix(rep(w_j, nc), nrow =nc, ncol=nc, byrow=TRUE)
                        mintermedia3 <-(mintermedia1+mintermedia2)*c1
                        mintermedia4 <- (MW*c2-mintermedia3)^2
                        Ow4 <- sum(MERROR*mintermedia4)

                        K <- (Ow1-Ow2)/c2
                        SdK <- sqrt((Ow4-(Ow1*Ow2-2*Ow2+Ow1)^2)/(SumaMatriz*(c2^4)))
                        CV <- SdK/K

                        return(list(Ow1=Ow1, Ow2=Ow2, Ow4=Ow4, K=K, SdK=SdK, CV=CV))

                      },

                    #' @description  Overall Tau agreement index and variance elements  are computed
                    #' @param VP vector of proportions (as matrix)
                    #' @examples
                    #' x <- t(matrix(c(35, 14,11,1,4,11,3,0,12,9,38,4,2,5,12,2), nrow = 4, ncol=4))
                    #' VP <-matrix(c(0.4, 0.1, 0.4, 0.1), ncol=4)
                    #' p<-MatCon$new(x)
                    #' p$DetailedTau(VP)

                    #Overall Tau agreement index and variance elements  are computed
                    DetailedTau = function(VP){

                        nc <- nrow(self$values)
                        SumaMatriz <-sum(self$values)

                        # In %
                        MERROR<- self$values/SumaMatriz

                        # UnWeighted marginals (prob)
                        pcol <- apply(MERROR,2,sum)
                        prow<- apply(MERROR,1,sum)


                        O1 <- sum(diag(MERROR)  )
                        O2 <- sum(VP*pcol)
                        O3 <- sum(diag(MERROR)*(VP+pcol))

                        mintermedia1<- matrix(rep(pcol, nc), nrow =nc, ncol=nc, byrow=FALSE)
                        mintermedia2<- matrix(rep(VP, nc), nrow =nc, ncol=nc, byrow=TRUE)
                        mintermedia3 <-(mintermedia1+mintermedia2)^2

                        O4 <- sum(MERROR*mintermedia3)

                        t1<- (1-O1)
                        t2<- (1-O2)
                        t3<- O1*t1/(t2^2)
                        t4<- 2*t1*(2*O1*O2-O3)/(t2^3)
                        t5<- (t1^2)*(O4-4*O2^2)/(t2^4)
                        Tau <- (O1-O2)/t2
                        SdT <- sqrt((t3+t4+t5)/SumaMatriz)
                        CV<- SdT/Tau

                        return(list(O1=O1, O2=O2, O3=O3,O4=O4, Tau=Tau, SdT=SdT, CV=CV))
                    },

                    #' @description  Quantity, Exchange and Shift values are computed
                    #' @param TI time interval (default value = 1)
                    #' @param SF Scale factor for results (default value = 1)
                    #' @examples
                    #' x <- t(matrix(c(0,1,1,2,0,0,0,2,0), nrow =3, ncol=3))
                    #' p<-MatCon$new(x)
                    #' p$QES(TI=1, SF=6)


                    # Quantity, Exchange and Shift values are computed
                    QES = function(TI=1, SF=1){
                      # Overall Quantity, Exchange and Shift values
                      # TI Time interval
                      # SF Scale factor
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
                            if (i>j){#diagonal 0?
                              ee[j,i] <-  (min(self$values[i,j], self$values[j,i]))*2
                            }else{
                              ee[j,i] <- 0
                            }
                          }
                        }

                        for (j in 1:nc){
                          d[j]<-d[j]+ sum(self$values[,j])+ sum(self$values[j,])-2*self$values[j,j]
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

                        return(list(ODifference=D, OQuantity=Q, OExchange=E, OShift=S, d=d, q=q, e=e, s=s))
                    }




                             ),

                             private = list(
                               nUsos = NULL
                             ),
                             active = list(

                             )

)

