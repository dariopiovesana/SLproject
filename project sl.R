dati<-read.table("dati.csv", header = T, sep=";", dec = ",")
dati<-dati[,-c(4,5)]

pMiss <- function(x){sum(is.na(x))/length(x)*100}
#Predittori per percentuale di NA
pMiss2 = apply(dati,2,pMiss)
hist(pMiss2, xlim = c(0,100), main="Percentuale NA per variabile")

#Osservazioni per percentuale di NA
pMiss1 = apply(dati,1,pMiss)
hist(pMiss1, xlim=c(0,100))

#Rimuovo le variabili con più del 10% di NA
dati_miss<-dati[,which(pMiss2<10)]

#BEST SUBSET SELECTION

#caso 1 variabile
vett1<-c()
new1<-c()
for (i in 2:72){
    r2<-summary(lm(dati_miss$X159~dati_miss[,i]))$adj.r.squared
    new1<-c(colnames(dati_miss)[i], r2)
    vett1<-c(vett1,new1)
  }
mat1<-matrix(vett1, length(vett1)/2, 2, byrow = T)
r2_1_var<-mat1[which.max(mat1[,2]),]

#caso 2 variabili
vett2<-c()
new2<-c()
for (i in 2:71){
  for (j in (i+1):72){
    r2<-summary(lm(dati_miss$X159~dati_miss[,i]+dati_miss[,j]))$adj.r.squared
    new2<-c(colnames(dati_miss)[i], colnames(dati_miss)[j], r2)
    vett2<-c(vett2,new2)
  }
}
mat2<-matrix(vett2, length(vett2)/3, 3, byrow = T)
r2_2_var<-mat2[which.max(mat2[,3]),]

predict(lm(X159~X157+X158, dati))

#caso 3 variabili
vett3<-c()
new3<-c()
for (i in 2:70){
  for (j in (i+1):71){
    for (z in (j+1):72){
    r2<-summary(lm(dati_miss$X159~dati_miss[,i]+dati_miss[,j]+dati_miss[,z]))$adj.r.squared
    new3<-c(colnames(dati_miss)[i], colnames(dati_miss)[j], colnames(dati_miss)[z], r2)
    vett3<-c(vett3,new3)
  }
  }
}
mat3<-matrix(vett3, length(vett3)/4, 4, byrow = T)
r2_3_var<-mat3[which.max(mat3[,4]),]

#caso 4 variabili
vett4<-c()
new4<-c()
for (i in 2:69){
  for (j in (i+1):70){
    for (z in (j+1):71){
      for (m in (z+1):72){
    r2<-summary(lm(dati_miss$X159~dati_miss[,i]+dati_miss[,j]+dati_miss[,z]+dati_miss[,m]))$adj.r.squared
    new4<-c(colnames(dati_miss)[i], colnames(dati_miss)[j], colnames(dati_miss)[z], colnames(dati_miss)[m], r2)
    vett4<-c(vett4,new4)
  }
  }
  }
}
mat4<-matrix(vett4, length(vett4)/5, 5, byrow = T)
r2_4_var<-mat4[which.max(mat4[,5]),]

#Salvo R^2 aggiustato del Best Subset con 3 e 4 variabili
write.table(mat3, "matrice_3.txt")
write.table(mat4, "matrice_4.txt")

#MSE di previsione del miglior modello
predict(lm(X159~X158+X157+X74+X53, dati_miss))
# i missing sono sempre gli stessi => rimuovo Belgio, UK, Svizzera e Norvegia
dati_miss<-dati_miss[-c(1, 19, 20, 21),]
pred<-c()
for (i in 1:17){
  m0<-lm(X159~X158+X157+X74+X53, dati_miss[-i,])
  prev<-predict(m0, dati_miss[i,])
  pred<-c(pred, prev)
}
MSE<-function(x,y) sum((x-y)^2)
MSE(pred, dati_miss$X159)

#cerco le variabili che creano un aumento in R^2 aggiustato rispetto a quello del miglior modello con 3 variabili
ordinate<-mat4[which(mat4[,5]>0.861 & mat4[,5]<1),] #variabili che apportano info: X21, X53, X54, X55, X68, X71, X74, X157, X158
dati_fin<-dati_miss[,c("X21", "X53", "X54", "X55", "X68", "X71", "X74", "X157", "X158", "X159")]

#BEST SUBSET SELECTION DELLE VARIABILI RIMASTE
require(leaps)
fit.bests <- regsubsets(X159~.,dati_fin, nvmax=9)
summary.best<-summary(fit.bests)$adjr2
#allora selezionerei il modello con 4 variabili che cambia rispetto al precedente: X21, X53, X157, X158, ma avendo un NA in X21 preferisco il precedente

#LOOCV LASSO E RIDGE CON LE 8 VARIABILI RIMASTE SENZA NA
require(glmnet)
K=17
X<-as.matrix(dati_fin[,-c(1,5,10)])
y<-dati_fin[, 10]
ridge.cv<-cv.glmnet(X,y,alpha=0, nfolds = K, grouped=FALSE)
lasso.cv<-cv.glmnet(X,y,alpha=1, nfolds = K, grouped=FALSE)
hatlambda.r <-ridge.cv$lambda.min
hatlambda.l <-lasso.cv$lambda.min
pred2<-c()
pred3<-c()
for (i in 1:17){
X.<-as.matrix(dati_fin[-i,-c(1,5,10)])
y.<-dati_fin[-i, 10]
X.star<-as.matrix(dati_fin[i,-c(1,5,10)])
ridge<-glmnet(X.,y.,alpha=0)
lasso<-glmnet(X.,y.,alpha=1)
prev2<-predict.glmnet(ridge, s=hatlambda.r, newx=X.star)
pred2<-c(pred2, prev2)
prev3<-predict.glmnet(lasso, s=hatlambda.l, newx=X.star)
pred3<-c(pred3, prev3)
}
MSE(pred2, y)
MSE(pred3, y)

#LOOCV LASSO E RIDGE CON LE MIGLIORI VARIABILI
require(glmnet)
K=17
X<-as.matrix(dati_fin[,c(2, 7, 8, 9)])
y<-dati_fin[, 10]
ridge.cv<-cv.glmnet(X,y,alpha=0, nfolds = K, grouped=FALSE)
lasso.cv<-cv.glmnet(X,y,alpha=1, nfolds = K, grouped=FALSE)
hatlambda.r <-ridge.cv$lambda.min
hatlambda.l <-lasso.cv$lambda.min
pred4<-c()
pred5<-c()
for (i in 1:17){
  X.<-as.matrix(dati_fin[-i,c(2, 7, 8, 9)])
  y.<-dati_fin[-i, 10]
  X.star<-as.matrix(dati_fin[i,c(2, 7, 8, 9)])
  ridge<-glmnet(X.,y.,alpha=0)
  lasso<-glmnet(X.,y.,alpha=1)
  prev4<-predict.glmnet(ridge, s=hatlambda.r, newx=X.star)
  pred4<-c(pred4, prev4)
  prev5<-predict.glmnet(lasso, s=hatlambda.l, newx=X.star)
  pred5<-c(pred5, prev5)
}
MSE(pred4, y)
MSE(pred5, y)

#KNN
library(caret)
loocv<-trainControl(method="loocv")
mod<-train(X159~X158+X157+X74+X53, dati_fin, method="knn", trainControl=loocv, metric="RMSE")
summary(mod)
pred.knn<-predict(mod, newdata = dati_fin)
MSE(pred.knn, y)

#Riadattamento Dataset
dati_fin$country<-dati[2:18,1]
dati_fin$country
dati_fin<-dati_fin[-12,]
dati_fin$Country<-c("BGR", "CZE", "DEU", "EST", "GRC", "FRA", "HRV", "ITA", "LVA", "LTU", "HUN", "AUT", "POL", "ROU", "SVN", "SVK")
dati_fin<-dati_fin[, -11]
colnames(dati_fin)<-c("Number of employees in 2016 Innovation core activities", "New or significantly improved products that were only new to the firm, Turnover of product innovative enterprises from new or significantly improved products", "New or significantly improved products that were new to the market, Turnover of product innovative enterprises from new or significantly improved products", "Unchanged or marginally modified products (of product innovators), Turnover of product innovative enterprises from new or significantly improved products", "Difficulties in obtaining public grants or subsidies, Innovative enterprises", "Lack of internal finance, Non-innovative enterprises by barrier against innovation activities", "Lack of qualified employees within enterprise, Non-innovative enterprises", "Job-to-job mobility of HRST", "Percentage of the ICT sector in GDP", "Percentage of the ICT personnel in total employment", "Country")
dati_fin<-write.csv(dati_fin, "dati_fin.csv")

#Interpretazione variabili
summary(lm(dati_fin[,10]~dati_fin[,2]+dati_fin[,7]+dati_fin[,8]+dati_fin[,9]))