#Inflacion ----
library(tidyverse)
library(dplyr)
library(readxl)
library(psych)
library(readxl)
INFLA_24 <- read_excel("C:/Investigador/iCloudDrive/La Salle/Investigación/Inflacion/INFLA_24.xlsx")
head(INFLA_24)                         
Infla<-INFLA_24
summary(Infla)  
#Mexico ----
Mexico<-filter(Infla,Country=="Mexico")
View(Mexico)
summary(Mexico$Avg_Income)
summary(Mexico$age)
hist(Mexico$Avg_Income, xlab = "Ingreso", ylab = "Frecuencia", main="Histograma")
#calculos del ingreso por genero en Mexico
Hombres<-filter(Mexico,gender==2)
summary(Hombres$Avg_Income)
summary(Hombres$age)
Mujeres<-filter(Mexico,gender==1)
summary(Mujeres$Avg_Income)
summary(Mujeres$age)
t.test(Hombres$Avg_Income,Mujeres$Avg_Income)
Hombres%>%count(wpsgpipy)
Mujeres%>%count(wpsgpipy)
Mexico%>%count(gender)
Mexico%>%count(wpsgpipy)
Mexico%>%count(Poultry_Fisch)
Mexico%>%count(Fruist_Veg)
Mexico%>%count(Meat_milk_P)
Mexico%>%count(N_alcohol)
Mexico%>%count(Alcohol_B)
Mexico%>%count(Tobacco)
Mexico%>%count(Elec_App)
Mexico%>%count(Cloth_Foot)
Mexico%>%count(Gasoline)
Mexico%>%count(Electricity)
Mexico%>%count(Gas)
Mexico%>%count(Water)
Mexico%>%count(Garbage_recy)
Mexico%>%count(Housing)
Mexico%>%count(mainten)
Mexico%>%count(Education)
Mexico%>%count(Trasnp)
Mexico%>%count(Communication)
Mexico%>%count(Health)
Mexico%>%count(Taxes)
Mexico%>%count(Changes_Inc)
Mexico%>%count(Exp_Chang_Inc)
Mexico%>%count(savings)
Mexico%>%count(relation_Status)
Mexico%>%count(Dwelling_Cond)
Mexico%>%count(level_Educa)
xtabs(~debts,data=Mexico)
xtabs(~debts,data=Hombres)
table(Debts=Mexico$debts,Gender=Mexico$gender)
table(Debts=Mexico$debts,Inflacion=Mexico$Changes_Inc)
table(Debts=Mexico$debts,Inflacion=Mexico$Changes_Inc)
table(Rubro=Mexico$wpsgpipy,Inflacion=Mexico$Changes_Inc)
table(Gender=Mexico$gender,Inflacion=Mexico$Changes_Inc)
table(Gender=Mexico$gender,Inflacion=Mexico$Housing)
table(Gender=Mexico$gender,Inflacion=Mexico$Gasoline)
fit<-lm(Avg_Income~age+Changes_Inc,data=Mexico)
summary(fit)
hist(log(Mexico$Avg_Income))
Mexico01<-Mexico %>%
  dplyr::select(age,Avg_Income,Changes_Inc) %>%
  group_by(age)
pairs.panels(Mexico01,main="México")
summary(Mexico01)
ggplot(data=Mexico)+
  geom_histogram(mapping = aes(x=Changes_Inc),binwidth = 0.5)
ggplot(data = Mexico,aes(x=age,y=Avg_Income))+geom_point()+geom_smooth()
ggplot(data = Mexico,aes(x=age,y=wpsgpipy))+geom_point()+geom_smooth()
ggplot(data = Mexico,aes(x=gender,y=Avg_Income,color=gender, size=debts))+geom_point()
joven<-filter(Mexico,age>=18,age<=30)
joven
str(joven)
summary(joven$Avg_Income)
table(Debts=joven$debts,Gender=joven$gender)
joven%>%count(wpsgpipy)
viejo<-filter(Mexico,age>=31)
str(viejo)
viejo%>%count(wpsgpipy)
summary(viejo$Avg_Income)

# ACP inflacion ----
library(readr)
Precios_24 <-  read_csv("C:/Investigador/iCloudDrive/La Salle/Investigación/Inflacion/Precios_24.csv")
View(Precios_24)
library(textshape)
Precios_24<-Precios_24[!duplicated(Precios_24$Concepto),]
Precios_24<-textshape::column_to_rownames(Precios_24,loc=1)
Pre<-as.data.frame(Precios_24)
Pre<-subset(Pre,select= -c(MICH_M, MICH_H,Cat))
str(Pre)
is.na(Pre)
#aNALISIS DE COMPONENTES PRINCIPALES
library(stats)
respca<-prcomp(Pre,scale=TRUE)
names(respca)
head(respca$rotation)[,1:6]# para ver que componente pesa mas
dim(respca$rotation)#me dice el numero de componentes
respca$sdev#desviaciones tipicas
respca$sdev^2#varianza
summary(respca)#me arroja la sd, proporcion de la varianza explicada y el accumulado
#pesos de los componentes; creamos un vector XX
xx<-respca$x
xx<-as.data.frame(xx)
#le pido que me cree el comp 1 y l comp2 y que me lo pase a mi base de datos
Precios_24$PC1<-xx$PC1
Precios_24$PC2<-xx$PC2
Precios_24cor<-subset(Precios_24,select = -c(Cat))
head(Precios_24cor)
cor(Precios_24cor)
#el PC1 correlaciona muy alto con GTO_M y GTO_H, fuerza, Durabilidad, etc.
#es un componente que resume muy bien todos los datos
#el componente dos CP2 solo es important en las dos primeras variables

#Probbamos otra posibilidad
respca1<-princomp(Pre,cor=TRUE)
names(respca1)
respca1$sdev
summary(respca1)
library(FactoMineR)
#los resultados son iguales
#probamos otro metoo PCA. Ya Se que son 6 componentes
respca2<-PCA(X=Pre,scale.unit = FALSE,ncp = 6,graph = TRUE)
print(respca2)
#les solicitamos los valores propios
head(respca2$eig)

#usamos otra libreria
library(factoextra)
get_pca(respca2)#inf sobre las variables
get_pca_var(respca2)#informacion sobre las variables
get_pca_ind(c)#informacion sobre las observaciones

#visualizacion
fviz_eig(respca2)
fviz_screeplot(respca2)
fviz_pca_ind(respca2)# expresion de las obseraciones sobre los compnentes
fviz_pca_ind(respca2,
             col.ind = "cos2",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=FALSE)
fviz_pca_ind(respca2,
             col.ind = "cos2",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)
#entre mas alejado del vertice mas importante
fviz_pca_var(respca2)
fviz_pca_var(respca2,
             col.var = "contrib",
             gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
             repel=TRUE)

fviz_pca_var(respca2,col.var = "cos2",
             geom.var = "arrow",
             labelsize=2,
             repel = FALSE)
fviz_pca_contrib(respca2,choice = "var")#variables
fviz_pca_contrib(respca2,choice = "ind")#individuos-heroes
biplot(x=respca1,scale = 0,cex=0.6,col=c("blue4","brown"))

biplot(x=respca1,scale = 0,cex=0.6,repel= TRUE,col=c("blue4","brown"))


# analisis cluster Inflacion ----
packages<-c("tibble","tidyverse","cluster","factoextra","NbClust")
ipak(packages)
library(factoextra)
library(NbClust)
Pre_1<-na.omit(Pre)
#ahora me pregunto cuantos clusters voy a tener
fviz_nbclust(Pre_1,kmeans,method="wss")#nos sugiere dos
fviz_nbclust(Pre_1,kmeans,method="silhouette")#nos suguiere tres
fviz_nbclust(Pre_1,kmeans,method="gap_stat")#tres
k3<-kmeans(Pre_1,centers = 3,nstart = 25)
#asi tengo donde quedarian clasificados cada uno de los
#heroes que existen
k3
str(k3)
#trabajamos con el de tres
fviz_cluster(k3,data = Pre_1)
#tenemos los 1)mediocres, 2)los buenos y 3)los supermalos
#ahora probemos distintas formas de representarlos
fviz_cluster(k3,data = Pre_1,ellipse.type = "euclid",repel = FALSE,star.plot=TRUE)
fviz_cluster(k3,data = Pre_1,ellipse.type = "norm")
fviz_cluster(k3,data = Pre_1,ellipse.type = "norm",palette="Set2",ggtheme = theme_minimal())           
#ordenamos toda la información por numero de clusters
Pre_1$clus<-as.factor(k3$cluster)
Pre_1

Pre_1$clus<-factor(Pre_1$clus)
#usamos la funcion gather para pasar a un formato data long
#le pido que considere todos los valores desde la columna de
#Inteligencia hasta combat, para que me los categorice por valor
data_long<-gather(Pre_1,caracteristica,valor,GTO_M:PUE_H,factor_key = TRUE)
data_long
#ahora lo grafico en ggplot
ggplot(data_long,aes(as.factor(x=caracteristica),y=valor,group=clus,colour=clus))+
  stat_summary(fun=mean,geom="pointrange",size=1)+
  stat_summary(geom="line")

