#Ejercicio shiny   #iris

library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)
library(formattable)
library(gridExtra)
library(ggExtra)
library(plotly)
library(rgeos)
library(sp)
library(shiny)
library(grid)

#cargamos los datos
data(iris)
#Para la clase ya los tenemos cargados

head(iris)
summary(iris)
str(iris)


df<-iris
df<-melt(df,id.vars = "Species",measure.vars=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width" ))
df<-dcast(df,Species~variable,mean)
formattable(df,list(area(col=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))~color_bar("red"))
)


df<-iris
df<-melt(df,id.vars = "Species",measure.vars=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width" ))
df<-dcast(df,Species~variable,sd)

for(i in 2:5){
  df[,i]<-signif(df[,i],2)
}


formattable(df,list(area(col=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))~color_bar("pink"))
)

test<-function(df) df[chull(df$Sepal.Length,df$Sepal.Width),]
iris2<-plyr::ddply(iris,"Species",test)
ggplot(iris,aes(Sepal.Length,Sepal.Width))+
  geom_point(data=iris,aes(color=Species))+
  geom_polygon(data=iris2,alpha=.3,aes(Sepal.Length,Sepal.Width,fill=Species))+
  theme(legend.position = "bottom",plot.title = element_text(size = 15,hjust = 0.5))+
  annotate("segment",x=6,xend=5.8,y=3.75,yend =4 ,arrow=arrow(),color="black")+
  annotate("segment",x=6.2,xend=6.2,y=3.65,yend =3.4 ,arrow=arrow(),color="black")+
  annotate("segment",x=6.1,xend=6,y=3.65,yend =3.4 ,arrow=arrow(),color="black")+
  annotate("text",x=6.21,y=3.72,label="marginal points",color="black",size=3)




#my_iris_function<-function(xaxis,yaxis,xnumber,ynumber)
  
#point<-iris(x=xnumber,y=ynumber)
  
#subset the data by its' Species levels  
Iris_setosa<-subset(iris,Species=="setosa")
Iris_versicolor<-subset(iris,Species=="versicolor")
Iris_virginica<-subset(iris,Species=="virginica")

###Es aqui donde ya no entiendo que pasa###


#create the function that only select the marginal points    
test<-function(df) df[chull(df[,xaxis],df[,yaxis]),]

#only select points which are marginal points by each Species levels   
iris2<-plyr::ddply(iris,"Species",test)

cname<-c(xaxis,yaxis,"Species")

#only select the row which is marginal points by Species==Iris-setosa and subset with cname
iris_species1<-plyr::ddply(Iris_setosa,"Species",test)[,cname]
#these two are the same as above 
iris_species2<-plyr::ddply(Iris_versicolor,"Species",test)[,cname]
iris_species3<-plyr::ddply(Iris_virginica,"Species",test)[,cname]

#subset the data with special Species levels with cname    
Iris_setosa<-Iris_setosa[,cname]
Iris_versicolor<-Iris_versicolor[,cname]
Iris_virginica<-Iris_virginica[,cname]

#deal with the point and region(polygon)    
pointSp <- SpatialPoints(point, proj4string = CRS("+proj=longlat"))

spe1_spatP <- gConvexHull(SpatialPoints(iris_species1[,1:2],
                                        proj4string = CRS("+proj=longlat")))
spe2_spatP <- gConvexHull(SpatialPoints(iris_species2[,1:2],
                                        proj4string = CRS("+proj=longlat")))
spe3_spatP <- gConvexHull(SpatialPoints(iris_species3[,1:2],
                                        proj4string = CRS("+proj=longlat")))

#detect if the point is in the region? if yes, return TRUE   
spe1_T_or_F<-gContains(spe1_spatP, pointSp)
spe2_T_or_F<-gContains(spe2_spatP, pointSp)
spe3_T_or_F<-gContains(spe3_spatP, pointSp)

#draw the ggplot    
graph<-ggplot(iris,aes(iris[,xaxis],iris[,yaxis]))+
  geom_point(data=iris,aes(color=Species))+
  
  #draw the polygon with marginal points    
  geom_polygon(data=iris2,alpha=.3,aes(iris2[,xaxis],iris2[,yaxis],fill=Species))+
  geom_point(data=point,aes (x=point[,1],y=point[,2]),shape=8,size=3)+
  theme(legend.position = "bottom",plot.title = element_text(size = 15,hjust = 0.5))


#if else for plot logic  
if((spe1_T_or_F==T)|any(iris_species1[,1]%in%point[,1]&iris_species1[,2]%in%point[,2])==T){
  graph+labs(x=xaxis,y=yaxis,title="You got an Iris-setosa !")
  
}else if((spe2_T_or_F==T)&(spe3_T_or_F==T)&
         (any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])&
          any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2]))|
         
         ((spe2_T_or_F==T)&(spe3_T_or_F==F)&
          ((any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])&
            any(iris_species3[,1]%in%point[,1]&iris_species3[,2]%in%point[,2]))==T))|
         
         ((spe2_T_or_F==F)&(spe3_T_or_F==T)&
          ((any(iris_species2[,1]%in%point[,1]&iris_species2[,2]%in%point[,2])&
            any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2]))==T))|
         
         (((spe2_T_or_F==F)&(spe3_T_or_F==F)&
           ((any(iris_species2[,1]%in%point[,1]&iris_species2[,2]%in%point[,2])&
             any(iris_species3[,1]%in%point[,1]&iris_species3[,2]%in%point[,2]))==T)))
){
  graph+labs(x=xaxis,y=yaxis,title="Maybe Iris-versicolor or Iris-virginica... \nUmm..., I don't sure ")  
  
  
}else if((spe2_T_or_F==T)&(spe3_T_or_F==T)&
         ((any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])&
           any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2]))==F)&
         (any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])==F)&
         (any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2])==F)
){
  graph+labs(x=xaxis,y=yaxis,title="Without sufficient info, \nI cannot tell you which the species the plant is")  
  
}else if((spe2_T_or_F==T)&(spe3_T_or_F==T)&
         (any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])==T)&
         (any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2])==F)){
  graph+labs(x=xaxis,y=yaxis,title="Oh! It seem you found an Iris-versicolor !")  
  
}else if((spe2_T_or_F==T)&(spe3_T_or_F==T)&
         (any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])==F)&
         (any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2]))
){
  graph+labs(x=xaxis,y=yaxis,title="My favorite flower, Iris-virginica !")  
  
  
}else if((spe2_T_or_F==T)&
         (any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])==T)|
         (spe3_T_or_F==T)&
         (any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])==T)
){
  graph+labs(x=xaxis,y=yaxis,title="Hey! where do you found it, \nthis Iris-versicolor")  
  
}else if((spe2_T_or_F==T)&
         (any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2])==T)|
         (spe3_T_or_F==T)&
         (any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2])==T)
){
  graph+labs(x=xaxis,y=yaxis,title="Beauty color, Iris-virginica !")  
  
}else if ((spe2_T_or_F==T)){
  graph+labs(x=xaxis,y=yaxis,title="Oh! this is an Iris-versicolor !") 
  
}else if((spe3_T_or_F==T)){
  graph+labs(x=xaxis,y=yaxis,title="Can you share an Iris-versicolor with me ?")
  
}else if((any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])==T)){
  graph+labs(x=xaxis,y=yaxis,title="Oh! this is a Iris-virginica !")
  
}else if((any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2])==T)){
  graph+labs(x=xaxis,y=yaxis,title="My favorite flower, Iris-virginica !")
  
}else if((spe2_T_or_F==F)&(spe3_T_or_F==F)&
         ((any(Iris_versicolor[,1]%in%point[,1]&Iris_versicolor[,2]%in%point[,2])&
           (any(Iris_virginica[,1]%in%point[,1]&Iris_virginica[,2]%in%point[,2])))==F)){
  graph+labs(x=xaxis,y=yaxis,title="Oh oh, \nthis could be not belong to iris species")
}
}

```
####Example 1
```{r}
my_iris_function("Sepal.Length","PetalWidthCm",5,1.75)
```

####Example2
```{r}
my_iris_function("Petal.Length","Sepal.Width",4.8,2.8)
```




