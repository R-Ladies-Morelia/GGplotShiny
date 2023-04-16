####R ladies
####Graficos
####Modificado de yu yang liu
####Maribel Arenas Navarro

library(ggplot2)
library(dplyr)
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




#Vemos los datos en una tabla con el paquete y la funcion formattable y le ponemos colores
#Los marcos de datos formateables son marcos de datos que se representan como una tabla HTML
#con funciones de formateador aplicadas, lo que se asemeja al formato condicional en Excel.

formattable(iris,list(
  area(col = c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))~color_bar("pink"),
  Species=color_tile("transparent", "darkorchid1")
))%>%
  as.datatable()

#media, el valor mas alto es el mas largo de la barra y viceversa

df<-iris
df<-melt(df,id.vars = "Species",measure.vars=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width" ))
df<-dcast(df,Species~variable,mean)
formattable(df,list(area(col=c(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))
                    ~color_bar("red")))


##Graficos de ggplot
#Separamos las especies

Iris_setosa<-subset(iris,Species=="setosa")
summary(Iris_setosa)
Iris_versicolor<-subset(iris,Species=="versicolor")
Iris_virginica<-subset(iris,Species=="virginica")



#por partes


ggplot(Iris_setosa,aes(Sepal.Length,Sepal.Width))+
  geom_point()+
  geom_smooth(method = 'loess',lty=5)+
  geom_vline(xintercept = mean(Iris_setosa$Sepal.Length),lty=6,color="darkorchid1",lwd=1,alpha=.5)+
  geom_hline(yintercept = mean(Iris_setosa$Sepal.Width),lty=6,color="deeppink",lwd=1,alpha=.5)+
  labs(x="Sepal Length (cm)",y="Sepal Width (cm)")


ggplot(Iris_setosa,aes(Sepal.Length,Sepal.Width))+
  geom_point()+
  geom_smooth(method = 'loess',lty=5)+
  geom_vline(xintercept = mean(Iris_setosa$Sepal.Length),lty=6,color="darkorchid1",lwd=1,alpha=.5)+
  geom_hline(yintercept = mean(Iris_setosa$Sepal.Width),lty=6,color="deeppink",lwd=1,alpha=.5)+
  labs(x="Sepal Length (cm)",y="Sepal Width (cm)")+
  theme(panel.grid.major.x = element_line(color="black",linetype = 3),
        panel.grid.minor.x = element_line(color="black",linetype = 3),
        panel.grid.major.y =element_line(color="black",linetype = 3),
        panel.grid.minor.y =element_line(color="black",linetype = 3))

a<-ggplot(Iris_setosa,aes(Sepal.Length,Sepal.Width))+
  geom_point()+
  geom_smooth(method = 'loess',lty=5)+
  geom_vline(xintercept = mean(Iris_setosa$Sepal.Length),lty=6,color="darkorchid1",lwd=1,alpha=.5)+
  geom_hline(yintercept = mean(Iris_setosa$Sepal.Width),lty=6,color="deeppink",lwd=1,alpha=.5)+
  labs(x="Sepal Length (cm)",y="Sepal Width (cm)")+
  theme(panel.grid.major.x = element_line(color="black",linetype = 3),
        panel.grid.minor.x = element_line(color="black",linetype = 3),
        panel.grid.major.y =element_line(color="black",linetype = 3),
        panel.grid.minor.y =element_line(color="black",linetype = 3))+
  annotate("segment",x=4.35,xend=4.4,y=mean(Iris_setosa$Sepal.Width)+0.55,yend =mean(Iris_setosa$Sepal.Width)+0.05 ,arrow=arrow(),color="deeppink")+
  annotate("text",x=4.35,y=mean(Iris_setosa$Sepal.Width)+0.65,label="Width \nMean",color="deeppink",size=3.5)+
  annotate("segment",x=mean(Iris_setosa$Sepal.Length)+0.2,xend=mean(Iris_setosa$Sepal.Length)+0.02,y=2.35,yend =2.35 ,arrow=arrow(),color="darkorchid1")+
  annotate("text",x=mean(Iris_setosa$Sepal.Length)+0.3,y=2.35,label="Length \nMean",color="darkorchid1",size=3.5)



b<-ggplot(Iris_setosa,aes(Sepal.Length))+
  geom_histogram(fill="cornflowerblue",alpha=.5,binwidth = 0.05)+
  geom_vline(xintercept = mean(Iris_setosa$Sepal.Length),lty=6,color="darkorchid1",lwd=1,alpha=.5)+
  labs(x="",y="",xaxt="")+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x =element_line(color="black",linetype = 3),
        panel.grid.minor.x =element_line(color="black",linetype = 3),
        plot.margin = unit(c(0,0.03,-0.85,0.64),"cm"))

c<-ggplot(Iris_setosa,aes(Sepal.Width))+
  geom_histogram(fill="deepskyblue2",alpha=.5,binwidth = 0.05)+
  geom_vline(xintercept = mean(Iris_setosa$Sepal.Width),lty=6,color="deeppink",lwd=1,alpha=.5)+
  labs(x="",y="",xaxt="")+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y =element_line(color="black",linetype = 3),
        panel.grid.minor.y =element_line(color="black",linetype = 3),
        plot.margin = unit(c(0.34,0,0.52,-.9),"cm"))+
  coord_flip() #Voltea las coordenadas cartesianas para que la horizontal se convierta en vertical y la vertical en horizontal

d<-ggplot(Iris_setosa,aes(Sepal.Length,Sepal.Width))+
  geom_boxplot(color="deepskyblue2",lwd=1)+
  geom_hline(yintercept = mean(Iris_setosa$Sepal.Width),lty=6,color="deeppink",lwd=1,alpha=.5)+
  labs(x="",y="",xaxt="")+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y =element_line(color="black",linetype = 3),
        panel.grid.minor.y =element_line(color="black",linetype = 3),
        plot.margin = unit(c(0.41,0,.6,-.6),"cm"))

e<-ggplot(Iris_setosa,aes(Sepal.Width,Sepal.Length))+
  geom_boxplot(color="deepskyblue2",lwd=1)+
  geom_hline(yintercept = mean(Iris_setosa$Sepal.Length),lty=6,color="darkorchid1",lwd=1,alpha=.5)+
  labs(x="",y="",xaxt="")+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x =element_line(color="black",linetype = 3),
        panel.grid.minor.x =element_line(color="black",linetype = 3),
        plot.margin = unit(c(.5,.2,-.5,.8),"cm"))+
  coord_flip()

Iris_setosa_Sepal<- ggplot(data=data.frame(x=0,y=0))+geom_point(aes(x=x,y=y),size=-1)+
  labs(x="",y="")+
  annotate('text', x = 0, y = 0, label = "Iris-setosa\nSepal",size=8)+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0,0,-1,-1),"cm"))


#unimos todas las graficas


grid.arrange(a,b,c,d,e,Iris_setosa_Sepal,
             layout_matrix=matrix(c(5,2,1,1,1,1,5,2,1,1,1,1,5,2,1,1,1,1,5,2,1,1,1,1,6,6,3,3,3,3,6,6,4,4,4,4),nrow = 6))

grid.arrange(b,d,c,e,a, ncol = 2)


