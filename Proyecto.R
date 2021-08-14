library(ggplot2)
library(dplyr)
library(DescTools)

setwd("C:/R/Proyecto/")
poblacion<-read.csv("Poblacion.csv", header = TRUE)
View(poblacion)
#head(poblacion)
#str(poblacion)
#summary(poblacion)

#La población de México es de 126,014,024 habitantes
sum(poblacion$POBTOT)

#De los cuales 64,382,441 son mujeres
sum(na.omit(poblacion$POBFEM))

#y 61,234,104 son hombres
sum(na.omit(poblacion$POBMAS))

#La distribución de la población por estados es:
poblacionestados<-data.frame(aggregate(x = poblacion$POBTOT,
                                       by = list(poblacion$NOM_ENT),
                                       FUN = sum))

#Gráfica de población por estados
ggplot(data=poblacionestados, aes(x=Group.1,y=x))+
  xlab("Estados")+ylab("Población")+
  ggtitle("Distribución de población")+
  geom_point(size=3)+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Población total del país con más de 1 millón de habitantes
M1<-filter(poblacion,poblacion$POBTOT>1000000)
View(M1)

#mostrar gráfica de pie de los municipios más habitados
tabla<-data.frame(M1$NOM_MUN,M1$POBTOT)

tabla <- tabla %>% 
  arrange(desc(M1)) %>%
  mutate(prop = M1$POBTOT / sum(M1$POBTOT) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(tabla, aes(x="", y=prop, fill=M1$NOM_MUN)) +
  ggtitle("Municipios con mayor población")+
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = M1$NOM_MUN), color = "black", size=4) +
  scale_fill_brewer(palette="Set1")

#Población total por estados "México", "Ciudad de México", "Jalisco"
 estado<-filter(poblacion,NOM_ENT %in% c("México", "Ciudad de México", "Jalisco"))
 
#La población de México, Ciudad de México y Jalisco es de 34,550,513
 sum(estado$POBTOT)
 
#Los mayores de edad en esos tres estados es de: 24,885,293
 sum(na.omit(estado$P_18YMAS))
 
#Mayores de edad en el estado de México son 11,968,539
pobedomex<-filter(poblacion,NOM_ENT %in% c("México"))
sum(na.omit(pobedomex$P_18YMAS))

#Mayores de edad en la Ciudad de México son 7,166,255
pobcdmx<-filter(poblacion,NOM_ENT %in% c("Ciudad de México"))
sum(na.omit(pobcdmx$P_18YMAS))

#Mayores de edad en Jalisco son 5,750,499
pobjal<-filter(poblacion,NOM_ENT %in% c("Jalisco"))
sum(na.omit(pobjal$P_18YMAS))

#Los tres estados representan el 28.53784% de la población mayor de 18 años
(sum(na.omit(estado$P_18YMAS))/sum(na.omit(poblacion$P_18YMAS)))*100

hist(poblacionestados$x, main = "Histograma de frecuencias", # Frecuencia
     xlab="Población", ylab = "Estados", col = "lightblue")

#El promedio de la población por estados es de 3,937,938 habitantes por estado
mean(poblacionestados$x)

#El estado con más población es México con 16,992,418 habitantes
max(poblacionestados$x)

#El estado con menos población es Colima con 731,319 habitantes
min(poblacionestados$x)

#Colima tiene una población equiparable a la de la alcaldía Álvaro Obregón de 
AO<-filter(poblacion,poblacion$POBTOT==759003)

#A manera de resumen los datos estadísticos se obtienen con:
summary(poblacionestados$x)

#La varianza es de: 1.074e+13
var(poblacionestados$x)

#La desviación estándar del data frame poblacionestados es de: 3,278,009
sd(poblacionestados$x)
#dado que la desviación estándar es muy alta, significa que los datos están muy dispersos

#La desviación estándar del data frama poblacion es de: 15,725.18
sd(poblacion$POBTOT)

#Plot
plot(x=poblacionestados$x,main="Gráfica de dispersión de población por estados",
     xlab="Estado", ylab="Pobación")