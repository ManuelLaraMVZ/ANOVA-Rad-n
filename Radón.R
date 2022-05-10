library("pacman")

p_load("vroom")

radon <- vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/ANOVA-Rad-n/main/Radon_dataframe.csv")

#Datos obtenidos de https://doi.org/10.1016/0160-4120(92)90067-E

#visualizar características
str(radon)
plot1 <- plot(radon$Diametro,radon$RR)

#crear una variable como factor para el anova

radon$Factor_D <- as.factor(radon$Diametro)

#realizar analisis de varianza

anova.rr <- aov(RR~Factor_D,
                data = radon)

summary(anova.rr)

##################################3
#tukey

p_load("dplyr")
resumen_radon <- group_by(radon,Factor_D) %>% 
  summarise(mean=mean(RR),
            sd=sd(RR), n=n())
resumen_radon

TukeyHSD(anova.rr)

#Copiar los datos a un .txt para facilitar su análisis

#CREAMOS UNA NUEVA COLUMNA EN EL ARCHIVO resumen_radón que tenga las interacciones

resumen_radon$Tukey <- c("a","ab","b","b", "c", "c")

#verificamos
View(resumen_radon)

#Guardamos el archivo
write.csv(x=resumen_radon,
           file="Resumen_radon.csv")

###############################################################3
#Gráfica

p_load("ggplot2",
       "ggsci")

#hay que asegurarse qye la variable numérica D este como factor (Ya se hizo), para que sea más facil manejarla

str(resumen_radon)

#Hay que hacer que todos los factores tengan el mismo número de decimales
levels(resumen_radon$Factor_D)[levels(resumen_radon$Factor_D)=="1.4"]<-"1.40"

#verificamos que haya cambiado
resumen_radon

barras1 <- resumen_radon %>% 
  ggplot(mapping = aes(x=Factor_D,
                       y=mean,
                       fill=Factor_D))+
  geom_bar(stat = "identity", 
           width = 0.8)
barras1

barras2 <- barras1+
  labs(x="Diámetro (mm)", 
       y="Liberación de Radón (%)")+
  theme_bw()
barras2

barras3 <- barras2+
  geom_errorbar(aes(ymin=mean-sd,  #agregamos error estandar
                    ymax=mean+sd,
                    width=0.2))+
  geom_text(aes(label=Tukey), #le decimos que diferencias poner
            nudge_x=0.25,     #REspecto al eje x que tanto cambia
            nudge_y = 5,      #respecto al eje y que tanto cambia
            size=3)+           #tamaño del símbolo
  scale_fill_simpsons()

barras3

#hay qu enotar que las letras hacen referencia a que tanto se pareen las barras, por ejemplo: a significa que se parece 0.37 y 0.51
# b significa que 0.51, 0.71 y 0.51 se parecen; c que 1.40 y 1.99 se parecen

ggsave(filename = "barras_radon.png",
       plot= barras3,
       width = 5,
       height = 5)
