
pacman::p_load(dplyr, sjlabelled, stargazer, sjmisc, summarytools, kableExtra, sjPlot, corrplot, ggplot2, webshot)

#Cargar base de datos 
load("input/data/proc/Desiguales_procesada.RData")

#Retirar casos perdidos NA
Desigualesproc_original <-Desiguales_procesada
dim(Desiguales_procesada)
sum(is.na(Desiguales_procesada))
Desigualesproc_sinna <-na.omit(Desiguales_procesada)
dim(Desigualesproc_sinna)
Desigualesproc_sinna <-sjlabelled::copy_labels(Desigualesproc_sinna,Desigualesproc_original)

#Coef. de correlacion
sjt.corr(Desigualesproc_sinna,
         triangle = "lower")
      #Arreglar etiquetas, pues tabla no lee los tildes.
Desigualesproc_sinna$satisfaccioneconomia <- set_label(x = Desigualesproc_sinna$satisfaccioneconomia,label = "Satisfaccion economia nacional")
sjt.corr(Desigualesproc_sinna,
         triangle = "lower")
CR <- cor(Desigualesproc_sinna)
plot_scatter(Desigualesproc_sinna, satisfaccioneconomia, percepciondesigualdad)

#Modelo de regresion
g2=ggplot(Desigualesproc_sinna, aes(x=satisfaccioneconomia, y=percepciondesigualdad)) +
  geom_point() +
    geom_smooth(method = lm, se=FALSE)
g2

Desigualesproc_sinna$difx=Desigualesproc_sinna$satisfaccioneconomia-mean(Desigualesproc_sinna$satisfaccioneconomia) 
Desigualesproc_sinna$dify=Desigualesproc_sinna$percepciondesigualdad-mean(Desigualesproc_sinna$percepciondesigualdad)

Desigualesproc_sinna$difcru=Desigualesproc_sinna$difx*Desigualesproc_sinna$dify
Desigualesproc_sinna$difx2=Desigualesproc_sinna$difx^2
Desigualesproc_sinna

sum(Desigualesproc_sinna$difcru)
sum(Desigualesproc_sinna$difx2)

reg1 <-lm(percepciondesigualdad ~ satisfaccioneconomia, data = Desigualesproc_sinna)
reg1
stargazer(reg1, type = "text")
sjPlot::tab_model(reg1, show.ci = FALSE, file = "reg1_tab.html")
webshot("reg1_tab.html","reg1_tab.png")

#Bondad de Ajuste; Residuos y R2

Desigualesproc_sinna$estimado <- predict(reg1)

Desigualesproc_sinna$residuo <- residuals(reg1)

Desigualesproc_sinna %>% select(estimado, residuo)

#Suma de cuadrados y R2.

ss_tot<- sum((Desigualesproc_sinna$percepciondesigualdad-mean(Desigualesproc_sinna$percepciondesigualdad))^2); ss_tot

ss_reg<-sum((Desigualesproc_sinna$estimado-mean(Desigualesproc_sinna$percepciondesigualdad))^2); ss_reg

ss_err<-sum((Desigualesproc_sinna$percepciondesigualdad - Desigualesproc_sinna$estimado)^2);ss_err

summary(lm(percepciondesigualdad~satisfaccioneconomia, data = Desigualesproc_sinna))$r.squared

#Grafico 
ggplot(Desigualesproc_sinna, aes(x=satisfaccioneconomia, y=percepciondesigualdad))+
  geom_smooth(method="lm", se=FALSE, color="lightgrey") + 
  geom_segment(aes(xend=satisfaccioneconomia, yend=estimado), alpha = .2) + 
  geom_point(aes(color = abs(residuo), size = abs(residuo))) + 
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE) +
  geom_point(aes(y=estimado), shape =1) +
  theme_bw()