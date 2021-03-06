#Paquetes y librerias
pacman::p_load(dplyr, sjmisc, car, sjlabelled, summarytools)

#Cargar base de datos
Desiguales <-read.csv("input/data/original/PNUD_DES_2016_publica.csv")

#Seleccionar variables
Desiguales_procesada <-Desiguales %>% select(p8, p27)
names(Desiguales_procesada)
sjlabelled::get_label(Desiguales_procesada)

#Procesamiento variables de interes
frq(Desiguales_procesada$p8)

Desiguales_procesada <- Desiguales_procesada %>% rename("percepciondesigualdad"=p8,
                                    "satisfaccioneconomia"=p27) 

Desiguales_procesada$percepciondesigualdad <-recode(Desiguales_procesada$percepciondesigualdad, "c(88,99)=NA")
Desiguales_procesada$satisfaccioneconomia <-recode(Desiguales_procesada$satisfaccioneconomia, "c(88,99)=NA")

get_label(Desiguales_procesada$percepciondesigualdad)
get_label(Desiguales_procesada$satisfaccioneconomia)

Desiguales_procesada$percepciondesigualdad <-set_label(x = Desiguales_procesada$percepciondesigualdad, label = "Percepcion de desigualdad")
Desiguales_procesada$satisfaccioneconomia <-set_label(x = Desiguales_procesada$satisfaccioneconomia, label = "Satisfaccion con econom?a pa?s")

#Generar Tabla descriptiva
dfSummary(Desiguales_procesada, plain.ascii = FALSE)
view(dfSummary(Desiguales_procesada, headings=FALSE))

#Guardar Base de datos procesada
save(Desiguales_procesada,file = "input/data/proc/Desiguales_procesada.RData")
