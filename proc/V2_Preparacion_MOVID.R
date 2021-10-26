#---- Identificacion ----
# "Codigo de preparacion informe final (MOVID)"

#---- Librerias ----
install.packages("pacman")
pacman::p_load(dplyr, summarytools, sjmisc, car, sjlabelled, stargazer, scales, sessioninfo)

#---- Datos ----
rm(list=ls())       
options(scipen=999)   

movid_original <- read.csv("datos/MOVID-IMPACT.csv")

View(movid_original)

#---- Filtrar base----

proc_movid <- movid_original %>%
  filter(entrevistado == 1) %>% 
  select(sexo, edad, desigualdad_p = f3_1, informacion = f5_2, legitimidad = f3_3, peligro_p = f6)

View(proc_movid)


#---- Descriptivo ----
view(dfSummary(proc_movid, headings = FALSE, method = "render"), file= "Descriptivos.html")


#---- Procesamiento de variables ----

#---  Recodificacion valores y NA restantes ---
proc_movid$sexo <- recode(proc_movid$sexo, "'Hombre' = 0; 'Mujer' = 1")
proc_movid$sexo <- as_numeric(proc_movid$sexo)

proc_movid$desigualdad_p <- recode(proc_movid$desigualdad_p, "'No sabe' = NA; 'No responde' = NA; 
                          'Muy en desacuerdo' = 1; 'En desacuerdo' = 2;
                          'Indiferente' = 3; 'De acuerdo' = 4; 'Muy de acuerdo' = 5")
frq(proc_movid$desigualdad_p)

proc_movid$informacion <- recode(proc_movid$informacion, "'No sabe' = NA; 'No responde' = NA; 
                          'Muy en desacuerdo' = 1; 'En desacuerdo' = 2;
                          'Indiferente' = 3; 'De acuerdo' = 4; 'Muy de acuerdo' = 5")
frq(proc_movid$informacion)


proc_movid$peligro_p <- recode(proc_movid$peligro_p, "'No sabe' = NA; 'Nada peligroso' = 1;
                        'Algo peligroso' = 2; 'Bastante peligroso' = 3; 'Muy peligroso' = 4;
                        'Extremadamente peligroso' = 5; 'No responde' = NA")
frq(proc_movid$peligro_p)


proc_movid$legitimidad <- recode(proc_movid$legitimidad, "'No sabe' = NA; 'No responde' = NA; 
                          'Muy en desacuerdo' = 1; 'En desacuerdo' = 2;
                          'Indiferente' = 3; 'De acuerdo' = 4; 'Muy de acuerdo' = 5")
frq(proc_movid$legitimidad)

proc_movid$legitimidad_dico <- recode(proc_movid$legitimidad, "'No sabe' = NA; 'No responde' = NA; 
                          '1' = 0; '2' = 0;
                          '3' = 0; '4' = 1; '5' = 1")
frq(proc_movid$legitimidad_dico)


#--- Etiquetamiento (variables y valores) ---
                                
proc_movid$desigualdad_p <- set_label(x = proc_movid$desigualdad_p,label = "Percepcion de desigualdad frente a la pandemia")
proc_movid$informacion <- set_label(x = proc_movid$informacion,label = "Constancia en informarse sobre los avances del coronavirus")
proc_movid$peligro_p <- set_label(x = proc_movid$peligro_p,label = "Percepcion de peligro ante el COVID-19")
proc_movid$legitimidad <- set_label(x = proc_movid$legitimidad,label = "Grado de legitimidad de las medidas sanitarias")
proc_movid$legitimidad_dico <- set_label(x = proc_movid$legitimidad_dico,label = "Grado de legitimidad de las medidas sanitarias. Expresada de manera dicotomica")
proc_movid$sexo <- set_label(x = proc_movid$sexo,label = "Sexo")
proc_movid$edad <- set_label(x = proc_movid$edad,label = "Edad")


sjlabelled::get_label(proc_movid) 

proc_movid$sexo <- set_labels(proc_movid$sexo, labels=c( "Hombre" = 0, "Mujer" = 1))

proc_movid$desigualdad_p <- set_labels(proc_movid$desigualdad_p,
                               labels=c( "Muy en desacuerdo"=1,
                                         "En desacuerdo"=2,
                                         "Indiferente"=3,
                                         "De acuerdo"=4,
                                         "Muy de acuerdo"=5))

proc_movid$informacion <- set_labels(proc_movid$informacion,
                                       labels=c( "Muy en desacuerdo"=1,
                                                 "En desacuerdo"=2,
                                                 "Indiferente"=3,
                                                 "De acuerdo"=4,
                                                 "Muy de acuerdo"=5))


proc_movid$peligro_p <- set_labels(proc_movid$peligro_p,
                                   labels=c( "Nada peligroso"=1,
                                             "Algo peligroso"=2,
                                             "Bastante peligroso"=3,
                                             "Muy peligroso"=4,
                                             "Extremadamente peligroso"=5)) 
sjlabelled::get_label(proc_movid) 


# Eliminar datos perdidos
proc_movid <-na.omit(proc_movid)

# Guardar la base lista para trabajar como .RData 
save(proc_movid,file = "datos/proc_movid.RData")


#---- Informacion de la sesion de R ----
sessionInfo()

#R version 4.0.3 (2020-10-10)
#Platform: x86_64-pc-linux-gnu (64-bit)
#Running under: Ubuntu 20.04.2 LTS

                        