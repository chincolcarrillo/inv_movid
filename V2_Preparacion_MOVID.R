#---- 0. Identificacion ----
# "Codigo de preparacion informe final (MOVID) - opcion 2"
# 04 de julio de 2021
# Se realiza un codigo de preparacion de datos para posterior analisis

#---- 1. Librerias ----
install.packages("pacman")
pacman::p_load(dplyr, summarytools, sjmisc, car, sjlabelled, stargazer, scales, sessioninfo)


#---- 2. Datos ----
rm(list=ls())       
options(scipen=999)   

movid_original <- read.csv("datos/MOVID-IMPACT.csv")

View(movid_original)

#---- 3. Filtrar base, seleccionar variables y codificar NAs para construccion de variable ----

proc_movid <- movid_original %>%
  filter(entrevistado == 1) %>% 
  select(sexo, edad, neduc = a8a, ingreso = g47_monto, tingreso= g48, id_encuesta, legitimidad = f3_3, peligro_p = f6) %>% 
  mutate(tingreso = car::recode(tingreso,"c('No responde','No sabe') = NA")) 

View(proc_movid)

#---- 4. Crear variable de ingreso_pp ----
# incluir personas por hogar en variable familia
familia <- count(movid_original, vars = id_encuesta)
proc_movid$familia <- familia$n

proc_movid <- proc_movid %>%
  mutate(tingreso = case_when(tingreso == "Menos de $200 mil pesos" ~ 200000,
                              tingreso == "Entre $200 y 350 mil pesos" ~ 275000,
                              tingreso == "Entre $351 y 500 mil pesos" ~ 425500,
                              tingreso == "Entre 501 y 800 mil pesos" ~ 650500,
                              tingreso == "Entre 801 mil y 1 millón 200 mil pesos" ~ 1000500,
                              tingreso == "Entre 1 millón 201 mil y 2 millones de pesos" ~ 1600500,
                              tingreso == "Entre 2 millones y 5 millones de pesos" ~ 3500000,
                              tingreso == "Más de 5 millones de pesos" ~ 5000000))

proc_movid$ingreso_i <- ifelse(test = (is.na(proc_movid$ingreso)), 
                               yes = proc_movid$tingreso,         
                               no = proc_movid$ingreso)            

proc_movid$ingreso_pp <- proc_movid$ingreso_i/proc_movid$familia
descr(proc_movid$ingreso_pp)

#---- 5. Descriptivo ----
view(dfSummary(proc_movid, headings = FALSE, method = "render"))
# cambiar sexo a 0 (hombre) y 1 (mujer)
# cambiar legitimidad a numero y crear version dicotomica
# cambiar peligro a numero
# reclasificar neduc

#---- 6. Procesamiento de variables ----

#--- 6.1. Recodificacion valores y NA restantes ---
proc_movid$sexo <- recode(proc_movid$sexo, "'Hombre' = 0; 'Mujer' = 1")
proc_movid$sexo <- as_numeric(proc_movid$sexo)

proc_movid$neduc <- recode(proc_movid$neduc, "'Nunca asistió' = 1; 
                         'Especial (Diferencial)' = 2;
                         'Básica' = 2;
                         'Media Científico-Humanista' = 3;
                         'Media Técnica-Profesional' = 3;
                         'Superior Técnica (CFT o I. Profesional)' = 4;
                         'Superior Universitaria (Pregrado)' = 5;
                         'Postgrado' = 6;
                         'No sabe/no responde' = NA")
frq(proc_movid$neduc)

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


#--- 6.2. Etiquetamiento (variables y valores) ---
                                
sjlabelled::get_label(proc_movid) 

# crear label de variable
proc_movid$ingreso_pp <- set_label(x = proc_movid$ingreso_pp,label = "Ingreso disponible per capita del hogar en Noviembre 2020")
proc_movid$neduc <- set_label(x = proc_movid$neduc,label = "Nivel educacional alcanzado")
proc_movid$peligro_p <- set_label(x = proc_movid$peligro_p,label = "Percepcion de peligro ante el COVID-19")
proc_movid$legitimidad <- set_label(x = proc_movid$legitimidad,label = "Grado de legitimidad con las medidas sanitarias")
proc_movid$legitimidad_dico <- set_label(x = proc_movid$legitimidad_dico,label = "Legitimidad con las autoridades sanitarias. Expresada de manera dicotomica")
proc_movid$sexo <- set_label(x = proc_movid$sexo,label = "Sexo")
proc_movid$edad <- set_label(x = proc_movid$edad,label = "Edad")


#crear label de codigo
proc_movid$sexo <- set_labels(proc_movid$sexo, labels=c( "Hombre" = 0, "Mujer" = 1))

proc_movid$neduc <- set_labels(proc_movid$neduc,
                               labels=c( "Nunca asistió"=1,
                                         "Educación Básica y/o Especial (Diferencial)"=2,
                                         "Media Cientifico-Humanista / Media Técnica-Profesional"=3,
                                         "Superior Técnica (CFT o I. Profesional)"=4,
                                         "Superior Universitaria (Pregrado)"=5,
                                         "Postgrado"=6)) 


proc_movid$peligro_p <- set_labels(proc_movid$peligro_p,
                                   labels=c( "Nada peligroso"=1,
                                             "Algo peligroso"=2,
                                             "Bastante peligroso"=3,
                                             "Muy peligroso"=4,
                                             "Extremadamente peligroso"=5)) 
sjlabelled::get_label(proc_movid) 


#---- 7. Generar base de datos preparada para analisis ----
# Sacar variables innecesarias
proc_movid <- proc_movid %>% select(ingreso_pp, neduc, peligro_p, legitimidad, legitimidad_dico, sexo, edad)
# Eliminar datos perdidos
proc_movid <-na.omit(proc_movid)
# Revisar
view(dfSummary(proc_movid, headings = FALSE, method = "render"))


# Guardar la base lista para trabajar como .RData 
save(proc_movid,file = "datos/proc_movid.RData")


#---- 8. Informacion de la sesion de R ----
sessionInfo()

#R version 4.0.3 (2020-10-10)
#Platform: x86_64-pc-linux-gnu (64-bit)
#Running under: Ubuntu 20.04.2 LTS

                        