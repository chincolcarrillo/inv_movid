#---- 0. Identificacion ----
# "Codigo de analisis informe final (MOVID) - opcion 2"
# 04 de julio de 2021
# Se realiza un codigo de analisis de regresion multiple y regresion logistica


#---- 1. Librerias ----
pacman::p_load(dplyr, #manipulacion de datos,
               stargazer, #tablas
               summarytools, #tablas
               kableExtra, #tablas
               sjmisc, #tablas
               sjPlot, #tablas y graficos
               ggpubr, #graficos
               gridExtra, #unir graficos
               webshot, #descargar foto de tablas a partir de html
               corrplot, #correlacion
               texreg, #mostrar regresion multiple
               sessioninfo) #info sesion de trabajo


#---- 2. Datos ----
rm(list=ls())
options(scipen=999)

load("datos/proc_movid.RData")
names(proc_movid)
frq(proc_movid)


#---- 3. Estadisticos descriptivos ----
#--- 3.1. Tabla descriptiva de variables para seccion metodologia (con NAs) ---
view(dfSummary(proc_movid, headings = FALSE))


# En caso de que queramos poner las variables de control en anexo:
movid_hipotesis <- proc_movid %>% select(legitimidad, ingreso_pp, neduc, peligro_p) 
movid_control <- proc_movid %>% select(sexo, edad) 
view(dfSummary(proc_movid, headings = FALSE), file = "tabla_descr1.html")
webshot("tabla_descr1.html","tabla_descr1.png")

#---- 4. Exploracion de asociacion entre variables ---
# Esta version va con casos observados y desviacion estandar:
proc_movid %>% 
  select(legitimidad,neduc) %>% # 
  dplyr::group_by(Educacion=sjlabelled::as_label(neduc)) %>%
  dplyr::summarise(Obs.=n(),Promedio=mean(legitimidad),SD=sd(legitimidad))

#--- b. Grafico de caja y bigotes ---
### Me convence mas que usemos este para puesto
plot_grpfrq(proc_movid$legitimidad,proc_movid$neduc,
            type = "box")
plot_grpfrq(proc_movid$legitimidad,proc_movid$peligro_p,
            type = "box")


#--- 4.2. Opciones para relacion continua-continua (ingreso y edad) ---

#--- a. Tabla de correlaciones (Pearson) ---
#Con otras variables
# para poder sacar correlaciones

movid_continuas <- proc_movid %>% select(legitimidad, neduc, edad) 

# correlaciones de pearson
M <- cor(movid_continuas)
M

# tabla con correlaciones de pearson 
### (me convence mas que usemos esta)
tab_corr(movid_continuas,
         triangle = "lower", file = "tabla3.html")
webshot("tabla3.html","tabla3.png")


#--- b. Scatterplot ---
plot_scatter(proc_movid, edad, legitimidad)
plot_scatter(proc_movid, ingreso_pp, legitimidad)

g_reg = ggplot(proc_movid, aes(x=ingreso_pp, y=legitimidad)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  labs(y="Legitimidad con las autoridades sanitarias", 
       x="Ingreso per capita del hogar en Noviembre 2020")
g_reg
ggsave("grafico2.png",g_reg)

g_reg2 = ggplot(proc_movid, aes(x=neduc, y=legitimidad)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)+
  labs(y="Legitimidad con las autoridades sanitarias", 
       x="Nivel educacional")
g_reg2
ggsave("grafico3.png",g_reg2)


#---- 5. Regresion multiple ----

#--- 5.1. Preparar variables categoricas ---
proc_movid$peligro_p<- as_factor(proc_movid$peligro_p)
proc_movid$neduc<- as_factor(proc_movid$neduc)

#--- 5.2. Regresiones ---
puesto_model <- lm(legitimidad ~ neduc, data = proc_movid)

ingreso_model <- lm(legitimidad ~ ingreso_pp, data = proc_movid)

peligro_model <- lm(legitimidad ~ peligro_p, data = proc_movid)

todos_model <- lm(legitimidad ~ neduc+ingreso_pp+peligro_p, data = proc_movid)

control_model <- lm(legitimidad ~ neduc+ingreso_pp+peligro_p+sexo+edad, data = proc_movid)


#--- 5.3. Tablas de regresiones ---

# Tabla completa con los 5 modelos
sjPlot::tab_model(list(puesto_model,ingreso_model, peligro_model, todos_model, control_model),
                  show.se=TRUE,
                  show.ci=FALSE,
                  digits=3,
                  p.style = "stars",
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
                  string.pred = "Predictores",
                  string.est = "β", file = "modelo_tabla_1.html")

webshot("modelo_tabla_1.html","modelo_tabla_1.png")


#--- 5.4. Grafico del Modelo 7 ---
# En este grafico se puede visualizar el valor del coeficiente de regresion en el punto
# ademas del intervalo de confianza que se extiende hacia los costados, y se ve que 
# en los modelos estadisticamente significativos este no pasa por el cero
# (recordar que se esta usando un 0.95 de confianza)

sjPlot::plot_model(control_model,ci.lvl = c(0.95), title = "",
                   vline.color = "grey",line.size = 1)


#---- Regresión Logistica----

view(dfSummary(proc_movid, headings = FALSE, method = "render"))


proc_movid$peligro_pf <- factor(proc_movid$peligro_p, labels=c("Nada peligroso", "Algo peligroso", "Bastante peligroso", "Muy peligroso", "Extremadamente peligroso"))

m00 <- glm(legitimidad_dico~1,data = proc_movid,family = "binomial")
m01 <- glm(legitimidad_dico~sexo,data = proc_movid,family = "binomial")
m02 <- glm(legitimidad_dico~edad,data = proc_movid,family = "binomial")
m03 <- glm(legitimidad_dico~sexo+edad+peligro_pf, data = proc_movid,family = "binomial")

m00$coefficients
m01$coefficients
m02$coefficients
m03$coefficients

htmlreg(l = list(m00,m01,m02,m03),
        custom.coef.names=c("Intercepto","Sexo (Mujer=1)","Edad", "Algo peligroso", "Bastante peligroso", "Muy peligroso", "Extremadamente peligroso"),
        custom.model.names = c("Modelo 0","Modelo 1","Modelo 2", "modelo 3"), file = "tabla regresion-logistica.html")


webshot("tabla regresion-logistica.html","tabla regresion-logistica.png")


test01<- anova(m00,m01,test = "Chisq")
test02<- anova(m00,m02,test = "Chisq")
test03<- anova(m00,m03,test = "Chisq")
lrt01<- rbind(test01,test02,test03) %>% unique()
row.names(lrt01) <- c("Modelo nulo",
                      "Modelo 1",
                      "Modelo 2",
                      "Modelo 3")
knitr::kable(lrt01,digits = 3, caption = "Test de devianza entre modelos") %>%
  cat(., file = "tabla test.html")

test.pvalues1<- test01$`Pr(>Chi)`[2]
test.pvalues2<- test02$`Pr(>Chi)`[2]
test.pvalues3<- test03$`Pr(>Chi)`[2]

1-(logLik(m01)[1]/ logLik(m00)[1]) # modelo 1 vs modelo nulo
1-(logLik(m02)[1]/ logLik(m00)[1]) # modelo 2 vs modelo nulo
1-(logLik(m03)[1]/ logLik(m00)[1]) # modelo 3 vs modelo nulo

mfr2.00 <- DescTools::PseudoR2(m00)
mfr2.01 <- DescTools::PseudoR2(m01)
mfr2.02 <- DescTools::PseudoR2(m02)
mfr2.03 <- DescTools::PseudoR2(m03)

r2<- as.data.frame(cbind(c(mfr2.00,mfr2.01,mfr2.02,mfr2.03)))
rownames(r2) <- c("Modelo nulo",
                  "Modelo 1",
                  "Modelo 2",
                  "Modelo 3")

knitr::kable(r2,digits = 3, col.names = c("McFadden R2")) %>%
  cat(., file = "McFadden R2.html")

or <- texreg::extract(m03)
or@coef <- exp(or@coef)

htmlreg(l = list(m03,or), doctype = F,caption = "",caption.above = T,
        custom.model.names = c("Modelo 3", "Modelo 3 (OR)"),
        custom.coef.names=c("Intercepto","Sexo (Mujer=1)","Edad", "Algo peligroso", "Bastante peligroso", "Muy peligroso", "Extremadamente peligroso"),
        ci.force = c(TRUE,TRUE),
        override.coef = list(coef(m03),or@coef),
        custom.gof.rows=list("Deviance Test ($p$)" = c(test.pvalues3,
                                                       test.pvalues3),
                             "Pseudo R2" = c(mfr2.03,mfr2.03)),
        custom.note = "$^{***}$ p < 0.001; $^{**}$ p < 0.01; $^{*}$ p < 0.05 <br> Errores estándar entre paréntesis. <br> **Nota**: La significancia estadística de los coeficientes en unidades de Odds ratio está calculada en base a los valores $t$, <br> los cuales a su vez se calculan en base a $log(Odds)/SE$", file = "tabla logistica final.html")

webshot("tabla logistica final.html","tabla logistica final.png")


plot02<- plot_model(m03,vline.color = "grey")
plot02
ggsave("grafico4.png",plot02)
plot01<- plot_model(m03,vline.color = "grey",transform = NULL)
plot01
ggsave("grafico5.png",plot01)
plot_grid(list(plot02,plot01),tags = c(" "," "),
          margin = c(0,0,0,0))

#---- 6. Informacion de la sesion de R ----
sessionInfo()

#R version 4.0.3 (2020-10-10)
#Platform: x86_64-pc-linux-gnu (64-bit)
#Running under: Ubuntu 20.04.2 LTS

