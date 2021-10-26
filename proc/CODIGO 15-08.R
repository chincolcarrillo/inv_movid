#---- 0. Identificacion ----
# "Codigo de analisis informe final (MOVID) "

#---- 1. Librerias ----
pacman::p_load(dplyr,
               stargazer,
               summarytools, 
               kableExtra,
               sjmisc, 
               sjPlot, 
               ggpubr,
               gridExtra, 
               webshot,
               corrplot, 
               texreg, 
               ggmosaic,
               finalfit,
               DescTools,
               sessioninfo) 


#---- Datos ----
rm(list=ls())
options(scipen=999)

load("INFORME FINAL/datos/proc_movid.RData")
names(proc_movid)
frq(proc_movid)


#---- Estadisticos descriptivos ----
view(dfSummary(proc_movid, headings = FALSE), file = "tabla_descr1.html")


movid_control <- proc_movid %>% select(sexo, edad) 
view(dfSummary(movid_control, headings = FALSE), file = "movid_control.html")
webshot("movid_control.html","movid_control.png")
proc_movid$sexo<- as_factor(proc_movid$sexo)

#---- Regresion multiple ----

proc_movid$peligro_p<- as_factor(proc_movid$peligro_p)
proc_movid$informacion<- as_factor(proc_movid$informacion)
proc_movid$desigualdad_p<- as_factor(proc_movid$desigualdad_p)


#Regresiones
informacion_model <- lm(legitimidad ~ informacion, data = proc_movid)

percepcion_model <- lm(legitimidad ~ desigualdad_p, data = proc_movid)

peligro_model <- lm(legitimidad ~ peligro_p, data = proc_movid)

todos_model <- lm(legitimidad ~ informacion+desigualdad_p+peligro_p, data = proc_movid)

control_model <- lm(legitimidad ~ informacion+desigualdad_p+peligro_p+sexo+edad, data = proc_movid)

#---- Tablas de regresiones ---

# Tabla completa con los 5 modelos
sjPlot::tab_model(list(informacion_model,percepcion_model, peligro_model, todos_model, control_model),
                  show.se=TRUE,
                  show.ci=FALSE,
                  digits=3,
                  p.style = "stars",
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
                  string.pred = "Predictores",
                  string.est = "β", file = "modelo_tabla_1.html")

webshot("modelo_tabla_1.html","modelo_tabla_1.png")

# ** Cambio de persona

#---- Grafico del Modelo ---

sjPlot::plot_model(control_model,ci.lvl = c(0.95), title = "",
                   vline.color = "grey",line.size = 1)


#---- Regresión Logistica----

proc_movid$peligro_p <- factor(proc_movid$peligro_p, labels=c("Nada peligroso", "Algo peligroso", "Bastante peligroso", "Muy peligroso", "Extremadamente peligroso"))

m00 <- glm(legitimidad_dico~1,data = proc_movid,family = "binomial")
m01 <- glm(legitimidad_dico~sexo,data = proc_movid,family = "binomial")
m02 <- glm(legitimidad_dico~edad,data = proc_movid,family = "binomial")
m03 <- glm(legitimidad_dico~sexo+edad+peligro_p, data = proc_movid,family = "binomial")
m04 <- glm(legitimidad_dico~sexo+edad+desigualdad_p, data = proc_movid,family = "binomial")
m05 <- glm(legitimidad_dico~sexo+edad+informacion, data = proc_movid,family = "binomial")
m06 <- glm(legitimidad_dico~sexo+edad+peligro_p+desigualdad_p, data = proc_movid,family = "binomial")
m07 <- glm(legitimidad_dico~sexo+edad+peligro_p+informacion, data = proc_movid,family = "binomial")
m08 <- glm(legitimidad_dico~sexo+edad+desigualdad_p+informacion, data = proc_movid,family = "binomial")

m09 <- glm(legitimidad_dico~sexo+edad+peligro_p+desigualdad_p+informacion, data = proc_movid,family = "binomial")


 
test01<- anova(m00,m01,test = "Chisq")
test02<- anova(m00,m02,test = "Chisq")
test03<- anova(m00,m03,test = "Chisq")
test04<- anova(m00,m04,test = "Chisq")
test05<- anova(m00,m05,test = "Chisq")
test06<- anova(m00,m06,test = "Chisq")
test07<- anova(m00,m07,test = "Chisq")
test08<- anova(m00,m08,test = "Chisq")
test09<- anova(m00,m09,test = "Chisq")
lrt01<- rbind(test01,test02,test03,test04,test05,test06,test07,test09,test09) %>% unique()


test.pvalues1<- test01$`Pr(>Chi)`[2]
test.pvalues2<- test02$`Pr(>Chi)`[2]
test.pvalues3<- test03$`Pr(>Chi)`[2]
test.pvalues4<- test04$`Pr(>Chi)`[2]
test.pvalues5<- test05$`Pr(>Chi)`[2]
test.pvalues6<- test06$`Pr(>Chi)`[2]
test.pvalues7<- test07$`Pr(>Chi)`[2]
test.pvalues8<- test08$`Pr(>Chi)`[2]
test.pvalues9<- test09$`Pr(>Chi)`[2]

1-(logLik(m01)[1]/ logLik(m00)[1]) # modelo 1 vs modelo nulo
1-(logLik(m02)[1]/ logLik(m00)[1]) # modelo 2 vs modelo nulo
1-(logLik(m03)[1]/ logLik(m00)[1]) # modelo 3 vs modelo nulo
1-(logLik(m04)[1]/ logLik(m00)[1]) # modelo 4 vs modelo nulo
1-(logLik(m05)[1]/ logLik(m00)[1]) # modelo 5 vs modelo nulo
1-(logLik(m06)[1]/ logLik(m00)[1]) # modelo 6 vs modelo nulo
1-(logLik(m07)[1]/ logLik(m00)[1]) # modelo 7 vs modelo nulo
1-(logLik(m08)[1]/ logLik(m00)[1]) # modelo 8 vs modelo nulo
1-(logLik(m09)[1]/ logLik(m00)[1]) # modelo 9 vs modelo nulo

mfr2.00 <- DescTools::PseudoR2(m00)
mfr2.01 <- DescTools::PseudoR2(m01)
mfr2.02 <- DescTools::PseudoR2(m02)
mfr2.03 <- DescTools::PseudoR2(m03)
mfr2.04 <- DescTools::PseudoR2(m04)
mfr2.05 <- DescTools::PseudoR2(m05)
mfr2.06 <- DescTools::PseudoR2(m06)
mfr2.07 <- DescTools::PseudoR2(m07)
mfr2.08 <- DescTools::PseudoR2(m08)
mfr2.09 <- DescTools::PseudoR2(m09)

r2<- as.data.frame(cbind(c(mfr2.00,mfr2.01,mfr2.02,mfr2.03,mfr2.04,mfr2.05,mfr2.06,mfr2.07,mfr2.08,mfr2.09)))


orm03 <- texreg::extract(m03)
orm03@coef <- exp(orm03@coef)

orm04 <- texreg::extract(m04)
orm04@coef <- exp(orm04@coef)

orm05 <- texreg::extract(m05)
orm05@coef <- exp(orm05@coef)

orm06 <- texreg::extract(m06)
orm06@coef <- exp(orm06@coef)

orm07 <- texreg::extract(m07)
orm07@coef <- exp(orm07@coef)

orm08<- texreg::extract(m08)
orm08@coef <- exp(orm08@coef)

orm09<- texreg::extract(m09)
orm09@coef <- exp(orm09@coef)

htmlreg(l = list(m00,m01,m02,m03,orm03,m04,orm04,m05,orm05,m06,orm06,m07,orm07,m08,orm08,m09,orm09), doctype = FALSE, file= "tabla logistica.html")






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


#---- Informacion de la sesion de R ----
sessionInfo()

#R version 4.0.3 (2020-10-10)
#Platform: x86_64-pc-linux-gnu (64-bit)
#Running under: Ubuntu 20.04.2 LTS
