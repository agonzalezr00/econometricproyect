rm(list = ls())

install.packages(c("stargazer", "lmtest", "car","lmtest","multiwayvcov","sandwich","nortest"))
library("stargazer")
library("lmtest")
library("multiwayvcov")
library("sandwich")
library("nortest")


base = EncuestaEconometriarespuestas
base$bogota=(base$`¿Ha permanecido en Bogotá durante los últimos 3 meses?`=="Sí")*1
base$familia=(base$`¿Con quién vive usted?`=="Con Familia")*1
base$consi = (base$`¿Cómo considera usted que tiene más probabilidades de contagiarse de COVID-19?`== "Ir al mercado/super-mercado")*1
base$supercerc=(base$`¿Vive usted a menos de 10 minutos de un supermercado?`=="Sí")*1
base$crapp = (base$`¿Cuántos Rappi pidió en Marzo del 2020?`-base$`¿Cuántos Rappi pidió en Febrero del 2020?`)
base$perrap = base$`En una escala de 1 a 10, ¿que tanta probabilidad cree que tiene usted de contagiarse al pedir un Rappi?`
base$permer = base$`En una escala de 1 a 10, ¿que tanta probabilidad cree que tiene usted de contagiarse al ir al mercado?`
base$cantp = base$`En marzo, ¿Cuantas personas vivian con usted?`

#Vamos a acotar la base de datos con quienes si mandaron evidencia
baseaux = subset.data.frame(base, base$`¿Desea participar en EL sorteo?`=="Sí")

#Miremos las variables sobre persepción probabilidad de vias de contagio
hist(base$perrap, main = "Contagios por Rappi", col = rainbow(10), xlab = "Persepcion sobre la probabilidad de constagio",ylab = "Frecuencia")
plot(density(base$perrap))
hist(base$permer,main = "Contagios por ir a un mercado", col = rainbow(10), xlab = "Persepcion sobre la probabilidad de constagio",ylab = "Frecuencia")
plot(density(base$permer))
ad.test(base$perrap) 
ad.test(base$permer)

hist(baseaux$perrap, main = "Contagios por Rappi", col = rainbow(10), xlab = "Persepcion sobre la probabilidad de constagio",ylab = "Frecuencia")
plot(density(baseaux$perrap))
hist(baseaux$permer,main = "Contagios por ir a un mercado", col = rainbow(10), xlab = "Persepcion sobre la probabilidad de constagio",ylab = "Frecuencia")
plot(density(baseaux$permer))
ad.test(baseaux$perrap) 
ad.test(baseaux$permer)

#Modelos
modelo = lm(crapp~ consi + supercerc + cantp ,data = base)
modeloaux = lm(crapp ~ consi+ supercerc + cantp ,data = baseaux)


modelof = lm(crapp~ supercerc + perrap+ permer+cantp ,data = base)
modeloauxf =lm(crapp~supercerc + perrap + permer+cantp ,data = baseaux) 

modelo1 = lm(crapp~ consi + supercerc,data = base)
modelo1aux = lm(crapp ~ consi+ supercerc,data = baseaux)


modelo1f = lm(crapp~ supercerc + perrap+ permer,data = base)
modeloaux1f =lm(crapp~supercerc + perrap + permer,data = baseaux) 

stargazer( modelo1, modelo1aux, modelo1f,modeloaux1f, type = "text")
stargazer(modelo, modeloaux, modelof,modeloauxf,type = "text")

#Pruebas de homocedasticidad
bptest(modelo)   
bptest(modelo, ~ fitted(modelo) + I(fitted(modelo)^2) ) 

bptest(modeloaux)   
bptest(modeloaux, ~ fitted(modeloaux) + I(fitted(modeloaux)^2) ) 

bptest(modelof)   
bptest(modelof, ~ fitted(modelof) + I(fitted(modelof)^2) ) 

bptest(modeloauxf)   
bptest(modeloauxf, ~ fitted(modeloauxf) + I(fitted(modeloauxf)^2) ) 



#Corrección
clas1=coeftest(modelo)
erroresrobustos1<- coeftest(modelo, vcov. = vcovHC(modelo,"HC1"))
stargazer(modelo, modelo, type = "text", se=list(clas1[,"Std. Error"], erroresrobustos1[,"Std. Error"]))


clas2=coeftest(modeloaux)
erroresrobustos2<- coeftest(modeloaux, vcov. = vcovHC(modeloaux,"HC1"))
stargazer(modeloaux, modeloaux, type = "text", se=list(clas2[,"Std. Error"], erroresrobustos2[,"Std. Error"]))


clas3=coeftest(modelof)
erroresrobustos3<- coeftest(modelof, vcov. = vcovHC(modelof,"HC1"))
stargazer(modelof, modelof, type = "text", se=list(clas3[,"Std. Error"], erroresrobustos3[,"Std. Error"]))


clas4=coeftest(modeloauxf)
erroresrobustos4<- coeftest(modeloauxf, vcov. = vcovHC(modeloauxf,"HC1"))
stargazer(modeloauxf, modeloauxf, type = "text", se=list(clas4[,"Std. Error"], erroresrobustos4[,"Std. Error"]))




