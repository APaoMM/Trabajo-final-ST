library(ggplot2)
library(dplyr)
library(readxl)
library(stats)
library(seasonal)
library(seasonalview)
library(shiny)
library(stats)
library(MASS)
library(strucchange)
library(zoo)
library(sandwich)
library(urca)
library(lmtest)
library(vars)
library(tidyr)
library(forecast)
library(mFilter)


#dat <- read_excel("C:/Users/Jack/Documents/FACULTAD DE ECONOMÍA/9° semestre/Series de tiempo/Trabajo final/BD Sdt.xlsx",
 #                 sheet = "2004", col_names = TRUE)
dat<-read_excel("C:/Users/Jack/Documents/FACULTAD DE ECONOMÍA/9° semestre/Series de tiempo/Trabajo final/Base SDT.xlsx",
                  sheet = "2004", col_names = TRUE)
head(dat)

#Convertir a las variables en series de tiempo.
M2 <- ts(dat$M2,
        start = c(2004,1),
         frequency = 12)

pp <- ts(dat$PETROLEO,
         start = c(2004,1),
         frequency = 12)

FIX <- ts(dat$FIX,
         start = c(2004,1),
         frequency = 12)

CETES <- ts(dat$CETES,
         start = c(2004,1),
         frequency = 12)

IMAI <- ts(dat$IMAI,
            start = c(2004,1),
            frequency = 12)

INPP <- ts(dat$INPP,
           start = c(2004,1),
           frequency = 12)

#Desestacionalizar las series y crear una nueva variable a partir de ello.
M2_Seas <- seas(M2)
plot(M2_Seas, main="Oferta monetaria M2, (2004.1-2020.12)", 
     ylab="M2 (miles de pesos)", xlab="Período", xlim=c(2004,2021),
     sub="Elaboración propia con datos de Banxico", cex.sub=0.6)
M2D <- final(M2_Seas)

pp_Seas <- seas(pp)
plot(pp_Seas, main="Precio de petróleo, (2004.1-2020.12)", 
     ylab="Precio petróleo (dólar x barril)", xlab="Período", xlim=c(2004,2021),
     sub="Elaboración propia con datos de Banxico", cex.sub=0.6)
ppD <- final(pp_Seas)

FIX_Seas <- seas(FIX)
plot(FIX_Seas, main="Tipo de cambio, (2004.1-2020.12)", 
     ylab="Precio petróleo (dólar x barril)", xlab="Período", xlim=c(2004,2021),
     sub="Elaboración propia con datos de Banxico", cex.sub=0.6)
FIXD <- final(FIX_Seas)

CETES_Seas <- seas(CETES)
plot(CETES_Seas, main="Tasa de interés de corto plazo, (2004.1-2020.12)", 
     ylab="Ti CETES a 28 días", xlab="Período", xlim=c(2004,2021),
     sub="Elaboración propia con datos de Banxico", cex.sub=0.6)
CETESD <- final(CETES_Seas)

INPP_Seas <- seas(INPP)
plot(INPP_Seas, main="Inflación doméstica, (2004.1-2020.12)", 
     ylab="índice nacional de Precios al Productor", xlab="Período", xlim=c(2004,2021),
     sub="Elaboración propia con datos de INEGI", cex.sub=0.6)
INPPD <- final(INPP_Seas)

IMAI_Seas<-seas(IMAI)
plot(IMAI_Seas, main="Índice mensual actividad industrial, (2004.1-2020.12)", 
     ylab="IMAI", xlab="Período", xlim=c(2004,2021),
     sub="Elaboración propia con datos de INEGI", cex.sub=0.6)
IMAID<-final(IMAI_Seas)


#Obtener diferencias o diferencias logarítmicas
DifCETES<- ts(as.numeric(CETESD) - lag(as.numeric(CETESD), k = 1), 
             start = c(2004,1),frequency = 12)

DIMAI<- ts(log(as.numeric(IMAI)) - lag(log(as.numeric(IMAI)), k = 1), 
                 start = c(2004,1),frequency = 12)

DLM2D <- ts(log(as.numeric(M2D)) - lag(log(as.numeric(M2D)), k = 1), 
             start = c(2004,1),frequency = 12)

DppD<- ts(log(as.numeric(ppD)) - lag(log(as.numeric(ppD)), k = 1), 
          start = c(2004,1),frequency = 12)

DFIXD<- ts(log(as.numeric(FIXD)) - lag(log(as.numeric(FIXD)), k = 1), 
           start = c(2004,1),frequency = 12)

DCETESD<- ts(log(as.numeric(CETESD)) - lag(log(as.numeric(CETESD)), k = 1), 
             start = c(2004,1),frequency = 12)

DINPPD<- ts(log(as.numeric(INPPD)) - lag(log(as.numeric(INPPD)), k = 1), 
               start = c(2004,1),frequency = 12)

plot(DLM2D, main="     Diferencia logarítmica
     Oferta monetaria M2", ylab="DL Agregado M2", xlab="Período", xlim=c(2004,2021),
     col="dodgerblue", sub="Elaboración propia con datos de Banxico", cex.sub=0.6)

plot(DppD, main="     Diferencia logarítmica
     del precio del petróleo", ylab="DL Precio del petróleo", xlab="Período", xlim=c(2004,2021),
     col="lawngreen", sub="Elaboración propia con datos de Banxico", cex.sub=0.6)

plot(DFIXD, main="      Diferencia logarítmica
     del tipo de cambio", ylab="DL Tipo de cambio", xlab="Período", xlim=c(2004,2021),
     col="purple2", sub="Elaboración propia con datos de Banxico", cex.sub=0.6)

plot(DCETESD, main="    Diferencia logarítmica
     Inflación doméstica", ylab="DL CETES", xlab="Período", xlim=c(2004,2021),
     col="hotpink2", sub="Elaboración propia con datos de Banxico", cex.sub=0.6)

plot(DINPPD, main="Diferencia inflación doméstica", 
     ylab="DL índice de precios al productor", xlab="Período", xlim=c(2004,2021),
     col="orange2", sub="Elaboración propia con datos de INEGI", cex.sub=0.6)

plot(DIMAI, main="        Diferencia logarítmica
     índice mensual de actividad industrial", ylab="DL IMAI",
     xlab="Período", xlim=c(2004,2021), col="indianred2", 
     sub="Elaboración propia con datos de INEGI", cex.sub=0.6)

#Serie en niveles (desestacionalizada)
Dato<-data.frame(cbind(M2D,ppD,FIXD,CETESD,INPPD,IMAID))
Dato<-na.omit(Dato)
head(Dato)
#Serie con diferencias/diferencias logarítmicas
DatosD<-data.frame(cbind(DLM2D,DppD,DFIXD,DifCETES,DINPPD,DIMAI))
DatosD<-na.omit(DatosD)
head(DatosD)

##PRUEBAS RAÍCES UNITARIAS

#DICKY FULLER

#Oferta monetaria
#En niveles
summary(ur.df(Dato[,1], type="trend", lags=0)) # -3.43<-0.73 Ho: No estacionaria
summary(ur.df(Dato[,1], type="drift", lags=0)) # -2.88<3.15  Ho: No estacionaria
summary(ur.df(Dato[,1], type="none", lags=0))  # -1.95<10.14 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,1], type="trend", lags=0)) # -3.43>-14.15 Ha: Es estacionaria
summary(ur.df(DatosD[,1], type="drift", lags=0)) # -2.88>-14.11 Ha: Es estacionaria
summary(ur.df(DatosD[,1], type="none", lags=0))  # -1.95>-9.37 Ha: Es estacionaria


#Precio del petróleo
#En niveles
summary(ur.df(Dato[,2], type="trend", lags=0)) # -3.43<-2.19 Ho: No estacionaria
summary(ur.df(Dato[,2], type="drift", lags=0)) # -2.88<-1.94  Ho: No estacionaria
summary(ur.df(Dato[,2], type="none", lags=0))  # -1.95<-0.52 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,2], type="trend", lags=0)) # -3.43>-10.81 Ha: Es estacionaria
summary(ur.df(DatosD[,2], type="drift", lags=0)) # -2.88>-10.81 Ha: Es estacionaria
summary(ur.df(DatosD[,2], type="none", lags=0))  # -1.95>-10.83 Ha: Es estacionaria


#Tipo de cambio
#En niveles
summary(ur.df(Dato[,3], type="trend", lags=0)) # -3.43<-2.56 Ho: No estacionaria
summary(ur.df(Dato[,3], type="drift", lags=0)) # -2.88<-0.65 Ho: No estacionaria
summary(ur.df(Dato[,3], type="none", lags=0))  # -1.95<1.078 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,3], type="trend", lags=0)) # -3.43>-10.59 Ha: Es estacionaria
summary(ur.df(DatosD[,3], type="drift", lags=0)) # -2.88>-10.61 Ha: Es estacionaria
summary(ur.df(DatosD[,3], type="none", lags=0))  # -1.95>-10.56 Ha: Es estacionaria



#Tasa de interés CP
#En niveles
summary(ur.df(Dato[,4], type="trend", lags=0)) # -3.43<-1.12 Ho: No estacionaria
summary(ur.df(Dato[,4], type="drift", lags=0)) # -2.88<-0.73 Ho: No estacionaria
summary(ur.df(Dato[,4], type="none", lags=0))  # -1.95<-0.42 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,4], type="trend", lags=0)) # -3.43>-8.82 Ha: Es estacionaria
summary(ur.df(DatosD[,4], type="drift", lags=0)) # -2.88>-8.85 Ha: Es estacionaria
summary(ur.df(DatosD[,4], type="none", lags=0))  # -1.95>-8.86 Ha: Es estacionaria


#Inflación doméstica
#En niveles
summary(ur.df(Dato[,5], type="trend", lags=0)) # -3.43<-1.39  Ho: No estacionaria
summary(ur.df(Dato[,5], type="drift", lags=0)) # -2.88<0.167  Ho: No estacionaria
summary(ur.df(Dato[,5], type="none", lags=0))  # -1.95<8.62 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,5], type="trend", lags=0)) # -3.43>-10.18 Ha: Es estacionaria
summary(ur.df(DatosD[,5], type="drift", lags=0)) # -2.88>-10.11 Ha: Es estacionaria
summary(ur.df(DatosD[,5], type="none", lags=0))  # -1.95>-7.70 Ha: Es estacionaria


#Indicador mensual de actividad industrial
#En niveles
summary(ur.df(Dato[,6], type="trend", lags=0)) # -3.43>-3.57 Ha: Es estacionaria
summary(ur.df(Dato[,6], type="drift", lags=0)) # -2.88>-3.35 Ha: Es estacionaria
summary(ur.df(Dato[,6], type="none", lags=0))  # -1.95<0.037 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,6], type="trend", lags=0)) # -3.43>-12.02 Ha: Es estacionaria
summary(ur.df(DatosD[,6], type="drift", lags=0)) # -2.88>-12.04 Ha: Es estacionaria
summary(ur.df(DatosD[,6], type="none", lags=0))  # -1.95>-12.07 Ha: Es estacionaria


#DICKY FULLER AUMENTADA
#p = int{4*(T/100)^(1/4)}
#  = int(4*(204/100)^(1/4))
#  = int{4*1.19511}
#  = 4.78

#Oferta monetaria
#En niveles
summary(ur.df(Dato[,1],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43<-0.48 Ho: No estacionaria
summary(ur.df(Dato[,1],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88<2.78  Ho: No estacionaria
summary(ur.df(Dato[,1], type="none", lags=4))  # -1.95<5.89 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,1],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43>-5.81 Ha: Es estacionaria
summary(ur.df(DatosD[,1],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88>-5.72 Ha: Es estacionaria
summary(ur.df(DatosD[,1], type="none", lags=4))  # -1.95>-2.77 Ha: Es estacionaria


#Precio del petróleo
#En niveles
summary(ur.df(Dato[,2],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43<-2.69 Ho: No estacionaria
summary(ur.df(Dato[,2],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88<-2.47  Ho: No estacionaria
summary(ur.df(Dato[,2], type="none", lags=4))  # -1.95<-0.72 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,2],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43>-6.63 Ha: Es estacionaria
summary(ur.df(DatosD[,2],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88>-6.57 Ha: Es estacionaria
summary(ur.df(DatosD[,2], type="none", lags=4))  # -1.95>-6.58 Ha: Es estacionaria


#Tipo de cambio
#En niveles
summary(ur.df(Dato[,3],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43<-2.79 Ho: No estacionaria
summary(ur.df(Dato[,3],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88<-0.54 Ho: No estacionaria
summary(ur.df(Dato[,3], type="none", lags=4))  # -1.95<1.047 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,3],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43>-6.52 Ha: Es estacionaria
summary(ur.df(DatosD[,3],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88>-6.52 Ha: Es estacionaria
summary(ur.df(DatosD[,3], type="none", lags=4))  # -1.95>-6.39 Ha: Es estacionaria



#Tasa de interés CP
#En niveles
summary(ur.df(Dato[,4],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43<-2.10 Ho: No estacionaria
summary(ur.df(Dato[,4],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88<-2.07 Ho: No estacionaria
summary(ur.df(Dato[,4], type="none", lags=4))  # -1.95<-1.09 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,4],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43>-3.46 Ha: Es estacionaria
summary(ur.df(DatosD[,4],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88>-3.47 Ha: Es estacionaria
summary(ur.df(DatosD[,4], type="none", lags=4))  # -1.95>-3.47 Ha: Es estacionaria


#Inflación doméstica
#En niveles
summary(ur.df(Dato[,5],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43<-2.15  Ho: No estacionaria
summary(ur.df(Dato[,5],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88<0.14  Ho: No estacionaria
summary(ur.df(Dato[,5], type="none", lags=4))  # -1.95<3.87 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,5],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43>-14.18 Ha: Es estacionaria
summary(ur.df(DatosD[,5],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88>-14.11 Ha: Es estacionaria
summary(ur.df(DatosD[,5], type="none", lags=4))  # -1.95>-7.74 Ha: Es estacionaria


#Indicador mensual de actividad industrial
#En niveles
summary(ur.df(Dato[,6],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43>-5.55 Ha: Es estacionaria
summary(ur.df(Dato[,6],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88<-2.67 Ha: Es estacionaria
summary(ur.df(Dato[,6], type="none", lags=4))  # -1.95<0.096 Ho: No estacionaria

#En diferencias
summary(ur.df(DatosD[,6],type = "Z-tau", model = "trend", use.lag = 4)) # -3.43>-8.38 Ha: Es estacionaria
summary(ur.df(DatosD[,6],  type = "Z-tau", model = "constant", use.lag = 4)) # -2.88>-8.37 Ha: Es estacionaria
summary(ur.df(DatosD[,6], type="none", lags=4))  # -1.95>-8.39 Ha: Es estacionaria


#PHILLIPLS PERRON

#Oferta monetaria
#En niveles
summary(ur.pp(Dato[,1],type = "Z-tau", model = "trend", use.lag = 4)) # Ho: No estacionaria
summary(ur.pp(Dato[,1],  type = "Z-tau", model = "constant", use.lag = 4)) #Ho: No estacionaria

#En diferencias
summary(ur.pp(DatosD[,1],type = "Z-tau", model = "trend", use.lag = 4)) #Ha: Es estacionaria
summary(ur.pp(DatosD[,1],  type = "Z-tau", model = "constant", use.lag = 4)) #Ha: Es estacionaria


#Precio del petróleo
#En niveles
summary(ur.pp(Dato[,2],type = "Z-tau", model = "trend", use.lag = 4)) #Ho: No estacionaria
summary(ur.pp(Dato[,2],  type = "Z-tau", model = "constant", use.lag = 4)) # Ho: No estacionaria

#En diferencias
summary(ur.pp(DatosD[,2],type = "Z-tau", model = "trend", use.lag = 4)) # Ha: Es estacionaria
summary(ur.pp(DatosD[,2],  type = "Z-tau", model = "constant", use.lag = 4)) #Ha: Es estacionaria


#Tipo de cambio
#En niveles
summary(ur.pp(Dato[,3],type = "Z-tau", model = "trend", use.lag = 4)) # Ho: No estacionaria
summary(ur.pp(Dato[,3],  type = "Z-tau", model = "constant", use.lag = 4)) # Ho: No estacionaria

#En diferencias
summary(ur.pp(DatosD[,3],type = "Z-tau", model = "trend", use.lag = 4)) # Ha: Es estacionaria
summary(ur.pp(DatosD[,3],  type = "Z-tau", model = "constant", use.lag = 4)) # Ha: Es estacionaria



#Tasa de interés CP
#En niveles
summary(ur.pp(Dato[,4],type = "Z-tau", model = "trend", use.lag = 4)) # Ho: No estacionaria
summary(ur.pp(Dato[,4],  type = "Z-tau", model = "constant", use.lag = 4)) # Ho: No estacionaria

#En diferencias
summary(ur.pp(DatosD[,4],type = "Z-tau", model = "trend", use.lag = 4)) # Ha: Es estacionaria
summary(ur.pp(DatosD[,4],  type = "Z-tau", model = "constant", use.lag = 4)) # Ha: Es estacionaria


#Inflación doméstica
#En niveles
summary(ur.pp(Dato[,5],type = "Z-tau", model = "trend", use.lag = 4)) # Ho: No estacionaria
summary(ur.pp(Dato[,5],  type = "Z-tau", model = "constant", use.lag = 4)) # Ho: No estacionaria

#En diferencias
summary(ur.pp(DatosD[,5],type = "Z-tau", model = "trend", use.lag = 4)) # Ha: Es estacionaria
summary(ur.pp(DatosD[,5],  type = "Z-tau", model = "constant", use.lag = 4)) # Ha: Es estacionaria


#Indicador mensual de actividad industrial
#En niveles
summary(ur.pp(Dato[,6],type = "Z-tau", model = "trend", use.lag = 4)) # Ha: Es estacionaria
summary(ur.pp(Dato[,6],  type = "Z-tau", model = "constant", use.lag = 4)) #  Ha: Es estacionaria

#En diferencias
summary(ur.pp(DatosD[,6],type = "Z-tau", model = "trend", use.lag = 4)) #  Ha: Es estacionaria
summary(ur.pp(DatosD[,6], type = "Z-tau", model = "constant", use.lag = 4)) #Ha: Es estacionaria

#Transformar el IMAI en brecha del producto ahora que ya se sabe que los datos son estacionarios
#De acuerdo con Morales Castro (2012), la brecha del producto se obtiene de aplicar el filtro 
#Hodrick-Prescott a la diferencia del logaritmo natual del IMAI (nuestra proxy del Índice de Volumen de la Producción Industrial)

FIMAI<-hpfilter(DIMAI)
FBrecha<-ts(FIMAI$x, frequency=12, start=c(2004,1))
FBrecha<-data.frame(FBrecha)
Brecha<-ts(FBrecha, frequency=12, start=c(2004,1))

## a este ((DLM2D,DFIXD,DINPPD,d24s[185]=-1) le falla normalidad
#Datos <- data.frame(cbind(DLM2D,DFIXD,DINPPD,DCETESD))
Fecha=1:204
Datos<-data.frame(cbind(Fecha,DLM2D,DFIXD,DINPPD,DCETESD,DppD,Brecha))
Datos<-na.omit(Datos)
Datos<-ts(Datos, frequency = 12, start=c(2004,2))


var1<-vars::VAR(Datos, p=1, type = )
var2<-vars::VAR(Datos, p=2)
var3<-vars::VAR(Datos, p=3)
var4<-vars::VAR(Datos, p=4)
var5<-vars::VAR(Datos, p=5)
var6<-vars::VAR(Datos, p=6)
var12<-vars::VAR(Datos, p=12)
var18<-vars::VAR(Datos, p=18)
var24<-vars::VAR(Datos, p=24)

vars::roots(var1) 
vars::roots(var2)
vars::roots(var3)
vars::roots(var4)
vars::roots(var5)
vars::roots(var6)
vars::roots(var12)
vars::roots(var18)
vars::roots(var24)


plot(roots(var3))

#source("C:/Users/tic12/OneDrive/Escritorio/Economía/Nucleo terminal/Economía cuantitativa/Series de tiempo/arroots.R")
#source("C:/Users/tic12/OneDrive/Escritorio/Economía/Nucleo terminal/Economía cuantitativa/Series de tiempo/plot.armaroots.R")
source("C:/Users/Jack/Documents/FACULTAD DE ECONOMÍA/9° semestre/Series de tiempo/Trabajo final/arroots.R")
source("C:/Users/Jack/Documents/FACULTAD DE ECONOMÍA/9° semestre/Series de tiempo/Trabajo final/plot.armaroots.R")

#Graficar los errores 
error12<-residuals(var3$varresult$DINPPD)
plot(error12)
error12<-residuals(var3$varresult$DCETESD)
plot(error12)
error12<-residuals(var3$varresult$DFIXD)
plot(error12)
error12<-residuals(var3$varresult$DLM2D)
plot(error12)

#Datos <- data.frame(cbind(DLM2D,DFIXD,DINPPD,DCETESD))
#[192][191][54][153][169]
#var3

vars::VARselect(Datos, lag.max = 12, type = "both") #Ideal: 4 rezagos

summary(var3, equation= "DFIXD") 
normality.test(var3)
serial.test(var3, lags.bg = 1, type = "BG")
serial.test(var3, lags.bg = 2, type = "BG")
serial.test(var3, lags.bg = 3, type = "BG")
serial.test(var3, lags.bg = 6, type = "BG")
serial.test(var3, lags.bg = 12, type = "BG")
arch.test(var3, lags.multi = 3)


#dummy1<-as.numeric(dat$Fecha=="2448-10-01 UTC", 1)
#dummy1
dum<-as.data.frame(seasonaldummy(Datos))
dum$Dic<-ifelse(Datos[with(Datos, order(Datos$Fecha)),
                                        ]$date_shortcut == "Dic",1,0)
dum <- as.data.frame(seasonaldummy(DLM2D))
dum$Dic <- as.numeric(substr(dat$Fecha, 6, 7) == "12", 1)

+d08s=ts(dummy1, start=2004, frequency = 12)
d08s<-replace(d08s, which(d08s==1),0)
d08s[54]=1
d08s

dummy2<-as.numeric(dat$Fecha=="2008-10-01 UTC", 1)
dummy2

d20s=ts(dummy2, start=2004, frequency = 12)
d20s<-replace(d08s, which(d08s==1),0)
d20s[160]=0
d20s[162]=-1
d20s[182]=-1
d20s[185]=-1
d20s[192]=1

d20s


IR_FIX <- irf(var3, n.ahead = 12, boot = TRUE, 
              ci = 0.95, response = "DFIXD", ortho = TRUE, cumulative = TRUE)

plot(IR_FIX)

#[160][162][182]

#[58] octubre 2008
#[183]marzo,[184] abril, [185]mayo 2020

### modelo tentativo
#Datos <- data.frame(cbind(DLM2D,DFIXD,DINPPD))
#vars::VARselect(Datos, lag.max = 12, type = "both")
#normality.test(var3)-->no pasa
#serial.test(var3, lags.bg = 1, type = "BG")-->pasa
#serial.test(var3, lags.bg = 2, type = "BG")-->pasa
#serial.test(var3, lags.bg = 4, type = "BG")-->pasa
#serial.test(var3, lags.bg = 6, type = "BG")-->pasa
#arch.test(var3, lags.multi = 1)-->pasa

###modelo 2
#Datos <- data.frame(cbind(DLM2D,DFIXD,DINPPD,d20s))
#Datos<-na.omit(Datos)

#var12<-vars::VAR(Datos, p=12)

v#ars::roots(var12)


#vars::VARselect(Datos, lag.max = 18, type = "both")

#normality.test(var12)
#serial.test(var12, lags.bg = 1, type = "BG")
#serial.test(var12, lags.bg = 2, type = "BG")
#serial.test(var12, lags.bg = 4, type = "BG")
#serial.test(var12, lags.bg = 6, type = "BG")
#serial.test(var12, lags.bg = 12, type = "BG")
#arch.test(var12, lags.multi = 12)


#dummy1<-as.numeric(dat$Fecha=="2008-10-01 UTC", 1)
#dummy1


#d20s=ts(dummy2, start=2004, frequency = 12)
#d20s<-replace(d08s, which(d08s==1),0)
#d20s[58]=0
#d20s[185]=-1
#d20s