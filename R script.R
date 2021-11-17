library(cbar)
library(bigtime)
library(expm)
library(greekLetters)
library(ggfortify)
library(latex2exp)
library(tstools)
library(plotly)
library(knitr)
library(wesanderson)
library(astsa)
library(extrafont)
library(lattice)
library(fastVAR)
library(imputeTS)
library(stinepack)
library(GPArotation)
library(tsfa)
library(egcm)
library(orcutt)
library(lmtest)
library(tseries)
library (zoo)
library (forecast)
library(BigVAR)
library(glmnet)
library(fastVAR)
library(sparsevar)
library(bigtime)
library(lassovar)
library(vars)
library(robustHD)
library(aTSA)
library(sparsevar)
library(naniar)
library(pheatmap)
library(PAA)
library(urca)
library(strucchange)
library(naniar)
library(DataCombine)
library(stargazer)
setwd("c:/Users/Sara/Desktop")
#sink("ouput.csv")


obj_2r<-as.data.frame(obj_2r)
#missing data visualization
tiff(filename = "Missing.tiff",
     width = 6500, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(0, 0, 0, 0))
vis_miss(obj_2r)
dev.off()

HOSP.NUMB.IMP<-na_interpolation(obj_2r[,10], option = "linear")
TER.EDU.IMP<-na_kalman(obj_2r[,16], model="StructTS", smooth = TRUE)
LS.IMP<-na_interpolation(obj_2r[,21], option = "linear")
FP.CLINICS.IMP<-na_interpolation(obj_2r[,22], option = "linear")
CPR.IMP<-na_kalman(obj_2r[,23], model="StructTS", smooth = TRUE)
EDU.BUD.IMP<-na_kalman(obj_2r[,8],model="StructTS", smooth = TRUE)
HLTH.BUD.IMP<-na_kalman(obj_2r[,9],model="StructTS", smooth = TRUE)
HE.PCAP.USD.IMP<-na_kalman(obj_2r[,7], model="StructTS",smooth = TRUE)




#My bigvar model


obj_2r[,24]<-LS.IMP
names(obj_2r)[24] <- "LS.IMP"


obj_2r[,25]<-EDU.BUD.IMP
names(obj_2r)[25] <- "EDU.BUD.IMP"

obj_2r[,26]<-HLTH.BUD.IMP
names(obj_2r)[26] <- "HLTH.BUD.IMP"


obj_2r[,27]<-HE.PCAP.USD.IMP
names(obj_2r)[27] <- "HE.PCAP.USD.IMP"

obj_2r[,28]<-HOSP.NUMB.IMP
names(obj_2r)[28] <- "HOSPL.NUMB.IMP"

obj_2r[,29]<-FP.CLINICS.IMP
names(obj_2r)[29] <- "FP.CLINICS.IMP"

obj_2r[,30]<-TER.EDU.IMP
names(obj_2r)[30] <- "TER.EDU.IMP"

obj_2r[,31]<-CPR.IMP
names(obj_2r)[31] <- "CPR.IMP"



#imputation in many plots
dev.new()
tiff(filename = "plot1.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot1<-ggplot_na_imputations(obj_2r[,10],HOSP.NUMB.IMP, xlab = "Time",
                             ylab = "HOSPL.NUMB.IMP")
dev.off()

dev.new()
tiff(filename = "plot2.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot2<-ggplot_na_imputations(obj_2r[,16],TER.EDU.IMP, xlab = "Time",
                             ylab = "TER.EDU.IMP")
dev.off()

dev.new()

tiff(filename = "plot3.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot3<-ggplot_na_imputations(obj_2r[,21],LS.IMP, xlab = "Time",
                             ylab = "LS.IMP")
dev.off()

dev.new()

tiff(filename = "plot4.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot4<-ggplot_na_imputations(obj_2r[,22], FP.CLINICS.IMP,xlab = "Time",
                             ylab = "FP.CLINICS.IMP")
dev.off()

dev.new()

tiff(filename = "plot5.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot5<-ggplot_na_imputations(obj_2r[,23], CPR.IMP,xlab = "Time",
                             ylab = "CPR.IMP")
dev.off()
dev.new()

tiff(filename = "plot6.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot6<-ggplot_na_imputations(obj_2r[,8],EDU.BUD.IMP,xlab = "Time",
                             ylab = "EDU.BUD.IMP")
dev.off()

dev.new()

tiff(filename = "plot7.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot7<-ggplot_na_imputations(obj_2r[,9],HLTH.BUD.IMP,xlab = "Time",
                             ylab = "HLTH.BUD.IMP")
dev.off()
dev.new()

tiff(filename = "plot8.tiff",
     width = 3200, height = 3200, units = "px", pointsize = 15,
     compression = "lzw",res=600)
plot8<-ggplot_na_imputations(obj_2r[,7],HE.PCAP.USD.IMP,xlab = "Time",
                             ylab = "HE.PCAP.USD.IMP")
dev.off()

tiff(filename = "Imputation.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 10,
     compression = "lzw",res=300)
par(mar=c(2, 2, 2, 2))
ggarrange(
  plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,labels= c("A", "B","C","D","E","F","G","H"),
  common.legend = TRUE, legend = "bottom",main="")
dev.off()

#Taking natural log of each series

obj_2r_trial<-log(obj_2r)
obj_2r_orig<-obj_2r_trial


#keep columns without NA
colSums(is.na(obj_2r))

obj_2r<-obj_2r[,colSums(is.na(obj_2r)) == 0]
obj_2r_orig<-obj_2r_orig[,colSums(is.na(obj_2r_orig)) == 0]
obj_2r_trial<-obj_2r_trial[,colSums(is.na(obj_2r_trial)) == 0]
#kpss test

kpss.test(obj_2r[,1])
kpss.test(obj_2r[,2])
kpss.test(obj_2r[,3])
kpss.test(obj_2r[,4])
kpss.test(obj_2r[,5])
kpss.test(obj_2r[,6])
kpss.test(obj_2r[,7])
kpss.test(obj_2r[,8])
kpss.test(obj_2r[,9])
kpss.test(obj_2r[,10])
kpss.test(obj_2r[,11])
kpss.test(obj_2r[,12])
kpss.test(obj_2r[,13])
kpss.test(obj_2r[,14])
kpss.test(obj_2r[,15])
kpss.test(obj_2r[,16])
kpss.test(obj_2r[,17])
kpss.test(obj_2r[,18])
kpss.test(obj_2r[,19])
kpss.test(obj_2r[,20])
kpss.test(obj_2r[,21])
kpss.test(obj_2r[,22])
kpss.test(obj_2r[,23])

#save in csv file
write.csv(obj_2r,'obj_2r.csv')
obj_2r<- ts(obj_2r, start=1990, end=2017, frequency=1)

obj_2r_trial<- ts(obj_2r_trial, start=1990, end=2017, frequency=1)

obj_2r_orig <- ts(obj_2r_orig, start=1990, end=2017, frequency=1)


#ADF test

df1<-ur.df(obj_2r[,1], type = ("none"), lags = 3, selectlags = "AIC")
df2<-ur.df(obj_2r[,2], type = ("none"), lags = 3, selectlags = "AIC")
df3<-ur.df(obj_2r[,3], type = ("none"), lags = 3, selectlags = "AIC")
df4<-ur.df(obj_2r[,4], type = ("none"), lags = 3, selectlags = "AIC")
df5<-ur.df(obj_2r[,5], type = ("none"), lags = 3, selectlags = "AIC")
df6<-ur.df(obj_2r[,6], type = ("none"), lags = 3, selectlags = "AIC")
df7<-ur.df(obj_2r[,7], type = ("none"), lags = 3, selectlags = "AIC")
df8<-ur.df(obj_2r[,8], type = ("none"), lags = 3, selectlags = "AIC")
df9<-ur.df(obj_2r[,9], type = ("none"), lags = 3, selectlags = "AIC")
df10<-ur.df(obj_2r[,10], type = ("none"), lags = 3, selectlags = "AIC")
df11<-ur.df(obj_2r[,11], type = ("none"), lags = 3, selectlags = "AIC")
df12<-ur.df(obj_2r[,12], type = ("none"), lags = 3, selectlags = "AIC")
df13<-ur.df(obj_2r[,13], type = ("none"), lags = 3, selectlags = "AIC")
df14<-ur.df(obj_2r[,14], type = ("none"), lags = 3, selectlags = "AIC")
df15<-ur.df(obj_2r[,15], type = ("none"), lags = 3, selectlags = "AIC")
df16<-ur.df(obj_2r[,16], type = ("none"), lags = 3, selectlags = "AIC")
df17<-ur.df(obj_2r[,17], type = ("none"), lags = 3, selectlags = "AIC")
df18<-ur.df(obj_2r[,18], type = ("none"), lags = 3, selectlags = "AIC")
df19<-ur.df(obj_2r[,19], type = ("none"), lags = 3, selectlags = "AIC")
df20<-ur.df(obj_2r[,20], type = ("none"), lags = 3, selectlags = "AIC")
df21<-ur.df(obj_2r[,21], type = ("none"), lags = 3, selectlags = "AIC")
df22<-ur.df(obj_2r[,22], type = ("none"), lags = 3, selectlags = "AIC")
df23<-ur.df(obj_2r[,23], type = ("none"), lags = 3, selectlags = "AIC")



summary(df1)
summary(df2)
summary(df3)
summary(df4)
summary(df5)
summary(df6)
summary(df7)
summary(df8)
summary(df9)
summary(df10)
summary(df11)
summary(df12)
summary(df13)
summary(df14)
summary(df15)
summary(df16)
summary(df17)
summary(df18)
summary(df19)
summary(df20)
summary(df21)
summary(df22)
summary(df23)


#Drawing the series in one plot
dev.new()
tiff(filename = "ALL-SERIES.tiff",
     width = 2000, height = 1000, units = "px", pointsize = 6,
     compression = "lzw",res=400)
par(mfrow = c(5, 5))
par(cex = 0.6)
par(mar = c(2, 4, 1, 0.5), oma = c(1,1, 1, 0.5))
for (i in 1:23) {
  plot(1, 1, type = "n")
  tsplot(x=Time,y= obj_2r[,i])
  mtext(letters[i], side = 3, line = 2, adj = 0.1, cex = 0.6)
}

#differencing
obj_2r_trial<- diff(obj_2r_trial,lag=1, differences=2)  #good

#change the ordering of the columns
obj_2r<-as.matrix(obj_2r)
obj_2r_orig<-as.matrix(obj_2r_orig)
obj_2r_trial<-as.matrix(obj_2r_trial)
obj_2r<-obj_2r[,c(1,2,3,4,8,5,9,6,7,15,16,14,22,19,13,18,23,21,11,10,12,17,20)]
obj_2r_orig<-obj_2r_orig[,c(1,2,3,4,8,5,9,6,7,15,16,14,22,19,13,18,23,21,11,10,12,17,20)]
obj_2r_trial<-obj_2r_trial[,c(1,2,3,4,8,5,9,6,7,15,16,14,22,19,13,18,23,21,11,10,12,17,20)]


#Save the mean and std of each column

mean<-apply(obj_2r_trial,2,mean)

std<-apply(obj_2r_trial,2,sd)

#Standardization

k<-23
obj_2r_trial<- obj_2r_trial- (c(rep(1, nrow(obj_2r_trial)))) %*% t(c(apply(obj_2r_trial, 2, mean)))
for (i in 1:k) { obj_2r_trial[, i] <- obj_2r_trial[, i]/apply(obj_2r_trial, 2, sd)[i] } 



#fastvar pack  (good results)
Var1<-SparseVARX(y=as.matrix(obj_2r_trial[,c(1:16)]), x=obj_2r_trial[,c(17:23)],p=1,b=1, getdiag=T,h=13)
Var2<-SparseVARX(y=as.matrix(obj_2r_trial[,c(1:16)]), x=obj_2r_trial[,c(17:23)],p=2,b=2, getdiag=T,h=1)
Var3<-SparseVARX(y=as.matrix(obj_2r_trial[,c(1:16)]), x=obj_2r_trial[,c(17:23)],p=3,b=3, getdiag=T,h=1)
Var4<-SparseVARX(y=as.matrix(obj_2r_trial[,c(1:16)]), x=obj_2r_trial[,c(17:23)],p=4,b=4, getdiag=T,h=1)

#Using only endog in forecasting
Var<-SparseVAR(y=as.matrix(obj_2r_trial[,c(1:16)]),p=1, getdiag=T,h=1)

coef1<-coef(Var1)
coef2<-coef(Var2)
coef3<-coef(Var3)
coef4<-coef(Var4)
coef<-coef(Var)
prop.table(table(coef1!=0))
prop.table(table(coef2!=0))
prop.table(table(coef3!=0))
prop.table(table(coef4!=0))


VAR.diag(Var1)
VAR.diag(Var2)
VAR.diag(Var3)
VAR.diag(Var4)


tiff(filename = "Fastvar,l1.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(t(coef(Var)))
dev.off()



#predict with endog
predict<-predict(Var, n.ahead = 13)

#you can use also,keeping the values of the exog constant
xnew<-matrix(nrow=13,ncol=7)
xnew[1,]<-x[26,]
for (i in 2:10) {xnew[i,]=xnew[i-1,]}

predict_const<-VARXpredict(y, x, 1, 1,coef1,n.ahead = 13,xnew = xnew)

#Changing policies
nx<-matrix(nrow=15,ncol=7)
nx[1,]<-exp(obj_2r_orig[28,c(17:23)])
for (i in 2:15) {nx[i,1]=nx[i-1,1]+(0.05)}  #0.05 each year to cpr
for (i in 2:15) {nx[i,5]=nx[i-1,5]+(0.05)}   #0.05 each year to interanational trade
for (i in 2:15) {nx[i,2]=nx[i-1,2]+(10)}
nx[2,3]<-17.67
nx[3,3]<-16.77
for (i in 4:15) {nx[i,3]=nx[i-1,3]-0.2}
nx[2,4]<-0.144
for (i in 3:15) {nx[i,4]=nx[i-1,4]-0.005}#check it
for (i in 2:15) {nx[i,6]=nx[i-1,6]+0.01}
for (i in 2:15) {nx[i,7]=nx[i-1,7]}
nx<-log(nx)
nx <- diff(nx,lag=1, differences=2)
nx<-standardize(nx)
#nx[,3]<-obj_2r_trial[26,19]
nx[,7]<-obj_2r_trial[26,23]
#Transforemation of forecastion
predict_policy<-VARXpredict(y, x, 1, 1,coef1,n.ahead = 13,xnew = nx)
#a<-standardize(obj_2r_orig, centerFun = mean, scaleFun = sd)
#forecast<-t(forecast1)
#Newforecast<-rbind(obj_2r_trial,forecast)

#To repeat row #s<-matrix(GDP_F, nrow=2, ncol=length(GDP_F), byrow=TRUE)


Newforecast<-rbind(obj_2r_trial[,c(1:16)],predict_const)
Newforecast<-rbind(obj_2r_trial[,c(1:16)],predict_policy)
Newforecast<-rbind(obj_2r_trial[,c(1:16)],predict)



#GDP
GDP_F<-destandardized(Newforecast[,15], mean[15], std[15])
GDP_F<-diffinv(GDP_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),15])
#GDP_F<-exp(GDP_F)
#GDP_F<-GDP_F/1000000000      #convert to billions
GDP_F<-round(GDP_F,digits=2)


GDP_F<-ts(GDP_F,start=1990,end=2030)

GDP_F_const<-GDP_F


tiff(filename = "GDP_forecast.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=300)
par(mar=c(2,2,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(GDP_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(GDP_F,                     lwd=3, col="black")
lines(window(GDP_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(GDP_F, start=2017),type="o", lwd=3, col=culer3)
text(GDP_F, labels=GDP_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted log(GDP)', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

#calculate percentage change
Out_GDP_const <- change(GDP_F_const, Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_GDP <- change(as.data.frame(GDP_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
GDP_table<-stargazer(Out_GDP_const,type="text",summary=F,out="c:/Users/Sara/Desktop/GDP_const.txt")
GDP_table<-stargazer(Out_GDP,type="text",summary=F,out="c:/Users/Sara/Desktop/GDP_F.txt")

#CBR
CBR_F<-destandardized(Newforecast[,6], mean[6], std[6])
CBR_F<-diffinv(CBR_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),6])
CBR_F<-exp(CBR_F)
tsplot(CBR_F)

CBR_F<-ts(CBR_F,start=1990,end=2030)

CBR_F<-round(CBR_F,digits=3)
CBR_F_const<-CBR_F

tiff(filename = "CBR_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,2,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(CBR_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(CBR_F,                     lwd=3, col="black")
lines(window(CBR_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(CBR_F, start=2017),type="o", lwd=3, col=culer3)
text(CBR_F, labels=CBR_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted crude birth rate', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()
Out_CBR_const <- change(as.data.frame(CBR_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_CBR <- change(as.data.frame(CBR_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
CBR_table_const<-stargazer(Out_CBR_const,type="text",summary=F,out="c:/Users/Sara/Desktop/CBR_const.txt")
CBR_table<-stargazer(Out_CBR,type="text",summary=F,out="c:/Users/Sara/Desktop/CBR_F.txt")



#Cardio
Cardio_F<-destandardized(Newforecast[,1], mean[1], std[1])
Cardio_F<-diffinv(Cardio_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),1])
Cardio_F<-exp(Cardio_F)

Cardio_F<-ts(Cardio_F,start=1990,end=2030)

Cardio_F<-round(Cardio_F,digits=3)
Cardio_F_const<-Cardio_F

tiff(filename = "cardio.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,2,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(Cardio_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(Cardio_F,                     lwd=3, col="black")
lines(window(Cardio_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(Cardio_F, start=2017),type="o", lwd=3, col=culer3)
text(Cardio_F, labels=Cardio_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted burden of cardiovascular diseases', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()
Out_Cardio_const <- change(as.data.frame(Cardio_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_Cardio <- change(as.data.frame(Cardio_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Cardio_table_const<-stargazer(Out_Cardio_const,type="text",summary=F,out="c:/Users/Sara/Desktop/Cardio_const.txt")
Cardio_table<-stargazer(Out_Cardio,type="text",summary=F,out="c:/Users/Sara/Desktop/Cardio_F.txt")




#resp
resp_F<-destandardized(Newforecast[,2], mean[2], std[2])
resp_F<-diffinv(resp_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),2])
resp_F<-exp(resp_F)

resp_F<-ts(resp_F,start=1990,end=2030)

resp_F<-round(resp_F,digits=4)
resp_F_const<-resp_F

tiff(filename = "resp.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 10,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(1,0,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(resp_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=1)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(resp_F,                     lwd=3, col="black")
lines(window(resp_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(resp_F, start=2017),type="o", lwd=3, col=culer3)
text(resp_F, labels=resp_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted burden of respiratory diseases', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

Out_resp_const <- change(as.data.frame(resp_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_resp<- change(as.data.frame(resp_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
resp_table_const<-stargazer(Out_resp_const,type="text",summary=F,out="c:/Users/Sara/Desktop/resp_const.txt")
resp_table<-stargazer(Out_resp,type="text",summary=F,out="c:/Users/Sara/Desktop/resp_F.txt")


#diab
diab_F<-destandardized(Newforecast[,3], mean[3], std[3])
diab_F<-diffinv(diab_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),3])
diab_F<-exp(diab_F)

Out <- PercChange(as.data.frame(diab_F), Var = 'diab_F',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
diab_F<-ts(diab_F,start=1990,end=2030)

diab_F<-round(diab_F,digits=4)
diab_F_const<-diab_F

tiff(filename = "diab.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(diab_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(diab_F,                     lwd=3, col="black")
lines(window(diab_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(diab_F, start=2017),type="o", lwd=3, col=culer3)
text(diab_F, labels=diab_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted burden of diabetes and kidney diseases', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()


Out_diab_const <- change(as.data.frame(diab_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_diab<- change(as.data.frame(diab_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
diab_table_const<-stargazer(Out_diab_const,type="text",summary=F,out="c:/Users/Sara/Desktop/diab_const.txt")
diab_table<-stargazer(Out_diab,type="text",summary=F,out="c:/Users/Sara/Desktop/diab_F.txt")


#neoplasms
neop_F<-destandardized(Newforecast[,4], mean[4], std[4])
neop_F<-diffinv(neop_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),4])
neop_F<-exp(neop_F)

neop_F<-ts(neop_F,start=1990,end=2030)

neop_F<-round(neop_F,digits=4)
neop_F_const<-neop_F

tiff(filename = "neop.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(neop_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(neop_F,                     lwd=3, col="black")
lines(window(neop_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(neop_F, start=2017),type="o", lwd=3, col=culer3)
text(neop_F, labels=neop_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted burden of neoplasms', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

Out_neop_const <- change(as.data.frame(neop_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_neop<- change(as.data.frame(neop_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
neop_table_const<-stargazer(Out_neop_const,type="text",summary=F,out="c:/Users/Sara/Desktop/neop_const.txt")
neop_table<-stargazer(Out_neop,type="text",summary=F,out="c:/Users/Sara/Desktop/neop_F.txt")


#savings
savings_F<-destandardized(Newforecast[,10], mean[10], std[10])
savings_F<-diffinv(savings_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),10])
#savings_F<-exp(savings_F)
tsplot(savings_F)
Out_savings_const<- PercChange(as.data.frame(savings_F), Var = 'savings_F',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_savings_const

savings_F<-ts(savings_F,start=1990,end=2030)

savings_F<-round(savings_F,digits=4)
savings_F_const<-savings_F

tiff(filename = "savings_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(savings_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(savings_F,                     lwd=3, col="black")
lines(window(savings_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(savings_F, start=2017),type="o", lwd=3, col=culer3)
text(savings_F, labels=savings_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted burden of log(savings)', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

Out_savings_const <- change(as.data.frame(savings_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_savings<- change(as.data.frame(savings_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
savings_table_const<-stargazer(Out_savings_const,type="text",summary=F,out="c:/Users/Sara/Desktop/savings_const.txt")
savings_table<-stargazer(Out_savings,type="text",summary=F,out="c:/Users/Sara/Desktop/savings_F.txt")


#ls
ls_F<-destandardized(Newforecast[,11], mean[11], std[11])
ls_F<-diffinv(ls_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),11])
#ls_F<-exp(ls_F)
tsplot(ls_F)
ls_F<-ts(ls_F,start=1990,end=2030)

ls_F<-round(ls_F,digits=4)
ls_F_const<-ls_F

tiff(filename = "ls_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(ls_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(ls_F,                     lwd=3, col="black")
lines(window(ls_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(ls_F, start=2017),type="o", lwd=3, col=culer3)
text(ls_F, labels=ls_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted log(labor supply)', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

Out_ls_const <- change(as.data.frame(ls_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_ls<- change(as.data.frame(ls_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
ls_table_const<-stargazer(Out_ls_const,type="text",summary=F,out="c:/Users/Sara/Desktop/ls_const.txt")
ls_table<-stargazer(Out_ls,type="text",summary=F,out="c:/Users/Sara/Desktop/ls_F.txt")


#lFPR
lfpr_F<-destandardized(Newforecast[,9], mean[9], std[9])
lfpr_F<-diffinv(lfpr_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),9])
lfpr_F<-exp(lfpr_F)
tsplot(lfpr_F)
lfpr_F<-ts(lfpr_F,start=1990,end=2030)

lfpr_F<-round(lfpr_F,digits=4)
lfpr_F_const<-lfpr_F

tiff(filename = "lfpr_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(lfpr_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(lfpr_F,                     lwd=3, col="black")
lines(window(lfpr_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(lfpr_F, start=2017),type="o", lwd=3, col=culer3)
text(lfpr_F, labels=lfpr_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted log(labor supply)', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

Out_lfpr_const <- change(as.data.frame(lfpr_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_lfpr<- change(as.data.frame(lfpr_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
lfpr_table_const<-stargazer(Out_lfpr_const,type="text",summary=F,out="c:/Users/Sara/Desktop/lfpr_const.txt")
lfpr_table<-stargazer(Out_lfpr,type="text",summary=F,out="c:/Users/Sara/Desktop/lfpr_F.txt")

#ter.edu
ter_F<-destandardized(Newforecast[,13], mean[13], std[13])
ter_F<-diffinv(ter_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),13])
ter_F<-exp(ter_F)
tsplot(ter_F)

ter_F<-ts(ter_F,start=1990,end=2030)

ter_F<-round(ter_F,digits=4)
ter_F_const<-ter_F

tiff(filename = "ter_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(ter_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(ter_F,                     lwd=3, col="black")
lines(window(ter_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(ter_F, start=2017),type="o", lwd=3, col=culer3)
text(ter_F, labels=ter_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted proportion of labor force with tertiary education', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

Out_ter_const <- change(as.data.frame(ter_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_ter<- change(as.data.frame(ter_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
ter_table_const<-stargazer(Out_ter_const,type="text",summary=F,out="c:/Users/Sara/Desktop/ter_const.txt")
ter_table<-stargazer(Out_ter,type="text",summary=F,out="c:/Users/Sara/Desktop/ter_F.txt")


#pop
pop_F<-destandardized(Newforecast[,7], mean[7], std[7])
pop_F<-diffinv(pop_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),7])
pop_F<-exp(pop_F)
#tsplot(pop_F)
pop_F<-ts(pop_F,start=1990,end=2030)

pop_F<-round(pop_F,digits=4)
pop_F_const<-pop_F

tiff(filename = "pop_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 10,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(pop_F, type='n', main='', ylab='', xlab='')
#pointLabel(pop_F,cex=1)
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(pop_F,                     lwd=3, col="black")
lines(window(pop_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(pop_F, start=2017),type="o", lwd=3, col=culer3)
text(pop_F, labels=pop_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted population growth', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()

Out_pop_const <- change(as.data.frame(pop_F_const), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
Out_pop<- change(as.data.frame(pop_F), Var = 'x',type = 'proportion',NewVar = 'PercentChange',slideBy = -1)
pop_table_const<-stargazer(Out_pop_const,type="text",summary=F,out="c:/Users/Sara/Desktop/pop_const.txt")
pop_table<-stargazer(Out_pop,type="text",summary=F,out="c:/Users/Sara/Desktop/pop_F.txt")

#ADR
ADR_F<-destandardized(Newforecast[,8], mean[8], std[8])
ADR_F<-diffinv(ADR_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),8])
ADR_F<-exp(ADR_F)
tsplot(ADR_F)

ADR_F<-ts(ADR_F,start=1990,end=2030)

ADR_F<-round(ADR_F,digits=4)
ADR_F_const<-ADR_F

tiff(filename = "ADR_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(ADR_F, type='n', main='', ylab='', xlab='')

rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(ADR_F,                     lwd=3, col="black")
lines(window(ADR_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(ADR_F, start=2017),type="o", lwd=3, col=culer3)
text(ADR_F, labels=ADR_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted age dependency ratio', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()


#capital
cap_F<-destandardized(Newforecast[,12], mean[12], std[12])
cap_F<-diffinv(cap_F,lag=1,differences=2,xi=obj_2r_orig[c(1:2),12])
cap_F<-exp(cap_F)
tsplot(cap_F)
cap_F<-ts(cap_F,start=1990,end=2030)

cap_F<-round(cap_F,digits=4)
cap_F_const<-cap_F

tiff(filename = "cap_F.tiff",
     width = 4000, height = 3000, units = "px", pointsize = 12,
     compression = "lzw",res=400)
par(mar=c(2,3,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(cap_F, type='n', main='', ylab='', xlab='')
#axis(1, at = seq(1990, 2030, by = 1),las=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="grey")
abline(v=2017,col='blue',lty=2)
lines(cap_F,                     lwd=3, col="black")
lines(window(cap_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(cap_F, start=2017),type="o", lwd=3, col=culer3)
text(cap_F, labels=cap_F, cex= 0.5, pos=3)
mtext('Time', side=1, line=2, col="black", font=1, cex=1.25)
mtext('Forecasted log(physical capital)', side=2, line=2, col="black", font=1, las=0, cex=1.25)
dev.off()





#Sparse matrices

#Sparsevar package
y<- matrix(obj_2r_trial[,c(1:16)],26,16)
x<-matrix(obj_2r_trial[,c(17:23)],26,7)

fit<-fitVARX(y, x,p = 3,m = 3,penalty = "ENET",alpha=c(0.5, 0.6, 0.7, 0.8, 0.9,1), method = "cv",h=1,nfolds=3) #

plotVAR(fit)

forecast1<-computeForecasts(fit,20)

A1<-fit[["newA"]][[1]]
A1<-A1[1:16,1:16]
colnames(A1)<-colnames(obj_2r_trial[,1:16])
rownames(A1)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "Lag1_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(A1)
dev.off()



A2<-fit[["newA"]][[2]]
A2<-A2[1:16,1:16]
colnames(A2)<-colnames(obj_2r_trial[,1:16])
rownames(A2)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "Lag2_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(A2)
dev.off()


A3<-fit[["A"]][[3]]
A3<-A3[1:16,1:16]
colnames(A3)<-colnames(obj_2r_trial[,1:16])
rownames(A3)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "Lag3_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(A3)
dev.off()

A4<-fit[["newA"]][[4]]
A4<-A4[1:16,1:16]
colnames(A4)<-colnames(obj_2r_trial[,1:16])
rownames(A4)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "Lag4_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(A4)
dev.off()


prop.table(table(A3!=0))

B1<-fit[["B"]][[1]]
colnames(B1)<-colnames(obj_2r_trial[,17:23])
rownames(B1)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "B1_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(B1)
dev.off()

B2<-fit[["B"]][[2]]
colnames(B2)<-colnames(obj_2r_trial[,17:23])
rownames(B2)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "B2_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(B2)
dev.off()

B3<-fit[["B"]][[3]]
colnames(B3)<-colnames(obj_2r_trial[,17:23])
rownames(B3)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "B3_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(B3)
dev.off()

B4<-fit[["B"]][[4]]
colnames(B4)<-colnames(obj_2r_trial[,17:23])
rownames(B4)<-colnames(obj_2r_trial[,1:16])
tiff(filename = "B4_ENET.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
plotMatrix(B4)
dev.off()

L1<-cbind(A1,B1)
L2<-cbind(A2,B2)
L3<-cbind(A3,B3)
L4<-cbind(A4,B4)
#Impulse reponse function

#The effect of predictors on GDP



tiff(filename = "GDP.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)

par(mfrow=c(3, 3))
par(mar = c(2, 5, 0, 0.5))

plot(irf$oirf[15,12,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(CAP.CURRUSD) - Response (GDP)")
lines(eb$oirfLB[15,12,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[15,12,], type="l",lty=2, lwd=2,col="red")

plot(irf$oirf[15,13,], ylim=c(-0.2,0.2),type="l",xlab="Time",lwd=2,ylab="Impulse(TER.EDU.IMP) - Response (GDP)")
lines(eb$oirfLB[15,13,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[15,13,], type="l",lty=2, lwd=2,col="red")

plot(irf$oirf[15,14,], ylim=c(-0.2,0.2), type="l",lwd=2,xlab="Time",ylab="Impulse(HE.PCAP.USD.IMP) - Response (GDP)")
lines(eb$oirfLB[15,14,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[15,14,], type="l",lty=2, lwd=2,col="red")


plot(irf$oirf[15,1,], ylim=c(-0.2,0.2), type="l",lwd=2,xlab="Time",ylab="Impulse(Cardiovascular diseases) - Response (GDP)")
lines(eb$oirfLB[15,1,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[15,1,], type="l",lty=2, lwd=2,col="red")

plot(irf$oirf[15,2,], ylim=c(-0.2,0.2), type="l",lwd=2,xlab="Time",ylab="Impulse(Chronic respiratory diseases) - Response (GDP)")
lines(eb$oirfLB[15,2,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[15,2,], type="l",lty=2, lwd=2,col="red")



plot(irf$oirf[15,3,], ylim=c(-0.2,0.2), type="l",lwd=2,xlab="Time",ylab="Impulse(Diabetes & kidney diseases) - Response (GDP)")
lines(eb$oirfLB[15,3,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[15,3,], type="l",lty=2, lwd=2,col="red")



plot(irf$oirf[15,4,], ylim=c(-0.2,0.2), type="l",lwd=2,xlab="Time",ylab="Impulse(Neoplasms) - Response (GDP)")
lines(eb$oirfLB[15,4,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[15,4,], type="l",lty=2, lwd=2,col="red")


dev.off()




#What affects burden
#cardio

tiff(filename = "burden.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 12,
     compression = "lzw",res=400)

par(mfrow=c(2, 3))
par(mar = c(3, 5, 0, 0.5))

plot(irf$oirf[1,16,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(HLTH.BUD.IMP) - Response (Cardiovascular diseases)")
lines(eb$oirfLB[1,16,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[1,16,], type="l",lty=2, lwd=2,col="red")

plot(irf$oirf[1,5,], ylim=c(-0.2,0.2),type="l",xlab="Time",lwd=2,ylab="Impulse(LE) - Response (Cardiovascular diseases)")
lines(eb$oirfLB[1,5,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[1,5,], type="l",lty=2, lwd=2,col="red")

plot(irf$oirf[1,8,], ylim=c(-0.2,0.2), type="l",lwd=2,xlab="Time",ylab="Impulse(ADR) - Response (Cardiovascular diseases)")
lines(eb$oirfLB[1,8,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[1,8,], type="l",lty=2, lwd=2,col="red")

#respiratory


plot(irf$oirf[2,16,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(HLTH.BUD.IMP) - Response (Chronic respiratory diseases)")
lines(eb$oirfLB[2,16,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[2,16,], type="l",lty=2, lwd=2,col="red")

plot(irf$oirf[2,14,], ylim=c(-0.2,0.2),type="l",xlab="Time",lwd=2,ylab="Impulse(HE.PCAP.USD.IMP) - Response (Chronic respiratory diseases)")
lines(eb$oirfLB[2,14,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[2,14,], type="l",lty=2, lwd=2,col="red")

#diab

plot(irf$oirf[3,16,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(HE.PCAP.USD.IMP) - Response (Diabetes and kidney diseases)")
lines(eb$oirfLB[3,16,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[3,16,], type="l",lty=2, lwd=2,col="red")

dev.off()


#Demographic variables

tiff(filename = "Indircet det.tiff",
     width = 5000, height = 3500, units = "px", pointsize = 10,
     compression = "lzw",res=400)
par(mfrow=c(3, 4))
par(mar = c(3, 5, 0, 0.5))

#CBR

plot(irf$oirf[6,16,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(HLTH.BUD.IMP) - Response (CBR)")
lines(eb$oirfLB[6,16,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[6,16,], type="l",lty=2, lwd=2,col="red")

#POP growth

plot(irf$oirf[7,16,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(HLTH.BUD.IMP) - Response (POP.GRTH)")
lines(eb$oirfLB[7,16,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[7,16,], type="l",lty=2, lwd=2,col="red")


plot(irf$oirf[7,14,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(HE.PCAP.USD.IMP) - Response (POP.GRTH)")
lines(eb$oirfLB[7,14,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[7,14,], type="l",lty=2, lwd=2,col="red")


plot(irf$oirf[7,6,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(CBR) - Response (POP.GRTH)")
lines(eb$oirfLB[7,6,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[7,6,], type="l",lty=2, lwd=2,col="red")

#ADR 

plot(irf$oirf[8,6,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(CBR) - Response (ADR)")
lines(eb$oirfLB[8,6,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[8,6,], type="l",lty=2, lwd=2,col="red")



#LFPR

plot(irf$oirf[9,1,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(Cardiovascular diseases) - Response (LFPR)")
lines(eb$oirfLB[9,1,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[9,1,], type="l",lty=2, lwd=2,col="red")


#Savings
plot(irf$oirf[10,6,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(CBR) - Response (SAV.CURRUSD)")
lines(eb$oirfLB[10,6,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[10,6,], type="l",lty=2, lwd=2,col="red")

plot(irf$oirf[10,11,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(LS) - Response (SAV.CURRUSD)")
lines(eb$oirfLB[10,11,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[10,11,], type="l",lty=2, lwd=2,col="red")



#LS


plot(irf$oirf[11,3,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(Diabetes and kidney diseases) - Response (LS)")
lines(eb$oirfLB[11,3,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[11,3,], type="l",lty=2, lwd=2,col="red")


#cap

plot(irf$oirf[12,10,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(SAV.CURRUSD) - Response (CAP.CURRUSD)")
lines(eb$oirfLB[12,10,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[12,10,], type="l",lty=2, lwd=2,col="red")


#He per cap

plot(irf$oirf[14,10,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(SAV.CURRUSD) - Response (HE.PCAP.USD)")
lines(eb$oirfLB[14,10,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[14,10,], type="l",lty=2, lwd=2,col="red")


#Health budget


plot(irf$oirf[16,15,], ylim=c(-0.2,0.2),type="l",lwd=2,xlab="Time",ylab="Impulse(GDP.CURRUSD) - Response (HLTH.BUD.IMP)")
lines(eb$oirfLB[16,15,], type="l",lwd=2,lty=2, col="red")
lines(eb$oirfUB[16,15,], type="l",lty=2, lwd=2,col="red")

dev.off()





#Windows in plots
culer1 = rgb(242, 153, 216, max=255)
culer2 = rgb(208,  73, 242, max=255)
culer3 = rgb( 77, 161, 249, max=255)
culer4 = rgb(  0, 200, 225, max=255)
culer5 = rgb(124, 231, 251, max=255)
par(mar=c(2,2,1,1)+2, mgp=c(3,.2,0), las=1, cex.main=2, tcl=0, col.axis="black", bg="white")
plot(GDP_F, type='n', main='', ylab='', xlab='')
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col='white')
grid(lty=1, col="white")
lines(GDP_F,                     lwd=3, col="black")
lines(window(GDP_F, start=1990),type="o", lwd=3, col=culer2)
lines(window(GDP_F, start=2000),type="o", lwd=3, col=culer3)
lines(window(GDP_F, start=2010),type="o", lwd=3, col=culer4)
lines(window(GDP_F, start=2020),lty=3,type="o", lwd=3, col="red")
lines(window(GDP_F, start=2030),lty=3,type="o", lwd=3, col="red")
abline(v=2017,col='blue',lty=2)
text(GDP_F, labels=GDP_F, cex= 0.7, pos=3)     #Add labels
mtext('Time', side=1, line=2, col=culer3, font=2, cex=1.25)
mtext('Disease burden', side=2, line=2, col=culer3, font=2, las=0, cex=1.25)














