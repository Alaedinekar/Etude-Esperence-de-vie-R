##part1



EspVie = read.table("EsperanceVie.csv",sep=",",header=TRUE)
esp_fra = which(EspVie$Pays=='FRA')
y = EspVie$Esperance[esp_fra]
x = EspVie$Annee[esp_fra]
reg_lin <- lm(y~x)
cor(y,x)
summary(reg_lin)
c = data.frame(x = c(2020,2025,2030,2050))
predict(reg_lin,c)
plot(x,y,xlab='Année',ylab='Esperance', pch = "x",col='blue',main='Regression esperance et année France')
abline(reg_lin)

esp_jpn = which(EspVie$Pays=='JPN')
y = EspVie$Esperance[esp_jpn]
x = EspVie$Annee[esp_jpn]
reg_lin <- lm(y~x)
cor(y,x)
summary(reg_lin)
f1 = data.frame(x = c(2020,2025,2030,2050))
predict(reg_lin,f1)
dev.new()
plot(x,y,xlab='Année',ylab='Esperance',col='blue',main='Regression esperance et année Japon')
abline(reg_lin)


##part2

DepSante=read.table("DepSante2015.csv",sep=",",header=TRUE)
x=DepSante$Esperance
y=DepSante$Pays
z=DepSante$Depense
barplot(x,names=y,ylab="esperance de vie",xlab="Pays",main="Esperance de vie en fonction des pays")
barplot(x,names=z,ylab="esperance de vie",xlab="depense Santé",main="Esperance de vie en fonction des dépenses santé")
mean(x)
mean(z)
sd(z)
median(z)
quantile(z)
quantile(z,0.75) - quantile(z,0.25)
sd(z)
dev.new()
hist(z,breaks=c(500,1500,2500,3500,4500,5500,10000))

reg_lin2 <- lm(x~z)
plot(x,z,xlab='esperence de vie',ylab='depense santé',col='red',main= "graphique montrant l'evolution de l'esperance de vie en fonction des depenses santé")

t <- seq(70,90,0.1)
Exi <- sum(x)
Y1 = log(z)
Excarre<- sum(x^2)

n<- length(x)

k2<- (sum(Y1*x)- (Exi*sum(Y1)/n))/(Excarre - ((Exi)^2)/n)

k1<- (sum(Y1)/n) - (k2*(Exi/n))

w <- exp(k1 + (k2*t)) 

lines(t,w,col="black")

reg_lin3<-lm(x~Y1)
abline(reg_lin3)
reg_lin3$coef












