
x <- c(506, 508, 499, 503, 504, 510, 497, 512, 514, 505, 493, 496, 506, 502, 509, 496)
#1metodo maxima verosimilitud+momentos
#2intervalo de confianza
#3test de hipotesis
#intervalos de confianza
#caso 1->condiciomes
#x normal
#conozco sigma
#IC para la media al 95%
#margen de error->z0.025(qnorm(0.975))*sigma/sqrt(n
#fórmula de IC->
#a
xbar<-mean(x)
sigma<-sqrt(25)
n<-length(x)
z005<-qnorm(0.95)#alpha=0.1, confianza 0.9 (90%)
c(xbar-z005*sigma/sqrt(n),xbar+z005*sigma/sqrt(n))#interval de confianza
#instalar libreria
install.packages("BSDA")
library(BSDA)
z.test(x,sigma.x=sigma,conf.level=0.9)#calcul directe de interval de confianza
#hipotesis
#hipotesi nula mu=mu0
#hipotesi alternativa mu no igual a mu0
#probar la hipotesis de que las cajas tienen un peso diferente a 500 gramos
#H0:mu=500
#H1:mu!=500(mu0=500)
zc<-qnorm(0.95)#zcrítico
zc
mu0<-500
zobs<-(xbar-mu0)/(sigma/sqrt(n))
zobs
z.test(x,sigma.x=sigma,conf.level=0.9,mu=mu0)
pvalue<-2*pnorm(-zobs)
pvalue
#para cola superior
z.test(x,sigma.x=sigma,conf.level=0.9,mu=mu0,alternative="greater")#less(cola inferior)
###
n<-(qnorm(0.975)*sigma)^2
n
z025<-qnorm(0.975)
c(xbar-z025*sigma/sqrt(n),xbar+z025*sigma/sqrt(n))
#caso 2 IC99%
xbar<-mean(x)
n<-length(x)
t005<-qt(0.995,n-1)
s<-sd(x)
c(xbar-t005*s/sqrt(n),xbar+t005*s/sqrt(n))
s
t.test(x,conf.level=0.99)#calcula directament
#hiotesis
#H0:mu=mu0=500
#H1:mu!=mu0=500 (dos colas=two.sided)
t.test(x,alternative="two.sided",conf.level=0.99,mu=500)
