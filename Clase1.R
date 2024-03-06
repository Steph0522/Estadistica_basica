library(tidyverse)

set.seed(123)


data_normal<- rnorm(200)
hist(data_normal, col='steelblue', main='Normal')

set.seed(1254)

data_no_normal<- rexp(100, rate=3)
hist(data_no_normal, col='red', main='No normal')

set.seed(123)


plot(density(data_normal), main="Normal")

plot(density(data_no_normal), main="No Normal")

set.seed(123)


qqnorm(data_normal)
qqline(data_normal)

qqnorm(data_no_normal)
qqline(data_no_normal)

set.seed(124)


shapiro.test(data_normal)

ks.test(data_normal, "pnorm")

shapiro.test(data_no_normal)


ks.test(data_no_normal, "pnorm")

data("ToothGrowth")
data("iris")

data("ToothGrowth")
boxplot(len ~ supp, data=ToothGrowth, col=c("red", "blue"), main="Dientes")

data("iris")
boxplot(Petal.Width ~ Species, data=iris, col=c("pink", "purple", "cyan"), main="Flores")

aggregate(len ~ supp, data = ToothGrowth, var)

68.32 /  43.63

aggregate(Petal.Width ~ Species, data = iris, var)


r1<-0.03910612 / 0.01110612 #versicolor vs setosa
r2<-0.07543265 / 0.01110612 #virginca vs setosa
r3<-0.07543264 / 0.03910612 #virginica vs versicolor
cbind(r1,r2,r3)

m1<-lm(len ~ supp, data=ToothGrowth)
par(mfrow = c(1, 2))
plot(m1, which=c(1,3))

m2<-lm(Petal.Width ~ Species, data=iris)
par(mfrow = c(1, 2))
plot(m2, which=c(1,3))


var.test(len ~ supp, data = ToothGrowth) 


lmtest::bptest(m2) #sobre un modelo

library(car)
leveneTest(m2)

## ?fligner.test

data<- data.frame(Congelador=1:10,
                  Temperatura=c(-2.14,-0.8,
                                -2.75,-2.58,   
                                -2.26,-2.46,
                                -1.33,-2.85,
                                -0.93,-2.01))
shapiro.test(data$Temperatura)$p.value

prom = mean(data$Temperatura); desves = sd(data$Temperatura); sn = sqrt(nrow(data));mu = 0
t=(prom-mu)/(desves/sn)
t;pt(t, df=9)

t.test(data$Temperatura, mu = 0, alternative = "less")

data_pareada<- data.frame(Pan=1:10,
                         Temp_pre=c(20.83,19.72,19.64,20.09,22.25,
                                   20.83,21.31,22.50,21.17,19.57),
                         Temp_post=c(100.87,98.58,109.09,121.83,122.78,
                                     111.41,103.96,121.81,127.85,115.17))

data_pareada$Diferencia<- data_pareada$Temp_post - data_pareada$Temp_pre
                                

knitr::kable(data_pareada, align = "c")

shapiro.test(data_pareada$Diferencia)

prom = mean(data_pareada$Diferenci); desves = sd(data_pareada$Diferenci); sn = sqrt(nrow(data_pareada));mu = 0
t=(prom-mu)/(desves/sn)
t;pt(t, df=9, lower.tail = FALSE)

t.test(data_pareada$Temp_post, data_pareada$Temp_pre, mu = 0, alternative = "greater", paired = TRUE)

freidora<- data.frame(
  Oster1=c(4.72,7.40,3.50,3.85,4.47,
           4.09,6.34,3.30,7.13,4.99),
  Oster2=c(9.19,11.44,9.64,12.09,10.80,
           12.71,10.04,9.06,6.31,9.44))
ro1<- freidora$Oster1-mean(freidora$Oster1)
ro2<- freidora$Oster2-mean(freidora$Oster2)

shapiro.test(c(ro1,ro2))

knitr::kable(freidora, align = "c", format = "pipe", padding = 0)

prom1 = mean(freidora$Oster1)
prom2=mean(freidora$Oster2)

s=((var(freidora$Oster1)*9)+(var(freidora$Oster2)*9))/(10+10-2)
t=(prom1-prom2-0)/(sqrt(s/10+s/10))
t; pt(-6.8527, df=18, lower.tail=TRUE)+pt(6.8527, df=18, lower.tail=FALSE)

t.test(freidora$Oster1, freidora$Oster2, mu=0, paired=FALSE, alternative="two.sided", var.equal=TRUE)

t.test(freidora$Oster1, freidora$Oster2, mu=0, paired=FALSE, alternative="two.sided", var.equal=FALSE)

data_u<- data.frame(Control=c(12, 22, 38, 42, 50),
                    Tratamiento=c(14, 25, 40, 44, 48))


knitr::kable(data_u, align = "c")

dat<- data.frame(Tipo= c("C", "E","C", "E","C", "E", "C", "E", "E", "C"), Valor=c(12,14,22,25,38,40,42,44,48,50))
knitr::kable(dat, align = "c")


dat2<-data.frame(C=12, E=14, C=22, E=25, C=38, E=40, C=42, E=44, E=48, C=50, check.names = F)
knitr::kable(dat2, align = "c")

wilcox.test(data_u$Control, data_u$Tratamiento, paired = FALSE)

data_w<- data.frame(Observ=c(1:11),
                    Antes=c(1,8,0,0,1,3,3,3,5,6,5),
                    Después=c(3,5,-2,-1,1,-5,-7,-362,-1,0,-10))
data_w$Diferencia<- data_w$Antes-data_w$Después


knitr::kable(data_w, align = "c")%>% 
  kableExtra::kable_paper(full_width = F)

knitr::kable(data_w %>% dplyr::arrange(-Diferencia), align = "c")%>% 
  kableExtra::kable_paper(full_width = F)

dato<- data_w %>% dplyr::arrange(-Diferencia) %>% mutate(Rango=c(10,9,8,7,5.5,5.5,4,2.5,1,0,2.5)) 
knitr::kable(dato, align = "c")%>% 
  kableExtra::kable_paper(full_width = F)

dato<- data_w %>% dplyr::arrange(-Diferencia) %>% mutate(Rango=c(10,9,8,7,5.5,5.5,4,2.5,1,0,2.5)) %>% mutate(Signo=c("+", "+", "+","+", "+", "+","+","+","+","=","-"), Rango_Signo=paste0(Signo,Rango)) 
knitr::kable(dato, align = "c")%>% 
  kableExtra::kable_paper(full_width = F)

T_pos = 10+9+8+7+5.5+5.5+4+2.5+1
T_neg = 2.5

T_pos; T_neg
Two_tailed_W = max(T_pos,T_neg)
Two_tailed_W

wilcox.test(data_w$Antes, data_w$Después, paired = TRUE)

Grupo1 <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
Grupo2 <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 

my_data <- data.frame( 
                group = rep(c("G1", "G2"), each = 9),
                weight = c(Grupo1,  Grupo2)
                )


with(my_data, shapiro.test(weight[group == "G1"]))

#Prueba de normalidad
with(my_data, shapiro.test(weight[group == "G2"])) 

Grupo1 <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
Grupo2 <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 

my_data <- data.frame( 
                group = rep(c("G1", "G2"), each = 9),
                weight = c(Grupo1,  Grupo2)
                )


var.test(weight ~ group, data = my_data)

Grupo1 <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
Grupo2 <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 

my_data <- data.frame( 
                group = rep(c("G1", "G2"), each = 9),
                weight = c(Grupo1,  Grupo2)
                )


t.test(Grupo1, Grupo2, var.equal = TRUE)
