library(tidyverse)

datos<- data.frame(Tratamiento1=c(-3.10,0.18,-0.72,0.09,-1.66),
                  Tratamiento2=c(7.28,3.06,4.74,5.29,7.88),
                  Tratamiento3=c(0.12,5.51,5.72,5.93,6.56),
                  Tratamiento4=c(8.18,9.05,11.21,7.31,8.83))

ro1<- datos$Tratamiento1-mean(datos$Tratamiento1)
ro2<- datos$Tratamiento2-mean(datos$Tratamiento1)
ro3<- datos$Tratamiento3-mean(datos$Tratamiento1)
ro4<- datos$Tratamiento4-mean(datos$Tratamiento1)


shapiro.test(c(ro1,ro2, ro3, ro4))

library(tidyverse)
datost<- datos %>% pivot_longer(cols = everything(), names_to = "Tratamiento", values_to = "Valor")

lmtest::bptest(lm(Valor~Tratamiento, data = datost))



SCT1= 5*(mean(datos$Tratamiento1)-mean(as.matrix(datos)))^2
SCT2= 5*(mean(datos$Tratamiento2)-mean(as.matrix(datos)))^2
SCT3= 5*(mean(datos$Tratamiento3)-mean(as.matrix(datos)))^2
SCT4= 5*(mean(datos$Tratamiento4)-mean(as.matrix(datos)))^2

SCT= (SCT1+SCT2+SCT3+SCT4); SCT

SCE1= sum((datos$Tratamiento1-mean(datos$Tratamiento1))^2)
SCE2= sum((datos$Tratamiento2-mean(datos$Tratamiento2))^2)
SCE3= sum((datos$Tratamiento3-mean(datos$Tratamiento3))^2)
SCE4= sum((datos$Tratamiento4-mean(datos$Tratamiento4))^2)

SCE = (SCE1+SCE2+SCE3+SCE4); SCE

STT1= sum((datos$Tratamiento1-mean(as.matrix(datos)))^2)
STT2= sum((datos$Tratamiento2-mean(as.matrix(datos)))^2)
STT3= sum((datos$Tratamiento3-mean(as.matrix(datos)))^2)
STT4= sum((datos$Tratamiento4-mean(as.matrix(datos)))^2)

STT = (STT1+STT2+STT3+STT4); STT

CMT=SCT/(4-1);CMT

CME=SCE/(20-4);CME

valor_F= CMT/CME; valor_F

valor_p<-pf(valor_F, df1=4-1, df2=20-4, lower.tail=FALSE); valor_p


anova_R<- aov(Valor~Tratamiento, data = datost)
summary(anova_R)    


valor_F;valor_p


qcri<-qtukey(p = 0.95, nmeans = 4, df = 16)

sqme<- sqrt(CME/5)

HSD_val<- qcri*sqme


qcri; HSD_val

dif1<- mean(datos$Tratamiento3)-mean(datos$Tratamiento1)
dif2<- mean(datos$Tratamiento2)-mean(datos$Tratamiento1)
dif3<- mean(datos$Tratamiento4)-mean(datos$Tratamiento1)
dif4<- mean(datos$Tratamiento2)-mean(datos$Tratamiento3)
dif5<- mean(datos$Tratamiento4)-mean(datos$Tratamiento3)
dif6<- mean(datos$Tratamiento4)-mean(datos$Tratamiento2)


dif1>HSD_val;dif2>HSD_val;dif3>HSD_val;dif4>HSD_val;dif5>HSD_val;dif6>HSD_val
up<-dif1+(qcri*sqme)
low<-dif1-(qcri*sqme)
data.frame(up,low)

anov<- aov(Valor~Tratamiento,data = datost)
TukeyHSD(anov, "Tratamiento", ordered = T)

plot(TukeyHSD(anov, "Tratamiento", ordered = T))

library(agricolae)
hsd_agricolae<-HSD.test(anov, "Tratamiento", group = TRUE)
plot(hsd_agricolae)


library(agricolae)
SNK.test(anov, "Tratamiento", console = TRUE)

library(agricolae)
snk<-SNK.test(anov, "Tratamiento", console = FALSE)
plot(snk)

library(agricolae)
duncan.test(anov, "Tratamiento", console = TRUE)

library(agricolae)
duncan<-agricolae::duncan.test(anov, "Tratamiento")
plot(duncan)

data("PlantGrowth")
