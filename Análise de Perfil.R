library(gridExtra)
library(readxl)
library(ggplot2)
library(xtable)
library(lmtest)
library(nortest)
library(GGally)
library(reshape2)
library(MVN)

# Tratamento dos dados
dados=as.data.frame(D25)
dados=dados[,-c(1,9,17,25)]
str(dados)
dados[] = lapply(dados, as.numeric)
trat1=dados[-c(10),c(1:7)]
trat2=dados[-c(9,10),c(8:14)]
trat3=dados[-c(10),c(15:21)]
trat4=dados[,c(22:28)]
names(trat1)=c("T1","T2","T3","T4","T5","T6","T7")
names(trat2)=c("T1","T2","T3","T4","T5","T6","T7")
names(trat3)=c("T1","T2","T3","T4","T5","T6","T7")
names(trat4)=c("T1","T2","T3","T4","T5","T6","T7")

# Normalidade univariada - gráficos
cores <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7") 

nomesv_t1=c("T1 (Trat1)","T2 (Trat1)","T3 (Trat1)","T4 (Trat1)","T5 (Trat1)","T6 (Trat1)","T7 (Trat1)")
nomesv_t2=c("T1 (Trat2)","T2 (Trat2)","T3 (Trat2)","T4 (Trat2)","T5 (Trat2)","T6 (Trat2)","T7 (Trat2)")
nomesv_t3=c("T1 (Trat3)","T2 (Trat3)","T3 (Trat3)","T4 (Trat3)","T5 (Trat3)","T6 (Trat3)","T7 (Trat3)")
nomesv_t4=c("T1 (Trat4)","T2 (Trat4)","T3 (Trat4)","T4 (Trat4)","T5 (Trat4)","T6 (Trat4)","T7 (Trat4)")

qqt1=list()
qqt2=list()
qqt3=list()
qqt4=list()

for (i in 1:7) {
  qqt1[[i]]=eval(substitute(
    ggplot(trat1,aes(sample=trat1[,i])) +
      stat_qq(col="black",size=2) +
      stat_qq_line(col=cores[i],size=1.5)+
      labs(x="Quantis teóricos", y="Quantis amostrais",title=nomesv_t1[i])+
      theme_bw()+
      theme(title = element_text(size = 11),axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9)),
    list(i = i)))
  qqt2[[i]]=eval(substitute(
    ggplot(trat2,aes(sample=trat2[,i])) +
      stat_qq(col="black",size=2) +
      stat_qq_line(col=cores[i],size=1.5)+
      labs(x="Quantis teóricos", y="Quantis amostrais",title=nomesv_t2[i])+
      theme_bw()+
      theme(title = element_text(size = 11),axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9)),
    list(i = i)))
  qqt3[[i]]=eval(substitute(
    ggplot(trat3,aes(sample=trat3[,i])) +
      stat_qq(col="black",size=2) +
      stat_qq_line(col=cores[i],size=1.5)+
      labs(x="Quantis teóricos", y="Quantis amostrais",title=nomesv_t3[i])+
      theme_bw()+
      theme(title = element_text(size = 11),axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9)),
    list(i = i)))
  qqt4[[i]]=eval(substitute(
    ggplot(trat4,aes(sample=trat4[,i])) +
      stat_qq(col="black",size=2) +
      stat_qq_line(col=cores[i],size=1.5)+
      labs(x="Quantis teóricos", y="Quantis amostrais",title=nomesv_t4[i])+
      theme_bw()+
      theme(title = element_text(size = 11),axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9)),
    list(i = i)))
}
grid.arrange(qqt1[[1]],qqt1[[2]],qqt1[[3]],qqt1[[4]],qqt1[[5]],qqt1[[6]],qqt1[[7]], ncol=4)
grid.arrange(qqt2[[1]],qqt2[[2]],qqt2[[3]],qqt2[[4]],qqt2[[5]],qqt2[[6]],qqt2[[7]], ncol=4)
grid.arrange(qqt3[[1]],qqt3[[2]],qqt3[[3]],qqt3[[4]],qqt3[[5]],qqt3[[6]],qqt3[[7]], ncol=4)
grid.arrange(qqt4[[1]],qqt4[[2]],qqt4[[3]],qqt4[[4]],qqt4[[5]],qqt4[[6]],qqt4[[7]], ncol=4)

# Normalidade univariada - testes
uninormal_t1=c()
uninormal_t2=c()
uninormal_t3=c()
uninormal_t4=c()

for (i in 1:7) {
  uninormal_t1[i]=c(shapiro.test(trat1[,i])$p.value)
  uninormal_t2[i]=c(shapiro.test(trat2[,i])$p.value)
  uninormal_t3[i]=c(shapiro.test(trat3[,i])$p.value)
  uninormal_t4[i]=c(shapiro.test(trat4[,i])$p.value)
}
uninormal <- rbind(uninormal_t1, uninormal_t2, uninormal_t3, uninormal_t4)
uninormal



# Normalidade multivariada
mvn(trat1, mvnTest = "mardia", univariateTest = "SW")$multivariateNormality
mvn(trat2, mvnTest = "mardia", univariateTest = "SW")$multivariateNormality
mvn(trat3, mvnTest = "mardia", univariateTest = "SW")$multivariateNormality
mvn(trat4, mvnTest = "mardia", univariateTest = "SW")$multivariateNormality

mvn(trat1, mvnTest = "hz", univariateTest = "SW")$multivariateNormality
mvn(trat2, mvnTest = "hz", univariateTest = "SW")$multivariateNormality
mvn(trat3, mvnTest = "hz", univariateTest = "SW")$multivariateNormality
mvn(trat4, mvnTest = "hz", univariateTest = "SW")$multivariateNormality

mvn(trat1, mvnTest = "royston", univariateTest = "SW")$multivariateNormality
mvn(trat2, mvnTest = "royston", univariateTest = "SW")$multivariateNormality
mvn(trat3, mvnTest = "royston", univariateTest = "SW")$multivariateNormality
mvn(trat4, mvnTest = "royston", univariateTest = "SW")$multivariateNormality



# Igualdade var-cov
library(multiUS)
BoxMTest(X = rbind(trat1,trat2,trat3,trat4), cl = as.factor(c(rep("Trat1",9),rep("Trat2",8),rep("Trat3",9),rep("Trat4",10))),
         alpha = 0.01,test = "ChiSq")
BoxMTest(X = rbind(trat1,trat2,trat3,trat4), cl = as.factor(c(rep("Trat1",9),rep("Trat2",8),rep("Trat3",9),rep("Trat4",10))),
         alpha = 0.01,test = "F")



# Análise de perfil
library(profileR)
trat1$Grupo <- "Trat1"
trat2$Grupo <- "Trat2"
trat3$Grupo <- "Trat3"
trat4$Grupo <- "Trat4"

dados <- rbind(trat1, trat2, trat3, trat4)
respostas <- dados[, c("T1", "T2", "T3", "T4","T5","T6","T7")]

teste_paralelismo <- pbg(respostas, dados$Grupo)
summary(teste_paralelismo)

# Comparações múltiplas
library(emmeans)
# Código fonte da construção da manova retirado da função pbg
x <- as.data.frame(respostas)
y <- as.factor(dados$Grupo)
k = ncol(x)
cnt <- c(1, -1, rep(0, k - 2))
for (i in 2:(k - 1)) {
  cnt <- c(cnt, c(rep(0, (i - 1)), 1, -1, rep(0, (k - 
                                                    i - 1))))
}
cont1 <- matrix(cnt, nrow = k, ncol = (k - 1))
xm <- as.matrix(x)
deviation1 <- xm %*% cont1
fit1 <- manova(deviation1 ~ y, na.action = na.omit)

emm <- emmeans(fit1, ~ y)
comparacoes <- pairs(emm)
comparacoes