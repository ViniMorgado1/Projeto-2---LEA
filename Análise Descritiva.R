library(dplyr)
library(nortest)
library(ggplot2)
library(gridExtra)
dados_d2<- read_excel("D25.xlsx")
potassio <- c(dados_d2$T1, dados_d2$T2,dados_d2$T3,dados_d2$T4,dados_d2$T5,dados_d2$T6,dados_d2$T7)
tratamentos <- as.factor(c(rep("1", 9), rep("2", 8), rep("3", 9), rep("4", 10)))
individuos <- as.factor(seq(1,36,1))
tempos<- as.factor(c(rep("T1", 36), rep("T2", 36), rep("T3", 36), rep("T4", 36),rep("T5", 36),rep("T6", 36),rep("T7", 36)))

# tabela
tabela <- data.frame(individuos, tratamentos, tempos, potassio)
tabela

plot1 <- tabela %>% filter(tratamentos == "1") %>%
  ggplot(aes(x = as.numeric(tempos), y = potassio, color = individuos)) +
  geom_point() +
  geom_line() +
  ggtitle("Tratamento 1") + 
  labs(x = "Tempo (minutos)", y = "Nível de Potássio") +
  theme_minimal() +
  theme(legend.position = "none")

plot2 <- tabela %>% filter(tratamentos == "2") %>%
  ggplot(aes(x = as.numeric(tempos), y = potassio, color = individuos)) +
  geom_point() +
  geom_line() +
  ggtitle("Tratamento 2") + 
  labs(x = "Tempo (minutos)", y = "Nível de Potássio") +
  theme_minimal() +
  theme(legend.position = "none")

plot3 <- tabela %>% filter(tratamentos == "3") %>%
  ggplot(aes(x = as.numeric(tempos), y = potassio, color = individuos)) +
  geom_point() +
  geom_line() +
  ggtitle("Tratamento 3") + 
  labs(x = "Tempo (minutos)", y = "Nível de Potássio") +
  theme_minimal() +
  theme(legend.position = "none")

plot4 <- tabela %>% filter(tratamentos == "4") %>%
  ggplot(aes(x = as.numeric(tempos), y = potassio, color = individuos)) +
  geom_point() +
  geom_line() +
  ggtitle("Tratamento 4") + 
  labs(x = "Tempo (minutos)", y = "Nível de Potássio") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(plot1, plot2, plot3, plot4, nrow = 4)


#boxplot - tempos e tratamentos
tabela %>% 
  ggplot(aes(x=tempos,y=potassio,color=tratamentos)) +
  geom_boxplot()+
  ylab("Potássio")+
  theme_bw() +  
  xlab("Tempos")

# medidas descritivas
calcular_medidas <- function(dados_tratamento) {
  dados_tratamento %>%
    group_by(tempos) %>%
    summarise(
      Minimo = min(potassio, na.rm = TRUE),
      Q1 = quantile(potassio, 0.25, na.rm = TRUE),
      Mediana = median(potassio, na.rm = TRUE),
      Media = mean(potassio, na.rm = TRUE),
      Q3 = quantile(potassio, 0.75, na.rm = TRUE),
      Maximo = max(potassio, na.rm = TRUE),
      Variancia = var(potassio, na.rm = TRUE)
    )
}


tabela_tratamento_1 <- tabela %>% filter(tratamentos == "1") %>% calcular_medidas()
tabela_tratamento_2 <- tabela %>% filter(tratamentos == "2") %>% calcular_medidas()
tabela_tratamento_3 <- tabela %>% filter(tratamentos == "3") %>% calcular_medidas()
tabela_tratamento_4 <- tabela %>% filter(tratamentos == "4") %>% calcular_medidas()


#gráfico de interação
interaction.plot(x.factor     = tabela$tempos,
                 trace.factor = tabela$tratamentos,
                 response     = tabela$potassio,
                 fun = mean,
                 type="b",
                 col=c("red","green","skyblue","purple"),  
                 pch=c(19, 17),             
                 fixed=TRUE,                    
                 leg.bty = "o",
                 ylab="Potássio",
                 xlab="Tempos")