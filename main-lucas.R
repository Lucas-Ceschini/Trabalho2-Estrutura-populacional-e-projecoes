library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)


manipulacao<-function(dados){
  dados$`Faixa Etária`<-str_replace_all(dados$`Faixa Etária`,'anos','')
  dados$`Faixa Etária`<-str_replace_all(dados$`Faixa Etária`,'ano','')
  dados$`Faixa Etária`<-str_replace_all(dados$`Faixa Etária`,'Menor','0 -')
  dados$`Faixa Etária`<-str_replace_all(dados$`Faixa Etária`,' a ',' - ')
  dados$`Faixa Etária`<-str_replace_all(dados$`Faixa Etária`,'e mais','+')
  dados$`Faixa Etária`<-str_replace_all(dados$`Faixa Etária`,'1 - 4','01 - 04')
  dados$`Faixa Etária`<-str_replace_all(dados$`Faixa Etária`,'5 - 9','05 - 09')
  masculino<-dados[,1:2]
  masculino$sexo<-c('Masculino')
  feminino<-dados[,c(1,3)]
  feminino$sexo<-c('Feminino')
  colnames(feminino)[2] <- "pop"
  colnames(masculino)[2] <- "pop"
  piramide<-rbind(masculino,feminino)
  return(piramide)
}


grafico<-function(dado){
  x<-ggplot(data = dado, 
         mapping = aes(x = `Faixa Etária`,
                       y = ifelse(test = sexo == "Feminino",  yes = pop, no = -pop), fill = sexo)) +
    geom_bar(stat = "identity") +
    #scale_y_continuous(labels = abs, limits = (max(dado$pop))* c(-1,1)) +
    labs(y = "População", x = "Faixa etária (em anos)") +
    scale_fill_brewer(palette = "Set1", direction = -1) +
    coord_flip()
  return(x)
}


# Ano 1991
dado<-read_excel('pop1991.xlsx')
dado<-dado[-13,]
grafico(manipulacao(dado))


# Ano 2000
dado<-read_excel('pop2000.xlsx')
dado<-dado[-13,]
grafico(manipulacao(dado))

# Ano 2010
dado<-read_excel('pop2010.xlsx')
dado<-dado[-13,]
grafico(manipulacao(dado))

# Ano 2015
dado<-read_excel('pop2015.xlsx')
colnames(dado)[1] <- "Faixa Etária"
dado<-dado[c(-12,-13),]
grafico(manipulacao(dado))

# Ano 2020
dado<-read_excel('pop2020.xlsx')
colnames(dado)[1] <- "Faixa Etária"
dado<-dado[-12,]
grafico(manipulacao(dado))

# Ano 2030
dado<-read_excel('pop2030.xlsx')
colnames(dado)[1] <- "Faixa Etária"
dado<-dado[-12,]
grafico(manipulacao(dado))



 