#Q1)----------------------------------------------------------------------------
print("Questao 1")
dados <- c(3600, 3545, 3658, 3498, 3657, 3425, 3785, 3254, 3266, 3641,
           3687, 3698, 3621, 3654, 3554, 3569, 3598, 3578, 3567, 3574)
           
media <- mean(dados)
print("Media")
print(media)

variancia <- var(dados)
print("variancia")
print(variancia)

desvio_padrao <- sd(dados)
coeficiente_variacao <- (desvio_padrao / media) * 100
print("coeficiente de variacao")
print(coeficiente_variacao)


#Q2)-------------------------------------------------------------------------------

print("Questao 2")
dados <- c(4.0, 4.5, 5.0, 5.0, 5.0, 5.5, 6.0, 6.0, 6.5, 6.5, 6.5,
            6.5, 7.0, 7.0, 7.0, 7.0, 7.0, 7.0, 7.5, 8.5, 9.0, 9.0,
            9.0, 9.5, 10.0, 10.0, 10.5, 10.5, 11.0, 12.0, 12.5, 13.0, 13.0)
           
media <- mean(dados)


variancia <- var(dados)
print("Variancia")
print(variancia)

desvio_padrao <- sd(dados)
coeficiente_variacao <- (desvio_padrao / media) * 100
print("coeficiente de variacao")
print(coeficiente_variacao)


#Q3)------------------------------------------------------------------------------

print("Questao 3")
producao <- data.frame(Ano = 1992:2001, Ton = c(12, 15, 18, 22, 17, 14, 18, 23, 29, 12))
media <- mean(producao$Ton)
print("Media")
media
mediana <- median(producao$Ton)
print("Mediana")
mediana
desvio_padrao <- sd(producao$Ton)
print("Desvio padrao")
desvio_padrao


#Q4)----------------------------------------------------------------------------------

print("Questao 4")
dados <- c(3.83, 4.08, 4.59, 5.80, 7.81, 6.31, 8.37, 7.50, 9.30, 5.98,
           6.78, 5.23, 7.50, 6.62, 2.80, 6.51, 5.27, 5.44, 6.08, 6.66,
           8.49, 3.86, 2.91, 5.82, 7.47, 6.52, 6.61, 7.80, 5.62, 8.23)
           
hist(dados)



#Q5)----------------------------------------------------------------------------------

print("Questao 5")
producao <- c(9.3, 7.8, 8.3, 10.1, 10.2, 9.5, 8.7, 9.0, 8.7, 9.7,
              9.1, 8.8, 3.6, 9.4, 3.6, 8.9, 9.2, 9.4, 11.4, 3.1,
              9.6, 3.1, 2.0, 9.8, 8.7, 9.0, 8.6, 9.2, 10.1, 9.3)

media <- mean(producao)
print("Media")
media

mediana <- median(producao)
print("Mediana")
mediana

boxplot(producao)

hist(producao)


#Q6)----------------------------------------------------------------------------------

print("Questao 6")
milho <- c(82.1, 74.9, 80.4, 85.3, 90.5, 82.4, 85.1, 82.7, 75.4, 80.7,
80.8, 82.7, 89.1, 87.2, 82.1, 81.6, 86.8, 86.1, 79.2, 79.1)

media <- mean(milho)
print("Media")
media

mediana <- median(milho)
print("Mediana")
mediana


hist(milho)


#Q7)--------------------------------------------------------------------------------

print("Questao 7")

ganho <- c(2.94, 3.38, 2.49, 3.52, 2.97, 2.09, 2.91, 1.74, 4.27, 5.17,
           2.27, 1.79, 3.16, 2.47, 5.99, 2.55, 3.29, 2.61, 1.99, 2.76)

media <- mean(ganho)
print("Media")
media

mediana <- median(ganho)
print("Mediana")
mediana

quartis <- quantile(ganho, probs = c(0.25, 0.5, 0.75), type=2)
print("Quartis")
quartis


hist(ganho)

#Q8)--------------------------------------------------------------------------------------------

print("Questao 8")
dados <- data.frame(
  Intervalo = c("[8,10)", "[10,12)", "[12,14)", "[14,16)", "[16,18)", "[18,20]"),
  fi = c(3, 9, 7, NA, 4, 3),
  Fi = c(10, 12, NA, 27, NA, 30),
  fr = c(NA, 30.0, NA, 76.7, 90.0, 10.0),
  Fr = c(10.0, 40.0, NA, 76.7, NA, 100.0)
)

dados$Intervalo <- factor(dados$Intervalo, ordered = TRUE)

dados$fi[4] <- dados$Fi[4] - dados$Fi[3]
dados$fi[3] <- dados$Fi[3] - dados$Fi[2]
dados$fi[6] <- dados$Fi[6] - dados$Fi[5]

dados$fr <- dados$fi / sum(dados$fi)
dados$Fr <- cumsum(dados$fr)

dados$Intervalo_num <- as.numeric(levels(dados$Intervalo))[dados$Intervalo]

media <- sum(dados$Intervalo_num * dados$fr)
print("Media")
media


#Q9)------------------------------------------------------------------------------------

print("Questao 9")
pesos <- c(961, 967, 971, 974, 979, 982, 992, 996, 997, 998,
           998, 999, 1000, 1000, 1002, 1002, 1002, 1003, 1004, 1004,
           1005, 1005, 1007, 1007, 1008, 1009, 1009, 1009, 1010, 1013,
           1014, 1016, 1016, 1017, 1017, 1020, 1021, 1021, 1022, 1024,
           1025, 1026, 1027, 1028, 1028, 1028, 1029, 1030, 1030, 1030,
           1030, 1031, 1031, 1032, 1032, 1032, 1033, 1034, 1034, 1034,
           1038, 1038, 1039, 1040, 1041, 1041, 1041, 1042, 1049, 1052,
           1054, 1056, 1065, 1069, 1071, 1072, 1079, 1082, 1083, 1084)


limite_A <- quantile(pesos, probs = 0.2)
limite_B <- quantile(pesos, probs = 0.5)
limite_C <- quantile(pesos, probs = 0.8)
limite_D <- max(pesos)

print("quantile do limite_A")
limite_A
print("quantile do limite_B")
limite_B
print("quantile do limite_C")
limite_C
print("Maximo de pesos do limite_D")
limite_D


#Q10)--------------------------------------------------------------------------------------

print("Questao 10")

loc1 <- c(9.4, 9.2, 10.2, 12.5, 12.2, 10.5, 12.1, 10.2, 11.4, 10.1, 10.1, 8.7, 8.5, 6.5, 9.6)

loc2 <- c( 10.8, 11.7, 11.5, 11.7, 11, 11.9, 11.3, 11.7,
10.5, 10.7, 11.6, 12.4, 11.8, 12.4, 12.5)


media1 <- mean(loc1)
media2 <- mean(loc2)

print("media da Localidade 1: ")
media1

print("media da localidade 2: ")
media2


var1 <- var(loc1)
var2 <- var(loc2)

print("variancia da Localidade 1: ")
var1

print("variancia da localidade 2: ")
var2



dev1 <- sd(loc1)
dev2 <- sd(loc2)

print("desvio padrao da Localidade 1: ")
dev1

print("desvio padrao da localidade 2: ")
dev2

boxplot(loc1, loc2)



#Q11)-------------------------------------------------------------------------------------

print("Questao 11")

mediaB <- 95.8
VariaciaA <- 39.6

coeficiente_variacaoA <- 7.266574
coeficiente_variacaoB <- 8.647028


mediaA <- (sqrt(VariaciaA) / coeficiente_variacaoA) * 100
print("Media de A")
mediaA


VariaciaB <- (coeficiente_variacaoB * mediaB) / 100
print("Variacia de B")
VariaciaB

#Q12)---------------------------------------------------------------------------------------

print("Questao 12 letra A")

#A)-------------------------------------------------------------------------------------------------
# Dados fornecidos

  estado_civil <- c("Solteiro", "Casado", "Casado", "Solteiro", "Solteiro", "Casado", "Solteiro", "Solteiro", "Casado", "Solteiro", "Casado", "Solteiro", "Solteiro", "Casado", "Casado", "Solteiro", "Casado", "Casado", "Solteiro", "Solteiro", "Casado", "Solteiro", "Solteiro", "Casado", "Casado", "Casado", "Solteiro", "Casado", "Casado", "Casado", "Solteiro", "Casado", "Casado", "Solteiro", "Casado", "Casado")
  grau_instrucao <- c("Ens. Fund.", "Ens. Fund.", "Ens. Fund.", "Ens. Médio", "Ens. Fund.", "Ens. Fund.", "Ens. Fund.", "Ens. Fund.", "Ens. Médio", "Ens. Médio", "Ens. Médio", "Ens. Fund.", "Ens. Médio", "Ens. Fund.", "Ens. Médio", "Ens. Médio", "Ens. Médio", "Ens. Fund.", "Superior", "Ens. Médio", "Ens. Médio","Ens. Médio", "Ens. Fund.", "Superior", "Ens. Médio", "Ens. Médio", "Ens. Fund.", "Ens. Médio", "Ens. Médio", "Ens. Médio", "Superior", "Ens. Médio", "Superior", "Superior", "Ens. Médio", "Superior")
  num_filhos <- c(0, 1, 2, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 3, 0, 0, 1, 2, 0, 0, 1, 0, 0, 0, 2, 2, 0, 0, 5, 2, 0, 1, 3, 0, 2, 3)
  salario <- c(4.56, 5.25, 5.73, 6.26, 6.66, 6.86, 7.39, 7.59, 7.44, 8.12, 8.46, 8.74, 8.95, 9.13, 9.35, 9.77, 9.8, 10.53, 10.76, 11.06, 11.59, 12, 12.79, 13.23, 13.60, 13.85, 14.69, 14.71, 15.99, 16.22, 16.61, 17.26, 18.75, 19.40, 23.3)
  idade <- c(26, 32, 36, 20, 40, 28, 41, 43, 34, 23, 33, 27, 37, 44, 30, 38, 31, 39, 25, 37, 30, 34, 41, 26, 32, 35, 46, 29, 40, 35, 31, 36, 43, 33, 48, 42)
  procedencia <- c("Interior", "Capital", "Capital", "Outro", "Outro", "Interior", "Interior", "Capital", "Capital", "Outro", "Interior", "Capital", "Outro", "Outro", "Interior", "Outro", "Capital", "Outro", "Interior", "Interior", "Outro", "Capital", "Outro", "Outro", "Interior", "Outro", "Outro", "Interior", "Interior", "Capital", "Outro", "Interior", "Capital", "Capital", "Capital", "Interior")


 
# Frequência absoluta para a variável estado civil
print("freq_abs_estado_civil")
summary(factor(estado_civil))


# Frequência absoluta para a variável grau de instrução
print("freq_abs_grau_instrucao")
summary(factor(grau_instrucao))


# Frequência absoluta para a variável número de filhos
print("freq_abs_num_filhos")
summary(factor(num_filhos))


# Frequência absoluta para a variável região de salario
print("freq_abs_salario")
summary(factor(salario))


# Frequência absoluta para a variável região de idade
print("freq_abs_idade")
summary(factor(idade))


# Frequência absoluta para a variável região de procedência
print("freq_abs_procedencia")
summary(factor(procedencia))



# Frequência relativa para a variável estado civil
print("freq_rel_estado_civil")
table(estado_civil)/length(estado_civil)


# Frequência relativa para a variável grau de instrução
print("freq_rel_grau_instrucao")
table(grau_instrucao)/length(grau_instrucao)


# Frequência relativa para a variável número de filhos
print("freq_rel_num_filhos")
table(num_filhos)/length(num_filhos)


# Frequência relativa para a variável número de salario
print("freq_rel_salario")
table(salario)/length(salario)


# Frequência relativa para a variável número de idade
print("freq_rel_idade")
table(idade)/length(idade)


# Frequência relativa para a variável região de procedência
print("freq_abs_procedencia")
table(procedencia)/length(procedencia)


# Porcentagem para a variável estado civil
print("Porcentagem_estado_civil")
table(estado_civil)/length(estado_civil)*100


# Porcentagem para a variável grau de instrução
print("Porcentagem_grau_instrucao")
table(grau_instrucao)/length(grau_instrucao)*100


# Porcentagem para a variável número de filhos
print("Porcentagem_num_filhos")
table(num_filhos)/length(num_filhos)*100

# Porcentagem para a variável número de salario
print("Porcentagem_num_salario")
table(salario)/length(salario)*100

# Porcentagem para a variável número de idade
print("Porcentagem_idade")
table(idade)/length(idade)*100

# Porcentagem para a variável região de procedência
print("Porcentagem_procedencia")
table(procedencia)/length(procedencia)*100



# Porcentagem acumulada para a variável estado civil
print("Porcentagem_acumulada_estado_civil")
cumsum(table(estado_civil)/length(estado_civil)*100)


# Porcentagem acumulada para a variável grau de instrução
print("Porcentagem_acumulada_instrucao")
cumsum(table(grau_instrucao)/length(grau_instrucao)*100)


# Porcentagem acumulada para a variável número de filhos
print("Porcentagem_acumulada_filhos")
cumsum(table(num_filhos)/length(num_filhos)*100)


# Porcentagem acumulada para a variável número de salario
print("Porcentagem_acumulada_salario")
cumsum(table(salario)/length(salario)*100)

# Porcentagem acumulada para a variável número de idade
print("Porcentagem_acumulada_idade")
cumsum(table(idade)/length(idade)*100)

# Porcentagem acumulada para a variável região de procedência
print("Porcentagem_acumulada_procedencia")
cumsum(table(procedencia)/length(procedencia)*100)

#B)--------------------------------------------------------------------------------------------------------
print("letra B")

# Gráfico de pizza para a variável grau de instrução
pie(table(grau_instrucao), labels=names(table(grau_instrucao)))


# Gráfico de pizza para a variável região de procedência
pie(table(procedencia), labels=names(table(procedencia)))


#C)-----------------------------------------------------------------------------------------------
print("letra C")
# Gráfico de barras para a variável estado civil
barplot(table(estado_civil), xlab="Estado civil")

# Gráfico de barras para a variável número de filhos
barplot(table(num_filhos), xlab="num_filhos")

#D)-----------------------------------------------------------------------------------------------

print("letra D")

# Histograma para a variável salário
hist(salario)

# Histograma para a variável idade
hist(idade)

#E)-------------------------------------------------------------------------------------

# Média da variável número de filhos
media <- mean(num_filhos, na.rm=TRUE)
print("media de filhos")
print(media)

# Mediana da variável idade
mediana <- median(idade)
print("Mediana de idade")
mediana


quartis <- quantile(salario, probs = c(0.25, 0.75), type=2)
print("Quartis")
quartis
