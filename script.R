#1. Abra o arquivo cancer.txt como um data frame e faça um sumário estatístico dos dados.
arq <- read.table(file = "cancer.txt", header = T);arq
summary(arq)
##########################################################################
#2. Faça o histograma das variáveis LDH, ALB e N. Use a descrição da coluna como título de cada um. Use cores diferentes.

#ajustar os parâmetros de gráficos
par(mfrow=c(1, 3))

hist(
  arq[, 'LDH'], 
  main = "Enzima, lactate dehydrogenase", 
  xlab = "LDH",
  ylab = "frequência absoluta",
  col = 'red'
)

hist(
  arq[, 'ALB'],
  main = "Albumina",
  xlab = "ALB",
  ylab = "frequência absoluta",
  col = "blue"
)

hist(
  arq[, 'N'],
  main = "Nitrogênio na uréia",
  xlab = "N",
  ylab = "frequência absoluta",
  col = "green"
)
par(mfrow=c(1, 1))
############################################################
