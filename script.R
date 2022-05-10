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
#3. Médicos afirmam que o grupo dos falso positivos é mais jovem do que o dos falso negativos. O que você diria a respeito? Justifique sua resposta baseando-se em gráficos e medidas de tendência central.
#fazer um boxplot para cada grupo
#e verificar o resultado de summary para cada grupo

par(mfrow = c(1, 2))
#idade dos falsos positivos: 4
boxplot(
  arq[arq$Grupo==4, 'Idade'],
  main = "Falso positivo",
  ylab = 'Idade',
  col = "lightblue"
)

#idade dos falsos negativos: 1
boxplot(
  arq[arq$Grupo==1, 'Idade'],
  main = "Falso negativo",
  ylab = "Idade",
  col = 'lightgreen'
)

summary(arq[arq$Grupo==4, 'Idade'])
summary(arq[arq$Grupo==1, 'Idade'])

par(mfrow = c(1, 1))
############################################################
#4. Compare a glicose dos pacientes falso positivos e dos falso negativos. Para isso, use a média, a mediana, a variância e o desvio padrão. 

gfp <- arq[arq$Grupo==4, 'GL'];gfp

gfn <- arq[arq$Grupo==1, 'GL']; gfn

s <- sprintf(
  "Resultados\nMédia(gfp): %.2f\nMédia(gfn): %.2f\nMediana(gfp): %.2f\nMediana(gfn): %.2f\nVariância(gfp): %.2f\nVariância(gfn): %.2f\nDesvio padrão(gfp): %.2f\nDesvio padrão(gfn): %.2f\n",
  mean(gfp),
  mean(gfn),
  median(gfp),
  median(gfn),
  var(gfp),
  var(gfn),
  sd(gfp),
  sd(gfn)
)

print(s)

s

cat(s)
############################################################








