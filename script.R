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
#5.  Obtenha as medidas de posição e variabilidade para a variável Idade do conjunto de dados como um todo. Façamos um teste com arq['Idade'] para ver o problema quando manipulamos um dataframe
arq['Idade']
class(arq['Idade'])
mean(arq['Idade'])

arq['Idade']
arq[ , 'Idade']

arq[, 'Idade']
class(arq[, 'Idade'])

mean (arq[, 'Idade'])

idade_tot <- arq$Idade;idade_tot
class(idade_tot)

s_idade_total <- sprintf(
  'Média: %.2f\nMediana: %.2f\nVariância: %.2f\nDesvio padrão: %.2f\n',
  mean(idade_tot),
  median(idade_tot),
  var(idade_tot),
  sd(idade_tot)
)
cat(s_idade_total)
###############################################################################
#6.  Faça o mesmo do exercício 5, mas agora para cada tipo de diagnóstico. Compare os resultados obtidos. Construa um data frame para idade e para facilitar a visualização e a comparação.

#             total falso_negativo negativo positivo falso_positivo
#média            54            45
#mediana
#variância
#desvio padrão

#construir quatro estruturas de dados: cada uma tem a coleção de idades de um dos grupos
#FN: 1
#N: 2
#P: 3
#FP: 4
idade_fn = arq[arq$Grupo==1, 'Idade']
idade_n = arq[arq$Grupo==2, 'Idade']
idade_p = arq[arq$Grupo==3, 'Idade']
idade_fp = arq[arq$Grupo==4, 'Idade']

#construir mais uma estrutura de dados com todas as idades
idade_tot <- arq[, 'Idade']

#para cada estrutura, construir um vetor contendo sua média, mediana, variância e desvio padrão (dica: use c(mean(col1), median(col1)...))
v_tot <- c (mean(idade_tot), median(idade_tot), var(idade_tot), sd(idade_tot));v_tot
v_fn <- c(mean(idade_fn), median(idade_fn), var(idade_fn), sd(idade_fn));v_fn
v_n <- c(mean(idade_n), median (idade_n), var(idade_n), sd(idade_n)); v_n
v_p <- c(mean(idade_p), median(idade_p), var(idade_p), sd(idade_p)); v_p
v_fp <- c(mean(idade_fp), median(idade_fp), var(idade_fp), sd(idade_fp)); v_fp


#um vetor com os nomes das linhas nomes c('média', 'mediana'...)
nomes_das_linhas <- c('Média', 'Mediana', "Variância", "Desvio Padrão")
#construir o data frame com data.frame
#o vetor de nomes se encaixa no parâmetro row.names do dataframe
df <- data.frame(
  total=v_tot,
  falso_negativo=v_fn,
  negativo=v_n,
  positivo=v_p,
  falso_positivo=v_fp,
  row.names=nomes_das_linhas
); df


##########################################################################
#7. Crie uma amostra aleatória do arquivo cancer.txt com 100 linhas e faça o sumário de medidas de posição. Compare as medidas de posição da variável Idade da amostra com as medidas de posição da variável Idade do conjunto completo. O que você observa?
sample(c(1, 2, 3), 4, replace = T)


sample(arq$Ident, 100)

is.element(arq$Ident, sample(arq$Ident, 100))

#
arq100 <- arq[is.element(arq$Ident, sample(arq$Ident, 100)), ];arq100

summary(arq)
summary(arq100)
#########################################################################
#8Como você pode verificar, com certeza, se há algum valor NA no arquivo cancer.txt?
#?any
#any
#is.na
any(is.na(arq))
#########################################################################
#9. Qual a menor idade comum aos 4 grupos de diagnósticos?
#usar idade_fp, idade_n, idade_p, idade_fn
#usar intersect para encontrar valores em comum em duas coleções
#intersect(c(1, 2), c(1, 3), c(3))
#intersect somente opera com dois operandos
#min
idades_falsos <- intersect(idade_fp, idade_fn);idades_falsos
idades_verdadeiros <- intersect(idade_p, idade_n);idades_verdadeiros
idades_comuns <- intersect(idades_falsos, idades_verdadeiros);idades_comuns
min(idades_comuns)
#########################################################################
