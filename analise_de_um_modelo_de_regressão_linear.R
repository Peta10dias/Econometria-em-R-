#Olá esse projeto mostra uma analise de um modelo atraves de regressao linear 

#  Com os dados constantes da "tabela1", responda:
install.packages("readxl")
library(readxl)
base = read_excel('tabela1.xlsx')
base$ano <- NULL
rownames(base) <- c(1981,1982,1983,1984,1985,1986,1987,1988,1989,1990)
attach(base)
# a)Estime a equa��o de demanda por energia el�trica, utilizando o modelo simples de regress�o 
#     linear. Coloque a fun��o estimada aqui no script.

modelo1 = lm(formula = quantidade.demanda ~ tarifa )
summary(modelo1)
# Y = 158,8052 + (-0,5567)B1

# b)Interprete os coeficientes estimados e descreva a rela��o que existe entre a vari�vel 
#     explicatitiva e a vari�vel explicada. Considere o n�vel de 5% de signific�ncia. 

#
#A hip�tese h0 fala que nenhum coeficiente � significativo, analisando a estat�stica F notamos que o p-valor � menor que 5%, logo a h0 est� rejeitada
#Assim assumimos a hip�tese alternativa que pelo menos um coeficiente � diferente que 0.
#A equa��o mostra que o coeficiente para tarifa � -0.5567. O coeficiente indica que para cada unidade adicional na tarifa a demanda diminui cerca de 0,5567.
# O pr da variavel tarifa(B1) � de 0.025972 ou seja dentro do padr�o de significancia do erro logo � significante.


# c)Interprete o coeficiente de determina��o.
# O R-Squared do nosso modelo ficou entre aproximadamente 48%, um coeficiente baixo que  indica que o modelo n�o explica nada da variabilidade dos dados de resposta ao redor de sua m�dia.


############################################################################

#  Considerando o banco de dados "tabela2".

base2 = read_excel('tabela2.xlsx')
attach(base2)

# Estime a fun��o de produ��o em que a vari�vel explicada ser� n�mero de po�os perfurados 
#    de petr�leo (Y, milhares); e, as vari�veis explicativas s�o:
#    pre�o do barril (X1, em d�lares constantes, 1972=100); 
#    produ��o dom�stica (X2, milh�es de barris por dia); 
#    PNB em d�lares constates (X3, 1972=100);
#    vari�vel tempo (X4, t = 1, 2, ..., 31). Expresse no script do programa a fun��o estimada!
attach(base2)
modelo2 = lm(formula = Y ~ X1 + X2 + X3 + X4 )
summary(modelo2)
#  Interprete os coeficientes estimados e descreva a rela��o existente entre a vari�vel 
#    explicatitiva e a vari�vel explicada. Considere o n�vel de signific�ncia igual a 5%.
#   Y = -9.798930 + (2.700179)B1 + (3.045134)B2 + (-0.015994)B3 + (-0.023347)B4
#A hip�tese h0 fala que nenhum coeficiente � significativo, analisando a estat�stica F notamos que o p-valor � de  0.0001133
#logo a h0 est� rejeitada.
#Assim assumimos a hip�tese alternativa que pelo menos um coeficiente � diferente que 0.

# O pr da variavel pre�o do barril(B1) � de 0.000664 ou seja dentro do padr�o de significancia do erro logo � significante.
# O pr da variavel produ��o dom�stica(B2) � de 0.003297 ou seja dentro do padr�o de significancia do erro logo � significante para o modelo .
# O pr da variavel PNB em d�lares constates(B3) � de 0.062335 ou seja fora do padr�o de significancia do erro logo � insignificante para o modelo.
# O pr da variavel tempo(B4) � de 0.932603 ou seja fora do padr�o de significancia do erro logo � insignificante para o modelo.
 
#  O que acontece se o termo de intercepto for retirado da regress�o? 
#   Como voc� interpreta os resultados a partir da exclus�o do termo de intecepto? Explique no script do R.
modelo3 = lm(formula = Y ~ - 1 + X1 + X2 + X3 + X4 )
summary(modelo3)
# notamos que com a retirada do intercepto o R-squared do nosso modelo fica com 98%, ou seja a linha est� muito pr�ximo dos pontos, mostrando que o modelo est� bem ajustado aos dados
# o p-valor desse modelo � menor que 5% assim ele fica dentro do nosso intervalo de confian�a.
# 
#  Verifique se h� correla��o entre cada vari�vel explicativa e a explicada. Explique seus resultados no script do R.  
cor(base2)
#A variavel Y tem correla��o negativa com a variav�l X2, X3 e X4 ou seja quando Y aumenta as v�riav�is diminuem em 0.42, 0.55 e 0.52, respectivamente. e correla��o positiva em 0.13 com a vari�vel X1.
#A v�riavel X1 tem correla��o negativa com a variavel X2 em 0.30 e positiva com as v�riaveis 
#  Verifique se h� outliers no seu banco de dados a partir das an�lises realizadas atrav�s dos 
#   res�duos amostrais. N�o esque�a de elaborar os gr�ficos. Explique seus resultados no script do R.


###########################################################


# Crie um dataframe que contenha 3 vari�veis, j� incluindo a vari�vel explicada ou 
#             dependente. Use os comandos "rep" e "sample", podem usar a fun��o mais simples (concatenar "c"). Depois:
fabrica <- data.frame( produ��o_de_cerveja = sample(1:100,4, replace = FALSE),
                    demanda_por_cerveja = sample(1:100,4,replace = FALSE),
                    numero_de_fabricas = sample(1:10,4,replace = FALSE))

rownames(fabrica) <- c(2019,2020,2021,2022)
attach(fabrica)
# a) Verifique se h� correla��o entre cada vari�vel explicativa e a explicada. 
#    Explique seus resultados no script do R.
cor(fabrica)
# a correla��o entre as v�riaveis numero de fabricas e produ��o de cerveja � significante positivamente , e a numero de fabricas e demanda por cerveja � inversamente significante.j� a corre��o entre produ��o e demanda � insignificante.


# b) Em seguida, regrida o modelo que voc� achar pertinente a partir das conclus�es extra�das da 
#     alternativa a) e considerando o dataframe que voc� criou. Exponha os resultados no script do R.
modelo4 = lm(formula = produ��o_de_cerveja ~ -1 + numero_de_fabricas)
summary(modelo4)
# c) Depois da regress�o, verifique se o(s) coeficiente(s) estimado(s) �/s�o significativo(s) e 
#    qual a rela��o (positiva ou negativa) entre as vari�veis explicativas e a explicada. 
#    Considere a signific�ncia do erro de 5%. Explique seus resultados no script do R.

#A hip�tese h0 fala que nenhum coeficiente � significativo, analisando a estat�stica F notamos que o p-valor � 0.03034 logo menor que 5%, logo a h0 est� rejeitada
#Assim assumimos a hip�tese alternativa que pelo menos um coeficiente � diferente que 0.
#A equa��o mostra que o coeficiente para numero  de fabricas � de 11.773. O coeficiente indica que para cada fabrica a produ��o aumenta em 11.773 no intervalo analisado .
# O pr da variavel numero_de_fabricas � de 0,0303 ou seja dentro do padr�o de significancia do erro logo � significante.


# d) Qual a conclus�o do seu modelo? Explique seus resultados no script do R.
# assumindo o R-squared de 0.8338 o modelo explica 83% da reta,Quanto mais vari�ncia for explicada pelo modelo de regress�o, mais pr�ximos os pontos de dados estar�o em rela��o � linha de regress�o ajustada.


