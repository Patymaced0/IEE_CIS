### CARREGANDO OS PACOTES ###----

pacman::p_load(read.csv, tidyverse, ggplot2, readxl,dplyr,lubridate, nortest,scales)
library(pacman)

### CARREGANDO OS DADOS ###----
setwd("C:/Users/patym/OneDrive/Área de Trabalho/IEE_CIS")
freedom = read_excel("freedom.xlsx")
GERAL = read_excel("geral.xlsx")
MEDALS = read_excel("medals.xlsx")
HAPINESS = read_excel("hapiness.xlsx")

### PADRONIZACAO ###---
cores <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")

theme_padra <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores),
      scale_colour_manual(values = cores)
    )
  )
}



### QUESTAO 01 ###
# DADOS FALTANTES #
# Nesse irei optar por remover as observacoes com dados faltantes, pois 
#o número de observacoes com valores ausentes for pequeno em relacao ao 
#tamanho total do conjunto de dados, você pode optar por remover as observacoes que possuem NA.

FREEDOM = na.omit(freedom)

### QUESTAO 02 ###---
MEDALS <- subset(MEDALS, select = -...7) #removendo uma coluna, que so tem observacoes NA.

# Renomeando as colunas #
colnames(MEDALS) <- c("Países","Ouro","Prata","Bronze","Total","Ranking por total")

MEDALS$Ouro <- as.numeric(MEDALS$Ouro)


# Numero total de medalhas por pais
MEDALS$Total_Medalhas <- rowSums(MEDALS[, c("Ouro", "Prata", "Bronze")], na.rm = TRUE)

### QUESTAO 03 ###---

# Calcula a pontuacao total das medalhas
MEDALS$Pontuacao <- (MEDALS$Ouro * 3) + (MEDALS$Prata * 2) + MEDALS$Bronze

# Ordena os dados pelo total de medalhas em ordem decrescente
dados_ranking <- MEDALS %>%
  arrange(desc(Pontuacao))

# Seleciona os dez primeiros países
dez_melhores_paises <- head(dados_ranking, 10)

## GRAFICO ##
ggplot(dez_melhores_paises) +
  aes(x = fct_reorder(Países, Pontuacao, .desc=F), y = Pontuacao, label = Pontuacao) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.3, hjust = -0.2,
    size = 2
  ) + 
  labs(x = "Países", y = "Número de medalhas") +
  theme_padra() +
  coord_flip()


### QUESTAO 04 ###----

# Calculando a média de alfabetização por região
media_por_regiao <- aggregate(`Literacy (%)` ~ Region, data = GERAL, FUN = mean)

### QUESTAO 05 ###----
options(scipen = 999) # formatação das unidades 

DADOS_CELULARES <- select(GERAL,Country,`Phones (per 1000)`, Population)


# Ordenar os dados em ordem decrescente
dados_ordenados <- DADOS_CELULARES[order(DADOS_CELULARES$`Phones (per 1000)`, decreasing = TRUE), ]

# Selecionar os cinco primeiros países
dados_ordenados$Valor_Arredondado <- round(DADOS_CELULARES$`Phones (per 1000)`, 2)
top_cinco_paises <- head(dados_ordenados, 5)

## GRÁFICO ## 
ggplot(top_cinco_paises) +
  aes(x = fct_reorder(Country,Valor_Arredondado, .desc=F), y = Valor_Arredondado, label = Valor_Arredondado) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.3, hjust = -0.2,
    size = 2
  ) + 
  labs(x = "Celulares", y = "Número de Celulares por Mil Habitantes") +
  theme_padra() +
  coord_flip()

### QUESTAO 06 ###----

FREEDOM_2018 <- subset(FREEDOM, year == 2018)

### QUESTAO 07 ###----

EVOLUCAO <- select(FREEDOM, year,countries, hf_score)

EVOLUCAO_Brasil <- filter(EVOLUCAO, countries == "Brazil")

## GRAFICO ##

ggplot(EVOLUCAO_Brasil) +
  aes(x= year, y=hf_score, group=1) +
  geom_line(size=1,colour="#A11D21") + geom_point(colour="#A11D21",size=2) +
  labs(x="Ano", y="Brasil") +
  theme_padra()

### QUESTAO 08 ###----

media_populacional <- GERAL %>%
  group_by(Region) %>%
  summarize(media_populacao = mean(Population))

indice_regiao <- which.max(media_populacional$media_populacao)
regiao_maior_media <- media_populacional$Region[indice_regiao]

#Resposta: ASia (Ex. Near East)

### QUESTAO 09 ###----
cor <- cor(GERAL$Population, GERAL$`Net migration`)

## GRAFICO ##
ggplot(GERAL) +
  aes(x = Population, y = `Net migration`) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "População",
    y = "Migração" )+ 
  theme_padra()
# a partir do grafico, é 0, com alguns valores discrepantes e alem disso 
# valores proximos a 0 indicam uma correlação fraca ou inexistente..

### QUESTAO 10 ###----

colnames(MEDALS) <- c("Países","Bronze","Prata","Ouro","Total","Ranking por total","Total_Medalhas","Pontuação")

# Calcula a pontuacao total das medalhas
MEDALS$Pontuacao1 <- (MEDALS$Bronze) + (MEDALS$Prata * 2) + (MEDALS$Ouro *3)

# Ordena os dados pelo total de medalhas em ordem decrescente
dados_ranking1 <- MEDALS %>%
  arrange(desc(Pontuacao1))

# Seleciona os dez primeiros países
dez_melhores_paises1 <- head(dados_ranking1, 10)

## GRAFICO ##
ggplot(dez_melhores_paises1) +
  aes(x = fct_reorder(Países, Pontuacao1, .desc=F), y = Pontuacao1, label = Pontuacao1) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.3, hjust = -0.2,
    size = 2
  ) + 
  labs(x = "Países", y = "Número de medalhas") +
  theme_padra() +
  coord_flip()


### QUESTAO 11 ###----
### QUESTAO 12 ###----

cor_infantilidade <- cor(GERAL$`Infant mortality (per 1000 births)`,GERAL$Deathrate)

## GRAFICO ##
ggplot(GERAL) +
  aes(x = `Infant mortality (per 1000 births)`, y = GERAL$Deathrate) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Índice de Natalidade",
    y = "Índice de Mortalidade" )+ 
  theme_padra()



### QUESTAO 13 ###----
#correlação entre a generosidade da população e o indice de agricultura dos paises
HAPINESS <- rename(HAPINESS, Country = Countryorregion)

GERAL_HAPI <- merge(GERAL, HAPINESS, by = "Country")

# Calcular a correlação entre generosidade e índice de agricultura
correlacao <- cor(GERAL_HAPI$Generosity, GERAL_HAPI$Agriculture) # é uma correlação fraca

## GRAFICO ##
ggplot(GERAL_HAPI) +
  aes(x = Generosity, y = Agriculture) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Generosidade",
    y = "Índice de Agricultura" )+ 
  theme_padra()


### QUESTAO 14 ###----
### QUESTAO 15 ###----

aumentos <- diff(GERAL$Deathrate)

indice_pais <- which.max(aumentos)
pais_maior_aumento <- GERAL$Country[indice_pais]
print(pais_maior_aumento)

#Resposta: Lebanon

### QUESTAO 16 ###----
### QUESTAO 17 ###----
### QUESTAO 18 ###----
### QUESTAO 19 ###----
### QUESTAO 20 ###----