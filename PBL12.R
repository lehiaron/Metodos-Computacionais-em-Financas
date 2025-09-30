#-----------------------------------------------------------------------
# 1. PREPARAÇÃO DO AMBIENTE E DADOS
#-----------------------------------------------------------------------

# As linhas abaixo são para instalar os pacotes, caso você ainda não os tenha.
# Elas só precisam ser executadas uma vez.
#install.packages("tidyverse")
#install.packages("quantmod")

# Carrega o pacote 'tidyverse', um conjunto de ferramentas para manipulação e visualização de dados (inclui dplyr e ggplot2).
library(tidyverse)
# Carrega o pacote 'quantmod' para baixar dados do mercado financeiro.
library(quantmod)

# Cria um vetor nomeado que mapeia um nome amigável para o ticker do ativo no Yahoo Finance.
tickers <- c(ITUB4 = "ITUB4.SA", LREN3 = "LREN3.SA", VALE3 = "VALE3.SA", RADL3 = "RADL3.SA")
# Baixa os dados históricos para os tickers especificados a partir de 2016.
getSymbols(unname(tickers), from = "2016-01-01", src = "yahoo", auto.assign = TRUE)

# Extrai apenas a coluna de preços de fechamento ajustados ("Adjusted") para cada ativo.
lista_precos <- lapply(unname(tickers), function(s) Ad(get(s)))
# Une todas as séries de preços em um único objeto de série temporal.
precos <- do.call(merge, lista_precos)
# Renomeia as colunas com os nomes amigáveis definidos anteriormente.
colnames(precos) <- names(tickers)

# Converte os preços diários em mensais, pegando o último preço de cada mês.
precos_mensais <- precos[endpoints(precos, "months"), ]
# Calcula os retornos logarítmicos mensais e remove o primeiro valor 'NA'.
retornos <- diff(log(precos_mensais)) %>% na.omit()

# Cria uma tabela (tibble) com as estatísticas básicas (média e desvio padrão) de cada ativo.
tabela_ativos <- tibble(
  ativo = colnames(retornos),
  media = colMeans(retornos),
  desvio = apply(retornos, 2, sd)
)


#-----------------------------------------------------------------------
# 2. ANÁLISE DE PORTFÓLIO COM 2 ATIVOS (ITUB4, LREN3)
#-----------------------------------------------------------------------

# Seleciona os retornos apenas para os 2 ativos de interesse.
ret2 <- retornos[, c("ITUB4", "LREN3")]
# Calcula o vetor de médias e a matriz de covariância, que são os inputs para os cálculos de portfólio.
medias2 <- colMeans(ret2) ; matriz2 <- cov(ret2)

# Gera milhares de combinações de portfólios com 2 ativos.
portf2 <- tibble(ITUB4 = seq(0,1,0.01)) %>%  # Gera pesos para ITUB4 de 0% a 100%, com passo de 1%.
  mutate(LREN3 = 1 - ITUB4) %>%             # O peso de LREN3 é o que falta para completar 100%.
  rowwise() %>%                            # Agrupa por linha para que as contas sejam feitas para cada portfólio.
  mutate(
    # Calcula o retorno esperado do portfólio (média ponderada dos retornos).
    retorno = ITUB4 * medias2["ITUB4"] + LREN3 * medias2["LREN3"],
    # Calcula o risco do portfólio (desvio padrão), usando a fórmula matricial sqrt(w' * COV * w).
    risco = sqrt(c(ITUB4, LREN3) %*% matriz2 %*% c(ITUB4, LREN3))
  ) %>% 
  ungroup() # Desagrupa os dados.

# Encontra o portfólio de Mínima Variância (menor risco) ordenando pela coluna 'risco' e pegando a primeira linha.
minvar2 <- portf2 %>% 
  arrange(risco) %>% 
  slice(1)


#-----------------------------------------------------------------------
# 3. ANÁLISE DE PORTFÓLIO COM 3 ATIVOS (ITUB4, LREN3, VALE3)
#-----------------------------------------------------------------------

# Repete o processo: seleciona retornos e calcula médias e matriz de covariância.
ret3 <- retornos[, c("ITUB4", "LREN3", "VALE3")]
medias3 <- colMeans(ret3) ; matriz3 <- cov(ret3)

# Gera combinações de portfólios com 3 ativos.
portf3 <- expand.grid(ITUB4 = seq(0,1,0.02), LREN3 = seq(0,1,0.02)) %>% as_tibble() %>% # Cria todas as combinações de pesos para os 2 primeiros.
  mutate(VALE3 = 1 - ITUB4 - LREN3) %>% # O peso do terceiro ativo completa 100%.
  filter(VALE3 >= 0) %>% # Filtra apenas as combinações válidas (sem pesos negativos, ou seja, sem venda a descoberto).
  rowwise() %>% 
  mutate(
    # Calcula retorno e risco para a carteira de 3 ativos.
    retorno = ITUB4 * medias3["ITUB4"] + LREN3 * medias3["LREN3"] + VALE3 * medias3["VALE3"],
    risco = sqrt(c(ITUB4, LREN3, VALE3) %*% matriz3 %*% c(ITUB4, LREN3, VALE3))
  ) %>% 
  ungroup()

# Encontra o portfólio de Mínima Variância para 3 ativos.
minvar3 <- portf3 %>% 
  arrange(risco) %>% 
  slice(1)


#-----------------------------------------------------------------------
# 4. ANÁLISE DE PORTFÓLIO COM 4 ATIVOS
#-----------------------------------------------------------------------

# Repete o processo para os 4 ativos.
ret4 <- retornos[, c("ITUB4", "LREN3", "VALE3", "RADL3")]
medias4 <- colMeans(ret4) ; matriz4 <- cov(ret4)

# Gera combinações de portfólios com 4 ativos. O passo (0.05) é maior para reduzir o número de cálculos.
portf4 <- expand.grid(ITUB4 = seq(0,1,0.05), LREN3 = seq(0,1,0.05), VALE3 = seq(0,1,0.05)) %>% as_tibble() %>% 
  mutate(RADL3 = 1 - ITUB4 - LREN3 - VALE3) %>%
  filter(RADL3 >= 0) %>% 
  rowwise() %>% 
  mutate(
    # Calcula retorno e risco para a carteira de 4 ativos.
    retorno = ITUB4 * medias4["ITUB4"] + LREN3 * medias4["LREN3"] + VALE3 * medias4["VALE3"] + RADL3 * medias4["RADL3"],
    risco = sqrt(c(ITUB4, LREN3, VALE3, RADL3) %*% matriz4 %*% c(ITUB4, LREN3, VALE3, RADL3))
  ) %>% 
  ungroup()

# Encontra o portfólio de Mínima Variância para 4 ativos.
minvar4 <- portf4 %>% 
  arrange(risco) %>% 
  slice(1)


#-----------------------------------------------------------------------
# 5. SUMÁRIO DOS RESULTADOS
#-----------------------------------------------------------------------

# Junta os resultados das 3 carteiras de mínima variância em uma única tabela para comparação.
bind_rows(`2_ativos`=minvar2, `3_ativos`=minvar3, `4_ativos`=minvar4, .id="caso") %>%
  mutate(across(where(is.numeric), ~ round(.x, 6))) %>% # Arredonda os resultados numéricos.
  print(n=Inf) # Imprime todas as linhas da tabela no console.


#-----------------------------------------------------------------------
# 6. VISUALIZAÇÃO GRÁFICA (FRONTEIRA EFICIENTE)
#-----------------------------------------------------------------------

# Carrega a biblioteca para gráficos.
library(ggplot2)

# Gráfico para o portfólio de 2 ativos.
g2 <- ggplot(portf2, aes(risco, retorno)) +
  geom_point(alpha=.25, size=1) + # Plota todos os portfólios possíveis (a fronteira).
  geom_point(data=tabela_ativos %>% filter(ativo %in% c("ITUB4","LREN3")),
             aes(desvio, media, color=ativo), size=3) + # Plota os ativos individuais.
  geom_point(data=minvar2, aes(risco, retorno), shape=8, size=3) + # Destaca a carteira de mínima variância com uma estrela.
  labs(title="Fronteira Eficiente: ITUB4 x LREN3", x="Risco (DP mensal)", y="Retorno médio",
       subtitle="★ = carteira de mínima variância") +
  theme_minimal()

# Gráfico para o portfólio de 3 ativos.
g3 <- ggplot(portf3, aes(risco, retorno)) +
  geom_point(alpha=.25, size=1) +
  geom_point(data=tabela_ativos %>% filter(ativo %in% c("ITUB4","LREN3","VALE3")),
             aes(desvio, media, color=ativo), size=3) +
  geom_point(data=minvar3, aes(risco, retorno), shape=8, size=3) +
  labs(title="Fronteira Eficiente: ITUB4, LREN3, VALE3", x="Risco (DP mensal)", y="Retorno médio",
       subtitle="★ = carteira de mínima variância") +
  theme_minimal()

# Gráfico para o portfólio de 4 ativos.
g4 <- ggplot(portf4, aes(risco, retorno)) +
  geom_point(alpha=.25, size=1) +
  geom_point(data=tabela_ativos, aes(desvio, media, color=ativo), size=3) +
  geom_point(data=minvar4, aes(risco, retorno), shape=8, size=3) +
  labs(title="Fronteira Eficiente: ITUB4, LREN3, VALE3, RADL3", x="Risco (DP mensal)", y="Retorno médio",
       subtitle="★ = carteira de mínima variância") +
  theme_minimal()

# Imprime os três gráficos gerados.
print(g2); print(g3); print(g4)

# ERRO DE SINTAXE: O código original tinha uma chave '}' extra aqui, sem uma correspondente de abertura.
# } # Removida