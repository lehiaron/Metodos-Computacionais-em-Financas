#-----------------------------------------------------------------------
# 1. DEFINIÇÃO DOS PARÂMETROS
#-----------------------------------------------------------------------

# Parâmetros financeiros e da simulação
mu = 0.10             # mu: Retorno esperado (drift) anualizado do ativo. Não é usado na precificação via simulação risco-neutra.
sigma = 0.25          # sigma: Volatilidade anualizada do ativo.
rf = 0.05             # rf: Taxa de juros livre de risco anualizada. Usada para o drift na simulação e para o desconto.
s0 = 35               # s0: Preço inicial do ativo no tempo t=0.
T = (21*2)/252        # T: Tempo de maturidade da opção, em anos. (42 dias úteis / 252 dias úteis/ano).
dt = 1/252            # dt: O tamanho do passo de tempo da simulação, representando um dia útil.
n_steps = ceiling(T/dt) # n_steps: Número total de passos na simulação (neste caso, 42 passos).
n_paths = 10000       # n_paths: Número de trajetórias de preço a serem simuladas.

# Define uma semente para o gerador de números aleatórios.
# Isso garante que a simulação possa ser reproduzida com os mesmos resultados.
set.seed(123)

#-----------------------------------------------------------------------
# 2. SIMULAÇÃO DE MONTE CARLO PARA O PREÇO DO ATIVO
#-----------------------------------------------------------------------

# Cria uma matriz 'S' para armazenar as trajetórias dos preços.
# As linhas representam os passos de tempo e as colunas representam cada simulação.
S <- matrix(NA, nrow = n_steps + 1, ncol = n_paths)

# Inicializa a primeira linha da matriz com o preço inicial s0 para todas as simulações.
S[1,] <- s0

# Loop aninhado para gerar as trajetórias de preço.
for (j in 1:n_paths) { # Loop externo: itera sobre cada uma das N trajetórias.
    for (t in 1:n_steps) { # Loop interno: itera sobre cada passo de tempo dentro de uma trajetória.
      
      # Gera um choque aleatório (epsilon) a partir de uma distribuição normal.
      epsilon <- rnorm(1, mean = 0, sd = sqrt(dt))
      
      # Aplica a fórmula do Movimento Geométrico Browniano para simular o preço no próximo passo.
      # A simulação é feita sob a medida risco-neutra, por isso o drift é a taxa 'rf'.
      S[t + 1, j] <- S[t,j] + rf*S[t,j]*dt + sigma*S[t,j]*epsilon
    }
}

# Plota todas as trajetórias de preço simuladas para visualização.
matplot(0:n_steps, S, type = "l", main = "Simulações de Monte Carlo", xlab="Passos de Tempo (dias)", ylab="Preço do Ativo")

#-----------------------------------------------------------------------
# 3. PRECIFICANDO UMA OPÇÃO ASIÁTICA (ASIAN OPTION)
#-----------------------------------------------------------------------
# A opção asiática tem seu payoff baseado na média do preço do ativo durante sua vida.

k <- 40 # k: Preço de exercício (strike price) da opção.

# Calcula a média do preço do ativo para cada trajetória simulada.
S_avg <- colMeans(S)

# Calcula o payoff para uma Call Asiática: max(Média do Preço - Strike, 0).
payoff_asian_call <- pmax(S_avg-k,0)
# Calcula o payoff para uma Put Asiática: max(Strike - Média do Preço, 0).
payoff_asian_put <- pmax(k-S_avg,0)

# Calcula o preço da Call Asiática hoje.
# É a média dos payoffs de todas as simulações, trazida a valor presente descontando pela taxa livre de risco.
# NOTA: O código original usava `exp(-rf*t)`. `t` é uma variável do loop e teria o valor final de 42.
# O correto é usar `T`, o tempo total de maturidade em anos.
asian_call <- exp(-rf*T)*mean(payoff_asian_call)
asian_put <- exp(-rf*T)*mean(payoff_asian_put)


#-----------------------------------------------------------------------
# 4. PRECIFICANDO UMA OPÇÃO EUROPEIA PADRÃO
#-----------------------------------------------------------------------
# A opção europeia tem seu payoff baseado apenas no preço final do ativo na data de maturidade.

k <- 40 # Preço de exercício.

# Extrai o preço final (St) de cada trajetória, que está na última linha da matriz S.
St <- S[(n_steps+1),]

# Calcula o payoff para uma Call Europeia: max(Preço Final - Strike, 0).
payoff_call <- pmax((St - k),0)
# Calcula o preço da Call hoje: média dos payoffs descontada a valor presente.
# NOTA: Correção de `t` para `T` aplicada aqui também.
normal_call <- exp(-rf*T)*mean(payoff_call)
View(normal_call) # Exibe o resultado em uma nova janela no RStudio.

# Calcula o payoff para uma Put Europeia: max(Strike - Preço Final, 0).
payoff_put <- pmax((k - St),0)
# Calcula o preço da Put hoje.
normal_put <- exp(-rf*T)*mean(payoff_put)
View(normal_put) # Exibe o resultado.


#-----------------------------------------------------------------------
# 5. PRECIFICANDO UMA OPÇÃO PUT FORWARD-START
#-----------------------------------------------------------------------
# Esta é uma opção que começa em uma data futura (t2), com um strike conhecido, e expira em t1.
# O método aqui é usar a fórmula de Black-Scholes no tempo t2 para cada trajetória
# e depois descontar o valor médio para o tempo t=0.

# O comentário original indica "T = 2 meses", que são os 42 dias totais da simulação.
# O código abaixo parece precificar uma opção que começa no dia 21 (t2) e vence no dia 22 (t1).
# Isso resulta em uma opção de maturidade muito curta (1 dia), o que pode não ser a intenção original.

ST_1 <-S[21 + 1,]  # Preço do ativo no dia 21, para cada uma das N simulações.
t1 <- (21+1)/252   # Tempo de vencimento final da opção em anos (dia 22).
t2 <- 21/252       # Tempo de início da opção em anos (dia 21).

# maturidade_opcao = t1 - t2 # Maturidade efetiva da opção (neste caso, 1/252)

# Parâmetros d1 e d2 da fórmula de Black-Scholes, calculados para cada trajetória.
# A maturidade usada na fórmula é `t1`, o que é conceitualmente confuso.
# O correto seria usar a maturidade restante da opção (t1 - t2).
d1 <- (log(ST_1/k) + (rf + 0.5*sigma^2)*t1)/(sigma*sqrt(t1))
d2 <- d1 - sigma*sqrt(t1)

# Calcula o preço da Put Forward-Start.
# 1. `k*exp(-rf*t1)*pnorm(-d2) - (ST_1*pnorm(-d1))`: Preço da put no tempo `t2` via Black-Scholes, para cada trajetória.
# 2. `mean(...)`: Calcula o valor esperado da opção no tempo `t2`.
# 3. `*exp(-rf*t2)`: Desconta o valor esperado de `t2` para o tempo `0`.
f_put <- mean(k*exp(-rf*t1)*pnorm(-d2) - (ST_1*pnorm(-d1)))*exp(-rf*t2)
