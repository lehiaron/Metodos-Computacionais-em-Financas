# --- Parâmetros da Simulação ---
P0 <- 50.0       # Preço inicial da ação em R$
mu <- 0.10       # Drift (taxa de retorno esperada)
sigma <- 0.25    # Volatilidade
T <- 1.0         # Horizonte de tempo em anos
N <- 252         # Número de passos no tempo (dias úteis em um ano)
dt <- T / N      # Intervalo de tempo
num_simulations <- 100000  # Número de simulações de Monte Carlo
set.seed(42)

random_shocks <- matrix(rnorm(num_simulations * N), nrow = num_simulations, ncol = N)

# Inicializa uma matriz para armazenar as trajetórias dos preços
price_paths <- matrix(0, nrow = num_simulations, ncol = N + 1)
price_paths[, 1] <- P0

# Calcula o preço para cada passo de tempo em todas as simulações
for (t in 2:(N + 1)) {
  price_paths[, t] <- price_paths[, t - 1] * exp((mu - 0.5 * sigma^2) * dt +
                                                   sigma * sqrt(dt) * random_shocks[, t - 1])
}

# 1) Probabilidade de o preço da ação estar acima de R$ 70 após um ano
final_prices <- price_paths[, N + 1]
prob_1 <- sum(final_prices > 70) / num_simulations

# 2) Probabilidade de o preço da ação estar acima de R$ 70 em algum momento do próximo ano
max_prices <- apply(price_paths, 1, max)
prob_2 <- sum(max_prices > 70) / num_simulations

# 3) Probabilidade de o preço estar acima de R$ 70 no final do ano E ter caído abaixo de R$ 50 em algum momento
min_prices <- apply(price_paths, 1, min)
condition_3 <- (final_prices > 70) & (min_prices < 50)
prob_3 <- sum(condition_3) / num_simulations

# 4) Probabilidade de o preço estar acima de R$ 70 no final do ano E abaixo de R$ 50 nos primeiros seis meses
first_six_months_paths <- price_paths[, 1:(N/2 + 1)]
min_first_six_months <- apply(first_six_months_paths, 1, min)
condition_4 <- (final_prices > 70) & (min_first_six_months < 50)
prob_4 <- sum(condition_4) / num_simulations

# 5) Probabilidade de o preço cruzar o limiar de R$ 70 (de baixo para cima) três vezes em um ano
threshold <- 70
crossings_count <- apply(price_paths, 1, function(path) {
  crossings <- sum(path[1:N] < threshold & path[2:(N+1)] >= threshold)
  return(crossings == 3)
})
prob_5 <- sum(crossings_count) / num_simulations

