# Instalar e carregar pacotes necessários
# install.packages("ggplot2")
# install.packages("tictoc")
# install.packages("doParallel")
# install.packages("patchwork")

library(ggplot2)
library(tictoc)
library(doParallel)
library(patchwork) # Para organizar os gráficos

# --- Parâmetros Globais ---
S0 <- 35      # Preço inicial do ativo
K_barrier <- 35 # Strike da opção de barreira
K_chooser <- 36 # Strike da opção chooser
H <- 30       # Barreira (down-and-in)
rf <- 0.05    # Taxa livre de risco
sigma <- 0.25 # Volatilidade
mu <- 0.10    # Drift (usado apenas para simulação do mundo real, não para precificação)
T_total <- 2/12 # Maturidade total (2 meses)
T_choice <- 1/12 # Tempo para a escolha (1 mês)

# Parâmetros da Simulação de Monte Carlo
N_sim <- 100000     # Número de simulações (histórias/trajetórias)
N_steps <- 60       # Número de passos (observações diárias em ~2 meses)
dt <- T_total / N_steps # Intervalo de tempo

# --- 1. Precificação da Opção de Barreira ---

# Semente para reprodutibilidade
set.seed(42)

# Matriz para armazenar as trajetórias de preço
price_paths <- matrix(0, nrow = N_steps + 1, ncol = N_sim)
price_paths[1, ] <- S0

# Simulação das trajetórias sob a medida neutra ao risco (usando rf, não mu)
for (i in 1:N_sim) {
  for (t in 2:(N_steps + 1)) {
    z <- rnorm(1) # Amostra da distribuição normal padrão
    price_paths[t, i] <- price_paths[t-1, i] * exp((rf - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
  }
}

# Cálculo do Payoff
final_prices <- price_paths[N_steps + 1, ]
min_prices <- apply(price_paths, 2, min)

# O payoff é > 0 apenas se a barreira for atingida (min_price <= H)
# E se o preço final for maior que o strike (final_price > K)
payoffs_barrier <- ifelse(min_prices <= H, pmax(final_prices - K_barrier, 0), 0)

# Preço da opção: valor presente do payoff esperado
price_barrier <- mean(payoffs_barrier) * exp(-rf * T_total)

cat(sprintf("1) Preço da Opção de Barreira (Down-and-In Call): R$ %.4f\n", price_barrier))

# --- 2. Precificação da Opção Chooser (Solução Analítica) ---

# Função para a fórmula de Black-Scholes
BS_pricer <- function(S, K, T, r, sigma, type = "call") {
  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (type == "put") {
    price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }
  return(price)
}

# Componente 1: Call com maturidade T
price_call_comp <- BS_pricer(S0, K_chooser, T_total, rf, sigma, type = "call")

# Componente 2: Put com strike ajustado e maturidade t_choice
K_adj <- K_chooser * exp(-rf * (T_total - T_choice))
price_put_comp <- BS_pricer(S0, K_adj, T_choice, rf, sigma, type = "put")

# Preço da Chooser
price_chooser <- price_call_comp + price_put_comp

cat(sprintf("\n2) Preço da Opção Chooser (Fórmula Analítica): R$ %.4f\n", price_chooser))

# --- 3. Análise de Precisão da Simulação de Monte Carlo ---

# Função para precificar a Chooser com Monte Carlo
price_chooser_mc <- function(n_sim, S0, K, T_total, T_choice, r, sigma) {
  # Simula o preço do ativo até o momento da escolha (T_choice)
  z <- rnorm(n_sim)
  price_at_choice <- S0 * exp((r - 0.5 * sigma^2) * T_choice + sigma * sqrt(T_choice) * z)
  
  # Na data da escolha, calcula o valor da call e da put restantes
  # O tempo restante para o vencimento é T_total - T_choice
  remaining_T <- T_total - T_choice
  call_values <- BS_pricer(price_at_choice, K, remaining_T, r, sigma, type = "call")
  put_values <- BS_pricer(price_at_choice, K, remaining_T, r, sigma, type = "put")
  
  # O valor da chooser na data da escolha é o máximo entre a call e a put
  chooser_values_at_choice <- pmax(call_values, put_values)
  
  # O preço hoje é o valor presente do valor esperado na data da escolha
  price <- mean(chooser_values_at_choice) * exp(-r * T_choice)
  return(price)
}

# Executar 100 vezes para cada cenário de simulação
n_runs <- 100
set.seed(123) # Semente para reprodutibilidade
prices_1000 <- replicate(n_runs, price_chooser_mc(1000, S0, K_chooser, T_total, T_choice, rf, sigma))
prices_5000 <- replicate(n_runs, price_chooser_mc(5000, S0, K_chooser, T_total, T_choice, rf, sigma))

# Criar dataframe para o ggplot
df_prices <- data.frame(
  price = c(prices_1000, prices_5000),
  simulation = factor(rep(c("1000 Histórias", "5000 Histórias"), each = n_runs))
)

# Plot dos histogramas
hist_plot <- ggplot(df_prices, aes(x = price, fill = simulation)) +
  geom_histogram(alpha = 0.7, bins = 20, position = "identity") +
  geom_vline(aes(xintercept = price_chooser, linetype = "Preço Analítico"), color = "red", size = 1) +
  scale_linetype_manual(name = "", values = c("Preço Analítico" = "dashed")) +
  labs(
    title = "Distribuição dos Preços da Opção Chooser",
    subtitle = "Comparação entre 1000 e 5000 histórias por simulação (100 execuções)",
    x = "Preço da Opção (R$)",
    y = "Frequência",
    fill = "Nº de Histórias"
  ) +
  theme_minimal()

print(hist_plot)

# --- Otimização com Processamento Paralelo ---

# Número de simulações para o teste de performance
N_PERF_TEST <- 1000000

# Versão Sequencial
tic("Execução Sequencial (1000000 histórias)")
price_seq <- price_chooser_mc(N_PERF_TEST, S0, K_chooser, T_total, T_choice, rf, sigma)
toc_seq <- toc()

# Versão Paralela
# Função otimizada para o foreach
price_chooser_mc_parallel <- function(n_sim, S0, K, T_total, T_choice, r, sigma) {
  # Simula o preço do ativo até o momento da escolha (T_choice)
  z <- rnorm(n_sim)
  price_at_choice <- S0 * exp((r - 0.5 * sigma^2) * T_choice + sigma * sqrt(T_choice) * z)
  
  # Divide o trabalho entre os workers
  values <- foreach(s = price_at_choice, .combine = 'c') %dopar% {
    remaining_T <- T_total - T_choice
    call_val <- BS_pricer(s, K, remaining_T, r, sigma, type = "call")
    put_val <- BS_pricer(s, K, remaining_T, r, sigma, type = "put")
    max(call_val, put_val)
  }
  
  price <- mean(values) * exp(-r * T_choice)
  return(price)
}

# Configurar o cluster paralelo
num_cores <- detectCores() - 1 # Usar todos os cores menos um
cl <- makeCluster(num_cores)
registerDoParallel(cl)

tic("Execução Paralela (1000000 histórias)")
# Exportar a função BS_pricer para cada worker
clusterExport(cl, "BS_pricer")
price_par <- price_chooser_mc_parallel(N_PERF_TEST, S0, K_chooser, T_total, T_choice, rf, sigma)
toc_par <- toc()

# Parar o cluster
stopCluster(cl)

cat("\n--- Comparação de Performance ---\n")
cat(sprintf("Preço (Sequencial): R$ %.4f\n", price_seq))
cat(sprintf("Preço (Paralelo):   R$ %.4f\n", price_par))