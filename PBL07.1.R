#install.packages(c("ggplot2", "tictoc", "doParallel", "patchwork"))

# Carregar pacotes
library(ggplot2)
library(tictoc)
library(doParallel)
library(patchwork) 

# --- PARÂMETROS GLOBAIS DO MODELO ---
S0        <- 35      # Preço inicial do ativo
K_barrier <- 35      # Strike da opção de barreira
K_chooser <- 36      # Strike da opção chooser
H         <- 30      # Nível da barreira (down-and-in)
rf        <- 0.05    # Taxa livre de risco
sigma     <- 0.25    # Volatilidade
# mu (drift real) não é usado na precificação neutra ao risco
T_total   <- 2/12    # Maturidade total (2 meses)
T_choice  <- 1/12    # Tempo para a escolha (1 mês)

# Parâmetros da Simulação de Monte Carlo
N_sim_barrier <- 100000 # Simulações para a opção de barreira
N_steps       <- 60     # Passos de tempo (aprox. dias úteis em 2 meses)
dt            <- T_total / N_steps # Intervalo de tempo discreto

# Semente de aleatoriedade para garantir resultados reprodutíveis
set.seed(42)

# ===================================================================
# 1. PRECIFICAÇÃO DA OPÇÃO DE BARREIRA (DOWN-AND-IN CALL)
# ===================================================================
# Matriz para armazenar as trajetórias de preço
price_paths <- matrix(0, nrow = N_steps + 1, ncol = N_sim_barrier)
price_paths[1, ] <- S0

# Simulação das trajetórias sob a medida neutra ao risco (usando rf)
for (i in 1:N_sim_barrier) {
  for (t in 2:(N_steps + 1)) {
    z <- rnorm(1)
    price_paths[t, i] <- price_paths[t-1, i] * exp((rf - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
  }
}

# Cálculo do Payoff
final_prices <- price_paths[N_steps + 1, ]
min_prices   <- apply(price_paths, 2, min)

# O payoff só existe se a barreira for atingida (min_prices <= H)
payoffs_barrier <- ifelse(min_prices <= H, pmax(final_prices - K_barrier, 0), 0)

# Preço da opção: valor presente do payoff esperado
price_barrier <- mean(payoffs_barrier) * exp(-rf * T_total)

toc_barrier <- toc()

cat(sprintf("1) Preço da Opção de Barreira (Down-and-In Call): R$ %.4f\n", price_barrier))

# ===================================================================
# 2. PRECIFICAÇÃO DA OPÇÃO CHOOSER (SOLUÇÃO ANALÍTICA)
# ===================================================================
# Função para a fórmula de Black-Scholes (usada em várias partes)
BS_pricer <- function(S, K, T_mat, r, vol, type = "call") {
  d1 <- (log(S / K) + (r + 0.5 * vol^2) * T_mat) / (vol * sqrt(T_mat))
  d2 <- d1 - vol * sqrt(T_mat)
  
  if (type == "call") {
    price <- S * pnorm(d1) - K * exp(-r * T_mat) * pnorm(d2)
  } else if (type == "put") {
    price <- K * exp(-r * T_mat) * pnorm(-d2) - S * pnorm(-d1)
  }
  return(price)
}

# Preço via fórmula de Rubinstein: Chooser = Call(T) + Put(t_choice, K')
# Componente 1: Call europeia padrão
price_call_comp <- BS_pricer(S0, K_chooser, T_total, rf, sigma, type = "call")

# Componente 2: Put com strike ajustado e vencimento em t_choice
K_adj <- K_chooser * exp(-rf * (T_total - T_choice))
price_put_comp <- BS_pricer(S0, K_adj, T_choice, rf, sigma, type = "put")

# Preço final da Chooser
price_chooser_analytical <- price_call_comp + price_put_comp

cat(sprintf("2) Preço da Opção Chooser (Fórmula Analítica): R$ %.4f\n", price_chooser_analytical))

# ===================================================================
# 3. ANÁLISE DE PRECISÃO, HISTOGRAMAS E OTIMIZAÇÃO
# ===================================================================
# --- 3.1 Histograma de Preços ---
# Função para precificar a Chooser com Monte Carlo
price_chooser_mc <- function(n_sim_mc, S, K, T_tot, T_cho, r, vol) {
  # Simula o preço do ativo até o momento da escolha
  z <- rnorm(n_sim_mc)
  price_at_choice <- S * exp((r - 0.5 * vol^2) * T_cho + vol * sqrt(T_cho) * z)
  
  # Calcula o valor da chooser na data da escolha (máximo entre a call e a put)
  remaining_T <- T_tot - T_cho
  call_values <- BS_pricer(price_at_choice, K, remaining_T, r, vol, type = "call")
  put_values <- BS_pricer(price_at_choice, K, remaining_T, r, vol, type = "put")
  chooser_values_at_choice <- pmax(call_values, put_values)
  
  # Preço hoje é o valor presente do valor esperado na data da escolha
  price <- mean(chooser_values_at_choice) * exp(-r * T_cho)
  return(price)
}

# Rodar 100 vezes para cada cenário de simulação
n_runs <- 100
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
  geom_vline(xintercept = price_chooser_analytical, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = price_chooser_analytical * 1.01, y = 15, label = "Preço Analítico", 
           angle = 90, vjust = -0.5, color = "red") +
  labs(
    title = "Distribuição dos Preços da Opção Chooser por Monte Carlo",
    subtitle = "Comparação da precisão entre 1000 e 5000 histórias (100 execuções)",
    x = "Preço da Opção (R$)",
    y = "Frequência",
    fill = "Nº de Histórias"
  ) +
  theme_minimal()

print(hist_plot)

toc_hist <- toc()
cat("Histograma gerado. A distribuição com 5000 histórias é mais concentrada, indicando maior precisão.\n\n")


# --- 3.2 Otimização com Processamento Paralelo ---

# Número de simulações para o teste de performance
N_PERF_TEST <- 5000000 
# Gerar os preços na data da escolha uma única vez para garantir comparação justa
price_at_choice_global <- S0 * exp((rf - 0.5 * sigma^2) * T_choice + sigma * sqrt(T_choice) * rnorm(N_PERF_TEST))

# Função Sequencial (para referência)
price_chooser_mc_seq <- function(price_at_choice, K, T_tot, T_cho, r, vol) {
  remaining_T <- T_tot - T_cho
  call_values <- BS_pricer(price_at_choice, K, remaining_T, r, vol, "call")
  put_values  <- BS_pricer(price_at_choice, K, remaining_T, r, vol, "put")
  
  price <- mean(pmax(call_values, put_values)) * exp(-r * T_cho)
  return(price)
}

# Função Paralela (baseada em chunks)
price_chooser_mc_efficient_parallel <- function(price_at_choice, K, T_tot, T_cho, r, vol) {
  n_sim <- length(price_at_choice)
  
  # O loop itera sobre "chunks" de índices
  all_chooser_values <- foreach(indices = split(1:n_sim, 1:getDoParWorkers()), .combine = 'c') %dopar% {
    chunk_prices <- price_at_choice[indices]
    remaining_T <- T_tot - T_cho
    call_vals <- BS_pricer(chunk_prices, K, remaining_T, r, vol, "call")
    put_vals  <- BS_pricer(chunk_prices, K, remaining_T, r, vol, "put")
    pmax(call_vals, put_vals)
  }
  
  price <- mean(all_chooser_values) * exp(-r * T_cho)
  return(price)
}

# Configurar e registrar o cluster paralelo
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Exportar a função BS_pricer para cada worker
clusterExport(cl, "BS_pricer")

cat("--- Comparando Performance: Sequencial vs. Paralelo Eficiente ---\n")
cat("Usando", N_PERF_TEST, "histórias e", num_cores, "núcleos.\n\n")

# Teste 1: Sequencial
tic("Execução Sequencial")
price_seq <- price_chooser_mc_seq(price_at_choice_global, K_chooser, T_total, T_choice, rf, sigma)
toc_seq <- toc()
cat(sprintf("Preço (Sequencial): R$ %.4f\n", price_seq))

# Teste 2: Paralelo
tic("Execução Paralela")
price_par_eff <- price_chooser_mc_efficient_parallel(price_at_choice_global, K_chooser, T_total, T_choice, rf, sigma)
toc_par_eff <- toc()
cat(sprintf("Preço (Paralelo): R$ %.4f\n", price_par_eff))

# Parar o cluster para liberar recursos
stopCluster(cl)

cat("\n--- Fim do Exercício ---\n") 