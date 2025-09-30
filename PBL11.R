# install.packages("Matrix")
# install.packages("ggplot2")
# install.packages("tidyr")

# Carregue as bibliotecas necessárias
library(ggplot2)
library(tidyr)
library(tidyverse)


#================================================================
# Função para Simulação de Monte Carlo para CDO
# (Esta função permanece inalterada)
#================================================================
run_cdo_simulation <- function(
    N = 100,                 # Número de empréstimos (bonds) na carteira
    T = 5,                   # Maturidade dos empréstimos em anos
    mu = 0.025,              # Crescimento da renda (ex: crescimento médio do PIB)
    sigma = 0.20,            # Volatilidade da renda
    rho = 0.2,               # Correlação entre as rendas dos tomadores
    initial_income = 100000,
    default_threshold = 50000,
    num_simulations = 10000,
    dt = 1/12                # Passo de tempo (mensal)
) {
  #' Executa a simulação de Monte Carlo para precificar as tranches de um CDO.
  
  num_steps <- as.integer(T / dt)
  
  # 1. Gerar choques aleatórios correlacionados
  corr_matrix <- matrix(rho, nrow = N, ncol = N)
  diag(corr_matrix) <- 1
  cholesky_factor <- tryCatch({
    t(chol(corr_matrix))
  }, error = function(e) {
    message(paste("Erro na decomposição de Cholesky para rho =", rho))
    return(NA)
  })
  
  if (any(is.na(cholesky_factor))) {
    return(list(prob_default = NA, pj = NA, pm = NA, ps = NA))
  }
  
  # Armazenar os resultados
  total_defaults <- 0
  payoffs_senior <- numeric(num_simulations)
  payoffs_mezzanine <- numeric(num_simulations)
  payoffs_junior <- numeric(num_simulations)
  
  for (i in 1:num_simulations) {
    Z_uncorrelated <- matrix(rnorm(N * num_steps), nrow = N, ncol = num_steps)
    Z_correlated <- cholesky_factor %*% Z_uncorrelated
    income_paths <- matrix(0, nrow = N, ncol = num_steps + 1)
    income_paths[, 1] <- initial_income
    
    for (t in 1:num_steps) {
      drift <- (mu - 0.5 * sigma^2) * dt
      shock <- sigma * sqrt(dt) * Z_correlated[, t]
      income_paths[, t + 1] <- income_paths[, t] * exp(drift + shock)
    }
    
    defaults_in_sim <- apply(income_paths < default_threshold, 1, any)
    num_defaults <- sum(defaults_in_sim)
    total_defaults <- total_defaults + num_defaults
    
    # 4. Calcular os payoffs da carteira e das tranches (NOVA LÓGICA)
    tranche_size <- N / 3
    P <- N - num_defaults
    
    ps_payoff <- min(tranche_size, P)
    payoffs_senior[i] <- ps_payoff
    
    pm_payoff <- min(tranche_size, P - ps_payoff)
    payoffs_mezzanine[i] <- pm_payoff
    
    pj_payoff <- min(tranche_size, P - ps_payoff - pm_payoff)
    payoffs_junior[i] <- pj_payoff
  }
  
  # 5. Calcular os valores esperados
  prob_default_individual <- total_defaults / (N * num_simulations)
  expected_pj <- mean(payoffs_junior)
  expected_pm <- mean(payoffs_mezzanine)
  expected_ps <- mean(payoffs_senior)
  
  return(list(
    prob_default = prob_default_individual,
    pj = expected_pj,
    pm = expected_pm,
    ps = expected_ps
  ))
}

#================================================================
# --- Script Principal ---
# (Esta parte permanece inalterada)
#================================================================
rho_values <- seq(0.1, 0.9, by = 0.1)
results_df <- data.frame(
  rho = rho_values,
  prob_default = numeric(length(rho_values)),
  pj = numeric(length(rho_values)),
  pm = numeric(length(rho_values)),
  ps = numeric(length(rho_values))
)

print("Executando simulações para diferentes níveis de correlação (rho)...")

for (i in 1:length(rho_values)) {
  rho <- rho_values[i]
  cat(sprintf("  Simulando para rho = %.2f\n", rho))
  sim_result <- run_cdo_simulation(rho = rho)
  results_df[i, -1] <- unlist(sim_result) # preenche a linha do data frame
}

#================================================================
# --- Plotar Resultados com ggplot2 ---
#================================================================
# --- Gráfico 1: Probabilidade de Default ---
plot_prob_default <- ggplot(results_df, aes(x = rho, y = prob_default)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Probabilidade de Default Individual vs. Correlação",
    x = "Correlação (ρ)",
    y = "Probabilidade de Default"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_continuous(breaks = rho_values) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

# --- Preparar dados para o Gráfico 2 ---
# Converter dados do formato "largo" para "longo"
tranche_size <- 100 / 3 # Valor nominal da tranche
results_long <- results_df %>%
  select(rho, ps, pm, pj) %>%
  pivot_longer(
    cols = c("ps", "pm", "pj"),
    names_to = "tranche",
    values_to = "payoff"
  ) %>%
  # Calcular o valor como % do nominal e criar uma variável de fator ordenada
  mutate(
    payoff_percent = (payoff / tranche_size) * 100,
    tranche = factor(tranche,
                     levels = c("ps", "pm", "pj"),
                     labels = c("Senior (Menor Risco)", "Mezzanine", "Junior (Maior Risco)"))
  )


# --- Gráfico 2: Valor Esperado das Tranches ---
plot_tranches <- ggplot(results_long, aes(x = rho, y = payoff_percent, color = tranche)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Valor Esperado das Tranches vs. Correlação",
    x = "Correlação (ρ)",
    y = "Valor Esperado (% do Valor Nominal)",
    color = "Tipo de Tranche" # Título da legenda
  ) +
  # Definir cores manualmente para manter a consistência
  scale_color_manual(values = c(
    "Senior (Menor Risco)" = "darkgreen",
    "Mezzanine" = "orange",
    "Junior (Maior Risco)" = "tomato"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = rho_values) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))


# Exibir os gráficos (eles aparecerão na aba "Plots" do RStudio, um de cada vez)
print(plot_prob_default)
print(plot_tranches)