library(tidyverse)


dt_train <- read.csv("/Users/mpa/Library/Mobile Documents/com~apple~CloudDocs/mba monografia/df_train_final.csv", sep = ",")
X_test <- read.csv("/Users/mpa/Library/Mobile Documents/com~apple~CloudDocs/mba monografia/X_test_scaled.csv", sep = ",")
y_test <- read.csv("/Users/mpa/Library/Mobile Documents/com~apple~CloudDocs/mba monografia/y_test.csv", sep = ",")


# Criar 10000 amostras bootstrap
set.seed(123)
bootstrap_dfs <- lapply(1:1000, function(i) {
  dt_train[sample(1:nrow(dt_train), size = 700, replace = TRUE), ]
})

# Ajustar regressão logística (sem regularização, usando glm)
coefs_list <- list()
intercepts_list <- c()

for (i in 1:1000) {
  df_boot <- bootstrap_dfs[[i]]
  model <- glm(target ~ ., data = df_boot, family = binomial(link = "logit"))
  
  coefs_list[[i]] <- coef(model)[-1]    # exclui intercepto
  intercepts_list[i] <- coef(model)[1]  # intercepto
}

# O tipo de objeto em coefs_list
class(coefs_list)  # é uma lista

# Distribuição amostral dos coeficientes
num_coefs <- length(coefs_list[[1]])
n_cols <- 3
plots_coefs <- list()

for (j in 1:num_coefs) {
  coef_amostral <- sapply(coefs_list, function(x) x[j])
  
  p <- ggplot(data.frame(coef = coef_amostral), aes(x = coef)) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black", alpha=0.9) +
    theme(plot.title = element_text(size=12)) +
        labs(title = paste("Distribuição amostral Beta",j),
         x = paste("Valor coeficiente"),
         y = "Frequência")
  plots_coefs[[j]] <- p
}
do.call(grid.arrange, c(plots_coefs, ncol = n_cols))

# Distribuição amostral do intercepto
p_intercept <- ggplot(data.frame(intercept = intercepts_list), aes(x = intercept)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black", alpha=0.9) +
  theme(plot.title = element_text(size=12)) +
      labs(title = "Distribuição amostral Intercepto",
       x = "Valor intercepto",
       y = "Frequência")
print(p_intercept)

# Exemplo de checagem das estruturas
length(coefs_list)      # Deve ser 1000
length(intercepts_list) # Deve ser 1000
length(coefs_list[[1]]) # Deve ser 9 (número de variáveis preditoras)

# ---------------------------------------------------------
# 2. Converter lista de coeficientes em matriz (1000 x 9)
#    Cada linha = uma réplica bootstrap
# ---------------------------------------------------------
coefs_matrix <- do.call(rbind, coefs_list)

# ---------------------------------------------------------
# 3. Calcular média amostral de cada parâmetro
#    (para cada coluna = coeficiente de uma variável)
# ---------------------------------------------------------
mean_coefs <- colMeans(coefs_matrix)           # Vetor de 9 médias
mean_intercept <- mean(intercepts_list)        # Média do intercepto

# ---------------------------------------------------------
# 4. Combinar intercepto e coeficientes médios em um único vetor
# ---------------------------------------------------------
params_mean <- c(mean_intercept, mean_coefs)
names(params_mean) <- c("(Intercept)", colnames(dt_train))[!colnames(dt_train) %in% "target"]

print("Estimativas médias dos parâmetros:")
print(params_mean)


# ---------------------------------------------------------
# 5. Construir o modelo manualmente com os parâmetros médios
#    Fórmula do logito: logit(p) = β0 + β1x1 + ... + β9x9
# ---------------------------------------------------------
# Adicionar uma coluna de 1s para o intercepto
X_matrix <- as.matrix(cbind(Intercept = 1, X_train_scaled))

# Calcular o logito (β0 + Σβi * Xi)
logit_values <- X_matrix %*% params_mean

# Converter logito em probabilidade (função logística)
p_hat <- 1 / (1 + exp(-logit_values))

# ---------------------------------------------------------
# 6. Criar um novo data frame com as probabilidades previstas
# ---------------------------------------------------------
df_pred <- data.frame(
  p_hat = as.vector(p_hat),
  Target = y_train
)

# Exibir as primeiras linhas
head(df_pred)

# ---------------------------------------------------------
# 7. Opcional: Visualizar a distribuição das probabilidades previstas
# ---------------------------------------------------------
library(ggplot2)

ggplot(df_pred, aes(x = p_hat, fill = as.factor(Target))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(
    title = "Distribuição das probabilidades previstas (modelo médio bootstrap)",
    x = "Probabilidade prevista de inadimplência",
    y = "Frequência",
    fill = "Target"
  ) +
  theme_minimal()





