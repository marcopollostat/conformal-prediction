
# Carregar pacotes necessários
library(dplyr)        # Manipulação de dados
library(ggplot2)      # Gráficos
library(caret)        # Split treino/teste
library(tidyr)        # Organização de dados
library(gridExtra)    # Combinar gráficos
library(scales)       # Funções auxiliares para eixos
library(forcats)      # Manipulação de variáveis fator

# 1. Carregar o dataset
df <- read.csv("/Users/mpa/Library/Mobile Documents/com~apple~CloudDocs/mba monografia/df_renamed_cols.csv", sep = ";")
#q. como obter o tipo das colunas
str(df)


# 2. Identificar tipos de variáveis
numerical_cols <- names(df)[sapply(df, is.numeric)]
continuos_cols <- df %>% select(where(is.numeric)) %>% select(where(~ n_distinct(.) > 10)) %>% names()
discrete_cols  <- df %>% select(where(is.numeric)) %>% select(where(~ n_distinct(.) <= 10)) %>% names()

# 3. Histograma das variáveis contínuas com limites de outlier
n_cols <- 3
plots_continuos <- list()
for (col in continuos_cols) {
  q25 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  q75 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  iqr <- q75 - q25
  cutt_off <- 1.5 * iqr
  lower_bound <- q25 - cutt_off
  upper_bound <- q75 + cutt_off
  
  p <- ggplot(df, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    geom_vline(xintercept = lower_bound, color = "red", linetype = "dashed") +
    geom_vline(xintercept = upper_bound, color = "blue", linetype = "dashed") +
    labs(title = paste("Distribuição de", col), x = col, y = "Frequência")
  
  plots_continuos[[col]] <- p
}

do.call(grid.arrange, c(plots_continuos, ncol = n_cols))

# 4. Estatísticas descritivas das variáveis contínuas
summary_stats <- df %>%
  select(all_of(continuos_cols)) %>%
  summary()
print(summary_stats)

# 5. Gráficos de barras para variáveis discretas
plots_discretas <- list()
n_cols <- 4
for (col in discrete_cols) {
  p <- ggplot(df, aes_string(x = col)) +
    geom_bar(fill = "lightgreen", color = "black") +
    labs(x = col, y = "Frequência")
  plots_discretas[[col]] <- p
}
do.call(grid.arrange, c(plots_discretas, ncol = n_cols))

# 6. Checar variáveis com baixa variabilidade (<1% de valores únicos)
unique_counts <- sapply(df[numerical_cols], function(x) length(unique(x)))
perc_unique <- unique_counts / nrow(df) * 100
to_del2 <- names(unique_counts[perc_unique < 1])
print(paste("Para deletar (<1%):", paste(to_del2, collapse = ", ")))

# Remover variáveis com baixa variabilidade
df <- df %>% select(-all_of(to_del2))

# 7. Gráficos de barras para variáveis categóricas
plots_categoricas <- list()
n_cols <- 3
for (col in categorical_cols) {
  p <- ggplot(df, aes_string(x = col)) +
    geom_bar(fill = "lightcoral", color = "black") +
    labs(title = paste("Distribuição de", col), x = col, y = "Frequência") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  plots_categoricas[[col]] <- p
}
do.call(grid.arrange, c(plots_categoricas, ncol = n_cols))

# 8. Split treino/teste
set.seed(42)
train_index <- createDataPartition(df$target, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data  <- df[-train_index, ]

X_train <- train_data %>% select(-target)
y_train <- train_data$target
X_test  <- test_data %>% select(-target)
y_test  <- test_data$target

cat("Train shape:", nrow(X_train), "Test shape:", nrow(X_test), "\n")

# 9. Calcular variância das variáveis numéricas
variancias <- sapply(X_train, function(x) var(as.numeric(x), na.rm = TRUE))
variancia_df <- data.frame(variancia = variancias)
print(variancia_df)

# 10. Padronizar variáveis numéricas (média 0, desvio 1)
X_train_scaled <- as.data.frame(scale(X_train))
df_train <- cbind(X_train_scaled, target = y_train)

# 11. Criar 1000 amostras bootstrap
set.seed(42)
bootstrap_dfs <- lapply(1:1000, function(i) {
  df_train[sample(1:nrow(df_train), size = 700, replace = TRUE), ]
})

# 12. Ajustar regressão logística (sem regularização, usando glm)
coefs_list <- list()
intercepts_list <- c()

for (i in 1:1000) {
  df_boot <- bootstrap_dfs[[i]]
  model <- glm(target ~ ., data = df_boot, family = binomial(link = "logit"))
  
  coefs_list[[i]] <- coef(model)[-1]    # exclui intercepto
  intercepts_list[i] <- coef(model)[1]  # intercepto
}

# 13. Distribuição amostral dos parâmetros (coeficientes)
num_coefs <- length(coefs_list[[1]])
n_cols <- 3
plots_coefs <- list()

for (j in 1:num_coefs) {
  coef_amostral <- sapply(coefs_list, function(x) x[j])
  
  p <- ggplot(data.frame(coef = coef_amostral), aes(x = coef)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Distribuição amostral do Beta", j),
         x = "Valor do coeficiente", y = "Frequência")
  
  plots_coefs[[j]] <- p
}

do.call(grid.arrange, c(plots_coefs, ncol = n_cols))


