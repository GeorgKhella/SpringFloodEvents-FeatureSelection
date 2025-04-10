# Carica i pacchetti
library(dplyr)
library(readr)

# Elenco dei file e nomi variabili
files <- list(
  discharge = "~/Downloads/data_discharge.csv",
  et = "~/Downloads/data_et.csv",
  precip = "~/Downloads/data_precip.csv",
  radiation = "~/Downloads/data_radiation.csv",
  snowmelt = "~/Downloads/data_snowmelt.csv",
  soil_moisture = "~/Downloads/data_soil_moisture.csv",
  temperature = "~/Downloads/data_temperature.csv"
)

# Funzione per estrarre dati per un solo catchment
extract_single_catchment <- function(file, var_name, id) {
  df <- read_csv(file, show_col_types = FALSE)
  col <- paste0("Catchment.", id)
  selected <- df[[col]]
  new_colname <- paste0(var_name)
  out <- data.frame(selected)
  names(out) <- new_colname
  return(out)
}

# ID dei due bacini
id_207 <- 207  # low
id_106 <- 106  # high

# Costruisci lista di variabili per ciascun catchment
vars_207 <- lapply(names(files), function(v) extract_single_catchment(files[[v]], v, id_207))
vars_106 <- lapply(names(files), function(v) extract_single_catchment(files[[v]], v, id_106))

# Costruisci la colonna date
n_days <- nrow(vars_207[[1]])
dates <- seq.Date(from = as.Date("1981-01-01"), by = "day", length.out = n_days)

# Crea i due dataset finali
data_207 <- bind_cols(date = dates, setNames(vars_207, names(files)))
data_106 <- bind_cols(date = dates, setNames(vars_106, names(files)))

data_207_original <- data_207  # conserva la versione con discharge per il LASSO

# Visualizza le prime righe
head(data_207)
head(data_106)




library(dplyr)
library(lubridate)
library(glmnet)
library(pROC)

# Costruzione target + rimozione discharge solo per logistic regression full
q90 <- quantile(data_207$discharge, 0.9, na.rm = TRUE)
data_207 <- data_207 %>%
  mutate(Y = as.integer(discharge > q90)) %>%
  select(-discharge)

# ==== 2. Creazione variabili laggate (fino a lag = 3 giorni) ====
add_lags <- function(df, varnames, max_lag = 3) {
  df_lagged <- df
  for (v in varnames) {
    for (lag in 1:max_lag) {
      df_lagged[[paste0(v, "_lag", lag)]] <- dplyr::lag(df[[v]], lag)
    }
  }
  return(df_lagged)
}

vars_to_lag <- setdiff(names(data_207), c("date", "Y"))
data_207_lagged <- add_lags(data_207, vars_to_lag, max_lag = 3)
data_207_lagged <- na.omit(data_207_lagged)

# ==== 3. Train/test split ====
train_data <- data_207_lagged %>% filter(year(date) < 2015)
test_data  <- data_207_lagged %>% filter(year(date) >= 2015)

# ==== 4. Regressione logistica completa (no penalizzazione) ====
model_full <- glm(Y ~ ., data = train_data %>% select(-date), family = binomial())

# ==== 5. Predizione ====
X_test <- test_data %>% select(-date, -Y)
y_test <- test_data$Y

probs <- predict(model_full, newdata = X_test, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# ==== 6. Confusion matrix e metriche ====
preds <- factor(preds, levels = c(0, 1))
y_test <- factor(y_test, levels = c(0, 1))
conf_matrix <- table(Predicted = preds, Actual = y_test)
print(conf_matrix)

TP <- ifelse("1" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix),
             conf_matrix["1", "1"], 0)
TN <- ifelse("0" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix),
             conf_matrix["0", "0"], 0)
FP <- ifelse("1" %in% rownames(conf_matrix) && "0" %in% colnames(conf_matrix),
             conf_matrix["1", "0"], 0)
FN <- ifelse("0" %in% rownames(conf_matrix) && "1" %in% colnames(conf_matrix),
             conf_matrix["0", "1"], 0)

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
recall <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
f1 <- ifelse((precision + recall) > 0, 2 * precision * recall / (precision + recall), NA)
auc_val <- auc(as.numeric(as.character(y_test)), probs)

# ==== 7. Stampa metriche ====
cat("=== Logistic Regression – Catchment 207 ===\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")













#### LASSO ####

# Pacchetti
library(glmnet)
library(tidyr)
library(purrr)
library(lubridate)
library(recipes)

# ----- 1. Costruzione della variabile binaria target -----
create_binary_target <- function(data, threshold_quantile = 0.9) {
  q90 <- quantile(data$discharge, probs = threshold_quantile, na.rm = TRUE)
  data$Y <- as.integer(data$discharge > q90)
  return(data)
}

data_207 <- create_binary_target(data_207_original)

# ----- 2. Creazione delle variabili laggate -----
add_lags <- function(df, varnames, max_lag = 3) {
  df_lagged <- df
  for (v in varnames) {
    for (lag in 1:max_lag) {
      df_lagged[[paste0(v, "_lag", lag)]] <- dplyr::lag(df[[v]], lag)
    }
  }
  return(df_lagged)
}

vars_to_lag <- setdiff(names(data_207), c("date", "discharge", "Y"))  # exclude outcome and date
data_207_lagged <- add_lags(data_207, vars_to_lag, max_lag = 3)
data_207_lagged <- na.omit(data_207_lagged)  # rimuove righe con NA introdotti dai lag

# ----- 3. Train/test split -----
train_data <- data_207_lagged %>% filter(year(date) < 2015)
test_data  <- data_207_lagged %>% filter(year(date) >= 2015)

# ----- 4. Preprocessing: standardizzazione -----
rec <- recipe(Y ~ ., data = train_data %>% select(-date, -discharge)) %>%
  step_normalize(all_predictors())

prep_rec <- prep(rec)
X_train <- bake(prep_rec, new_data = train_data) %>% select(-Y) %>% as.matrix()
y_train <- train_data$Y

X_test <- bake(prep_rec, new_data = test_data) %>% select(-Y) %>% as.matrix()
y_test <- test_data$Y

# ----- 5. LASSO logistic regression -----
set.seed(123)
lasso_model <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1, nfolds = 10)

# Lambda ottimale
lambda_best <- lasso_model$lambda.min

# Coefficienti selezionati
coef(lasso_model, s = "lambda.1se")

# ----- 6. Predizione e performance -----
probs <- predict(lasso_model, newx = X_test, s = "lambda.1se", type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Confusion matrix e metriche
table(Predicted = preds, Actual = y_test)

# Metriche
# ----- 6. Predizione e metriche (manuali) -----

# Predizione con soglia 0.5
probs <- predict(lasso_model, newx = X_test, s = "lambda.min", type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = preds, Actual = y_test)

TP <- conf_matrix["1", "1"]
TN <- conf_matrix["0", "0"]
FP <- conf_matrix["1", "0"]
FN <- conf_matrix["0", "1"]

# Calcolo metriche
accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall / (precision + recall)

cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1_score, 3), "\n")

# AUC (senza yardstick) 
library(pROC)
auc <- auc(y_test, probs)
cat("AUC:", round(auc, 3), "\n")



#### PC ALGO ####

# Pacchetti necessari
library(pcalg)
library(dplyr)
library(lubridate)
library(glmnet)
library(pROC)

# Crea variabile target e lag
data_lagged <- data_207 %>%
  mutate(Y = as.integer(discharge > quantile(discharge, 0.9))) %>%
  select(-discharge)

# Crea variabili laggate
add_lags <- function(df, vars, max_lag = 3) {
  for (v in vars) {
    for (lag in 1:max_lag) {
      df[[paste0(v, "_lag", lag)]] <- dplyr::lag(df[[v]], lag)
    }
  }
  return(df)
}
lag_vars <- setdiff(names(data_lagged), c("date", "Y"))
data_lagged <- add_lags(data_lagged, lag_vars, 3)
data_lagged <- na.omit(data_lagged)

# Standardizzazione e rimozione colonna "date"
data_std <- data_lagged %>%
  select(-date) %>%
  mutate(across(-Y, scale))  # scala tutto tranne Y


# Variabili
var_names <- names(data_std)

# Set up per pcalg
suffStat <- list(C = cor(data_std), n = nrow(data_std))

# Esegui PC algorithm
pc_fit <- pc(suffStat = suffStat,
             indepTest = gaussCItest,
             alpha = 0.05,
             labels = var_names,
             verbose = FALSE)

# Visualizza grafo orientato
# plot(pc_fit@graph)  # se vuoi, ma richiede Rgraphviz

# Trova Markov blanket del target "Y"
adj_mat <- as(pc_fit@graph, "matrix")
mb_vars <- names(which(adj_mat["Y", ] != 0 | adj_mat[, "Y"] != 0))
mb_vars <- setdiff(mb_vars, "Y")  # escludi Y stesso
cat("Markov blanket attorno a Y:", paste(mb_vars, collapse = ", "), "\n")

# Modello logistico usando solo le variabili MB
data_mb <- data_std %>%
  select(all_of(c(mb_vars, "Y")))

# Train-test split
data_mb$date <- data_lagged$date  # ripristina date per lo split

train <- data_mb %>% filter(year(date) < 2015) %>% select(-date)
test  <- data_mb %>% filter(year(date) >= 2015) %>% select(-date)

X_train <- model.matrix(Y ~ ., train)[, -1]
y_train <- train$Y

X_test <- model.matrix(Y ~ ., test)[, -1]
y_test <- test$Y

# Regressione logistica con L2
set.seed(123)
model <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)

# Predizione e metriche
probs <- predict(model, newx = X_test, s = "lambda.min", type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

TP <- sum(preds == 1 & y_test == 1)
TN <- sum(preds == 0 & y_test == 0)
FP <- sum(preds == 1 & y_test == 0)
FN <- sum(preds == 0 & y_test == 1)

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
auc_val <- auc(y_test, probs)

table(Predicted = preds, Actual = y_test)


cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")

# Mostra solo il Markov blanket attorno a Y
library(Rgraphviz)

nodes_MB <- c("Y", mb_vars)  # includi Y e suoi vicini
subgraph_MB <- subGraph(nodes_MB, pc_fit@graph)

plot(subgraph_MB)






#### MXM ####

#### Local Causal Discovery with MMPC (MXM package) ####
# Pacchetti
library(infotheo)
library(dplyr)
library(lubridate)
library(glmnet)
library(pROC)

# Calcolo mutual information tra Y e tutte le covariate
data_mi <- data_std
data_mi$Y <- as.factor(data_mi$Y)

mi_scores <- sapply(data_mi %>% select(-Y), function(x) {
  mutinformation(discretize(x), data_mi$Y)
})

# Seleziona le top-k covariate (es: 8)
top_k <- 8
selected_vars <- names(sort(mi_scores, decreasing = TRUE))[1:top_k]
cat("Top", top_k, "variabili per MI:", paste(selected_vars, collapse = ", "), "\n")

# Crea dataset
data_topmi <- data_std %>% select(all_of(selected_vars), Y)
data_topmi$date <- data_lagged$date

# Train/test split
train <- data_topmi %>% filter(year(date) < 2015) %>% select(-date)
test  <- data_topmi %>% filter(year(date) >= 2015) %>% select(-date)

X_train <- model.matrix(Y ~ ., train)[, -1]
y_train <- train$Y
X_test <- model.matrix(Y ~ ., test)[, -1]
y_test <- test$Y

# Modello L2
set.seed(123)
model_mi <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)

probs <- predict(model_mi, newx = X_test, s = "lambda.min", type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Metriche
TP <- sum(preds == 1 & y_test == 1)
TN <- sum(preds == 0 & y_test == 0)
FP <- sum(preds == 1 & y_test == 0)
FN <- sum(preds == 0 & y_test == 1)

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
auc_val <- auc(y_test, probs)

# Output
cat("Performance modello con top", top_k, "variabili per MI:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")











##### Prepara data_106 #####

# Costruzione della variabile binaria target
data_106 <- create_binary_target(data_106)

# Creazione variabili laggate
vars_to_lag_106 <- setdiff(names(data_106), c("date", "discharge", "Y"))
data_106_lagged <- add_lags(data_106, vars_to_lag_106, max_lag = 3)
data_106_lagged <- na.omit(data_106_lagged)

# Train/test split
train_data_106 <- data_106_lagged %>% filter(year(date) < 2015)
test_data_106  <- data_106_lagged %>% filter(year(date) >= 2015)

# Standardizzazione
rec_106 <- recipe(Y ~ ., data = train_data_106 %>% select(-date, -discharge)) %>%
  step_normalize(all_predictors())
prep_rec_106 <- prep(rec_106)

X_train_106 <- bake(prep_rec_106, new_data = train_data_106) %>% select(-Y) %>% as.matrix()
y_train_106 <- train_data_106$Y
X_test_106  <- bake(prep_rec_106, new_data = test_data_106) %>% select(-Y) %>% as.matrix()
y_test_106  <- test_data_106$Y




##### LASSO su data_106 #####

set.seed(123)
lasso_106 <- cv.glmnet(X_train_106, y_train_106, family = "binomial", alpha = 1)
probs_lasso_106 <- predict(lasso_106, newx = X_test_106, s = "lambda.min", type = "response")
preds_lasso_106 <- ifelse(probs_lasso_106 > 0.5, 1, 0)

conf_106 <- table(Predicted = preds_lasso_106, Actual = y_test_106)
TP <- conf_106["1", "1"]
TN <- conf_106["0", "0"]
FP <- conf_106["1", "0"]
FN <- conf_106["0", "1"]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
auc_val <- auc(y_test_106, probs_lasso_106)

cat("=== LASSO 106 ===\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")

# Regressione logistica completa su dati non standardizzati
glm_full_106 <- glm(Y ~ ., data = train_data_106 %>% select(-date, -discharge), family = binomial())

# Test set
X_test_full_106 <- test_data_106 %>% select(-date, -discharge, -Y)
y_test_106 <- test_data_106$Y

# Predizione
probs_full_106 <- predict(glm_full_106, newdata = X_test_full_106, type = "response")
preds_full_106 <- ifelse(probs_full_106 > 0.5, 1, 0)

# Metriche
preds_full_106 <- factor(preds_full_106, levels = c(0, 1))
y_test_106 <- factor(y_test_106, levels = c(0, 1))
conf_matrix_full_106 <- table(Predicted = preds_full_106, Actual = y_test_106)

TP <- conf_matrix_full_106["1", "1"]
TN <- conf_matrix_full_106["0", "0"]
FP <- conf_matrix_full_106["1", "0"]
FN <- conf_matrix_full_106["0", "1"]

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
auc_val <- auc(as.numeric(as.character(y_test_106)), probs_full_106)

cat("=== Full Logistic Regression – Catchment 106 ===\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")


##### PC ALGORITHM su data_106 #####

# Lag + target
data_lagged_106 <- data_106 %>%
  mutate(Y = as.integer(discharge > quantile(discharge, 0.9))) %>%
  select(-discharge)

lag_vars_106 <- setdiff(names(data_lagged_106), c("date", "Y"))
data_lagged_106 <- add_lags(data_lagged_106, lag_vars_106, 3)
data_lagged_106 <- na.omit(data_lagged_106)

# Standardizzazione
data_std_106 <- data_lagged_106 %>%
  select(-date) %>%
  mutate(across(-Y, scale))

# PC algorithm
suffStat_106 <- list(C = cor(data_std_106), n = nrow(data_std_106))
pc_fit_106 <- pc(suffStat = suffStat_106,
                 indepTest = gaussCItest,
                 alpha = 0.05,
                 labels = names(data_std_106),
                 verbose = FALSE)

# Estrai Markov Blanket
adj_106 <- as(pc_fit_106@graph, "matrix")
mb_106 <- names(which(adj_106["Y", ] != 0 | adj_106[, "Y"] != 0))
mb_106 <- setdiff(mb_106, "Y")

cat("Markov blanket attorno a Y (106):", paste(mb_106, collapse = ", "), "\n")

# Logistic regression solo su MB
data_mb_106 <- data_std_106 %>% select(all_of(c(mb_106, "Y")))
data_mb_106$date <- data_lagged_106$date

train <- data_mb_106 %>% filter(year(date) < 2015) %>% select(-date)
test  <- data_mb_106 %>% filter(year(date) >= 2015) %>% select(-date)

X_train <- model.matrix(Y ~ ., train)[, -1]
y_train <- train$Y
X_test <- model.matrix(Y ~ ., test)[, -1]
y_test <- test$Y

# Regressione logistica (L2)
set.seed(123)
model <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)
probs <- predict(model, newx = X_test, s = "lambda.min", type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Metriche
TP <- sum(preds == 1 & y_test == 1)
TN <- sum(preds == 0 & y_test == 0)
FP <- sum(preds == 1 & y_test == 0)
FN <- sum(preds == 0 & y_test == 1)

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
auc_val <- auc(y_test, probs)

cat("=== PC 106 ===\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")

# ====== PLOT DEL MARKOV BLANKET ======
library(Rgraphviz)

nodes_MB_106 <- c("Y", mb_106)
subgraph_MB_106 <- subGraph(nodes_MB_106, pc_fit_106@graph)

# Plot singolo
dev.new()
plot(subgraph_MB_106)






##### Local Causal Discovery with MMPC – Catchment 106 #####
#### MXM ####
#### MXM ####

#### Local Causal Discovery with MMPC (simulato via MI) – Catchment 106 ####
# Pacchetti
library(infotheo)
library(dplyr)
library(lubridate)
library(glmnet)
library(pROC)

# Calcolo mutual information tra Y e tutte le covariate
data_mi_106 <- data_std_106
data_mi_106$Y <- as.factor(data_mi_106$Y)

mi_scores_106 <- sapply(data_mi_106 %>% select(-Y), function(x) {
  mutinformation(discretize(x), data_mi_106$Y)
})

# Seleziona le top-k covariate (es: 8)
top_k <- 8
selected_vars_106 <- names(sort(mi_scores_106, decreasing = TRUE))[1:top_k]
cat("Top", top_k, "variabili per MI (106):", paste(selected_vars_106, collapse = ", "), "\n")

# Crea dataset
data_topmi_106 <- data_std_106 %>% select(all_of(selected_vars_106), Y)
data_topmi_106$date <- data_lagged_106$date

# Train/test split
train <- data_topmi_106 %>% filter(year(date) < 2015) %>% select(-date)
test  <- data_topmi_106 %>% filter(year(date) >= 2015) %>% select(-date)

X_train <- model.matrix(Y ~ ., train)[, -1]
y_train <- train$Y
X_test <- model.matrix(Y ~ ., test)[, -1]
y_test <- test$Y

# Modello L2
set.seed(123)
model_mi <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)

probs <- predict(model_mi, newx = X_test, s = "lambda.min", type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Metriche
TP <- sum(preds == 1 & y_test == 1)
TN <- sum(preds == 0 & y_test == 0)
FP <- sum(preds == 1 & y_test == 0)
FN <- sum(preds == 0 & y_test == 1)

accuracy <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1 <- 2 * precision * recall / (precision + recall)
auc_val <- auc(y_test, probs)

# Output
cat("Performance modello con top", top_k, "variabili per MI (simulazione MMPC) – 106:\n")
cat("Accuracy:", round(accuracy, 3), "\n")
cat("Precision:", round(precision, 3), "\n")
cat("Recall:", round(recall, 3), "\n")
cat("F1 Score:", round(f1, 3), "\n")
cat("AUC:", round(auc_val, 3), "\n")

