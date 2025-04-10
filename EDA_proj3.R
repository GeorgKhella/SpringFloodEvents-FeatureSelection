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

# Visualizza le prime righe
head(data_207)
head(data_106)

######### ---EDA--- ###########

library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)    
library(lubridate)

# Histogramma di discharge con soglia 90%

q90_207 <- quantile(data_207$discharge, 0.9)
q90_106 <- quantile(data_106$discharge, 0.9)

ggplot(data_207, aes(x = discharge)) +
  geom_histogram(bins = 50, fill = "#1f77b4", alpha = 0.7) +
  xlim(0, 25) +
  geom_vline(xintercept = q90_207, linetype = "dashed", color = "red") +
  labs(title = "Discharge distribution - Catchment 207",
       x = "Discharge [mm]", y = "Count")

ggplot(data_106, aes(x = discharge)) +
  xlim(0, 25) +
  geom_histogram(bins = 50, fill = "#2ca02c", alpha = 0.7) +
  geom_vline(xintercept = q90_106, linetype = "dashed", color = "red") +
  labs(title = "Discharge distribution - Catchment 106",
       x = "Discharge [mm]", y = "Count")


# Plot stagionale della discharge (primavera)

spring_207 <- filter(data_207, month(date) %in% 3:5)
spring_106 <- filter(data_106, month(date) %in% 3:5)

ggplot(spring_207, aes(x = date, y = discharge)) +
  geom_line(color = "#1f77b4") +
  geom_hline(yintercept = q90_207, linetype = "dashed", color = "red") +
  labs(title = "Spring Discharge (Mar–May) - Catchment 207")

ggplot(spring_106, aes(x = date, y = discharge)) +
  geom_line(color = "#2ca02c") +
  geom_hline(yintercept = q90_207, linetype = "dashed", color = "red") +
  labs(title = "Spring Discharge (Mar–May) - Catchment 106")

data_207 %>%
  mutate(month = month(date, label = TRUE)) %>%
  ggplot(aes(x = month, y = discharge)) +
  geom_boxplot(fill = "#1f77b4", alpha = 0.6) +
  labs(title = "Monthly discharge distribution - Catchment 207")

data_106 %>%
  mutate(month = month(date, label = TRUE)) %>%
  ggplot(aes(x = month, y = discharge)) +
  geom_boxplot(fill = "#2ca02c", alpha = 0.6) +
  labs(title = "Monthly discharge distribution - Catchment 106")

# Stationarity 

data_207$roll_q90 <- rollapply(data_207$discharge, width = 365, FUN = function(x) quantile(x, 0.9), fill = NA, align = "right")
data_106$roll_q90 <- rollapply(data_106$discharge, width = 365, FUN = function(x) quantile(x, 0.9), fill = NA, align = "right")

# Plot per Catchment 207
ggplot(data_207, aes(x = date)) +
  geom_line(aes(y = roll_q90), color = "#1f77b4") +
  geom_hline(yintercept = q90_207, linetype = "dashed", color = "red") +
  labs(title = "Rolling 90th Percentile - Catchment 207",
       y = "Discharge [mm]", x = "Date") +
  theme_minimal()

# Plot per Catchment 106
ggplot(data_106, aes(x = date)) +
  geom_line(aes(y = roll_q90), color = "#2ca02c") +
  geom_hline(yintercept = q90_106, linetype = "dashed", color = "red") +
  labs(title = "Rolling 90th Percentile - Catchment 106",
       y = "Discharge [mm]", x = "Date") +
  theme_minimal()


# Density Plots

# Long format
covariates <- c("et", "precip", "radiation", "snowmelt", "soil_moisture", "temperature")

data_207_long <- data_207 %>%
  select(date, all_of(covariates)) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value")

ggplot(data_207_long, aes(x = value)) +
  geom_density(fill = "#1f77b4", alpha = 0.6) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Density plots of covariates - Catchment 207")

# idem per 106
data_106_long <- data_106 %>%
  select(date, all_of(covariates)) %>%
  pivot_longer(-date, names_to = "variable", values_to = "value")

ggplot(data_106_long, aes(x = value)) +
  geom_density(fill = "#2ca02c", alpha = 0.6) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Density plots of covariates - Catchment 106")

# Boxplots

data_207_labeled <- data_207 %>%
  select(all_of(covariates)) %>%
  mutate(catchment = "207")

data_106_labeled <- data_106 %>%
  select(all_of(covariates)) %>%
  mutate(catchment = "106")

combined <- bind_rows(data_207_labeled, data_106_labeled) %>%
  pivot_longer(-catchment, names_to = "variable", values_to = "value")

ggplot(combined, aes(x = catchment, y = value, fill = catchment)) +
  geom_boxplot(outlier.size = 0.7) +
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(title = "Covariate comparison across catchments")

# Overlay densità discharge

discharge_df <- data.frame(
  discharge = c(data_207$discharge, data_106$discharge),
  catchment = rep(c("207", "106"), each = nrow(data_207))
)

ggplot(discharge_df, aes(x = discharge, fill = catchment)) +
  geom_density(alpha = 0.5) +
  labs(title = "Discharge comparison between catchments")







# Aggiungi mese e giorno per filtro primavera
data_106 <- data_106 %>% mutate(month = month(date), spring = month %in% 3:5)
data_207 <- data_207 %>% mutate(month = month(date), spring = month %in% 3:5)

# Calcola la soglia del 90° percentile della discharge in primavera
q90_106 <- quantile(data_106$discharge[data_106$spring], probs = 0.9)
q90_207 <- quantile(data_207$discharge[data_207$spring], probs = 0.9)

# Crea la variabile T: 1 se sopra soglia, 0 altrimenti
data_106 <- data_106 %>% mutate(T = as.integer(discharge > q90_106))
data_207 <- data_207 %>% mutate(T = as.integer(discharge > q90_207))

# Funzione per density plot condizionato
plot_conditional_densities <- function(data, vars, catchment_label) {
  data$T <- factor(data$T, labels = c("No Flood", "Flood"))
  data_long <- data %>%
    select(T, all_of(vars)) %>%
    pivot_longer(-T, names_to = "variable", values_to = "value")
  
  ggplot(data_long, aes(x = value, fill = T)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~variable, scales = "free", ncol = 3) +
    labs(title = paste("Conditional densities by flood status – Catchment", catchment_label),
         x = NULL, y = "Density", fill = "Flood Event") +
    theme_minimal(base_size = 11)
}

vars <- c("et", "precip", "radiation", "snowmelt", "soil_moisture", "temperature")

plot_conditional_densities(data_106, vars, "106")
plot_conditional_densities(data_207, vars, "207")





# Funzione per plottare ACF
plot_acfs <- function(data, vars, catchment_label) {
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 3, 0))  # layout + margini
  
  for (v in vars) {
    acf(data[[v]],
        lag.max = 30,
        main = paste("ACF of", v),
        xlab = "Lag (days)",
        ylab = "Autocorrelation")
  }
  
  # Titolo generale per il plot (in alto al centro)
  mtext(paste("Autocorrelation Structure of Variables – Catchment", catchment_label),
        outer = TRUE, cex = 1.3, font = 2, line = 1)
}


plot_acfs(data_106, vars, "106")
plot_acfs(data_207, vars, "207")


