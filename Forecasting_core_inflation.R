library(zoo)
library(tidyverse)
library(MARSS)
library(forecast)
library(dplyr)

file_paths <- list.files(path = "High Frequency Indicators", pattern = "*.csv", full.names = TRUE)
load_data <- function(path) {
  df <- read.csv(path)
  colnames(df) <- c("date", "value")
  df <- df %>%
    mutate(date = as.Date(paste0("01-", date), format = "%d-%b-%y"))
  return(df)
}

indicator_data <- lapply(file_paths, load_data)
combined_data <- reduce(indicator_data, full_join, by = "date")
colnames(combined_data)[-1] <- paste0("indicator_", seq_along(file_paths))

combined_data <- combined_data %>%
  mutate(across(starts_with("indicator_"), ~ (.-mean(. , na.rm = TRUE)) / sd(. , na.rm = TRUE)))


data_matrix <- as.matrix(combined_data[,-1])  
data_matrix <- t(data_matrix)  
dim(data_matrix)



data_matrix <- zoo::na.approx(data_matrix)

data_matrix <- data_matrix[, colSums(is.na(data_matrix)) == 0]


sum(is.na(data_matrix))  # Should be 0 for no missing values

weights <- c(0.3, 0.2, 0.2, 0.1, 0.05, 0.05, 0.1)
Z_matrix <- matrix(weights, nrow = 7, ncol = 1)


if (nrow(Z_matrix) != nrow(data_matrix)) {
  stop("Number of weights does not match the number of indicators")
}


model_list <- list(
  Z = Z_matrix,                 
  B = "diagonal and equal",   
  U = "zero",                   
  Q = "unconstrained",          
  R = "unconstrained",          
  x0 = "zero",                  
  A = "zero"                    
)


# Here we are using MARSS
demand_model <- MARSS(data_matrix, model = model_list, control = list(maxit = 100000))


demand_index <- demand_model$states[1, ]
length(demand_index)


# Subset combined_data to match the number of rows in demand_index
combined_data <- combined_data[1:length(demand_index), ]

# Assign demand_index to the adjusted combined_data
combined_data$demand_index <- demand_index
demand_index_data <- combined_data[,c(1,9)]

demand_index_pct_change <- (demand_index / lag(demand_index) - 1) * 100
length(demand_index)


demand_index_data <- data.frame(
  date = final_agg_data$date,  # Exclude the first date to match lengths
  demand_index_pct_change = demand_index_pct_change
)

demand_index_annual <- demand_index_data %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarize(demand_index_avg = mean(demand_index_pct_change, na.rm = TRUE))


ggplot(combined_data, aes(x = date, y = demand_index)) +
  geom_line(color = "blue") +
  labs(title = "Estimated Demand Index Over Time", x = "Date", y = "Demand Index") +
  theme_minimal()

                                            ## DEMAND INDEX ENDS HERE ##
##############################################################################################################################

cpi_file_paths <- list.files(path = "Different CPI Groups", pattern = "*.csv", full.names = TRUE)
component_1 <- read.csv("Different CPI Groups/Urban Clothing and Footwear CF.csv")
component_1_value <- component_1$X120.6
component_2 <- read.csv("Different CPI Groups/Urban Food and Beverages FB.csv")
component_2_value <- component_2$X123.7
component_3 <- read.csv("Different CPI Groups/Urban Fuel and Light.csv")
component_3_value <- component_3$X114
component_4 <- read.csv("Different CPI Groups/Urban Housing.csv")
component_4_value <- component_4$X118.1
component_5 <- read.csv("Different CPI Groups/Urban Miscellaneous.csv")
component_5_value <- component_5$X113.2
component_6 <- read.csv("Different CPI Groups/Urban PanTobacco and Intoxicants.csv")
component_6_value <- component_6$X128.1

length(demand_index)
length(component_2_value)


forecast_horizon <- 12

# ARIMA without seasonal component
model_comp1 <- auto.arima(component_1_value, xreg = demand_index, seasonal = FALSE)
forecast_comp1 <- forecast(model_comp1, xreg = demand_index[1:forecast_horizon])

model_comp2 <- auto.arima(component_2_value, xreg = demand_index, seasonal = FALSE)
forecast_comp2 <- forecast(model_comp2, xreg = demand_index[1:forecast_horizon])

model_comp3 <- auto.arima(component_3_value, xreg = demand_index, seasonal = FALSE)
forecast_comp3 <- forecast(model_comp3, xreg = demand_index[1:forecast_horizon])

model_comp4 <- auto.arima(component_4_value, xreg = demand_index, seasonal = FALSE)
forecast_comp4 <- forecast(model_comp4, xreg = demand_index[1:forecast_horizon])

model_comp5 <- auto.arima(component_5_value, xreg = demand_index, seasonal = FALSE)
forecast_comp5 <- forecast(model_comp5, xreg = demand_index[1:forecast_horizon])

model_comp6 <- auto.arima(component_6_value, xreg = demand_index, seasonal = FALSE)
forecast_comp6 <- forecast(model_comp6, xreg = demand_index[1:forecast_horizon])













str(forecast_comp1)

forecast_values <- data.frame(
  comp1 = forecast_comp1$mean,
  comp2 = forecast_comp2$mean,
  comp3 = forecast_comp3$mean,
  comp4 = forecast_comp4$mean,
  comp5 = forecast_comp5$mean,
  comp6 = forecast_comp6$mean
)


forecasted_cpi <- rowSums(forecast_values)
forecasted_cpi_df <- data.frame(forecasted_cpi)

forecast_dates <- seq(from = as.Date("2024-08-01"), by = "month", length.out = forecast_horizon)
forecasted_cpi_df$Date <- forecast_dates

# Create the plot
ggplot(forecasted_cpi_df, aes(x = Date, y = forecasted_cpi)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Disaggregated Demand Index", 
       x = "Date", 
       y = "Forecasted CPI Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))




# Get the minimum and maximum values of the forecasted CPI





####################################### Aggregate Forecasting ###################################

core_inflation <- read.csv("Core Inflation.csv")
colnames(core_inflation) <- c("date", "core_inflation_value")
core_inflation <- core_inflation %>% 
  mutate(date = as.Date(paste0("01-", date), format = "%d-%b-%y"))

final_data_agg <- demand_index_data %>% left_join(core_inflation, by = "date")
sum(is.na(final_data_agg))

core_inflation_values <- final_data_agg$core_inflation_value


arimax_model_agg <- auto.arima(core_inflation_values, xreg = demand_index, seasonal = TRUE)


forecast_horizon <- 12 #Horizon is 3-4 quaters
forecast_agg <- forecast(arimax_model_agg, xreg = demand_index[1:forecast_horizon])


correction_factor <- 186.0099 / 190.0251
forecast_agg <- forecast_agg$mean * correction_factor
str(forecast_agg)

start_date <- as.Date("2024-09-01")


forecast_horizon <- length(forecast_agg) 
forecast_dates <- seq.Date(from = start_date, by = "month", length.out = forecast_horizon)


forecast_agg_df <- data.frame(
  date = forecast_dates,
  core_inflation = as.numeric(forecast_agg)  # Convert to numeric if needed
)

forecast_values <- forecast_agg$mean


last_date <- max(core_inflation$date)
forecast_dates <- seq(last_date, by = "month", length.out = forecast_horizon)
combined_forecast_data <- data.frame(
  date = c(core_inflation$date, forecast_dates),
  core_inflation = c(core_inflation$core_inflation_value, forecast_values) #Replace `forecast_values` with your forecasted results
)






final_data_disagg <- combined_data_cpi %>%
  left_join(demand_index_data, by = "date")







ggplot(core_inflation, aes(x = date, y = core_inflation_value)) +
  geom_line(color = "blue") +  # Line plot
  labs(title = "Core Inflation Over Time", x = "Date", y = "Core Inflation Value") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(forecast_agg_df, aes(x = date, y = core_inflation)) +
  geom_line(color = "blue") +
  labs(title = "Aggregated Core Inflation Forecast 3-4 Quarters", x = "Month", y = "Core Inflation") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +  # Display years on x-axis
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
