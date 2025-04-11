##FINAL AGGREGATED VERSION

install.packages('astsa')
install.packages('tidyverse')
install.packages('KFAS')
install.packages('uroot')
install.packages('ggfortify')
install.packages('prophet')
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggcorrplot)
library(forecast)
library(tseries)
library(astsa)
library(stats)
library(car)
library(TSA)
library(tidyverse)
library(lmtest)
library(KFAS)
library(uroot)
library(ggfortify)
library(xts)
library(hts)
library(bsts)
library(data.table)
library(prophet)

# Load the dataset
df <- fread('Walmart_sales.csv')

# Format Date
df$Date <- as.Date(df$Date, format='%d-%m-%Y')
df <- df %>% arrange(Date, Store)

# Display basic information
cat("Basic Information:\n")
print(str(df))

# Summary statistics
cat("\nSummary Statistics:\n")
print(summary(df))

# Check for missing values
cat("\nMissing Values:\n")
print(colSums(is.na(df)))


# Time series visualization
ggplot(df %>% group_by(Date) %>% summarise(Total_Weekly_Sales = sum(Weekly_Sales)), aes(x=Date, y=Total_Weekly_Sales)) +
  geom_line(color='blue') +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title='Total Weekly Sales Over Time', x='Month', y='Total Weekly Sales') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# Sales by week for Jan-Dec 2011
df_2011 <- df %>% filter(year(Date) == 2011)

ggplot(df_2011 %>% group_by(Date) %>% summarise(Total_Weekly_Sales = sum(Weekly_Sales)),
       aes(x=Date, y=Total_Weekly_Sales)) +
  geom_line(color='red') +
  scale_x_date(date_labels="%b %d, %Y", date_breaks="1 week") +
  labs(title='Weekly Sales in 2011', x='Date', y='Total Weekly Sales') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# Number of data points per store
store_counts <- df %>% group_by(Store) %>% summarise(Count = n())
ggplot(store_counts, aes(x=factor(Store), y=Count)) +
  geom_bar(stat='identity', fill='purple') +
  labs(title='Number of Data Points by Store', x='Store', y='Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))



# Correlation heatmap
cor_matrix <- cor(df %>% select(Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment), use="complete.obs")
ggcorrplot(cor_matrix, lab=TRUE, title='Feature Correlation Heatmap')

# Trend and seasonality decomposition
#store_sales <- df %>% filter(Store == 1) %>% group_by(Date) %>% summarise(Weekly_Sales = sum(Weekly_Sales))
store_sales <- df %>% group_by(Date) %>% summarise(Weekly_Sales = sum(Weekly_Sales))
ts_data <- ts(store_sales$Weekly_Sales, frequency=52)
decomp <- decompose(ts_data)
plot(decomp)

# Holiday impact analysis
holiday_sales <- df %>% group_by(Holiday_Flag) %>% summarise(Average_Weekly_Sales = mean(Weekly_Sales))
ggplot(holiday_sales, aes(x=factor(Holiday_Flag), y=Average_Weekly_Sales)) +
  geom_bar(stat='identity', fill='steelblue') +
  labs(title='Impact of Holidays on Sales', x='Holiday (1: Yes, 0: No)', y='Average Weekly Sales') +
  theme_minimal()


# # Granger Causality Tests
cat("\nGranger Causality Tests:\n")
granger_test_temp <- grangertest(Weekly_Sales ~ Temperature, order=2, data=df)
granger_test_cpi <- grangertest(Weekly_Sales ~ CPI, order=2, data=df)
granger_test_unemployment <- grangertest(Weekly_Sales ~ Unemployment, order=2, data=df)
granger_test_fuel_price <- grangertest(Weekly_Sales ~ Fuel_Price, order=2, data=df)

print(granger_test_temp)
# The p-value is 0.049, which is less than 0.05 → We reject the null hypothesis.
# Conclusion: Temperature Granger-causes Weekly Sales, meaning that past temperature values can help predict future sales.
print(granger_test_cpi)
# p-value <.05, cpi granger causes weekly sales
print(granger_test_unemployment)
# p-value =0.112, unemployment does not granger cause weekly sales
print(granger_test_fuel_price)
# p-value <.05, Fuel Price granger causes weekly sales

# summarize data for total sales for all stores
df_total = df %>% group_by(Date) %>% summarise(Total_Weekly_Sales = sum(Weekly_Sales), CPI = mean(CPI), Unemployment = mean(Unemployment), avg_temp = mean(Temperature), Fuel_Price = mean(Fuel_Price), Holiday_Flag = max(Holiday_Flag))


ggplot(df_total, aes(x=Date, y=Total_Weekly_Sales)) +
  geom_line(color='blue') +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title='Total Weekly Sales Over Time', x='Month', y='Total Weekly Sales') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(df_total, aes(x=Date, y=CPI)) +
  geom_line(color='orange') +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title='CPI Over Time', x='Month', y='CPI') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(df_total, aes(x=Date, y=Unemployment)) +
  geom_line(color='gray') +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title='Unemployment Over Time', x='Month', y='Unemployment') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))


ggplot(df_total, aes(x=Date, y=avg_temp)) +
  geom_line(color='red') +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title='Avg Temperature Over Time', x='Month', y='Avg Temp') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(df_total, aes(x=Date, y=Fuel_Price)) +
  geom_line(color='darkblue') +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title='Fuel Price Over Time', x='Month', y='Fuel Price') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))

# sales with holiday weeks
ggplot(df_total, aes(x=Date, y=Total_Weekly_Sales)) +
  geom_line(color='blue') +
  geom_point(data=df_total %>% filter(Holiday_Flag == 1) ,
             aes(x=Date, y=Total_Weekly_Sales), color='red', size=2) +
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  labs(title='Total Weekly Sales Over Time with Holiday Weeks Highlighted', x='Month', y='Total Weekly Sales') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust=1))




######----------------------######
##### hierarchical approach ######
######----------------------######
data <- read.csv('/Users/lilycampbell/Desktop/Timeseries/Walmart_Sales.csv')
data$Date <- as.Date(data$Date, '%d-%m-%Y')
complete_set <- aggregate(data['Weekly_Sales'], by=data['Date'],sum)
test <- data[data$Date > as.Date('2011-12-31'),]
train <- data[data$Date < as.Date('2012-01-01'),]
complete_set_test <- na.omit(complete_set[data$Date > as.Date('2011-12-31'),])



# Arima for one store
values <- function(train, test, i){
  # Fit auto.arima with seasonality 
  fit <- auto.arima(ts(train[train['Store']==i,]['Weekly_Sales'],frequency=52 ), seasonal = TRUE)
  forecast <- forecast(fit,h=43)
  #print(summary(fit))
  forecast$mean
}

# Store all forecasts in a list (each element is a vector of length 43)
store_forecasts <- lapply(1:45, function(i) values(train, test, i))
aggregated_forecast <- Reduce(`+`, store_forecasts)
rmse_hier_arimax <- sqrt(mean((aggregated_forecast - complete_set_test$Weekly_Sales)^2, na.rm=TRUE))
print(paste("RMSE (in dollars):", round(rmse_hier_arimax, 2)))

actual_total_sales <- test %>%
  group_by(Date) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  arrange(Date)
n_common <- min(nrow(actual_total_sales), length(aggregated_forecast))

#forecasting df (forecasts and actuals on original scale)
plot_data <- data.frame(
  Date = actual_total_sales$Date[1:n_common],
  Actual_Sales = actual_total_sales$Total_Sales[1:n_common],
  Forecasted_Sales = aggregated_forecast[1:n_common]
)
library(scales) 
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual_Sales, color = "Actual Sales"), size = 1) +
  geom_line(aes(y = Forecasted_Sales, color = "ARIMA Forecasted Sales"),  size = 1) +
  labs(title = "ARIMA BU Forecast vs Actual Total Weekly Sales -  Test March 2012", 
       x = "Date", 
       y = "Total Weekly Sales ($)") +
  scale_color_manual(values = c("Actual Sales" = "blue", "ARIMA Forecasted Sales" = "Gold")) +
  scale_y_continuous(labels = comma) +  
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######------------------########
####### Prophet Approach ########
#######------------------########

# Forecast for each store separately
store_forecasts <- list()

stores <- unique(df$Store)

for (store in stores) {
  store_data <- df %>%
    filter(Store == store) %>%
    select(Date, Weekly_Sales, Holiday_Flag) %>%
    rename(ds = Date, y = Weekly_Sales)
  
  train_data <- store_data %>% filter(ds < "2012-01-01")
  test_data <- store_data %>% filter(ds >= "2012-01-01")
  
  m <- prophet(weekly.seasonality = TRUE, yearly.seasonality = TRUE, seasonality.mode = "multiplicative")
  m <- add_regressor(m, 'Holiday_Flag')
  m <- fit.prophet(m, train_data)
  
  future <- test_data %>% select(ds, Holiday_Flag)
  forecast <- predict(m, future)
  
  store_forecasts[[store]] <- data.frame(ds = future$ds, yhat = forecast$yhat, Store = store)
}

# Combine forecasts from all stores
total_forecast <- bind_rows(store_forecasts) %>%
  group_by(ds) %>%
  summarise(yhat_total = sum(yhat))

# Compare to actual total sales
actual_total_sales <- df %>%
  group_by(Date) %>%
  summarise(y_total = sum(Weekly_Sales)) %>%
  filter(Date >= "2012-01-01") %>%
  rename(ds = Date)

comparison <- merge(actual_total_sales, total_forecast, by = "ds")

# Compute RMSE
rmse_bottom_up <- sqrt(mean((comparison$y_total - comparison$yhat_total)^2, na.rm = TRUE))
print(paste("Bottom-Up Prophet RMSE:", round(rmse_bottom_up, 2)))

# Compute Relative RMSE (% of mean sales)
mean_sales <- mean(comparison$y_total, na.rm = TRUE)
relative_rmse_bu <- (rmse_bottom_up / mean_sales) * 100
print(paste("Relative RMSE (% of mean sales):", round(relative_rmse_bu, 2), "%"))
# "Bottom-Up Prophet RMSE: 2277909.8"
# Relative RMSE: 4.9 %


#######------------------########
##### Fourier Hierarchical ######
#######------------------########

data <- read.csv('Walmart_Sales.csv')
data$Date <- as.Date(data$Date, '%d-%m-%Y')
complete_set <- aggregate(data['Weekly_Sales'], by=data['Date'],sum)
test <- data[data$Date > as.Date('2011-12-31'),]
train <- data[data$Date < as.Date('2012-01-01'),]
complete_set_test <- na.omit(complete_set[data$Date > as.Date('2011-12-31'),])

# Fourier Time Series for one store
values <- function(train, test, i){
  # Create Fourier Terms and Combine with Exogenous Variables
  exog_vars_train <- train[train$Store == i, c("Holiday_Flag","CPI","Fuel_Price")]
  fourier_terms <- fourier(ts(train[train['Store']==i,]['Weekly_Sales'],frequency=52), K = 10)
  exog_train <- as.matrix(cbind(exog_vars_train,fourier_terms))
  
  # Fit auto.arima with fourier seasonality and exogenous terms
  fit <- auto.arima(ts(train[train['Store']==i,]['Weekly_Sales'],frequency=52), xreg = exog_train)
  
  # Create extended Fourier Terms trained on train set, and keep the test exog terms
  exog_vars_test <- test[test$Store == i, c("Holiday_Flag","CPI","Fuel_Price")]
  future_fourier <-fourier(ts(train[train['Store']==i,]['Weekly_Sales'],frequency=52),h=43,K=10)
  exog_test <- as.matrix(cbind(exog_vars_test, future_fourier))
  forecast <- forecast(fit,xreg=exog_test,h=43)
  
  #print(summary(fit))
  forecast$mean
}


# Store all forecasts in a list (each element is a vector of length 43)
store_forecasts <- lapply(1:45, function(i) values(train, test, i))
# Sum forecasts across all stores while keeping the 43-day structure
aggregated_forecast <- Reduce(`+`, store_forecasts)
rmse_bottom_up_fourier_arimax <- sqrt(mean((aggregated_forecast - complete_set_test$Weekly_Sales)^2))
print(paste("RMSE (in dollars):", round(rmse_bottom_up_fourier_arimax, 2)))

########---------------##########
######## BSTS Approach ##########
########---------------##########

train <- df %>% filter(year(Date) < 2012)
test  <- df %>% filter(year(Date) == 2012)

fit_bsts <- function(store_id, data) {
  
  store_data <- data %>% filter(Store == store_id)
  
  #y=log(sales)
  y <- log(store_data$Weekly_Sales)
  
  regressors <- store_data %>%
    select(Fuel_Price, CPI, Unemployment)
  
  ss <- list()
  ss <- AddLocalLevel(ss, y)  
  ss <- AddSeasonal(ss, y, nseasons = 52)  
  ss <- AddAutoAr(ss, y, lags = 2)
  
  #model 
  model <- bsts(y ~ ., state.specification = ss, 
                niter = 1000,  
                data = regressors)
  
  return(model)
}

#unique stores
store_ids <- unique(train$Store)

#fit model to each store
bsts_models <- lapply(store_ids, function(store) fit_bsts(store, train))

#forecast for all stores in 2012
forecast_bsts <- function(model, test) {
  newdata <- test %>%
    select(Fuel_Price, CPI, Unemployment)
  
  horizon <- length(unique(test$Date))  
  
  pred <- predict(model, horizon = horizon, newdata = newdata, burn = 500)
  
  return(exp(pred$mean))
}

store_forecasts <- lapply(store_ids, function(store) {
  test_subset <- test %>% filter(Store == store)  
  model_index <- match(store, store_ids)  
  forecast_bsts(bsts_models[[model_index]], test_subset)  
})

### aggregate forecasts using hierarchical time series ###
forecast_matrix <- do.call(cbind, store_forecasts)
level0_forecast <- rowSums(forecast_matrix) 

#total sales (original scale)
actual_total_sales <- test %>%
  group_by(Date) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  arrange(Date)

n_common <- min(nrow(actual_total_sales), length(level0_forecast))

#forecasting df (forecasts and actuals on original scale)
plot_data <- data.frame(
  Date = actual_total_sales$Date[1:n_common],
  Actual_Sales = actual_total_sales$Total_Sales[1:n_common],
  Forecasted_Sales = level0_forecast[1:n_common]
)

#RMSE
rmse <- sqrt(mean((plot_data$Actual_Sales - plot_data$Forecasted_Sales)^2))
print(paste("RMSE (in dollars):", round(rmse, 2)))

#plots
library(ggplot2)
library(scales)  

ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual_Sales, color = "Actual Sales"), size = 1) +
  geom_line(aes(y = Forecasted_Sales, color = "Forecasted Sales"), linetype = "dashed", size = 1) +
  labs(title = "Forecast vs Actual Total Weekly Sales", 
       x = "Date", 
       y = "Total Weekly Sales ($)") +
  scale_color_manual(values = c("Actual Sales" = "blue", "Forecasted Sales" = "red")) +
  scale_y_continuous(labels = comma) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#residual checks
all_resids <- lapply(bsts_models, function(mod) {
  res <- residuals(mod)  
  return(res)
})

agg_resids <- unlist(all_resids)

hist(agg_resids, main = "Histogram of Aggregated Residuals", xlab = "Residuals (log scale)")
acf(agg_resids, main = "ACF of Aggregated Residuals")

set.seed(0) 
sampled_resids <- sample(agg_resids, 5000)
sw <- shapiro.test(sampled_resids)
print(sw)

lb_test <- Box.test(agg_resids, lag = 20, type = "Ljung-Box")
print(lb_test)

########---------------##########
######## ARIMA Testing  ##########
########---------------##########
data <- read.csv('/Users/lilycampbell/Desktop/Timeseries/Walmart_Sales.csv')
data$Date <- as.Date(data$Date, '%d-%m-%Y')
complete_set <- aggregate(data['Weekly_Sales'], by=data['Date'],sum)
#test <- data[data$Date > as.Date('2011-12-31'),]
#train <- data[data$Date < as.Date('2012-01-01'),]
#complete_set_test <- na.omit(complete_set[data$Date > as.Date('2011-12-31'),])

test <- data[data$Date > as.Date('2012-02-28'),]
train <- data[data$Date < as.Date('2012-03-01'),]
complete_set_test <- na.omit(complete_set[data$Date > as.Date('2012-03-31'),])

#test <- data[data$Date > as.Date('2012-05-31'),]
#train <- data[data$Date < as.Date('2012-06-01'),]
#complete_set_test <- na.omit(complete_set[data$Date > as.Date('2012-05-31'),])


# Arima for one store
values <- function(train, test, i){
  # Fit auto.arima with seasonality 
  fit <- auto.arima(ts(train[train['Store']==i,]['Weekly_Sales'],frequency=52 ), seasonal = TRUE)
  forecast <- forecast(fit,h=43)
  #print(summary(fit))
  forecast$mean
}

# Store all forecasts in a list (each element is a vector of length 43)
store_forecasts <- lapply(1:45, function(i) values(train, test, i))
# Sum forecasts across all stores while keeping the 43-day structure
aggregated_forecast <- Reduce(`+`, store_forecasts)
rmse_hier_arimax <- sqrt(mean((aggregated_forecast - complete_set_test$Weekly_Sales)^2, na.rm=TRUE))
print(paste("RMSE (in dollars):", round(rmse_hier_arimax, 2)))

actual_total_sales <- test %>%
  group_by(Date) %>%
  summarise(Total_Sales = sum(Weekly_Sales)) %>%
  arrange(Date)
n_common <- min(nrow(actual_total_sales), length(aggregated_forecast))

#forecasting df (forecasts and actuals on original scale)
plot_data <- data.frame(
  Date = actual_total_sales$Date[1:n_common],
  Actual_Sales = actual_total_sales$Total_Sales[1:n_common],
  Forecasted_Sales = aggregated_forecast[1:n_common]
)
library(scales) 
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual_Sales, color = "Actual Sales"), size = 1) +
  geom_line(aes(y = Forecasted_Sales, color = "ARIMA Forecasted Sales"),  size = 1) +
  labs(title = "ARIMA BU Forecast vs Actual Total Weekly Sales -  Test March 2012", 
       x = "Date", 
       y = "Total Weekly Sales ($)") +
  scale_color_manual(values = c("Actual Sales" = "blue", "ARIMA Forecasted Sales" = "Gold")) +
  scale_y_continuous(labels = comma) +  
  scale_x_date(date_labels="%b %Y", date_breaks="1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


