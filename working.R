setwd("D:/13.study/1.Economic/3.2/project/working")
library(vars)
library(astsa)
library(strucchange)
library(forecast)
library(tsm)
library(vars)
library(mFilter)
library(seasonal)
library(seas)
library(devtools)
library(RJDemetra)
library(tseries)
library(dplyr)
library(ineq)
library(tidyr)
library(ggplot2)
library(haven)
library(zoo)
library(lubridate)

Macro_select_m <- read.csv("Macro_select_m.csv")
oil_set <- read.csv("oil_set.csv")
Ineq_m <- read.csv("Ineq_m.csv")


Monthly_working_data <- merge(merge(Macro_select_m, oil_set, by = c("Period")), Ineq_m, by = c("Period"))
Monthly_working_data$Period <- as.yearmon(Monthly_working_data$Period, "%YM%m")
rownames(Monthly_working_data) <- Monthly_working_data$Period
Monthly_working_data$Period <- NULL

date = seq.Date(from = as.Date('1977-01-01'), by = 'month', length.out = 516)
Monthly_working_data$date <- date

# function 
run_VAR <- function(df, var_names, lag_order, trend_order, result_name) {
  # install and load the needed packages
  if(!require(vars)){
    install.packages("vars")
    library(vars)
  }
  
  # extract the needed variables
  df_selected <- df[, var_names]
  exogen_df <- NULL
  
  # add time trend based on trend_order
  if(trend_order == 1) {
    exogen_df <- data.frame(trend = 1:nrow(df_selected))
  } else if(trend_order == 2) {
    exogen_df <- data.frame(trend = 1:nrow(df_selected), trend2 = (1:nrow(df_selected))^2)
  }
  
  # perform VAR regression
  if(is.null(exogen_df)) {
    var_model <- VAR(df_selected, p = lag_order, type = "const")
  } else {
    var_model <- VAR(df_selected, p = lag_order, type = "const", exogen = exogen_df)
  }
  
  # perform diagnostic tests
  serial_result <- serial.test(var_model)
  norm_result <- normality.test(var_model)
  hetero_result <- arch.test(var_model)
  stability_result <- stability(var_model)
  
  
  # save the results into a list
  result <- list(
    VAR_model = var_model,
    serial_test = serial_result,
    normality_test = norm_result,
    hetero_result  = hetero_result,
    stability_test = stability_result
  )
  
  # assign the result list to the specified name
  assign(result_name, result, envir = .GlobalEnv)
  
  # return the result list
  return(result)
}
add_seasonal_adjusted_M <- function(df, var_name){
  # 构建时间序列
  ts_var <- ts(df[[var_name]], start = c(1977,1), end = c(2019,12), frequency = 12)
  
  # 进行季节调整
  sa_var <- x13(ts_var)
  
  # 将季节调整后的序列添加到数据框
  df[[paste(var_name, "sa", sep="_")]] <- sa_var$final$series[, "sa"]
  
  return(df)
}






# seasonal adjustment 

Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Gini_tinc")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Gini_texp")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Gini_wage")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Gini_energy")

Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_SD_tinc")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_SD_texp")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_SD_wage")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_SD_energy")


Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_Diff_tinc")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_Diff_texp")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_Diff_wage")
Monthly_working_data <- add_seasonal_adjusted_M(Monthly_working_data, "Log_Diff_energy")




#test for stationary of data 
adf.test(Monthly_working_data$log_CPI)
adf.test(diff(Monthly_working_data$log_indu))
 n
adf.test(Monthly_working_data$LIBOR)
adf.test(diff(Monthly_working_data$Neer))
adf.test(Monthly_working_data$Unempl)


adf.test(Monthly_working_data$price)
adf.test(diff(Monthly_working_data$Log_price))


adf.test(diff(Monthly_working_data$Gini_tinc))

adf.test(Monthly_working_data$Log_SD_tinc)
adf.test(Monthly_working_data$Log_Diff_tinc)
adf.test(Monthly_working_data$Gini_texp)
adf.test(Monthly_working_data$Log_SD_texp)
adf.test(Monthly_working_data$Log_Diff_texp)

plot.ts(Monthly_working_data[,c("Log_price","log_indu","log_CPI","LIBOR","Neer","Unempl","Gini_energy_sa")])


# plots


plot(Monthly_working_data$price, type='l', main=' Price over Time', xlab='Time', ylab='Price')
plot(Monthly_working_data$Log_price, type='l', main='Log Price over Time', xlab='Time', ylab='Log Price')
plot(Monthly_working_data$Neer, type='l', main='neer over Time', xlab='Time', ylab='NEER')
plot(Monthly_working_data$LIBOR, type='l', main='libor over Time', xlab='Time', ylab='LIBOR')
plot(Monthly_working_data$Unempl, type='l', main='unemployment over Time', xlab='Time', ylab='Une')
plot(Monthly_working_data$Gini_tinc, type='l', main='Gini tinc over Time', xlab='Time', ylab='Gini tinc')
plot(Monthly_working_data$Gini_texp, type='l', main='Gini texp over Time', xlab='Time', ylab='Gini texp')
plot(Monthly_working_data$log_CPI, type='l', main='Log CPI over Time', xlab='Time', ylab='Log CP')
plot(Monthly_working_data$log_indu, type='l', main='Log indu over Time', xlab='Time', ylab='Log INDU')





#inc
selection_gini <- VARselect(Monthly_working_data[109:516,c("Log_price","log_indu","log_CPI","LIBOR","Neer","Unempl","Log_SD_tinc_sa")], lag.max = 30, type = "const")
print(selection_gini$selection)

energy_gini <- run_VAR(Monthly_working_data[109:516,],
                  c("Log_price","log_indu","log_CPI","LIBOR","Neer","Unempl","Log_Diff_energy_sa"),
                  4,
                  2,
                  "tinc_gini")
summary(energy_gini$VAR_model)
print(energy_gini$serial_test)
print(energy_gini$hetero_result)
print(energy_gini$normality_test)
tinc_gini$stability_test
acf(residuals(energy_gini$VAR_model))

plot(residuals(energy_gini$VAR_model)[,4], type = "l")

##########################
# Cholesky decomposition #
##########################

# assumption
# 1. only shock to oil price can shift oil price contemporaneously # UK economy is not a determinate factor of international oil price
# 2. only shock to oil price and industrial output can shift industrial output contemporaneously
# 3. only shock to oil price, industrial output and inflation can shift industrial inflation contemporaneously
# 4. only shock to oil price, industrial output, inflation and LIBOR can shift LIBOR contemporaneously
# 5. only shock to oil price, industrial output, inflation, LIBOR and exchange rate can shift exchange rate contemporaneously
# 6. only shock to oil price, industrial output, inflation, LIBOR, exchange rate and unemployment can shift unemployment contemporaneously
# 7. all shocks can shift inequality contemporaneously

a.mat <- diag(7)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
a.mat[4, 1] <- NA
a.mat[4, 2] <- NA
a.mat[4, 3] <- NA
a.mat[5, 1] <- NA
a.mat[5, 2] <- NA
a.mat[5, 3] <- NA
a.mat[5, 4] <- NA
a.mat[6, 1] <- NA
a.mat[6, 2] <- NA
a.mat[6, 3] <- NA
a.mat[6, 4] <- NA
a.mat[6, 5] <- NA
a.mat[7, 1] <- NA
a.mat[7, 2] <- NA
a.mat[7, 3] <- NA
a.mat[7, 4] <- NA
a.mat[7, 5] <- NA
a.mat[7, 6] <- NA
print(a.mat)

b.mat <- diag(7)
diag(b.mat) <- NA
print(b.mat)

Monthly_working_data[109:516,c("trend")]<- 1:nrow(Monthly_working_data[109:516,])
Monthly_working_data[109:516,c("trend2")] <- (1:nrow(Monthly_working_data[109:516,]))^2

exogen_df <- Monthly_working_data[109:516, c("trend", "trend2")]


# Perform the VAR regression
var_energy_gini <- VAR(Monthly_working_data[109:516, c("Log_price","log_indu","log_CPI","LIBOR","Neer","Unempl","Log_Diff_texp_sa")], 
                p = 4, 
                type = "const", 
                exogen = exogen_df)


#run SVAR

# inc 
svar_energy_gini <- SVAR(var_energy_gini, Amat = a.mat, Bmat = b.mat, max.iter = 10000, 
                 hessian = TRUE)

IRF_energy_gini <- irf(svar_energy_gini, response = "Log_Diff_texp_sa", 
               n.ahead = 100, ortho = TRUE, boot = TRUE,ci = 0.90)


par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)

plot(IRF_energy_gini)




##########################
#   Instrument variable  #
##########################


devtools::install_github('tylerJPike/sovereign')

library(sovereign)### VAR in vars is replaced by VAR in sovereign
svar_iv_tinc_gini <- sovereign::VAR(data = Monthly_working_data[, c("Oil.supply.news.shock","log_indu","log_CPI","LIBOR","Neer","Unempl","Gini_tinc","date")],
                                    horizon = 10,
                                    freq = "month",
                                    type = "const",
                                    p = 3,
                                    structure = "IV",
                                    instrument = "Oil.supply.news.shock",
                                    instrumented = "Gini_tinc")

irf = IRF(svar_iv_tinc_gini,CI = c(0.05,0.95))

# plot IRF
plot_irf(irf, responses = "Gini_tinc")

