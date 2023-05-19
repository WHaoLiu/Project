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

Macro_select_m <- read.csv("Macro_select_m.csv")
oil_set <- read.csv("oil_set.csv")
Ineq_m <- read.csv("Ineq_m.csv")

Monthly_working_data <- merge(merge(Macro_select_m, oil_set, by = c("Period")), Ineq_m, by = c("Period"))
Monthly_working_data$Period <- as.yearmon(Monthly_working_data$Period, "%YM%m")
rownames(Monthly_working_data) <- Monthly_working_data$Period
Monthly_working_data$Period <- NULL


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

# seasonal adjustment # I tried several method and ran with regression, but delete the VAR result

ts_data <- ts(Monthly_working_data$Gini_tinc
              , start = c(1977, 1), end = c(2019, 12), frequency = 12)
a <- x13(ts_data)
plot(a$final,type.chart= "sa-trend")

b <- decompose(ts_data, type = "multiplicative")
plot(b$x)

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
selection_gini <- VARselect(Monthly_working_data[,c("log_CPI","LIBOR","Neer","Unempl","log_indu","Log_price","Gini_tinc")], lag.max = 30, type = "both")
print(selection_gini$selection)

tinc_gini <- run_VAR(Monthly_working_data,
                  c("log_CPI","LIBOR","Neer","Unempl","log_indu","Log_price","Gini_tinc"),
                  3,
                  2,
                  "tinc_gini")
summary(tinc_gini$VAR_model)
print(tinc_gini$serial_test)
print(tinc_gini$hetero_result)
print(tinc_gini$normality_test)
tinc_gini$stability_test

acf(residuals(tinc_gini$VAR_model))







selection_log_sd <- VARselect(Monthly_working_data[,c("log_CPI","log_indu","LIBOR","Neer","Unempl","price","Log_SD_tinc")], lag.max = 36, type = "both")
print(selection_log_sd$selection)

tinc_log_sd <- run_VAR(Monthly_working_data,
                     c("log_CPI","log_indu","LIBOR","price","Log_SD_tinc"),
                     13,
                     2,
                     "tinc_log_sd")
summary(tinc_log_sd$VAR_model)
print(tinc_log_sd$serial_test)
print(tinc_log_sd$normality_test)
plot(tinc_log_sd$stability_test)





selection_log_diff <- VARselect(Monthly_working_data[,c("log_CPI","log_indu","LIBOR","Neer","Unempl","Log_price","Log_Diff_tinc")], lag.max = 36, type = "both")
print(selection_log_diff$selection)

tinc_log_diff <- run_VAR(Monthly_working_data,
                       c("log_CPI","log_indu","LIBOR","Neer","Unempl","Log_price","Log_Diff_tinc"),
                       13,
                       2,
                       "tinc_log_diff")
summary(tinc_log_diff$VAR_model)
print(tinc_log_diff$serial_test)
print(tinc_log_diff$normality_test)
plot(tinc_log_diff$stability_test)



















#exp

selection_gini_e <- VARselect(Monthly_working_data[,c("log_CPI","log_indu","LIBOR","Neer","Unempl","Log_price","Gini_texp")], lag.max = 36, type = "both")
print(selection_gini_e$selection)

texp_gini <- run_VAR(Monthly_working_data,
                     c("log_CPI","log_indu","LIBOR","Unempl","Log_price","Gini_texp"),
                     13,
                     2,
                     "texp_gini")
summary(texp_gini$VAR_model)
print(texp_gini$serial_test)
print(texp_gini$normality_test)
plot(texp_gini$stability_test)

acf(residuals(texp_gini$VAR_model))


selection_log_sd_e <- VARselect(Monthly_working_data[,c("log_CPI","log_indu","LIBOR","Neer","Unempl","price","Log_SD_texp")], lag.max = 36, type = "both")
print(selection_log_sd_e$selection)

texp_log_sd <- run_VAR(Monthly_working_data,
                       c("log_CPI","log_indu","LIBOR","Neer","Unempl","price","Log_SD_texp"),
                       14,
                       2,
                       "texp_log_sd")
summary(texp_log_sd$VAR_model)
print(texp_log_sd$serial_test)
print(texp_log_sd$normality_test)
plot(texp_log_sd$stability_test)

acf(residuals(texp_gini$VAR_model))


selection_log_diff_e <- VARselect(Monthly_working_data[,c("log_CPI","log_indu","LIBOR","Neer","Unempl","price","Log_Diff_texp")], lag.max = 36, type = "both")
print(selection_log_diff_e$selection)

texp_log_diff <- run_VAR(Monthly_working_data,
                         c("log_CPI","log_indu","LIBOR","Neer","Unempl","price","Log_Diff_texp"),
                         13,
                         2,
                         "texp_log_diff")
summary(texp_log_diff$VAR_model)
print(texp_log_diff$serial_test)
print(texp_log_diff$normality_test)
plot(texp_log_diff$stability_test)

acf(residuals(texp_gini$VAR_model))







