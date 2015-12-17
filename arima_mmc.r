# This is the main script for evaluation of MMC database
#  - Comparison of single time series models
#  - Comparison of metrics as a input of AR models
#  - Make animation of prediction and ground truth
# Usage: Put data files on ../data as "Pt#0X CMU.xlsx" or edit load_data.r

# Import libraries
library(animation)

source("load_data.r")
source("plot_data.r")
source("predict_data.r")

# Read data
used_metrics = c("Weight", "Systolic.BP", "Diastolic.BP", "BP.HR")
used_names = c("Session.Date", used_metrics)
mmc_data = read_mmc_data(4, used_names)
dates= as.POSIXlt(mmc_data$Session.Date)
metrics = mmc_data[used_metrics]

# Visualize Each Time Series
moving_ave_length=30
for(item in used_metrics){
  ts = metrics[[item]]
  plot_ts_and_ma(ts, dates, moving_ave_length, item)
  print(item)
}

# Fit model and return residual of ground truth and prediction by model
fit_model <- function(metrics, model_type="var", target="Weight", animation=F){
  min_step_for_modeling <- 100
  prediction_step <- 5 # number of points to be predicted
  evaluation_freq <- 1
  n_data = nrow(metrics)
  evaluation_points = min_step_for_modeling + prediction_step + 
    0:((n_data - min_step_for_modeling - prediction_step) / evaluation_freq) * evaluation_freq
  residual_data = data.frame(matrix(rep(NA, prediction_step), nrow=1))[-1,]
  
  for(evaluation_point in evaluation_points){
    input_step = evaluation_point - prediction_step
    ts = metrics[[target]]
    if(model_type=="var"){
      # VAR(AR) model selected by AIC criteria
      rs = var_prediction(metrics, dates, input_step, prediction_step, target, plot_prediction = animation)
    }else if(model_type=="var_target_diff"){
      # VAR Model, use diffferential sequences for target only
      rs = diff_var_prediction(metrics, dates, input_step, prediction_step, target, diff_names = c(target), plot_prediction = animation)
    }else if(model_type=="var_all_diff"){
      # VAR Model, use diffferential sequences for all sereis
      rs = diff_var_prediction(metrics, dates, input_step, prediction_step, target, diff_names = names(metrics), plot_prediction = animation)
    }else if(model_type=="auto_arima"){
      # ARIMA Model, Single time series only
      rs = auto_arima_prediction(ts, dates, input_step, prediction_step, target, plot_prediction = animation)
    }else if(model_type=="s_and_v"){
      # VAR(AR) model selected by split and validation
      rs = s_and_v_prediction(ts, dates, input_step, prediction_step, target, plot_prediction = animation)
    }else{
      print("model_type must be var, var_target_diff, var_all_diff or auto_arima")
    }
    residual_data <- rbind(residual_data, rs)
  }
  return(residual_data)
}

# Single time series comparison
compare_single_series_models <- function(timeseries, target){
  r_w_var = fit_model(timeseries, "var", target)
  print(mean(colMeans(abs(r_w_var[1:5]))))
  
  r_w_arima = fit_model(timeseries, "auto_arima", target)
  print(mean(colMeans(abs(r_w_arima[1:5]))))

  r_w_diff_var = fit_model(timeseries, "var_target_diff", target)
  print(mean(colMeans(abs(r_w_diff_var[1:5]))))

  r_w_s_and_v = fit_model(timeseries, "s_and_v", target)
  print(mean(colMeans(abs(r_w_s_and_v[1:5]))))
  
  y_max = max(c(colMeans(abs(r_w_var)), colMeans(abs(r_w_arima)), colMeans(abs(r_w_diff_var), colMeans(abs(r_w_s_and_v)))))
  y_min = min(c(colMeans(abs(r_w_var)), colMeans(abs(r_w_arima)), colMeans(abs(r_w_diff_var), colMeans(abs(r_w_s_and_v)))))
  ylim = c(y_min, y_max)
  
  ylab = "mean abs residual"
  xlab = "prediction steps"
  plot(colMeans(abs(r_w_var)), type="l", col= 1, ylim=ylim, ylab=ylab, xlab=xlab)
  par(new=T)
  plot(colMeans(abs(r_w_arima)), type="l", col=2, ylim=ylim ,ylab="", xlab="")
  par(new=T)
  plot(colMeans(abs(r_w_diff_var)), type="l", col=3, ylim=ylim ,ylab="", xlab="")
  par(new=T)
  plot(colMeans(abs(r_w_s_and_v)), type="l", col=7, ylim=ylim ,ylab="", xlab="")
  legend("topleft", legend=c("VAR", "ARIMA", "DIFF_VAR", "SPLIT"),
         col=c(1,2,3,7), lty=1)
  
}

target = "Weight"
target_ts <- metrics[target]
compare_single_series_models(target_ts, target)

compare_multi_metrics<- function(metrics, model_type){
  weight <- metrics["Weight"]
  w_hr <- metrics[c("Weight", "BP.HR")]
  w_bp <- metrics[c("Weight", "Systolic.BP", "Diastolic.BP")]

  r_w = fit_model(weight, model_type)
  r_w_bp = fit_model(w_bp, model_type)
  r_w_hr = fit_model(w_hr, model_type)
  r_all = fit_model(metrics, model_type)
  
  y_max = max(c(colMeans(abs(r_w)), colMeans(abs(r_w_bp)), colMeans(abs(r_w_hr)), colMeans(abs(r_all))))
  y_min = min(c(colMeans(abs(r_w)), colMeans(abs(r_w_bp)), colMeans(abs(r_w_hr)), colMeans(abs(r_all))))
  ylim = c(y_min, y_max)
  
  ylab = "mean abs residual"
  xlab = "prediction steps"
  plot(colMeans(abs(r_w)), type="l", col=3, ylim=ylim ,ylab=ylab, xlab=xlab)
  par(new=T)
  plot(colMeans(abs(r_w_hr)), type="l", col=4, ylim=ylim ,ylab="", xlab="")
  par(new=T)
  plot(colMeans(abs(r_w_bp)), type="l", col=5, ylim=ylim ,ylab="", xlab="")
  par(new=T)
  plot(colMeans(abs(r_all)), type="l", col=6, ylim=ylim ,ylab="", xlab="")
  legend("topleft", legend=c("W_ONLY", "W_HR", "W_BP", "ALL"),
         col=c(3, 4, 5, 6), lty=1)

  print(mean(colMeans(abs(r_w[1:5]))))
  print(mean(colMeans(abs(r_w_hr[1:5]))))
  print(mean(colMeans(abs(r_w_bp[1:5]))))
  print(mean(colMeans(abs(r_all[1:5]))))
}

compare_multi_metrics(metrics, "var_all_diff")
# compare_multi_metrics(metrics, "var")
# compare_multi_metrics(metrics, "var_target_diff")

# Making animation of prediction and 
Animation <- function(){
  model_type = "var_all_diff"
  saveGIF({residual_data = fit_model(metrics, model_type, animation=T)}, interval=0.12)
}

Animation()

