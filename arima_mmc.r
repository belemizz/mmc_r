# Import libraries
library(animation)

source("load_data.r")
source("plot_data.r")
source("predict_data.r")

# Set parameters and read data
filename = "../data/Pt#4 CMU.xlsx"

ma_length=30

# Read data
used_metrics = c("Weight", "Systolic.BP", "Diastolic.BP", "BP.HR")
used_names = c("Session.Date", used_metrics)
mmc_data = read_mmc_data(filename, used_names)

dates= as.POSIXlt(mmc_data$Session.Date)
metrics = mmc_data[used_metrics]

#weight =mmc_data$Weight
#SysBP = mmc_data$Systolic.BP
#DiasBP = mmc_data$Diastolic.BP
#BP_HR = mmc_data$BP.HR

# Plot time series and moving average
#ylabel = "Weight"
#n_data = length(weight)
#plot_ts_and_ma(ts, dates, ma_length, ylabel)

#list_ts = list(weight, SysBP, DiasBP, BP_HR)
#y_labels = c("weight", "SysBP", "DiasBP", "HR")
#plot_list_ts(list_ts, dates, y_labels)

# Weight ARIMA
evaluation <- function(metrics, prediction_method="var"){
  min_step_for_modeling <- 100
  prediction_step <- 5 # number of points predicted
  evaluation_freq <- 1
  n_data = nrow(metrics)
  evaluation_points = min_step_for_modeling + prediction_step + 
    0:((n_data - min_step_for_modeling - prediction_step) / evaluation_freq) * evaluation_freq
  residual_data = data.frame(matrix(rep(NA, prediction_step), nrow=1))[-1,]
  for(evaluation_point in evaluation_points){
    input_step = evaluation_point - prediction_step
    if(prediction_method=="auto_arima"){
      ts = metrics[["Weight"]]
      rs = auto_arima_prediction(ts, dates,
                                 input_step, prediction_step)
    }else if(prediction_method=="min_arima"){
      modeling_step = input_step - validation_step
      rs = ts_prediction(ts, dates,
                         modeling_step, validation_step, prediction_step,
                         max_ar_degree, max_i_degree, max_ma_degree,
                         arima_optim, visualize_optim_degree = F)
    }else if(prediction_method=="var"){
      rs = var_prediction(metrics, dates, input_step, prediction_step)
    }else if(prediction_method=="diff_var"){
      rs = diff_var_prediction(metrics, dates, input_step, prediction_step)
    }else if(prediction_method=="t_diff_var"){
      rs = t_diff_var_prediction(metrics, dates, input_step, prediction_step)
    }else{
      print("prediction_method must be auto_arima, min_arima or var")
    }
#    print(c(input_step, rs))
    residual_data <- rbind(residual_data, rs)
  }
  return(residual_data)
}

weight <- metrics["Weight"]

#Single time series comparison
ARvsARIMAvsDelAR <- function(timeseries){
  r_w_var = evaluation(timeseries, "var")
  r_w_arima = evaluation(timeseries, "auto_arima")
  r_w_diff_var = evaluation(timeseries, "diff_var")
  
  ylab = "mean abs residual"
  xlab = "prediction steps"
  ylim = c(2,3)
  plot(colMeans(abs(r_w_var)), type="l", col= 1, ylim=ylim, ylab=ylab, xlab=xlab)
  par(new=T)
  plot(colMeans(abs(r_w_arima)), type="l", col=2, ylim=ylim ,ylab="", xlab="")
  par(new=T)
  plot(colMeans(abs(r_w_diff_var)), type="l", col=3, ylim=ylim ,ylab="", xlab="")
  legend("topleft", legend=c("VAR", "ARIMA", "DEL_VAR"),
         col=c(1,2,3), lty=1)
  
  print(mean(colMeans(abs(r_w_var[1:5]))))
  print(mean(colMeans(abs(r_w_arima[1:5]))))
  print(mean(colMeans(abs(r_w_diff_var[1:5]))))
}

ARvsARIMAvsDelAR(weight)

# ARvsVAR <- function(){
# 
# }
 
DelARvsDelVAR<- function(metrics){
  weight <- metrics["Weight"]
  w_hr <- metrics[c("Weight", "BP.HR")]
  w_bp <- metrics[c("Weight", "Systolic.BP", "Diastolic.BP")]
  
  r_w_diff_var = evaluation(weight, "diff_var")
  r_w_bp_diff_var = evaluation(w_hr, "diff_var")
  r_w_hr_diff_var = evaluation(w_bp, "diff_var")
  r_all_diff_var = evaluation(metrics, "diff_var")
  
  ylab = "mean abs residual"
  xlab = "prediction steps"
  ylim = c(2,3)
  plot(colMeans(abs(r_w_diff_var)), type="l", col=3, ylim=ylim ,ylab=ylab, xlab=xlab)
  par(new=T)
  plot(colMeans(abs(r_w_hr_diff_var)), type="l", col=4, ylim=ylim ,ylab="", xlab="")
  par(new=T)
  plot(colMeans(abs(r_w_bp_diff_var)), type="l", col=5, ylim=ylim ,ylab="", xlab="")
  par(new=T)
  plot(colMeans(abs(r_all_diff_var)), type="l", col=6, ylim=ylim ,ylab="", xlab="")
  legend("topleft", legend=c("W_ONLY", "W_HR", "W_BP", "ALL"),
         col=c(3, 4, 5, 6), lty=1)

  print(mean(colMeans(abs(r_w_diff_var[1:5]))))
  print(mean(colMeans(abs(r_w_hr_diff_var[1:5]))))
  print(mean(colMeans(abs(r_w_bp_diff_var[1:5]))))
  print(mean(colMeans(abs(r_all_diff_var[1:5]))))
  
}

DelARvsDelVAR(metrics)

Animation <- function(){
  saveGIF({residual_data = evaluation(metrics, "diff_var")}, interval=0.12)
}

#Animation()

