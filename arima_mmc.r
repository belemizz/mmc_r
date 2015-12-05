# Import libraries
library(animation)

source("load_data.r")
source("plot_data.r")
source("predict_data.r")

# Set parameters and read data
filename = "../data/Pt#1 CMU.xlsx"
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

#Parameters
min_step_for_modeling = 100
prediction_step = 20 # number of points predicted
evaluation_freq = 5

# Weight ARIMA
n_data = nrow(metrics)
evaluation_points = min_step_for_modeling + prediction_step + 
  0:((n_data - min_step_for_modeling - prediction_step) / evaluation_freq) * evaluation_freq

evaluation <- function(metrics, prediction_method="var"){
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
    }else{
      print("prediction_method must be auto_arima, min_arima or var")
    }
    print(c(input_step, rs))
    residual_data <- rbind(residual_data, rs)
  }
  return(residual_data)
}

#animation = F
#if(animation){
#  saveGIF({residual_data = evaluation()}, interval=0.4)
#}else{
#  residual_data = evaluation()
#}

r_all = evaluation(metrics)

met_w_hr <- metrics
met_w_hr["Systolic.BP"] <- NULL
met_w_hr["Diastolic.BP"] <- NULL
r_w_hr = evaluation(met_w_hr)

met_w_bp <-metrics
met_w_bp["BP.HR"] <- NULL
r_w_bp <- evaluation(met_w_bp)

met_w <-metrics
met_w["BP.HR"] <- NULL
met_w["Systolic.BP"] <- NULL
met_w["Diastolic.BP"] <- NULL
r_w_var = evaluation(met_w, "var")
r_w_arima = evaluation(met_w, "auto_arima")

ylab = "mean abs residual"
xlab = "prediction steps"
ylim = c(2,4)

png("image.png", width=900, height=300, pointsize=12, bg="white")
par(mar=c(5,5,5,15))
plot(colMeans(abs(r_all)), type="l", col= 1, ylim=ylim, ylab="", xlab="")
par(new=T)
plot(colMeans(abs(r_w_bp)), type="l", col=2, ylim=ylim ,ylab="", xlab="")
par(new=T)
plot(colMeans(abs(r_w_hr)), type="l", col=3, ylim=ylim ,ylab="", xlab="")
par(new=T)
plot(colMeans(abs(r_w_var)), type="l", col=4, ylim=ylim, ylab = ylab, xlab=xlab)
par(new=T)
plot(colMeans(abs(r_w_arima)), type="l", col=5, ylim=ylim ,ylab="", xlab="")
par(xpd=T)
legend(par()$usr[2], par()$usr[4], legend=c("W+SysBP+DiasBP+HR", "W+SysBP+DiasBP", "W+HR", "W_only(VAR)","W_only(ARIMA)"),
       col=c(1,2,3,4,5), lty=1)
graphics.off()
