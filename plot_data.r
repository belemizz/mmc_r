# Function definitions
moving_average = function(x, n){
  filter(x, rep(1, n)) / n
}

plot_ts_and_ma = function(ts, dates, length, y_label){
  ma_ts = moving_average(ts, length)
  ylim = c(min(ts), max(ts))
  plot(dates, ts, type = "l", ylim=ylim, ylab=y_label)
  par(new=T)
  plot(dates, ma_ts, type="l", ylim=ylim, col=2, ylab="")
}

plot_list_ts <- function(list_ts, dates, y_labels){
  max_val = 0
  for(ts in list_ts){
    if(max(ts)>max_val){
      max_val = max(ts)
    }
  }
  
  for(idx in 1:length(list_ts)){
    if(idx > 1){
      par(new=T)
    }
    plot(dates, list_ts[[idx]], ylab = "", type="l", col=idx, ylim=c(0,max_val))
  }
  legend("topleft", legend=y_labels, col=c(1:length(list_ts)), lty=1)
}

plot_prediction=function(input_dates, input_ts, prediction_dates, gt_ts,
                         prediction_mean, prediction_lower, prediction_upper,
                         xlim, ylim, ylabel){

  if(mean(gt_ts) > mean(prediction_upper)){
    par(bg="pink")
  }else if(mean(gt_ts) < mean(prediction_lower)){
    par(bg="yellow")
  }else{
    par(bg="white")
  }
  plot(input_dates, input_ts, type="l",
       xlim=xlim, ylim=ylim, xlab="", ylab=ylabel)
  par(new=T)
  plot(prediction_dates, gt_ts, type="l", col=2,
       xlim=xlim, ylim=ylim, xlab="", ylab="", axes=T)
  par(new=T)
  plot(prediction_dates, prediction_mean, type="l", col=3,
       xlim=xlim, ylim=ylim, xlab="", ylab="")
  par(new=T)
  plot(prediction_dates, prediction_lower, type="l", col=4,
       xlim=xlim, ylim=ylim, xlab="", ylab="")
  par(new=T)
  plot(prediction_dates, prediction_upper, type="l", col=4,
       xlim=xlim, ylim=ylim, xlab="", ylab="")
  
  average_residual = mean(abs(prediction_mean - gt_ts))
  mtext(sprintf("%s:   %0.3f", rev(prediction_dates)[1], average_residual))
}



