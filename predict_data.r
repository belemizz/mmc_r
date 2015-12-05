library(forecast)
library(vars)

var_prediction <- function(metrics, dates, input_step, predcition_step, target="Weight"){

  ts = metrics[[target]]
  
  input_idx = 1:input_step
  input_metrics = metrics[input_idx,]
  input_ts = ts[input_idx]
  input_dates = dates[input_idx]
  
  prediction_idx = 1:prediction_step + input_step
  prediction_ts = ts[prediction_idx]
  prediction_dates = dates[prediction_idx]
  
  if(ncol(metrics) == 1){
    model = ar(input_metrics, order.max=10)
    preds = predict(model, n.ahead = prediction_step, ci = 0.90, dumvar = NULL)
    pred_mean = preds$pred
    pred_upper = pred_mean + preds$se
    pred_lower = pred_mean - preds$se
  }else{
    lag = VARselect(input_metrics, lag.max = 10, type="const")$selection[1]
    model = VAR(input_metrics, p=lag, type="const")
    predict_metrics = predict(model, n.ahead = prediction_step, ci = 0.90, dumvar = NULL)
    preds = predict_metrics$fcst[[target]]
    pred_mean = preds[,1]
    pred_upper = preds[,2]
    pred_lower = preds[,3]
  }
  
  xlim = as.POSIXct(c(rev(input_dates)[prediction_step*3], rev(prediction_dates)[1]))
  ylim = c(min(ts), max(ts))
  
  plot_prediction(input_dates, input_ts, prediction_dates, prediction_ts,
                  pred_mean, pred_lower, pred_upper, xlim, ylim, target)
  
  #return final residual
  
  pred_mean - prediction_ts
}

auto_arima_prediction = function(ts, dates,
                                 input_step, prediction_step, target="Weight"){
  input_idx = 1:input_step
  input_ts = ts[input_idx]
  input_dates = dates[input_idx]
  model = auto.arima(input_ts, ic="aic")
  
  prediction_idx = 1:prediction_step + input_step
  prediction_ts = ts[prediction_idx]
  prediction_dates = dates[prediction_idx]
  
  xlim = as.POSIXct(c(rev(input_dates)[prediction_step*3], rev(prediction_dates)[1]))
  ylim = c(min(ts), max(ts))
  
  preds = forecast(model, level=c(90), h=prediction_step)
  plot_prediction(input_dates, input_ts, prediction_dates, prediction_ts,
                  preds$mean, preds$lower, preds$upper,
                  xlim, ylim, target)
  preds$mean - prediction_ts 
}

ts_prediction = function(ts, dates,
                         modeling_step, validation_step, prediction_step,
                         max_ar_degree, max_i_degree, max_ma_degree,
                         optim_method, visualize_optim_degree, target="Weight"){
  
  # degree optimization
  modeling_idx = 1:modeling_step
  modeling_ts = ts[modeling_idx]
  modeling_dates = dates[modeling_idx]
  
  validation_idx = 1:validation_step + modeling_step
  validation_ts = ts[validation_idx]
  validation_dates = dates[validation_idx]
  
  optim_degree = optim_degree_arima(modeling_ts, validation_ts,
                                    max_ar_degree, max_i_degree, max_ma_degree,
                                    optim_method, F, visualize_optim_degree)
  # Prediction
  input_idx = 1:(modeling_step + validation_step)
  input_ts = ts[input_idx]
  input_dates = dates[input_idx]
  
  prediction_idx = 1:prediction_step + modeling_step + validation_step
  prediction_ts = ts[prediction_idx]
  prediction_dates = dates[prediction_idx]
  
  predict_xlim = as.POSIXct(c(rev(modeling_dates)[prediction_step*3], rev(prediction_dates)[1]))
  ylim = c(min(ts), max(ts))
  
  tryCatch({
    fit1 = arima(input_ts, optim_degree, optim.method = arima_optim)
    preds = predict(fit1,n.ahead = prediction_step, ci=0.90)
    
    par(new=F)
    plot(input_dates, input_ts, type="l",
         xlim=predict_xlim, xlab="",
         ylim=ylim)
    par(new=T)
    plot(prediction_dates, preds$pred, type="l", col=2,
         xlim=predict_xlim, xlab="",
         ylim=ylim, ylab="")
    par(new=T)
    plot(prediction_dates, prediction_ts, type="l", col=3,
         xlim=predict_xlim, xlab="",
         ylim=ylim, ylab="")
    
    pred_mean - prediction_ts
  },
  error = function(e) {
    message(e)
  })
}


optim_degree_arima = function(modeling_ts, validation_ts,
                              max_ar_degree, max_i_degree, max_ma_degree,
                              arima_optim, root_mean_sq, visualize){
  if(visualize){
    xlim = c(1, length(modeling_ts)+length(validation_ts))
    ylim = c(min(modeling_ts), max(modeling_ts))
    
    modeling_points = 1:length(modeling_ts)
    validation_points = 1:length(validation_ts) + length(modeling_ts)
    
    plot(modeling_points, modeling_ts, type="l",
         xlim=xlim, xlab="", ylim=ylim, ylab="")
    par(new=T)
    plot(validation_points, validation_ts, type="l", col=3,
         xlim=xlim, xlab="", ylim=ylim, ylab="")
  }
  
  means = c()
  params = list()
  for(i in 0:max_ar_degree){
    for(j in 0:max_i_degree){
      for(k in 0:max_ma_degree){
        tryCatch({
          order_param = c(i,j,k)
          fit1 = arima(modeling_ts, order_param, optim.method = arima_optim)
          validation = predict(fit1,validation_step)
          if(root_mean_sq){
            mean_residuals = norm(matrix(validation_ts-validation$pred))/validation_step
          }else{
            mean_residuals = abs(mean(validation_ts-validation$pred))
          }
          means = c(means, mean_residuals)
          params = c(params, list(order_param))
          
          if(visualize){
            par(new=T)
            plot(validation_points, validation$pred, type="l", col=2,
                 xlim=xlim, xlab="", ylim=ylim, ylab="")
          }
          
        },
        error = function(e) {
          print(order_param)
          message(e)
        })
      }
    }
  }
  optim_degree = params[[which.min(means)]]
}

