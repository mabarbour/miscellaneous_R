jensen_magnitude <- function(model.object, no.var.data, predict.type = "response"){
  model <- model.object 
  
  var.predict <- predict(model.object, type = predict.type)
  
  no.var.predict <- predict(model.object, newdata = no.var.data, type = predict.type)
  
  compare.predict <- data.frame(mean.with.var = mean(var.predict, na.rm = TRUE),
                                mean.no.var = mean(no.var.predict, na.rm = TRUE)) %>%
    mutate(Magnitude.Difference = mean.with.var/mean.no.var)
  compare.predict
}

jensen_magnitude_manyglm <- function(manyglm.object, original.data, comparison.data, predict.type = "response"){
  var.predict <- predict(manyglm.object, newdata = original.data, type = predict.type) 
  no.var.predict <- predict(manyglm.object, newdata = comparison.data, 
                            type = predict.type)
  compare.predict <- data.frame(response.variables = names(colMeans(var.predict)),
                                var.predict = colMeans(var.predict),
                                no.var.predict = colMeans(no.var.predict),
                                var.predict.SD = apply(var.predict, MARGIN = 2, FUN = sd))
  compare.predict <- mutate(compare.predict, 
                            Z.value = (var.predict - no.var.predict)/var.predict.SD,
                            Percent.Difference = (var.predict - no.var.predict)/no.var.predict*100)
  
  print(compare.predict)
}