xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
    rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  #
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  y_list <- c()
  sample_y <- function(x){return(sample(x, 10, replace = TRUE))}
  x_values <- unique(x)
  for(i in 0:(length(x_values)-1)){
    y_samp <- sample((y[(1+(10*i)):(10*(i+1))]))
    y_list <- c(y_list, y_samp)
  }
  #y_list <- unlist(y_list[1:length(y)])
  return(unlist(y_list))
}

genBootR = function(fit, err, rep = TRUE){
  #
  ### Sample the errors 
  sample_err <- sample(err, length(err), replace = FALSE)
  ### Add the errors to the fit to create a y vector
  new_fit <- fit + sample_err
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  return(new_fit)
}

fitModel = function(x, y, degree = 1){
  #
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if(degree == 1){fit_model = lm(y~x)}
  if(degree == 2){fit_model = lm(y~x+I(x^2))}
  coeff = fit_model$coefficients
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  #
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  if(is.null(fit)){model <- genBootY(data[,1], data[,2], rep = TRUE)}
  else{model <- genBootR(fit[,1], fit[,2])} 
  ### Use fitModel to fit a model to this bootstrap Y ##????
  model_fit <- fitModel(data[,1], model, degree)
  return(model_fit)
}

repBoot = function(data, B = 1000){
  #
  #quad_fit <- fitModel(data[,1], data[,2], degree= 2)
  #quad_fit_values <- quad_fit[1] + data[,1]*quad_fit[2] + data[,1]*data[,1]*quad_fit[3]
  fit_lm_q <- lm(data[,2]~data[,1] + I(data[,1]^2))
  quad_fit_values <- fit_lm_q$fitted.values
  quad_error <- fit_lm_q$residuals
  quad_fit_matrix <- as.matrix(cbind(quad_fit_values, quad_error))
  #line_fit <- fitModel(data[,1], data[,2], degree= 1)
  #line_fit_values <-  data[,1]*line_fit[[2]] + line_fit[[1]]
  #line_error <- data[,2] - line_fit_values
  #line_fit_values <- as.matrix(cbind(line_fit_values, line_error))
  fit_lm_l <- lm(data[,2]~data[,1])
  line_fit_values <- fit_lm_l$fitted.values
  line_error <- fit_lm_l$residuals
  line_fit_matrix <- as.matrix(cbind(line_fit_values, line_error))
  line_y <- c()
  line_r <- c()
  quad_y <- c()
  quad_r <- c() 
  for(i in 1:B){
    boot_line_y <- oneBoot(data, fit = line_fit_matrix)
    line_y <- cbind(line_y, boot_line_y)
    boot_line_r <- oneBoot(data)
    line_r <- cbind(line_r, boot_line_r)
    boot_quad_y <- oneBoot(data, fit = quad_fit_matrix, degree = 2)
    quad_y <- cbind(quad_y, boot_quad_y)
    boot_quad_r <- oneBoot(data, degree = 2)
    quad_r <- cbind(quad_r, boot_quad_r)
  }
  rep_list <- list((line_y), (line_r), (quad_y), (quad_r)) 
  return(rep_list)
}

### Set up the inputs you need for oneBoot, i.e.,
### create errors and fits for line and quadratic
### replicate a call to oneBoot B times
### format the return value so that you have a list of
### length 4, one for each set of coefficients
### each element will contain a data frame with B rows
### and one or two columns, depending on whether the 
### fit is for a line or a quadratic
### Return this list
##same instructions twice?
### Replicate a call to oneBoot B times for 
### each of the four conditions

### Format the return value so that you have a list of
### length 4, one for each set of coefficients
### each element will contain a matrix with B columns
### and two or three rows, depending on whether the 
### fit is for a line or a quadratic
### Return this list

#return(coeff)


bootPlot = function(x, y, coeff, trueCoeff=NULL){
  #
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  ### Make a scatter plot of data
  data_plot <- plot(x,y)
  ### Add lines or curves for each row in coeff
  if(nrow(coeff) == 2){
    mapply(abline, coeff[1,], coeff[2,], col = rgb(0,0,255, alpha = 170, maxColorValue = 255))
  }
  if(nrow(coeff)==3){
    mapply(function(a,b,c){
      curve(a*I(x^2) + b*x + c, add = TRUE,col = rgb(0,0,255,alpha = 170, maxColorValue = 255))},
      coeff[3,], coeff[2,], coeff[1,]
    )}
  if(length(trueCoeff) == 2){ #length or nrow of trueCoeff?
    mapply(abline, trueCoeff[1], trueCoeff[2], col = rgb(0,255,0,255, maxColorValue = 255))
  }
  if(length(trueCoeff)==3){
    mapply(function(a,b,c){
      curve(a*I(x^2) + b*x + c, add = TRUE,col = rgb(0,255,0,255, maxColorValue = 255))},
      trueCoeff[3], trueCoeff[2], trueCoeff[1]
    )}
}

par(mar=c(1,1,1,1))
### Use transparency
### You should use mapply to construct all 
### 1000 of the bootstrapped lines of best fit 
### Have a look at ?mapply for details.
### This can be done in ggplot2 or base graphics.

### Use trueCoeff to add true line/curve - 
###  Make the true line/curve stand out


### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  #
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
    bootPlot(myData$x, myData$y, 
             coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}



