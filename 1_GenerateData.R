# Use this script to create synthetic data

#----------------------------
#### Generate sales data ####
#----------------------------


## Generate a baseline monthly order rate
vehicle.sales <- rnorm(n = 12 * 30, mean = 400, sd = 40)

## Add seasonality
season.1 <- .05 * sinpi((1:length(vehicle.sales)) / 12) + 1
vehicle.sales <- vehicle.sales * season.1

## Add start/end of production ramps
start <- rep(0, times = 12 * 1)
ramp.up <- seq(from = 0, to = 1, by = 0.04)
ramp.down <- seq(from = 1, to = 0, by = -0.05)
end <- rep(0, times = 12 * 10)
production.months <- length(vehicle.sales) - (length(start) + length(ramp.up) + length(ramp.down) + length(end))
production <- rep(1, times = production.months)

production.ramps <- c(start, ramp.up, production, ramp.down, end)

vehicle.sales <- vehicle.sales * production.ramps

## Plot dataset
plot(vehicle.sales, type = "l")


#----------------------------------
#### Generate replacement data ####
#----------------------------------

# Use Weibull distribution for replacement due to wear or damage
weibull_hazard <- function(x, shape, peak = 1, valley = 0) {
  ## Weibull hazard function
  h.x <- shape * x ^ (shape - 1)
  
  if(shape != 1) {
  ## Scale to expected probabilities
  # Scale
  range <- (peak - valley) / 2
  range.h.x <- (range(h.x)[2] - range(h.x)[1]) / 2
  scaler <- range / range.h.x
  h.x <- h.x * scaler

    # Center
  center <- ((peak - valley) / 2) + valley
  center.h.x <- ((max(h.x) - min(h.x)) / 2) + min(h.x)
  h.x <- h.x + (center - center.h.x)
  } else {
    h.x <- h.x - (mean(h.x) - valley)
  }

  return(h.x)
}

zz <- weibull_hazard(1:100, shape = 5, peak = .8, valley = 0.05)
plot(zz)
