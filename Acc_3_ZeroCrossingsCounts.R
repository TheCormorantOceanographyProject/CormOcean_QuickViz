Basic Zero-Crossing Counter
count_zero_crossings <- function(x) {
  # Remove NA values
  x <- x[!is.na(x)]
  
  # Sign of each value: -1, 0, or 1
  s <- sign(x)
  
  # A zero crossing occurs when the sign changes between consecutive samples
  crossings <- sum(s[-1] != s[-length(s)] & s[-1] != 0 & s[-length(s)] != 0)
  
  return(crossings)
}

Example
acc <- c(-0.5, -0.2, 0.1, 0.6, -0.3, -0.1, 0.2)
count_zero_crossings(acc)

Version that ignores tiny noise around zero (recommended)

Accelerometer data often hovers around zero due to noise. A threshold avoids false crossings:
  
  count_zero_crossings_thresh <- function(x, thresh = 0.02) {
    x <- x[!is.na(x)]
    
    # Values within ±thresh are treated as zero
    x_adj <- ifelse(abs(x) < thresh, 0, x)
    
    s <- sign(x_adj)
    
    crossings <- sum(s[-1] != s[-length(s)] & s[-1] != 0 & s[-length(s)] != 0)
    
    return(crossings)
  }

Example
acc <- rnorm(100, 0, 0.01)   # mostly noise near zero
count_zero_crossings_thresh(acc, thresh = 0.02)

Version to count zero crossings per second

If your accelerometer is sampled at frequency fs (Hz):
  
  zero_crossings_per_sec <- function(x, fs, thresh = 0) {
    x <- x[!is.na(x)]
    if (thresh > 0) x <- ifelse(abs(x) < thresh, 0, x)
    
    s <- sign(x)
    crossings <- sum(s[-1] != s[-length(s)] & s[-1] != 0 & s[-length(s)] != 0)
    
    rate <- crossings / (length(x) / fs)
    return(rate)
  }
