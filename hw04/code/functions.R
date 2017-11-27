
# remove_missing function
# This function returns a vector without NA's
# Input: a vector
# Output: a vector without NA's

remove_missing <- function(x) {
  a <- !is.na(x)
  return(x[a])
}



# get_minimum function
# This function returns the minimum value of a numeric vector
# Input: a numeric vector
# Output: the minimum value of the vector

get_minimum <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(sort(x, decreasing = F)[1])
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(sort(x, decreasing = F)[1]) 
        }
    }
  }
}



# get_maximum function
# This functions returns the maximum value of a numeric vector
# Input: a numeric vector
# Output: the maximum value of the vector

get_maximum <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(sort(x, decreasing = T)[1])
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(sort(x, decreasing = T)[1]) 
        }
    }
  }
}



# get_range function
# This function returns the range of a numeric vector
# Input: a numeric vector
# Output: the range of the vector

get_range <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(get_maximum(x) - get_minimum(x))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(get_maximum(x) - get_minimum(x)) 
        }
    }
  }
}



# get_percentile10 function
# This function returns the 10th percentile of a numeric vector
# Input: a numeric vector
# Output: the 10th percentile of the vector

get_percentile10 <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(quantile(x, probs = 0.1, names = F))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(quantile(x, probs = 0.1, names = F)) 
        }
    }
  }
}



# get_percentile90 function
# This function returns the 90th percentile of a numeric vector
# Input: a numeric vector
# Output: the 90th percentile of the vector

get_percentile90 <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(quantile(x, probs = 0.9, names = F))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(quantile(x, probs = 0.9, names = F)) 
        }
    }
  }
}



# get_median function (with median1 subfunction)
# This function returns the median of a numeric vector
# Input: a numeric vector
# Output: the median of the vector

median1 <- function(x) {
  a <- sort(x)
  if (length(a) %% 2 != 0) {
    return(a[(length(a) + 1 ) / 2])
  }
  else if (length(a) %% 2 == 0) {
    return((a[length(a)/2] + a[length(a)/2 + 1])/2)
  }
}
  
get_median <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(median1(x))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(median1(x)) 
        }
    }
  }
}



# get_average function (with mean1 subfunction)
# This function returns the mean of a numeric vector
# Input: a numeric vector
# Output: the mean of the vector

mean1 <- function(x) {
  a = 0
  for (i in 1: length(x)) {
    a = a + x[i]
  }
  return(a/length(x))
}

get_average <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(mean1(x))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(mean1(x)) 
        }
    }
  }
}



# get_stdev function (with stdev1 subfunction)
# This function returns the standard deviation of a numeric vector
# Input: a numeric vector
# Output: the standard deviation of the vector

stdev1 <- function(x) {
  a = 0
  for (i in 1: length(x)) {
    a = a + (x[i] - get_average(x))^2
  }
  return(sqrt(a/(length(x)-1)))
}

get_stdev <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(stdev1(x))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(stdev1(x))
        }
    }
  }
}



# get_quartile1 function
# This function returns the first quartile of a numeric vector
# Input: a numeric vector
# Output: the first quartile of the vector

get_quartile1 <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(quantile(x, probs = 0.25, names = F))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(quantile(x, probs = 0.25, names = F))
        }
    }
  }
}



# get_quartile3 function
# This function returns the third quartile of a numeric vector
# Input: a numeric vector
# Output: the third quartile of the vector

get_quartile3 <- function(x, na.rm = T) {
  if (is.numeric(x) == F) {
    stop("non-numeric argument")
  } else if (is.numeric(x) == T) {
    if (na.rm == T) {
      x <- remove_missing(x)
      return(quantile(x, probs = 0.75, names = F))
    } else if (na.rm == F) {
      if (anyNA(x) == T) {
        return(NA) } else {
          return(quantile(x, probs = 0.75, names = F))
        }
    }
  }
}



# count_missing function
# This function returns the number of NA's of a numeric vector
# Input: a numeric vector
# Output: the number of NA's of the vector

count_missing <- function(x) {
  a = 0
  for (i in 1: length(x)) {
    if (is.na(x[i] == T)) {
      a = a + 1
    }
  }
  return(a)
}



# summary_stats function
# This function returns a list of summary stats of a numeric vector
# Input: a numeric vector
# Output: the summary stats of the vector

summary_stats <- function(x) {
  c <- c("minimum", "percent10", "quartile1", "median", "mean", "quartile3",
         "percent90", "maximum", "range", "stdev", "missing")
  a <- list(get_minimum(x), get_percentile10(x), get_quartile1(x), 
            get_median(x),
       get_average(x), get_quartile3(x), get_percentile90(x), 
       get_maximum(x), get_range(x), get_stdev(x), count_missing(x))
  names(a) <- c
  return(a)
}



# print_stats function
# This function prints a list of summary stats
# Input: a list of summary stats
# Output: a nice printed format of the stats

print_stats <- function(x) {
  c0 <- rep("##", length(x))
  c1 <- names(x)
  c1 <- sprintf("%-9s", c1)
  c2 <- rep(":", length(x))
  a <- sapply(x, function(x) x[[1]])
  for (i in 1:length(x)) {
    cat(paste(c0[i], ' ', c1[i], c2[i], ' ', 
              sprintf("%.4f", a[i]), '\n', sep = ''))
  }
}



# rescale100 function
# This function rescales a vector x
# Input: a vector x, mininimum of x, maximum of x
# Output: a rescaled vector with a scale of 0 to 100

rescale100 <- function(x, xmin, xmax) {
    z <- 100*(x - xmin)/(xmax - xmin)
    return(z)
}



# drop_lowest function
# This function drops the lowest value of a numeric vector
# Input: a numeric vector
# Output: a numeric vector

drop_lowest <- function(x) {
  for (i in 1: length(x)) {
    if (x[i] == min(x)) {
      x <- x[-i]
      return(x)
    }
  }
}



# score_homework function
# This function computes the average of hw scores
# Input: a vector of hw scores and logical argument "drop"
# Output: the average of hw scores

score_homework <- function(x, drop = T) {
  if (drop == T) {
    x <- drop_lowest(x)
    return(get_average(x))
  }
  else if (drop == F) {
    return(get_average(x))
  }
}



# score_quiz function
# This function computes the average of quiz scores
# Input: a vector of quiz scores and logical argument "drop"
# Output: the average of quiz scores

score_quiz <- function(x, drop = T) {
  if (drop == T) {
    x <- drop_lowest(x)
    return(get_average(x))
  }
  else if (drop == F) {
    return(get_average(x))
  }
}



# score_lab function
# This function calculates lab score based on attendence
# Input: number of attendence
# Output: lab score

score_lab <- function(x){
  if (x <= 6) {
    return(0)
  }
  else if (x >= 7) {
    x <- as.character(x)
    switch(x,
         "12" = 100,
         "11" = 100,
         "10" = 80,
         "9"= 60,
         "8" = 40,
         "7" = 20 )
  }
}






