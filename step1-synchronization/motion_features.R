#Takes as input a dataframe "ds" of behavioral code, time (as a datetime object), and variable number of sensor/axis dimensions,
# and a paramater "who" that refers to "parent" or "infant" as string to adjust the sensor naming and math to the appropriate set

motion_features <- function(ds, who) {
  require(psych)
  require(corrr)
  require(tidyverse)
  
  #Figure out coded behavior and time for window
  code <- ds %>% select(code)
  code_out <- ds %>% count(code, sort = T) %>% slice_head() %>% mutate(code_prop = n/nrow(ds)) %>% select(-n)
  # code_out$time <- median(ds$time_compressed, na.rm = T)
  # code_out$time_from <- median(ds$time_compressed, na.rm = T)
  # code_out$time_to <- median(ds$time_compressed, na.rm = T)
  code_out$time <- median(ds$time, na.rm = T)
  code_out$time_from <- min(ds$time, na.rm = T)
  code_out$time_to <- max(ds$time, na.rm = T)
  
  #Create a subset of data with only the sensor data
  # mot <- ds %>% select(-time, -code, -time_compressed)
  mot <- ds %>% select(-time, -code)
  
  
  #Create a list of summary stats and apply individual to every sensor
  na_mean <- function(x) mean(x, na.rm = T) 
  na_sd <- function(x) sd(x, na.rm = T)
  na_med <- function(x) median(x, na.rm = T)
  na_skew <- function(x) skew(x, na.rm = T)
  na_kurtosis <- function(x) kurtosi(x, na.rm = T)
  na_min <- function(x) min(x, na.rm = T)
  na_max <- function(x) max(x, na.rm = T)
  na_25 <- function(x)  as.numeric(quantile(x, probs = .25, na.rm = T))
  na_75 <- function(x)  as.numeric(quantile(x, probs = .75, na.rm = T))
  fx_list <- list(M = na_mean, SD = na_sd, MED = na_med, SKEW = na_skew, KURT = na_kurtosis, P25 = na_25, P75 = na_75)
  mot_features <- mot %>% summarize(across(everything(), fx_list))
  
  #Diff helper functions to use later on
  diff12 <- function(x)  mean(pull(x, 1) - pull(x,2), na.rm = T)
  diff13 <- function(x)  mean(pull(x, 1) - pull(x,3), na.rm = T)
  diff14 <- function(x)  mean(pull(x, 1) - pull(x,4), na.rm = T)
  diff23 <- function(x)  mean(pull(x, 2) - pull(x,3), na.rm = T)
  diff24 <- function(x)  mean(pull(x, 2) - pull(x,4), na.rm = T)
  diff34 <- function(x)  mean(pull(x, 3) - pull(x,4), na.rm = T)
  
  #Calculate summaries across axes for each sensor
  if (who == "parent") {
    sensor_features <- list(wacc = select(ds, starts_with("wacc")), 
                            hacc = select(ds, starts_with("hacc")), 
                            wgyr = select(ds, starts_with("wgyr")), 
                            hgyr = select(ds, starts_with("hgyr")))
    
  }
  if (who == "infant") {
    sensor_features <- list(lhacc = select(ds, starts_with("lhacc")), 
                            laacc = select(ds, starts_with("laacc")), 
                            lhgyr = select(ds, starts_with("lhgyr")), 
                            lagyr = select(ds, starts_with("lagyr")),
                            rhacc = select(ds, starts_with("rhacc")), 
                            raacc = select(ds, starts_with("raacc")), 
                            rhgyr = select(ds, starts_with("rhgyr")), 
                            ragyr = select(ds, starts_with("ragyr")))
    
  }
  sensor_names <- names(sensor_features)
  
  #Map summary functions to each sensor across axes
  sensor_sums <- map_dfc(sensor_features, ~ .x %>% 
                           summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(sensor_names[seq_along(.)],"_SUM"))  
  sensor_abssums <- map_dfc(sensor_features, ~ .x %>% mutate(across(everything(), abs)) %>% summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(sensor_names[seq_along(.)],"_MAG"))  
  sensor_corrs <- map_dfc(sensor_features, ~ .x %>% correlate(use = "pairwise.complete.obs") %>% stretch(na.rm = T, remove.dups = T) %>% pivot_wider(names_from = c("x","y"), names_prefix = "CORR_", values_from = "r"))
  sensor_abscorrs <- map_dfc(sensor_features, ~ .x %>% mutate(across(everything(), abs)) %>% correlate(use = "pairwise.complete.obs") %>% stretch(na.rm = T, remove.dups = T) %>% pivot_wider(names_from = c("x","y"), names_prefix = "ABSCORR_", values_from = "r"))
  
  diff_fx <- c("DIFF12", "DIFF13", "DIFF23")
  sensor_diffs <- map_dfc(sensor_features, ~ t(list(diff12 = diff12(.x), diff13 = diff13(.x), diff23 = diff23(.x)))) %>% set_names(simplify(map(sensor_names, ~ paste0(diff_fx[seq_along(diff_fx)],"_", .x))))
  sensor_absdiffs <-  map_dfc(sensor_features, 
                        ~ t(list(diff12 = diff12(.x %>% mutate(across(everything(), abs))),
                                 diff13 = diff13(.x %>% mutate(across(everything(), abs))),
                                 diff23 = diff23(.x %>% mutate(across(everything(), abs)))))) %>% set_names(simplify(map(sensor_names, ~ paste0("ABS",diff_fx[seq_along(diff_fx)],"_", .x))))
  
  #Calculate summaries across sensors for each axis
  cross_features <- list(xacc = select(ds, contains("acc_x")),
                         yacc = select(ds, contains("acc_y")),
                         zacc = select(ds, contains("acc_z")),
                         xgyr = select(ds, contains("gyr_x")),
                         ygyr = select(ds, contains("gyr_y")),
                         zgyr = select(ds, contains("gyr_z")))
  cf_names <- names(cross_features)
  
  sums <- map_dfc(cross_features, ~ .x %>% summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(cf_names[seq_along(.)],"_SUM"))  
  abssums <- map_dfc(cross_features, ~ .x %>% mutate(across(everything(), abs)) %>% summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(cf_names[seq_along(.)],"_MAG"))  
  corrs <- map_dfc(cross_features, ~ .x %>% correlate(use = "pairwise.complete.obs") %>% stretch(na.rm = T, remove.dups = T) %>% pivot_wider(names_from = c("x","y"), names_prefix = "CORR_", values_from = "r"))
  abscorrs <- map_dfc(cross_features, ~ .x %>% mutate(across(everything(), abs)) %>% correlate(use = "pairwise.complete.obs") %>% stretch(na.rm = T, remove.dups = T) %>% pivot_wider(names_from = c("x","y"), names_prefix = "ABSCORR_", values_from = "r"))
  
  #Get across sensor differences for each axis; two sensors (and 1 diff) for parent vs 4 (and 6 diffs) for infants
  if (who == "parent") {
    diff_fx <- c("DIFF12")
    diffs <- map_dfc(cross_features, ~ t(list(diff12 = diff12(.x)))) %>% set_names(simplify(map(cf_names, ~ paste0(diff_fx[seq_along(diff_fx)],"_", .x))))
    absdiffs <-  map_dfc(cross_features, ~ t(list(diff12 = diff12(.x %>% mutate(across(everything(), abs)))))) %>% set_names(simplify(map(cf_names, ~ paste0("ABS",diff_fx[seq_along(diff_fx)],"_", .x))))
  }
  if (who == "infant") {
    diff_fx <- c("DIFF12", "DIFF13", "DIFF14","DIFF23","DIFF24","DIFF34")
    diffs <- map_dfc(cross_features, 
                            ~ t(list(diff12 = diff12(.x), diff13 = diff13(.x), diff14 = diff14(.x), diff23 = diff23(.x), diff24 = diff24(.x), diff34 = diff34(.x)))) %>% 
      set_names(simplify(map(cf_names, ~ paste0(diff_fx[seq_along(diff_fx)],"_", .x))))
    
    absdiffs <-  map_dfc(cross_features, 
                                ~ t(list(diff12 = diff12(.x %>% mutate(across(everything(), abs))),
                                         diff13 = diff13(.x %>% mutate(across(everything(), abs))),
                                         diff14 = diff14(.x %>% mutate(across(everything(), abs))),
                                         diff23 = diff23(.x %>% mutate(across(everything(), abs))),
                                         diff24 = diff24(.x %>% mutate(across(everything(), abs))),
                                         diff34 = diff34(.x %>% mutate(across(everything(), abs)))))) %>% set_names(simplify(map(cf_names, ~ paste0("ABS",diff_fx[seq_along(diff_fx)],"_", .x))))
  }
  
  bind_cols(code_out, mot_features, sensor_sums, sensor_abssums, sensor_corrs, sensor_abscorrs, sensor_diffs, sensor_absdiffs, sums, abssums, corrs, abscorrs, diffs, absdiffs)
}