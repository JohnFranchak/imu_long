#Takes as input a dataframe "ds_t" of behavioral code, time (as a datetime object), and variable number of sensor/axis dimensions,
# and a paramater "who" that refers to "parent" or "infant" as string to adjust the sensor naming and math to the appropriate set

motion_features <- function(ds_t, who, complete = T) {
  require(psych)
  require(corrr)
  require(tidyverse)
  
  #Figure out coded behavior and time for window
  code <- ds_t %>% select(code)
  code_out <- ds_t %>% count(code, sort = T) %>% slice_head() %>% mutate(code_prop = n/nrow(ds_t)) %>% select(-n)
  # code_out$time <- median(ds_t_t$time_compressed, na.rm = T)
  # code_out$time_from <- median(ds_t$time_compressed, na.rm = T)
  # code_out$time_to <- median(ds_t$time_compressed, na.rm = T)
  code_out$time <- median(ds_t$time, na.rm = T)
  code_out$time_from <- min(ds_t$time, na.rm = T)
  code_out$time_to <- max(ds_t$time, na.rm = T)
  print(code_out)
  
  #Create a subset of data with only the sensor data
  # mot <- ds_t %>% select(-time, -code, -time_compressed)
  mot <- ds_t %>% select(-time, -code)
  
  
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
  
  if (complete == T) {
    #Diff helper functions to use later on
    diff12 <- function(x)  mean(pull(x, 1) - pull(x,2), na.rm = T)
    diff13 <- function(x)  mean(pull(x, 1) - pull(x,3), na.rm = T)
    diff14 <- function(x)  mean(pull(x, 1) - pull(x,4), na.rm = T)
    diff23 <- function(x)  mean(pull(x, 2) - pull(x,3), na.rm = T)
    diff24 <- function(x)  mean(pull(x, 2) - pull(x,4), na.rm = T)
    diff34 <- function(x)  mean(pull(x, 3) - pull(x,4), na.rm = T)
    
    #Calculate summaries across axes for each sensor
    if (who == "parent") {
      sensor_features <- list(wacc = select(ds_t, starts_with("wacc")), 
                              hacc = select(ds_t, starts_with("hacc")), 
                              wgyr = select(ds_t, starts_with("wgyr")), 
                              hgyr = select(ds_t, starts_with("hgyr")))
      
    }
    if (who == "infant") {
      sensor_features <- list(lhacc = select(ds_t, starts_with("lhacc")), 
                              laacc = select(ds_t, starts_with("laacc")), 
                              lhgyr = select(ds_t, starts_with("lhgyr")), 
                              lagyr = select(ds_t, starts_with("lagyr")),
                              rhacc = select(ds_t, starts_with("rhacc")), 
                              raacc = select(ds_t, starts_with("raacc")), 
                              rhgyr = select(ds_t, starts_with("rhgyr")), 
                              ragyr = select(ds_t, starts_with("ragyr")))
      
    }
    sensor_names <- names(sensor_features)
    
    correlate_robust <- function(data, prefix) {
      data_out = tryCatch({
        correlate(data, use = "pairwise.complete.obs") %>% 
          stretch(na.rm = T, remove.dups = T) %>% pivot_wider(names_from = c("x","y"), names_prefix = prefix, values_from = "r")
      }, warning = function(w) {
        temp <- tibble(x = NA, y = NA, z = NA) %>% mutate(across(everything(), as.numeric)) 
        colnames(temp) <- c(paste0(prefix, names(data)[1],"_",names(data)[2]),
                            paste0(prefix, names(data)[1],"_",names(data)[3]),
                            paste0(prefix, names(data)[2],"_",names(data)[3]))
        temp},
      error = function(e) {
        temp <- tibble(term = names(data), x = c(NA, NA, NA), y = c(NA, NA, NA), z = c(NA, NA, NA)) 
        colnames(temp) <- c(paste0(prefix, names(data)[1],"_",names(data)[2]),
                            paste0(prefix, names(data)[1],"_",names(data)[3]),
                            paste0(prefix, names(data)[2],"_",names(data)[3]))
        temp      
        })
      return(data_out)
    }
    
    #Map summary functions to each sensor across axes
    suppressMessages(
      sensor_sums <- map_dfc(sensor_features, ~ .x %>% 
                             summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(sensor_names[seq_along(.)],"_SUM")))
    suppressMessages(
    sensor_abssums <- map_dfc(sensor_features, ~ .x %>% mutate(across(everything(), abs)) %>% summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(sensor_names[seq_along(.)],"_MAG")))  
    suppressMessages(
    sensor_corrs <- map_dfc(sensor_features, ~ correlate_robust(.x, prefix = "CORR_")))
    suppressMessages(
    sensor_abscorrs <- map_dfc(sensor_features, ~ .x %>% mutate(across(everything(), abs)) %>% correlate_robust(prefix = "ABSCORR_")))
    
    
    diff_fx <- c("DIFF12", "DIFF13", "DIFF23")
    suppressMessages(
      sensor_diffs <- map_dfc(sensor_features, ~ t(list(diff12 = diff12(.x), diff13 = diff13(.x), diff23 = diff23(.x)))) %>% set_names(simplify(map(sensor_names, ~ paste0(diff_fx[seq_along(diff_fx)],"_", .x)))))
    suppressMessages(
    sensor_absdiffs <-  map_dfc(sensor_features, 
                          ~ t(list(diff12 = diff12(.x %>% mutate(across(everything(), abs))),
                                   diff13 = diff13(.x %>% mutate(across(everything(), abs))),
                                   diff23 = diff23(.x %>% mutate(across(everything(), abs)))))) %>% set_names(simplify(map(sensor_names, ~ paste0("ABS",diff_fx[seq_along(diff_fx)],"_", .x)))))
    
    #Calculate summaries across sensors for each axis
    cross_features <- list(xacc = select(ds_t, contains("acc_x")),
                           yacc = select(ds_t, contains("acc_y")),
                           zacc = select(ds_t, contains("acc_z")),
                           xgyr = select(ds_t, contains("gyr_x")),
                           ygyr = select(ds_t, contains("gyr_y")),
                           zgyr = select(ds_t, contains("gyr_z")))
    cf_names <- names(cross_features)
    suppressMessages(
    sums <- map_dfc(cross_features, ~ .x %>% summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(cf_names[seq_along(.)],"_SUM")))  
    suppressMessages(
    abssums <- map_dfc(cross_features, ~ .x %>% mutate(across(everything(), abs)) %>% summarize(m = sum(c_across(everything()), na.rm = T))) %>% set_names(~ paste0(cf_names[seq_along(.)],"_MAG"))) 
    suppressMessages(
    corrs <- map_dfc(cross_features, ~ .x %>% correlate(use = "pairwise.complete.obs") %>% stretch(na.rm = T, remove.dups = T) %>% pivot_wider(names_from = c("x","y"), names_prefix = "CORR_", values_from = "r")))
    suppressMessages(
    abscorrs <- map_dfc(cross_features, ~ .x %>% mutate(across(everything(), abs)) %>% correlate(use = "pairwise.complete.obs") %>% stretch(na.rm = T, remove.dups = T) %>% pivot_wider(names_from = c("x","y"), names_prefix = "ABSCORR_", values_from = "r")))
    
    #Get across sensor differences for each axis; two sensors (and 1 diff) for parent vs 4 (and 6 diffs) for infants
    if (who == "parent") {
      diff_fx <- c("DIFF12")
      suppressMessages(
        diffs <- map_dfc(cross_features, ~ t(list(diff12 = diff12(.x)))) %>% set_names(simplify(map(cf_names, ~ paste0(diff_fx[seq_along(diff_fx)],"_", .x)))))
      suppressMessages(
        absdiffs <-  map_dfc(cross_features, ~ t(list(diff12 = diff12(.x %>% mutate(across(everything(), abs)))))) %>% set_names(simplify(map(cf_names, ~ paste0("ABS",diff_fx[seq_along(diff_fx)],"_", .x)))))
    }
    if (who == "infant") {
      diff_fx <- c("DIFF12", "DIFF13", "DIFF14","DIFF23","DIFF24","DIFF34")
      suppressMessages(
        diffs <- map_dfc(cross_features, 
                              ~ t(list(diff12 = diff12(.x), diff13 = diff13(.x), diff14 = diff14(.x), diff23 = diff23(.x), diff24 = diff24(.x), diff34 = diff34(.x)))) %>% 
        set_names(simplify(map(cf_names, ~ paste0(diff_fx[seq_along(diff_fx)],"_", .x)))))
      
      suppressMessages(
        absdiffs <-  map_dfc(cross_features, 
                                  ~ t(list(diff12 = diff12(.x %>% mutate(across(everything(), abs))),
                                           diff13 = diff13(.x %>% mutate(across(everything(), abs))),
                                           diff14 = diff14(.x %>% mutate(across(everything(), abs))),
                                           diff23 = diff23(.x %>% mutate(across(everything(), abs))),
                                           diff24 = diff24(.x %>% mutate(across(everything(), abs))),
                                           diff34 = diff34(.x %>% mutate(across(everything(), abs)))))) %>% 
          set_names(simplify(map(cf_names, ~ paste0("ABS",diff_fx[seq_along(diff_fx)],"_", .x)))))
    }
    
    bind_cols(code_out, mot_features, sensor_sums, sensor_abssums, sensor_corrs, sensor_abscorrs, sensor_diffs, sensor_absdiffs, sums, abssums, corrs, abscorrs, diffs, absdiffs)
    } else
    {
      bind_cols(code_out, mot_features)
    }
  
}

