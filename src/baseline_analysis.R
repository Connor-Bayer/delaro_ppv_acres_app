#
# File Description: File to attempt to recreate the simulation results passed on by Greg,
#                   and apply them to a new framework
#
# Data Requirements - need data initially provided by Juan's team
#

library(pacman)

p_load(data.table, tidyverse, extraDistr)


#' convert a data.table with uplift_pred column (0 or 1) and uplift_test (0 or
#' 1) into a confusion matrix (list with names TP, TN, FP, FN
#'
#' @param path_trial: file path to get to delaro showcase baseline dataset
#' @param path_regions: file path to get to regional shapefile
#' @return sf-enabled data.table (list): data table with shapes attached at the regional
#' level
load_baseline_data <- function(path_trial, path_regions) {
  ## read in trial data
  # trialdata <- readxl::read_excel(path_trial, sheet = 1)
  trialdata = fread(path_trial)
  regional_shapes <- read_sf(path_regions)

  ## convert to simple features
  if('y_pred' %in% colnames(trialdata)){
    setDT(trialdata)[,c("uplift_pred", "uplift_test") := list(as.integer(y_pred),as.integer(y_true))]
  } else{
    setDT(trialdata)[,c("uplift_pred", "uplift_test") := list(as.integer(y_calculated),as.integer(y_true))]
  }
  trialdata <- st_as_sf(trialdata, coords = c("longitude","latitude"), remove = F)

  ## merge to business regions
  trialdata <- st_join(trialdata %>%
                          st_set_crs(st_crs(regional_shapes)),
                        regional_shapes,
                        join = st_within)

  trialdata <- merge(st_drop_geometry(trialdata), regional_shapes)

  return(trialdata)

}

#' convert a data.table with uplift_pred column (0 or 1) and uplift_test (0 or
#' 1) into a confusion matrix (list with names TP, TN, FP, FN
#'
#' @param dt: data.table: with columns `uplift_pred` and `uplift_test`
#' @param threshold: numeric between 0 and 1: redefine where 'treatment' occurs
#' @param test_col: name of column used to identify ground truth preds
#' @param logits_col: name of column with PROBABILITIES of predictions
#' @return list: confusion matrix with names TP, TN, FP, FN
 ccgenerate_cm_with_threshold<- function(dt, threshold, test_col, logits_col) {
  cm <- list()
  cm$TP <- dt[, sum(get(logits_col) >= threshold & get(test_col) == TRUE)]
  cm$TN <- dt[, sum(get(logits_col) < threshold & get(test_col) == FALSE)]
  cm$FP <- dt[, sum(get(logits_col) >= threshold & get(test_col) == FALSE)]
  cm$FN <- dt[, sum(get(logits_col) < threshold & get(test_col) == TRUE)]
  return(cm)
}

#' convert a data.table with uplift_pred column (0 or 1) and uplift_test (0 or
#' 1) into a confusion matrix (list with names TP, TN, FP, FN
#'
#' @param dt: data.table: with columns `uplift_pred` and `uplift_test`
#' @return list: confusion matrix with names TP, TN, FP, FN
generate_cm <- function(dt) {
    cm <- list()
    cm$TP <- dt[, sum(uplift_pred == 1 & uplift_test == 1)]
    cm$TN <- dt[, sum(uplift_pred == 0 & uplift_test == 0)]
    cm$FP <- dt[, sum(uplift_pred == 1 & uplift_test == 0)]
    cm$FN <- dt[, sum(uplift_pred == 0 & uplift_test == 1)]
    return(cm)
}

#' convert a data.table with uplift_pred column (0 or 1) and uplift_test (0 or
#' 1) into a confusion matrix (list with names TP, TN, FP, FN
#'
#' @param dt: data.table: with columns `uplift_pred` and `uplift_test`
#' @return list: confusion matrix with names TP, TN, FP, FN
generate_cm_by <- function(dt, by_column) {

  cm = dt[, .(TP = sum(uplift_pred == 1 & uplift_test == 1),
        TN = sum(uplift_pred == 0 & uplift_test == 0),
        FP = sum(uplift_pred == 1 & uplift_test == 0),
        FN = sum(uplift_pred == 0 & uplift_test == 1)
        ), by = by_column]

  return(cm)
}

#' Compute weights based on two factors: density of acres in program and
#' expected payout. (deprecated - changed approach)
#'
#' Idea: recursive call to this function to update dt until the sample number reaches 0
#'
#' @param dt: data.table: with columns `current_acres`, `baseline_acres`, `ppv`, `payment`
#' @param sample: boolean: whether or not to return a region for drop/gain
#' @return list: confusion matrix with names TP, TN, FP, FN
drop_add_data_weights <- function(dt, sample = 0, drop = TRUE,
                                  by_column = 'CP_REGION', vect = c()) {

  weights = list()
  dt<- dt %>% mutate(drop_weights = current_acres/acres*(1 - ppv)*payout)
  dt<- dt %>% mutate(gain_weights = acres/current_acres/((1 - ppv)*payout))
  dt = dt %>% group_by(iter) %>%mutate(gain_prob = softmax(gain_weights),
                                       drop_prob = softmax(drop_weights)) %>% ungroup()
  iters = c(1:max(dt$iter))

  if(sample > 0){
    if(drop){

      lst = lapply(iters, FUN = function(x){
        dt_sub = dt %>% filter(iter == x)
        selected = dt$region[rcat(1, (dt_sub$gain_prob))]
        dt_sub %<>% mutate(current_acres = ifelse(region == selected, current_acres - 10, current_acres))
        return(list(data = dt_sub, selected = selected))
      })

      dropped = vect

      dt = lapply(lst, function(l) l[[1]]) %>% rbindlist()
      selected = sapply(lst, function(l) l[[2]])
      dropped = append(selected, dropped)
      sample = sample - 10
      dt = rbindlist(list(dt, drop_add_data_weights(dt, sample = sample, drop = TRUE, vect = dropped)))

      return(dt)
    } else{
      iters = c(1:max(dt$iter))

      lst = lapply(iters, FUN = function(x){
        dt_sub = dt %>% filter(iter == x)
        selected = dt$region[rcat(1, (dt_sub$gain_prob))]
        dt_sub %<>% mutate(current_acres = ifelse(region == selected, current_acres + 10, current_acres))
        return(list(data = dt_sub, selected = selected))
      })

      added = vect

      dt = lapply(lst, function(l) l[[1]]) %>% rbindlist()
      selected = sapply(lst, function(l) l[[2]])
      added = append(selected, added)
      sample = sample - 10
      dt$dropped_acres = dt$dropped_acres + 10
      dt =drop_add_data_weights(dt, sample = sample, drop = FALSE, vect = added)
      return(dt)
    }


  }

  return(dt)
}

#' Compute softmax of given set of values to create probs based on values
#'
#' @param par: vector: set of values to reweight to probabilities
#' @return probs: vector: set of probabilies based on initial weights.

softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk)))
  }
  probs <- exp(par - Lk)
  return(probs)
}


#' generate a data.table of confusion matrices using a group by operation on
#' another data.table with `uplift_pred` columns and `uplift_test` columns, with
#' each row of the output being a distinct confusion matrix. Hence columns in
#' output contain TP, TN, FP, FN
#'
#' @param dt data.table: input data.table with `uplift_pred` and `uplift_test` columns
#' @param by_cols char(n): list of column names to group by
#' @return data.table with columns TP, TN, FN, and FP columns and any columns in `by_cols`
generate_cm_dt_by <- function(dt, by_cols = c("")) {
    cm_dt <- dt[, .(
        TP = sum(uplift_pred == 1 & uplift_test == 1),
        TN = sum(uplift_pred == 0 & uplift_test == 0),
        FP = sum(uplift_pred == 1 & uplift_test == 0),
        FN = sum(uplift_pred == 0 & uplift_test == 1)
    ), by = by_cols]
    setorderv(cm_dt, by_cols)
    return(cm_dt)
}


#' Given a confusion matrix, `fit` a dirichlet multinomial distribution and
#' sample to generate posterior confusion matrices
#'
#' @param cm list: confusion matrix with names(cm) containing TP, FP, TN, FN
#' @param n int: number of samples to draw - named list, where name == region_name
#' and value is number of draws from that region
#' @param size int: total number of observations in the sampled confusion
#'   matrices; defaults to same size as cm
sample_posterior_cms_by_region <- function(cm,
                                 n = 2e3,
                                 size = list(`CS Central Plains` = 15000,
                                             `CS Eastern Corn Belt` = 10000,
                                             `CS IL` = 70000,
                                             `CS Midwest` = 55000,
                                             `CS MN/WI` = 30000,
                                             `CS Northern Plains` = 90000,
                                             `CS South & East Coast` = 0,
                                             `CS West` = 20000),
                                 cm_class_order = c("TP", "FP", "TN", "FN"),
                                 size_variable = c('acre_bucket')){

  ## extract cm entries in consistent order

  if(!is.data.table(cm)){
    stop(paste0('Error: `cm` needs to be a data.table, I received an object of type: ',showClass(cm), 'Did you use generate_cm_by?'))
    return(cm)
  }

  cm[,annual_weight := (TP + TN + FP + FN)/sum(TP + TN + FP + FN), by = 'CP_REGION']
  byvar = unique(cm[[1]])
  dtout = data.table()

  for(var in byvar){
    if(is.data.table(size)){
      size_tmp = size[CP_REGION == var] %>% dplyr::select(matches(size_variable))
      #if(nrow(size_tmp))
      } else{
      size_tmp = size[[var]]
    }

    var_r = var

    cm_temp = list(TP = cm[cm[[1]] == var_r]$TP,
                   FP = cm[cm[[1]] == var_r]$FP,
                   TN = cm[cm[[1]] == var_r]$TN,
                   FN = cm[cm[[1]] == var_r]$FN)

    dirichlet_weights <- vapply(
      cm_class_order,
      function(name) cm_temp[[name]],
      FUN.VALUE = numeric(1)
    )


    #observed_data <- cm[,..cm_class_order]

    prior_weights <- c(0, 0, 0, 0)
    if (any(dirichlet_weights == 0)) {
      prior_weights <- c(1, 1, 1, 1) ## switch to maximum entropy proper prior
    }

    ## assumes an improper prior of rdirichlet(alpha = c(0,0,0,0))

    if((!is.null(size_tmp)) && (size_tmp > 0)){
      dt_samples <- as.data.table(rdirmnom(n = n, size = as.integer(size_tmp), alpha = prior_weights + dirichlet_weights))
      names(dt_samples) <- cm_class_order

      dt_samples[,names(cm[,1]) := var_r]

      dt_samples[,iter := c(1:n)]

      } else{
      dt_samples <- data.table()
    }


    dtout = rbindlist(list(dtout, dt_samples))

  }

  return(dtout)
}

#' Given a confusion matrix, `fit` a dirichlet multinomial distribution and
#' sample to generate posterior confusion matrices
#'
#' @param cm list: confusion matrix with names(cm) containing TP, FP, TN, FN
#' @param n int: number of samples to draw
#' @param size_dt list (data.table): data table with columns year, region, and size
#'
sample_posterior_cms_by_regionyr <- function(cm,
                                           n = 2e3,
                                           size_dt = size_dt,
                                           cm_class_order = c("TP", "FP", "TN", "FN")){

  ## extract cm entries in consistent order

  if(!is.data.table(cm)){
    stop(paste0('Error: `cm` needs to be a data.table, I received an object of type: ',showClass(cm), 'Did you use generate_cm_by?'))
    return(cm)
  }
  cm[,annual_weight := (TP + TN + FP + FN)/sum(TP + TN + FP + FN), by = 'CP_REGION']
  byvar = unique(cm[[1]])
  dtout = data.table()

  for(var in byvar){
    size_tmp = size[[var]]
    var_r = var

    cm_temp = list(TP = cm[cm[[1]] == var_r]$TP,
                   FP = cm[cm[[1]] == var_r]$FP,
                   TN = cm[cm[[1]] == var_r]$TN,
                   FN = cm[cm[[1]] == var_r]$FN)

    dirichlet_weights <- vapply(
      cm_class_order,
      function(name) cm_temp[[name]],
      FUN.VALUE = numeric(1)
    )


    #observed_data <- cm[,..cm_class_order]

    prior_weights <- c(0, 0, 0, 0)
    if (any(dirichlet_weights == 0)) {
      prior_weights <- c(1, 1, 1, 1) ## switch to maximum entropy proper prior
    }

    ## assumes an improper prior of rdirichlet(alpha = c(0,0,0,0))
    if(size_tmp > 0){
      dt_samples <- as.data.table(rdirmnom(n = n, size = max(size_tmp, 100), alpha = prior_weights + dirichlet_weights))
      names(dt_samples) <- cm_class_order

      dt_samples[,names(cm[,1]) := var_r]

      dt_samples[,iter := c(1:n)]

    } else{
      dt_samples <- data.table()
    }


    dtout = rbindlist(list(dtout, dt_samples))

  }

  return(dtout)
}


#' Given a confusion matrix, `fit` a dirichlet multinomial distribution and
#' sample to generate posterior confusion matrices
#'
#' @param cm list: confusion matrix with names(cm) containing TP, FP, TN, FN
#' @param n int: number of samples to draw
#' @param size int: total number of observations in the sampled confusion
#'   matrices; defaults to same size as cm
sample_posterior_cms <- function(cm,
                                 n = 2e4,
                                 size = cm[["TP"]] + cm[["FP"]] + cm[["TN"]] + cm[["FN"]],
    cm_class_order = c("TP", "FP", "TN", "FN")){

    ## extract cm entries in consistent order

    dirichlet_weights <- vapply(
         cm_class_order,
         function(name) cm[[name]],
         FUN.VALUE = numeric(1)
    )


    #observed_data <- cm[,..cm_class_order]

    prior_weights <- c(0, 0, 0, 0)
    if (any(dirichlet_weights == 0)) {
      prior_weights <- c(1, 1, 1, 1) ## switch to maximum entropy proper prior
    }

    ## assumes an improper prior of rdirichlet(alpha = c(0,0,0,0))

    dt_samples <- as.data.table(rdirmnom(n = n, size = max(size, 100), alpha = prior_weights + dirichlet_weights))

    names(dt_samples) <- cm_class_order

    return(dt_samples)
}


#' @param cm list/data.frame: confusion matrix with names(cm) containing TP, FP, TN, FN
ppv <- function(cm) {
  output <- cm[["TP"]] / (cm[["TP"]] + cm[["FP"]])
  return(round(output, 4))
}

npv <- function(cm) {
  output <- cm[["FP"]] / (cm[["TP"]] + cm[["FP"]])
  return(round(output, 4))
}

fpr <- function(cm) {
  output <- cm[["FN"]] / (cm[["TN"]] + cm[["FN"]])
  return(round(output, 4))
}

tpr <- function(cm) {
  output <- cm[["TN"]] / (cm[["TN"]] + cm[["FN"]])
  return(round(output, 4))
}

payout_per_acre <- function(cm, rebate_dollars_per_acre = 6) {
  output <- rebate_dollars_per_acre * cm[["FP"]] / (cm[["TP"]] + cm[["FP"]])
  return(round(output, 4))
}

sensitivity <- function(cm) {
  output <- cm[["TP"]] / (cm[["TP"]] + cm[["FN"]])
  return(round(output, 4))
}

perc_pred_uplift <- function(cm) {
    output <- (cm[["TP"]] + cm[["FP"]]) / (cm[["TP"]] + cm[["FP"]] + cm[["TN"]] + cm[["FN"]])
    return(round(output, 4))
}

num_obs <- function(cm) {
  return(cm[["TP"]] + cm[["FP"]] + cm[["TN"]] + cm[["FN"]])
}

fb_score <- function(cm, b=1) {
  return((1+b^2)*cm[["TP"]]/(((1+b^2)*cm[["TP"]] + b^2*cm[["FN"]] + cm[["FP"]])))
}

perc_pos_weights <- function(cmby, iter_column = NA) {
  if(is.na(iter_column)){
    cmby[,perc_positive := (TP+FP)/sum(TP+FP)]
  } else{
    cmby[,perc_positive := (TP+FP)/sum(TP+FP), by = iter_column]
  }
  return(cmby$perc_positive)
}


#'
calc_hdi <- function(cm, summary_func = fb_score, hdi_perc = 95, year = 2007, bucket) {
    cm = cm[latlonbucket == bucket]
    sampled_cms <- sample_posterior_cms(cm, n = 2e4)
    statistic_vals <- summary_func(sampled_cms)
    interval <- HDInterval::hdi(statistic_vals, credMass = hdi_perc/100)
    interval_str <- glue(
      "{round(interval[['lower']], 5)} to {round(interval[['upper']],5)}"
    )

}

### workhorse functions to feed to produce_initial_acreage_dist()

# use perc_predicted_uplift() for perc_positive

## a util function to get acreage weights
get_acreage_weights <- function(acres) {
  return(acres/sum(acres))
}

#' f.obj.nl: a function to calculate the convex objective function
#'
#' @param x a candidate set of weights
#' @param init_obj vector: c(weight_on_ppv, weight_on_distance)
#' @param num_regions integer: number of regions
#' @param perc_positive vector: percent of positives by region
#' @param acre_weights vector: percent of total acreages initially assigned by region
#' @param tot_acres double: vector of ratios of regional_acres/total_acres
#'
#' @return scalar - loss value
#' @export
#'
#' @examples
f.obj.nl <- function(x,init_obj, num_regions, perc_positive,acre_weights, tot_acres, spray_dist = F, init_ppv){
  ## calculates conv combinations of wt_1*expected_payment + wt_2*euc.dist(init_acres)
  if(!spray_dist){
    return(sum(sapply(c(1:num_regions), function(t) init_obj[1]*(1-init_ppv[t])*x[t] + min((1/tot_acres), 1)*init_obj[2]*(x[t] - acre_weights[t]*tot_acres)^2)))

  }
   else{
     return(sum(sapply(c(1:num_regions), function(t) perc_positive[t]*init_obj[1]*(1-init_ppv[t])*x[t] + min((1/tot_acres), 1)*perc_positive[t]^2*init_obj[2]*(x[t] - acre_weights[t]*tot_acres)^2)))
   }
}

#' f.obj_grad.nl: a function to calculate the gradient of the convex objective function
#'
#' @param x a candidate set of weights
#' @param init_obj vector: c(weight_on_ppv, weight_on_distance)
#' @param num_regions integer: number of regions
#' @param perc_positive vector: percent of positives by region
#' @param acre_weights vector: percent of total acreages initially assigned by region
#' @param tot_acres double: vector of ratios of regional_acres/total_acres
#'
#' @return scalar - loss value
#' @export
#'
#' @examples
f.obj_grad.nl <- function(x,init_obj, num_regions, perc_positive,acre_weights, tot_acres, spray_dist = F, init_ppv){
  if(!spray_dist){
    return(sapply(c(1:num_regions), function(t) init_obj[1]*(1-init_ppv[t]) + min((1/tot_acres), 1)*2*init_obj[2]*((x[t]) - acre_weights[t]*tot_acres)))

    }
  else{
    return(sapply(c(1:num_regions), function(t) init_obj[1]*(1-init_ppv[t])*perc_positive[t] + min((1/tot_acres), 1)*perc_positive[t]^2*2*init_obj[2]*((x[t]) - acre_weights[t]*tot_acres)))

  }
    }

#' f.con_nl: a function to return the equality constraint of the nl opt problem
#'
f.con_nl <- function(x,init_obj, num_regions, perc_positive,acre_weights,tot_acres = tot_acres, spray_dist = T, init_ppv){
  return(abs(sum(x) - 1))
}

#' f.jac_nl: a function to return the jacobian of the constraints of the nl opt problem
#'
f.jac_nl <- function(x,init_obj, num_regions, perc_positive,acre_weights,tot_acres, spray_dist = T, init_ppv){
  rep(1, num_regions)
}

#' generate_acre_buckets: given a target N by region, split new N into a
#' regional dist. by year
generate_acre_buckets <- function(data, new_acres, reg_list = c("CS Central Plains",
                                                                "CS Eastern Corn Belt",
                                                                "CS IL",
                                                                "CS Midwest",
                                                                "CS MN/WI",
                                                                "CS Northern Plains",
                                                                "CS South & East Coast",
                                                                "CS West")) {
  new_acres = sapply(new_acres, function(x) x)
  names(new_acres) <- reg_list
  data[,regional_annual_acres := .N, by = c('CP_REGION', 'loyo_year')]
  data[,regional_annual_acre_weight := regional_annual_acres/.N, by = c('CP_REGION')]
  data[,acre_bucket := round(new_acres[CP_REGION]*regional_annual_acre_weight)]
  return(unique(data[,c("acre_bucket","regional_annual_acre_weight", "loyo_year", "CP_REGION")]))
}


### Calculate initial acreage shares given balance of ppv/original-acres with nlopt

#' produce_initial_acreage_dist: function to find new acreage following convex
#'                                combination of maximum ppv and euclidean
#'                                distance from orig. solution
#'
#' @param acre_weights vector: vector of initial weights where length(acre_weights) = num_regions.
#'                      should be set s.t. eg for region 1, acre_weight[1]*total_acres = region_acres.
#'                      The ordering of this vector is important - must match other vectors.
#' @param f.obj.nl function: The function that calculates the objective function, given a new vector
#'                  of weights
#' @param f.obj_grad.nl function: Gradient of f.obj.nl wrt. acre weights entries
#' @param f.con_nl function: equality constraints on new acre_weights where they are written in form:
#'                  f(x) - C = 0. Function may return a matrix or single numeric element.
#' @param f.jac_nl function: jacobian of the equality constraints
#' @param init_obj vector: a vector of length 2 where the first entry represents the weight on the
#'                  ppv and the second represents the weight on distance to solution
#' @param num_regions integer: number of regions in sample - 8 in US, 6 in sample pre-2022, 7 in
#'                    sample post 2022, currently slated to deploy to 7 regions.
#' @param perc_positive vector: aligned with acre_weights, percentage of test data in region that
#'                      are predicted positive over total N. IE - (TP + FP)/(TP + FP + FN + TN)
#' @param tot_acre_ratio scalar: what percentage of total acres do you want your solution to match,
#'                        eg. for 80% of initial acres set tot_acre_ratio to .8
#' @param ub string: if 'existing_weights' will force algorithm to find solution where it will at most
#'            choose acre_weights. If anything else, then will not bound values from above.
#'
#' @return nloptr "results" object which is a list containing found solution
#' @export
#'
#' @examples

produce_initial_acreage_dist <- function(acre_weights,
                                         f.obj.nl,
                                         f.obj_grad.nl,
                                         f.con_nl,
                                         f.jac_nl,
                                         init_obj,
                                         num_regions,
                                         perc_positive,
                                         tot_acre_ratio,
                                         init_ppv,
                                         ub = c("existing_weights", "1"),
                                         spray_dist_use) {

    if(ub == "existing_weights"){
      acre_weights = unlist(acre_weights, use.names = T)
      res = nloptr(x0 = acre_weights,
               eval_f = f.obj.nl,
               eval_grad_f = f.obj_grad.nl,
               lb = rep(0, num_regions), ub = acre_weights[c(1:6,8)],
               eval_g_eq = f.con_nl,
               eval_jac_g_eq = f.jac_nl,
               opts = list("algorithm" = "NLOPT_LD_SLSQP",
                           "xtol_rel"=1.0e-34,
                           "print_level" = 2,
                           "check_derivatives" = TRUE),
               init_obj = init_obj,
               num_regions = num_regions,
               perc_positive = perc_positive,
               acre_weights = acre_weights,
               tot_acres = tot_acre_ratio,
               init_ppv = init_ppv,
               spray_dist = spray_dist_use)
    } else{
      res = nloptr(x0 = acre_weights,
                   eval_f = f.obj.nl,
                   eval_grad_f = f.obj_grad.nl,
                   lb = rep(0, num_regions), ub = rep(1,num_regions),
                   eval_g_eq = f.con_nl,
                   eval_jac_g_eq = f.jac_nl,
                   opts = list("algorithm" = "NLOPT_LD_SLSQP",
                               "xtol_rel"=1.0e-34,
                               "print_level" = 2,
                               "check_derivatives" = TRUE),
                   init_obj = init_obj,
                   num_regions = num_regions,
                   perc_positive = perc_positive,
                   acre_weights = acre_weights,
                   tot_acres = min(tot_acre_ratio,1),
                   init_ppv = init_ppv,
                   spray_dist = spray_dist_use)
    }

    ## check that new solution is not better than a simple re-scaling
    if((res$objective > res$eval_f((unlist(acre_weights)))$objective || init_obj[1] == 0)){
      res$solution = acre_weights*tot_acre_ratio
    }

  return(list(new_weights = res$solution, opt_value = res$objective, function_used = res$eval_f))

}



#' Generate data for app, using an initial weighting of existing vs. new
#' strategies and then
#'
#' @param acre_weights
#' @param f.obj.nl
#' @param f.obj_grad.nl
#' @param f.con_nl
#' @param f.jac_nl
#' @param init_obj
#' @param num_regions
#' @param perc_positive
#' @param tot_acre_ratio
#' @param ub
#'
#' @return list; each list element is a d.t. obj corresponding
#'               to metric of interest
#' @export
#'
#' @examples
generate_data_object_shiny_app <-function(.acre_weights,
                                          .f.obj.nl = f.obj.nl,
                                          .f.obj_grad.nl = f.obj_grad.nl,
                                          .f.con_nl = f.con_nl,
                                          .f.jac_nl = f.jac_nl,
                                          .init_obj,
                                          .num_regions,
                                          .perc_positive,
                                          .tot_acre_ratio,
                                          .total_acres,
                                          .ub = "1",
                                          preds,
                                          .init_ppv,
                                          .product_initial_acreage_dist = product_initial_acreage_dist,
                                          itersize = 1000,
                                          spray_dist = T,
                                          update_weights = T, threshold = .55,
                                          endyr = end_year){

  ## generate new confusion matrix based on provided threshold

  preds[,uplift_pred := ifelse(y_probability >= threshold, 1, 0)]
  cmby = generate_cm_by(preds, by_column = c('CP_REGION'))


  ## start by getting new sets of acreage weights, given strategy
  if(update_weights){
    new_acre_weights = produce_initial_acreage_dist(
      acre_weights = .acre_weights,
      f.obj.nl = .f.obj.nl,
      f.obj_grad.nl = .f.obj_grad.nl,
      f.con_nl = .f.con_nl,
      f.jac_nl = .f.jac_nl,
      init_obj = .init_obj,
      num_regions = .num_regions,
      perc_positive = .perc_positive,
      tot_acre_ratio = .tot_acre_ratio,
      ub = .ub,
      init_ppv = .init_ppv,
      spray_dist_use = spray_dist
    )$new_weights
  } else{
    new_acre_weights = .acre_weights
  }
  ## generate vector of acreages across regions
  #new_acres <- sapply(new_acre_weights, function(x) round(x*.total_acres))
  new_acres <- new_acre_weights*.total_acres

  ## given these acres, divide up each region's
  ## assigned acres into annual buckets to replicate
  ## original dataset
  acre_buckets <- unique(generate_acre_buckets(data = preds, new_acres))
  acre_buckets <- acre_buckets[CJ(CP_REGION = unique(acre_buckets$CP_REGION), loyo_year = unique(loyo_year)), on = .(CP_REGION, loyo_year)]
  setnafill(acre_buckets, 'const', fill = 0, cols = c("acre_bucket",
                                                      "regional_annual_acre_weight"))

  ## split out by year and construct a re-weighted ppv value by year, given
  ## the proportion of datapoints in the region found in the given bucket
  yr_lst = lapply(c(2007:endyr),function(yr) acre_buckets[loyo_year == yr])

  annualized_cms = lapply(yr_lst, function(x) {
    size_mat = x[,c('CP_REGION', 'acre_bucket')]
    return(sample_posterior_cms_by_region(cmby, n = itersize, size = size_mat))
  })

  lapply(c(1:length(yr_lst)), function(x) {
    annualized_cms[[x]][,loyo_year := unique(yr_lst[[x]]$loyo_year)]
  })

  out_table = rbindlist(annualized_cms)

  out1 = out_table[, .(TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN)),
                       by = .(iter, loyo_year)]
  out1$ppv = ppv(out1)
  out1 = out1[,.(ppv = mean(ppv)), by = .(iter)]

  out2 = out_table[, .(TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN)),
                   by = .(iter)]
  out2[,total_expected_payout := FP*6]

  out3 = out_table[, .(TP = sum(TP), FP = sum(FP), TN = sum(TN), FN = sum(FN)),
                   by = .(iter, CP_REGION, loyo_year)]
  out3$ppv = ppv(out3)
  out3[,.(ppv = mean(ppv), FP = sum(FP), TP = sum(TP), FN = sum(FN), TN = sum(TN)),
       by = .(iter, CP_REGION)]
  out3[,total_expected_payout := FP*6]

  ## sankey interactive plot data - need two data.tables, one
  ## for node labels, one for flows

  used_regions = (program_acre_weights %>% names())[new_acre_weights > 0.0005]
  num_used_regions = length(used_regions)
  print(used_regions)

  nodes = data.table(names = c("program acres",
                                used_regions,
                                "correctly enrolled acres", "paid-out acres",
                                "avoided payouts", "lost trial successes"))

  flows = data.table(source = c(rep(0, num_used_regions),
                               rep(c(1:num_used_regions), 4)))
  flows[,
           target := c(c(1:num_used_regions),
                       rep(num_used_regions + 1,num_used_regions),
                       rep(num_used_regions + 2, num_used_regions),
                       rep(num_used_regions + 3, num_used_regions),
                       rep(num_used_regions + 4, num_used_regions))]
  print(flows)
  flows_value = out3[order(match(CP_REGION, used_regions))][,
       .(TN = sum(TN), FP = sum(FP),
         FN = sum(FN), TP = sum(TP)),
       by = c('iter', 'CP_REGION')][,
                                    .(TN = round(median(TN)), FP = round(median(FP)),
                                     FN = round(median(FN)), TP = round(median(TP))), by = c('CP_REGION')]
  flows_value[,num := (TN + TP + FN + FP)]

  exact_acres = new_acre_weights[new_acre_weights > .00005]*.total_acres
  print(exact_acres)
  print(flows_value)
  flows[,value := c(exact_acres,
                    flows_value$TP,
                    flows_value$FP,
                    flows_value$TN,
                    flows_value$FN)]

  print(out3$ppv)

  return(list(default = out1,
              total_expenditure = out2,
              by_region = out3,
              nodes = nodes,
              flows = flows,
              value = 0.4322041,
              new_weights = new_acre_weights,
              new_acres = new_acres)) ## value is our 'equivalent' expected ppv
                                  ## under reweighting and averaging by year
                                  ## given current targeted acres


  }

