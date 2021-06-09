
library(future)
library(future.apply)
library(data.table)
library(lubridate)
library(ggforce)
library(magick)
library(tictoc)
library(dplyr)
library(tidyverse)

source('./SHAPlot.R')



cluster_lab = cl_lab = "roa_Median_-_peers_Volatility"
data_type = d_type = "original"
model = mod = "baseline"
mod_set_lab = "no_control"

{
  # load("C:/Users/Alessandro Bitetto/Downloads/UniPV/BCC-default/ML_fitting_input.RData")
  # source('./Help.R')
  # control_variables = c('Dummy_industry', 'Industry' , 'Dimensione_Impresa',  'segmento_CRIF', 'Regione_Macro') # todo: rimetti
  # target_var = "FLAG_Default"
  # additional_var = "PD"   # variable to be added to baseline model to check added value
  # fixed_variables = c("PD")   # variables to be always kept in the model, i.e. no shrinkage is applied
  # n_fold = 5   # todo: rimetti
  # algo_set = c("Elastic-net", "Random_Forest", "MARS", "SVM-RBF")    # see fit_model_with_cv() for allowed values
  # prob_thresh_cv = "best"    # probability threshold for cross-validation (in tuning)
  # prob_thresh_full = "best"    # probability threshold for full dataset
  # tuning_crit = "F1_test"  # "F1" or "AUC" or "Precision" or "Recall" or "Accuracy" for "_test" or "_train"
  # tuning_crit_full = "F1_train"   # same of tuning_crit but applied to full dataset model when using prob_thresh_full = "best"
  # tuning_crit_minimize = F    # if TRUE tuning_crit is minimized
  # tuning_crit_minimize_full = F    # if TRUE tuning_crit is minimized
  # balance_abi_ndg_fold = F    # if TRUE balance distribution of abi_ndg between train and test when y=1 in cross-validation
  # final_oversample_perc = 100     # percentage of oversampling (SMOTE)
  # 
  # 
  # # define perimeter
  # df_main = df_final_small %>%
  #   filter(year != 2014) %>%
  #   mutate(abi_ndg = paste0(abi, "_", ndg)) %>%
  #   mutate(Industry = substr(Industry, 1, 1),
  #          segmento_CRIF = gsub(" ", "_", segmento_CRIF),
  #          Regione_Macro = gsub("-", "_", Regione_Macro)) %>%
  #   select(-abi, -ndg) %>%
  #   select(abi_ndg, everything()) %>%
  #   as.data.frame()
  # 
  # # scale df_main, name target_var "y" and save scaling parameters for model coefficients rescaling
  # scaled_regressor_main = df_main %>%
  #   select(starts_with("BILA")) %>%
  #   mutate_all(~scale(., center=T, scale=T))
  # df_main_scaling = c()
  # for (var in colnames(scaled_regressor_main)){
  #   tt = scaled_regressor_main %>% pull(all_of(var)) %>% attributes()
  #   df_main_scaling = df_main_scaling %>%
  #     bind_rows(data.frame(variable = var, center = tt$`scaled:center`, scale = tt$`scaled:scale`, stringsAsFactors = F))
  #   scaled_regressor_main = scaled_regressor_main %>%
  #     mutate_at(vars(all_of(var)), function(x) { attributes(x) <- NULL; x })
  #   rm(tt)
  # }
  # 
  # if (model_setting == ""){
  #   contr_var = NULL
  # } else {
  #   contr_var = model_setting
  # }
  # mod_set_lab = model_setting_lab = ifelse(model_setting == "", "no_control", paste0("control_", model_setting))
  # 
  # # add dummy
  # if (model_setting != ""){
  #   scaled_regressor = scaled_regressor_main %>%
  #     bind_cols(dummy_cols(
  #       df_main %>% select(all_of(model_setting)),
  #       select_columns = model_setting,
  #       remove_first_dummy = T,
  #       remove_selected_columns = TRUE)
  #     )
  #   dummy_regressor = setdiff(colnames(scaled_regressor), colnames(scaled_regressor_main))
  # } else {
  #   scaled_regressor = scaled_regressor_main
  #   dummy_regressor = c()
  # }
  # main_regressor = colnames(scaled_regressor)
  # 
  # # set up final dataset
  # df_main_work = df_main %>%
  #   select(abi_ndg, year, all_of(target_var)) %>%
  #   rename(y = !!sym(target_var)) %>%
  #   bind_cols(scaled_regressor) %>%
  #   `rownames<-`(paste0("row_", 1:nrow(.)))
  # if (sum(is.na(df_main_work)) > 0){cat('\n###### missing in df_main_work')}
  # 
  # # prepare input of function that balance distribution of abi_ndg between train and test when y=1 balance_abi_ndg_class1()
  # abi_ndg_row_reference_class1 = df_main_work %>%
  #   filter(y == 1) %>%
  #   select(abi_ndg) %>%
  #   rownames_to_column("row_ind") %>%
  #   mutate(row_ind = gsub("row_", "", row_ind) %>% as.numeric()) %>%
  #   group_by(abi_ndg) %>%
  #   summarise(row_ind = paste0(sort(row_ind), collapse = ","))
  # 
  # abi_ndg_row_index = df_main_work %>%
  #   select(abi_ndg) %>%
  #   rownames_to_column("row_ind")
  # 
  # strat_fold = create_stratified_fold(df_main %>%
  #                                       rename(TARGET = !!sym(target_var)) %>%
  #                                       select(all_of(setdiff(c("TARGET", contr_var), ""))) %>%
  #                                       mutate_if(is.character, as.factor), inn_cross_val_fold = 1, out_cross_val_fold = n_fold,
  #                                     out_stratify_columns = setdiff(c("TARGET", contr_var), ""), out_stratify_target = T)
  # cv_ind = strat_fold$final_blocking %>%
  #   rename(fold = block,
  #          ind = index)
  # if (nrow(cv_ind) != nrow(df_main_work)){cat('\n##### observation missing in cv_ind')}
  # cv_ind_check = cv_ind %>% group_by(ind) %>% summarise(count = n())
  # if (nrow(cv_ind_check) != nrow(df_main_work) | max(cv_ind_check$count) != 1){cat('\n##### repeated observation in cv_ind')}
  # 
  # # define dataset
  # if (model == "baseline"){
  #   df_work = df_main_work %>%
  #     left_join(list_DD_CRIF_data[[cluster_lab]], by = c("abi_ndg", "year")) %>%
  #     select(y, all_of(main_regressor)) %>%
  #     `rownames<-`(rownames(df_main_work))
  #   
  # } else if (model == "additional_var"){
  #   df_work = df_main_work %>%
  #     left_join(list_DD_CRIF_data[[cluster_lab]], by = c("abi_ndg", "year")) %>%
  #     select(y, all_of(c(main_regressor, additional_var))) %>%
  #     `rownames<-`(rownames(df_main_work))
  # }
}

log_fitting = read.csv('./Distance_to_Default/Results/02_Fitted_models_performance.csv', sep=";", stringsAsFactors=FALSE)


# df_predictors = df_work %>% select(-y)

df_work = readRDS(paste0('./Distance_to_Default/Checkpoints/ML_model/04_feature_importance_input_',
                         mod_set_lab, "_", cl_lab, "_", mod, "_", d_type, '.rds'))
model_setting_block = log_fitting %>%
  filter(data_type == d_type) %>%
  filter(cluster_lab == cl_lab) %>%
  filter(model_setting_lab == mod_set_lab) %>%
  filter(model == mod) %>%
  select(model_setting_lab, cluster_lab, data_type, model, algo_type, rds)
method = "Permutation"
performance_metric = "F1"
n_workers = 5
verbose = 1
n_repetitions = 5
compare = "difference"
seed = 66
sample_size = 10
n_batch = 5

df_work = df_work %>% group_by(y) %>% filter(row_number() <= 300) %>% ungroup()    # test for PFI

aa = evaluate_feature_importance(df_work, model_setting_block, method,
                                       performance_metric = "F1", n_repetitions = 5, compare = "difference",
                                       verbose = 1, n_workers = 5, seed = 66)

aa1 = evaluate_feature_importance(df_work, model_setting_block, method = "SHAP",
                                 performance_metric = "F1", n_repetitions = 5, compare = "difference",
                                 sample_size = sample_size, n_batch = n_batch,
                                 verbose = 1, n_workers = 5, seed = 66)







# n_row = 100
# X = data.frame(medv = rnorm(n_row), medv2 = rnorm(n_row), var1 = runif(n_row)*2, var2 = runif(n_row), var3 = rnorm(n_row, 3, 10))
# library("randomForest")
# rf = randomForest(medv ~ ., data = X %>% select(-medv2), ntree = 50)
# lm_multi = lm(cbind(medv, medv2) ~ ., data=X)
# ff = function(x){x %>% mutate(dd = apply(., 1, function(x) x[1]/sum(x))) %>% pull(dd)}
# ff1 = function(x){predict(rf, newdata = x%>% as.list() %>% as.data.frame()) %>% as.list() %>% as.data.frame()}
# trained_model_prediction_function = list(mmod1 = ff, Random_Forest = ff1, pp_fun = ff2)


# regression - single with 2 models
library("randomForest")
dataSample = mtcars %>% select(-mpg)
rf_single = randomForest(mpg ~ ., data = mtcars, ntree = 50)
lm_single = lm(mpg ~ ., data=mtcars)
pred_fun_rf = function(x){predict(rf_single, newdata = x)}
pred_fun_lm = function(x){predict(lm_single, newdata = x)}
trained_model_prediction_function = list(RandFor = pred_fun_rf,
                                         LinMod = pred_fun_lm)

# regression - multiple
dataSample = mtcars %>% select(-mpg, -disp)
lm_multi = lm(cbind(mpg, disp) ~ ., data=mtcars)
pred_fun = function(x){predict(lm_multi, newdata = x)}
trained_model_prediction_function = list(LinMod = pred_fun)


# classification - single with probabilities
library("randomForest")
dataSample = mtcars %>% select(-vs)
rf = randomForest(vs ~ ., data=mtcars %>% mutate(vs = as.factor(vs)), ntree = 50)
pred_fun = function(x){predict(rf, newdata = x, type = "prob")[, 2]}
trained_model_prediction_function = list(RandFor = pred_fun)


# classification - multiple with probabilities
library("randomForest")
dataSample = iris %>% select(-Species)
rf = randomForest(Species ~ ., data=iris, ntree = 50)
pred_fun = function(x){predict(rf, newdata = x, type = "prob")}
trained_model_prediction_function = list(RandFor = pred_fun)


# classification - single/multiple with class (single predicted class)
library("randomForest")
dataSample = iris %>% select(-Species)
rf = randomForest(Species ~ ., data=iris, ntree = 50)
pred_fun = function(x){predict(rf, newdata = x, type = "class") %>% as.character()}
trained_model_prediction_function = list(RandFor = pred_fun)



sample_size = 30
seed = 22
obs_index_to_evaluate = c(1:nrow(dataSample))
obs_index_to_sample = c(1:nrow(dataSample))
obs_index_subset = data.frame(obs_index = obs_index_to_evaluate, subset = sample(c("c0", "c1"), length(obs_index_to_evaluate), replace = T))
n_workers = 5
n_batch=1
verbose = 1
adjust_shapley = T



# evaluate Shapley values
evaluate_SHAP = function(dataSample, sample_size = 100, trained_model_prediction_function = NULL, obs_index_to_evaluate = NULL,
                         obs_index_to_sample = NULL, obs_index_subset = NULL, adjust_shapley = FALSE, n_batch = 1, n_workers = 5, verbose = 1, seed = 66){
  
  # evaluate local SHAP values for single observations, global (signed) features effects, global SHAP features importance and input for plot_SHAP_summary().
  # https://christophm.github.io/interpretable-ml-book/shapley.html
  # https://christophm.github.io/interpretable-ml-book/shap.html
  
  # dataSample: data.frame of predictors ONLY.
  # sample_size: sample size to generate instances (coalitions) with shuffled features. The higher the more accurate the explanations become.
  # trained_model_prediction_function: named list of function(predictors) -> prediction (vector of values for regression,
  #                                   probabilities of "1" for binary classification).
  #                                   E.g. function(x){predict(rf, newdata = x)}. List names will be used as output label for each model.
  # obs_index_to_evaluate: integer vector - row index of observations to evaluate SHAP value. If NULL all observations will be used.
  # obs_index_to_sample: integer vector of index of rows to be used when sampling all coalitions. obs_index_to_evaluate must be a partition of obs_index_to_sample.
  #                      If NULL all observations will be used. It can be used to do stratified sampling and/or mitigate imbalance in the dataset.
  # obs_index_subset: data.frame of "obs_index", "subset". If not NULL contains subset for each obs_index_to_evaluate. All outputs are then evaluated
  #                 for each subset and all observations.
  # adjust_shapley: if TRUE adjust the sum of the estimated Shapley values to satisfy the local accuracy property, i.e. to equal the difference between the 
  #               model's prediction for of each observation and the average prediction over all the dataset.
  # n_batch: number of batch to split the evaluation. May speed up evaluation and save memory.
  # n_workers: number of workers for parallel calculation. Try not to exceed 30-40. If NULL, no parallelization will be used.
  # verbose: 1 to display calculation time, 0 for silent.
  # seed: seed for reproducibility
  
  # todo: descrizione dell'algoritmo. Description (estrai un sommario) e/o Details. Metti citazione anche alla pagina web degli Shapley
  
  # Evaluates Shapley values Phi according to Strumbelj and Kononenko (2014) procedure. For multiple outputs prediction, Shapley values are evaluated for each
  # prediction output. For classification task, Shapley values are evaluated by returning the median predicted class after including the j-th feature in the coalition,
  # excluding all coalitions that returned the same class predicted with the original features. If there are no class changes for the given observation and feature, NA
  # is returned.
  # Global feature effects is returned, i.e. (signed) average of Shapley values over all observations as well as SHAP feature importance, i.e. average of absolute
  # values of Shapley values over all observations. For classification task, Global feature effects returns, for each feature, the percentage of class changes:
  # "" means no changes in predicted class occurred (i.e. the feature is not important), else "class_O-class_C" reports the percentage of sampled coalitions
  # that led to a change in class from class_O (the class predicted with the original features) to class_C (the class predicted with the coalition). SHAP feature
  # importance sums all contributions that led to a class change, so Global feature effects will be aggregated to "" and "class_change" levels. If obs_index_subset
  # is not NULL, Global feature effects and SHAP feature importance will be reported for all observations together and for each single classes provided
  # in obs_index_subset alone.
  
  
  
  # Štrumbelj, Erik, and Igor Kononenko. "Explaining prediction models and individual predictions with feature contributions." Knowledge and information systems 41.3 (2014): 647-665.
  
  
  
  # Output:
  #   list of:
  #     - local_SHAP: complete list of SHAP values for each observation
  #                     data.frame with "feature", "class_lab", "phi", "phi_std", "feature_value", "obs_index", "model_name", "predicted_val", "predicted_val_avg"
  #                     "phi" and "phi_std" are the average and st.dev of SHAP values over all shuffled coalitions
  #                     "feature" is feature name and "feature_value" is the corresponding value from the original instance (obs_index reference)
  #                     "class_lab" is the label of model output. For single output is just "Prediction", otherwise contains names of target values.
  #                     "predicted_val" and "predicted_val_avg" are predicted value of single observation and columns average, respectively
  #     - global_features_effect: average of all local (single observation) SHAP values over all observation.
  #                              It is a signed features effect on predicted value.
  #                              data.frame with "model_name", "feature", "class_lab", "phi"
  #     - SHAP_feat_imp: SHAP feature importance. It's the average abs(SHAP values). Shows the magnitude of features effect on predicted value.
  #                     data.frame with "model_name", "feature", "class_lab", "phi"
  #     - summary_plot_data: input for plot_SHAP_summary()
  #     - type: "SHAP". Used in plot_feat_imp().
  # If obs_index_subset != NULL additional nested list is returned for each subset and all observations with global_features_effect and SHAP_feat_imp,
  # local_SHAP and summary_plot_data have an additional column "subset"
  
  # code adapted from https://github.com/christophM/iml/blob/master/R/Shapley.R
  
  
  # check for numeric predictors only
  invalid_col_type = dataSample %>% select_if(negate(is.numeric)) %>% colnames()
  if (length(invalid_col_type) > 0){
    oo = capture.output(print(sapply(dataSample %>% select(all_of(invalid_col_type)), class)))
    stop(paste0("Only numeric predictors supported:\n", paste0(oo, collapse = "\n")))
  }
  
  # set up data and NULL default
  dataSample = dataSample %>% setDT()
  n_features = ncol(dataSample)
  feature_names = colnames(dataSample)
  samp_len = length(obs_index_to_sample)
  if (is.null(obs_index_to_evaluate)){obs_index_to_evaluate = c(1:nrow(dataSample))}
  if (is.null(obs_index_to_sample)){obs_index_to_sample = c(1:nrow(dataSample))}
  if (length(intersect(obs_index_to_evaluate, obs_index_to_sample)) != length(obs_index_to_evaluate)){
    stop('"obs_index_to_evaluate" must be a partition of "obs_index_to_sample"')
  }
  
  # get names and type of prediction (e.g. name of classes for multi-class or target for multiple output regression and "numeric" or "class" prediction)
  class_mapping = c()
  prediction_type = c()
  for (tr_model in names(trained_model_prediction_function)){
    
    trained_model = trained_model_prediction_function[[tr_model]]
    sample_pred = trained_model(dataSample[1,] %>% as.data.frame() %>% select(all_of(colnames(dataSample))))
    prediction_type = c(prediction_type, is.numeric(sample_pred))
    sample_names = colnames(sample_pred)
    if (is.null(sample_names)){sample_names = "Prediction"}   # when prediction is 1-dim and without given name (i.e. output is not a named array)
    class_mapping = class_mapping %>%
      bind_rows(data.frame(model_name = tr_model, class_lab = paste0("Prediction_", 1:length(sample_names)), mapped_class = sample_names, stringsAsFactors = F))
  } # tr_model
  if (sum(prediction_type) == length(prediction_type)){
    prediction_type = "numeric"
  } else if (sum(prediction_type) == 0){
    prediction_type = "class"
  } else {
    stop('Prediction type is different for all models')
  }
  if (prediction_type == "class"){adjust_shapley = FALSE}

  generate_sample_index = function(obs_index){
    # generate indices of samples to be used as coalitions trying not to use duplicates. If sample_size > length(obs_index_to_sample)
    # duplicates are introduced.
    # Returns indices vector of length sample_size
    
    set.seed(seed + obs_index)
    out = sample(obs_index_to_sample, min(c(sample_size, samp_len)), replace = FALSE)
    
    if (sample_size > samp_len){
      out = c(out, sample(obs_index_to_sample, sample_size - samp_len, replace = TRUE))
    }
    
    return(out)
  }
  
  generate_coalitions = function(obs_index){
    # generate coalitions with shuffled features
    # final sample will have ncol=n_features
    #                        nrow=sample_size*n_features*2  - first half rows are data with features x_+j,
    #                                                         second half is x_-j of algo definition at linked page
    # column with obs_index is added as well
    
    # select instance
    x.interest = dataSample[obs_index,] %>% as.data.frame()
    
    n_row = nrow(dataSample)
    runs <- lapply(1:sample_size, function(m) {
      
      # randomly order features
      set.seed(seed + obs_index*m)
      new.feature.order <- sample(1:n_features)
      
      # randomly choose sample instance from dataSample to shuffle features order
      sample.instance.shuffled <- dataSample[list_generated_sample[[as.character(obs_index)]][m],
                                             new.feature.order,
                                             with = FALSE
      ]
      # shuffle interest instance with same features order
      x.interest.shuffled <- x.interest[, new.feature.order]
      
      # create new instances (with and without) for each feature
      featurewise <- lapply(1:n_features, function(k) {
        k.at.index <- which(new.feature.order == k)
        # replace sample.instance.shuffled at features > k  (X_+j of algo definition at linked page "k" is "j")
        instance.with.k <- x.interest.shuffled
        if (k.at.index < ncol(x.interest)) {
          instance.with.k[, (k.at.index + 1):ncol(instance.with.k)] <-
            sample.instance.shuffled[, (k.at.index + 1):ncol(instance.with.k), with = FALSE]
        }
        # replace sample.instance.shuffled at features >= k  (X_-j of algo definition)
        instance.without.k <- instance.with.k
        instance.without.k[, k.at.index] <- sample.instance.shuffled[,k.at.index,with = FALSE]
        cbind(instance.with.k[, feature_names],instance.without.k[, feature_names])
      })
      data.table::rbindlist(featurewise)
    })
    runs <- data.table::rbindlist(runs)
    dat.with.k <- data.frame(runs[, 1:(ncol(runs) / 2)])
    dat.without.k <- data.frame(runs[, (ncol(runs) / 2 + 1):ncol(runs)])
    
    return(rbind(dat.with.k, dat.without.k) %>% mutate(obs_index = obs_index))
  }
  
  Mode <- function(x, excl = "") {
    ux <- unique(x[x != excl])
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  generate_shapley = function(list_index, trained_model_name, prediction_type){
    
    # list_index: index of the list that contains all generate_coalitions() and predictions "list_predicted_data". obs_index will be extracted inside so to avoid
    #             index mismatch when list_generated_coal doesn't keep the order.
    # trained_model_name: model name from trained_model_prediction_function
    # prediction_type: "numeric" or "class" to identify prediction type
    #
    # Output: data.frame with "feature", "class_lab", "phi", "phi_std", "feature_value", "obs_index", "model_name"
    #         "phi" and "phi_std" are the average and st.dev of SHAP values over all shuffled coalitions
    #         "feature" is feature name and "feature_value" is the corresponding value from the original instance (obs_index reference)
    #         "class_lab" is the label of model output. For single output is just "Prediction_1", otherwise is "Prediction_xx" and will be mapped back to original by class_mapping
    
    # select sampled data and predictions from generated list
    data_predicted = list_predicted_data[[list_index]]
    obs_index = data_predicted$obs_index %>% unique()
    var_names = data_predicted %>% select(-obs_index, -starts_with("Prediction_")) %>% colnames()
    x.interest = dataSample[obs_index,] %>% as.data.frame()
    data_predicted = data_predicted %>% select(starts_with("Prediction_"))
    
    # evaluate Phi
    # split prediction in yhat_x_+j and yhat_-j
    y.hat.with.k <- data_predicted[1:(nrow(data_predicted) / 2), , drop = FALSE]
    y.hat.without.k <- data_predicted[(nrow(data_predicted) / 2 + 1):nrow(data_predicted), , drop = FALSE]
    if (prediction_type == "numeric"){
      y.hat.diff <- y.hat.with.k - y.hat.without.k   # these are Phi_j^m  -> will be averaged after (m=1,...,sample_size)
      cnames <- colnames(y.hat.diff)
      y.hat.diff <- cbind(
        data.table(feature = rep(var_names, times = sample_size)),
        y.hat.diff
      )
      # average over all sampled observation (for each feature) - also include std
      y.hat.diff <- data.table::melt(y.hat.diff, variable.name = "class_lab", value.name = "value", measure.vars = cnames) # reshape in long format [feature, class_lab, value]
      y.hat.diff <- y.hat.diff[, list("phi" = mean(value), "phi_std" = sd(value)), by = c("feature", "class_lab")]
      # y.hat.diff$class_lab <- NULL
    } else if (prediction_type == "class"){
      # if predicted class changed, take predicted class with "k", else ""
      y.hat.diff = data.frame(Prediction_1 = ifelse(y.hat.with.k$Prediction_1 != y.hat.without.k$Prediction_1, y.hat.with.k$Prediction_1, ""), stringsAsFactors = F)
      cnames <- colnames(y.hat.diff)
      y.hat.diff <- cbind(
        data.table(feature = rep(var_names, times = sample_size)),
        y.hat.diff
      )
      # mode over all sampled observation (for each feature) - also include std
      y.hat.diff <- data.table::melt(y.hat.diff, variable.name = "class_lab", value.name = "value", measure.vars = cnames) # reshape in long format [feature, class_lab, value]
      y.hat.diff <- y.hat.diff[, list("phi" = Mode(value, excl = ""), "phi_std" = ""), by = c("feature", "class_lab")]
    }
    y.hat.diff = y.hat.diff %>%
      left_join(data.frame(unlist(x.interest[1, ]), stringsAsFactors = F) %>%
                  setNames("feature_value") %>%
                  rownames_to_column("feature"), by = "feature") %>%
      mutate(obs_index = obs_index,
             model_name = trained_model_name)
    
    return(y.hat.diff)
  }
  
  
  # suppress messages
  if (verbose == 0){sink(tempfile());on.exit(sink())}
  
  # generate samples indices for coalitions
  list_generated_sample = lapply(obs_index_to_evaluate, generate_sample_index)
  names(list_generated_sample) = as.character(obs_index_to_evaluate)
  
  
  #### loop for each batch of obs_index_to_evaluate
  
  if (!is.null(n_workers)){
    options(future.globals.maxSize = 8000 * 1024^2)
    plan(multisession, workers = n_workers)
  }
  list_split = split(obs_index_to_evaluate, sort(obs_index_to_evaluate %% n_batch))
  start_time = Sys.time()
  split_time_val = local_SHAP = c()
  cat('\nStart time:', as.character(Sys.time()), '\n')
  for (split_name in names(list_split)){
    
    # split_name = names(list_split)[1]   # todo:rimuovi
    
    # check average batch time
    split_time = Sys.time()
    if (split_name != names(list_split)[1]){
      avg_time = seconds_to_period(mean(split_time_val, na.rm = T))
      avg_time_label = paste0('- batch avg time: ', lubridate::hour(avg_time), 'h:', lubridate::minute(avg_time), 'm:', round(lubridate::second(avg_time)))
    } else {
      avg_time_label = ''
    }
    cat('Generating coalitions and SHAP values for batch', paste0(as.numeric(split_name)+1, '/', length(list_split)),
        ' | last timestamp:', as.character(Sys.time()), avg_time_label, end = '')
    
    # generate all sample to be used for all trained models
    tic()
    if (!is.null(n_workers)){
      list_generated_coal <- future_lapply(list_split[[split_name]], generate_coalitions, future.packages = c("data.table"), future.seed = NULL)
    } else {
      list_generated_coal <- lapply(list_split[[split_name]], generate_coalitions)
    }
    gen_time = capture.output(toc()) %>% strsplit(" ") %>% .[[1]] %>% .[1] %>% as.numeric() %>% seconds_to_period()
    cat(paste0('(generate: ', lubridate::hour(gen_time), 'h:', lubridate::minute(gen_time), 'm:', round(lubridate::second(gen_time))), end = '')
    
    # generate SHAP values for all observations and all trained models
    tic()
    for (tr_model in names(trained_model_prediction_function)){
      
      # tr_model = names(trained_model_prediction_function)[1]   # todo: rimuovi
      
      
      # select model from trained_model_prediction_function
      trained_model = trained_model_prediction_function[[tr_model]]
      
      # predict trained model on list_generated_coal
      list_predicted_data = data.table::rbindlist(list_generated_coal)
      list_predicted_data = list_predicted_data %>%
        cbind(trained_model(list_predicted_data %>% as.data.frame() %>% select(all_of(colnames(dataSample)))) %>%
                data.frame(stringsAsFactors = F) %>%
                setNames(paste0("Prediction_", 1:ncol(.))))
      list_predicted_data = split(list_predicted_data , f = list_predicted_data$obs_index)
      
      # evaluate SHAP values
      if (!is.null(n_workers)){
        list_generated_SHAP <- future_lapply(1:length(list_predicted_data), generate_shapley, trained_model_name = tr_model, prediction_type = prediction_type, future.seed = NULL)
      } else {
        list_generated_SHAP <- lapply(1:length(list_predicted_data), generate_shapley, trained_model_name = tr_model, prediction_type = prediction_type)
      }

      # add observation predicted values and average observation predicted value
      predicted_obs = trained_model(dataSample[obs_index_to_evaluate, ] %>% as.data.frame()) %>%
        data.frame(stringsAsFactors = F) %>%
        setNames(paste0("Prediction_", 1:ncol(.))) %>%
        mutate(obs_index = obs_index_to_evaluate) %>%
        gather('class_lab', 'predicted_val', -c(obs_index))
      
      predicted_obs_avg = trained_model(dataSample %>% as.data.frame()) %>%
        data.frame(stringsAsFactors = F) %>%
        summarise_all(function(x) if (prediction_type == "numeric"){mean(x)} else {Mode(x)}) %>%
        setNames(paste0("Prediction_", 1:ncol(.))) %>%
        t() %>%
        data.frame(stringsAsFactors = F) %>%
        setNames("predicted_val_avg") %>%
        rownames_to_column("class_lab")
      
      # append results
      local_SHAP = local_SHAP %>%
        bind_rows(data.table::rbindlist(list_generated_SHAP) %>%
                    left_join(predicted_obs, by = c("class_lab", "obs_index")) %>%
                    left_join(predicted_obs_avg, by = "class_lab"))
    } # tr_model
    pred_time = capture.output(toc()) %>% strsplit(" ") %>% .[[1]] %>% .[1] %>% as.numeric() %>% seconds_to_period()
    cat(paste0(' - predict: ', lubridate::hour(pred_time), 'h:', lubridate::minute(pred_time), 'm:', round(lubridate::second(pred_time)), ')'), end = '\r')
    
    split_time_val = c(split_time_val, difftime(Sys.time(), split_time, units='secs'))
  } # split_name
  if (!is.null(n_workers)){future:::ClusterRegistry("stop")}
  tot_diff=seconds_to_period(difftime(Sys.time(), start_time, units='secs'))
  cat('\nTotal elapsed time', paste0(lubridate::hour(tot_diff), 'h:', lubridate::minute(tot_diff), 'm:', round(lubridate::second(tot_diff))), ' ', as.character(Sys.time()), '\n')
  if (n_features * length(obs_index_to_evaluate) * length(trained_model_prediction_function) * uniqueN(class_mapping$class_lab) != nrow(local_SHAP)){
    warning("Expected number of rows in generated local SHAP values doesn't match")
  }
  
  # map original class name
  local_SHAP = local_SHAP %>%
    as.data.frame() %>%
    arrange(model_name, obs_index, feature) %>%
    left_join(class_mapping, by = c("class_lab", "model_name")) %>%
    select(-class_lab) %>%
    rename(class_lab = mapped_class) %>%
    select(feature, class_lab, everything())
  
  # adjust Shapley
  if (adjust_shapley){
    local_SHAP = local_SHAP %>%
      group_by(model_name, obs_index, class_lab) %>%
      mutate(err = predicted_val - sum(phi) - predicted_val_avg,
             w = phi_std^2 / max(phi_std^2) * 1e6,
             adj = err * (w - (w * sum(w) / (1 + sum(w)))),
             phi_adj = phi + adj) %>%
      select(-phi, -err, -w, -adj) %>%
      rename(phi = phi_adj) %>%
      select(feature, class_lab, phi, everything()) %>%
      as.data.frame()
  }
  
  
  #### loop for all subsets (if any) to create global_features_effect and SHAP_feat_imp
  
  subset_set = data.frame(set = 'All observations', subset = '', stringsAsFactors = F)
  if (!is.null(obs_index_subset)){
    obs_index_subset = obs_index_subset %>% mutate(subset = as.character(subset))  # remove factors
    subset_set = subset_set %>%
      bind_rows(data.frame(set = paste0("subset ", obs_index_subset$subset %>% unique() %>% sort()),
                           subset = obs_index_subset$subset %>% unique() %>% sort(), stringsAsFactors = F))
    local_SHAP = local_SHAP %>%
      left_join(obs_index_subset, by = "obs_index")
  }
  list_output = list()
  for (subset_i in 1:nrow(subset_set)){
    
    if (subset_set$set[subset_i] != 'All observations'){
      tt_local_SHAP = local_SHAP %>%
        filter(subset == subset_set$subset[subset_i])
    } else {
      tt_local_SHAP = local_SHAP
    }
    
    # evaluate average of SHAP values. Proxy for GLOBAL signed impact on predictions
    if (prediction_type == "numeric"){
      global_features_effect = tt_local_SHAP %>%
        group_by(model_name, feature, class_lab) %>%
        summarize(phi = mean(phi), .groups = "drop") %>%
        arrange(model_name, class_lab, desc(abs(phi)))
    } else if (prediction_type == "class"){
      # evaluate the percentage of class changes for each feature (e.g. "" means no class change, "setosa->versicolor" means original "setosa" changed to "versicolor")
      global_features_effect = tt_local_SHAP %>%
        mutate(phi = ifelse(is.na(phi), predicted_val, phi)) %>%
        mutate(phi = ifelse(phi != predicted_val, paste0(predicted_val, "->", phi), "")) %>%
        select(-class_lab) %>%
        rename(class_lab = phi) %>%  # class_lab will always have only "Prediction" value
        group_by(model_name, feature, class_lab) %>%
        summarize(phi = n(), .groups = "drop") %>%
        group_by(model_name, feature) %>%
        mutate(phi = phi / sum(phi)) %>%
        ungroup() %>%
        arrange(model_name, feature, desc(abs(phi)))
    }
    
    # evaluate SHAP feature importance. Average of SHAP values absolute value
    if (prediction_type == "numeric"){
      SHAP_feat_imp = tt_local_SHAP %>%
        group_by(model_name, feature, class_lab) %>%
        summarize(phi = mean(abs(phi)), .groups = "drop") %>%
        arrange(model_name, class_lab, desc(phi))
    } else if (prediction_type == "class"){
      # sum all phi corresponding to class change
      SHAP_feat_imp = global_features_effect %>%
        mutate(class_lab = ifelse(class_lab != "", "class_change", "")) %>%
        group_by(model_name, feature, class_lab) %>%
        summarize(phi = sum(phi), .groups = "drop") %>%
        arrange(model_name, feature, desc(abs(phi)))
    }
    
    list_output[[subset_set$set[subset_i]]] = list(global_features_effect = global_features_effect,
                                                 SHAP_feat_imp = SHAP_feat_imp)
    
  } # subset_i
  features_level = list_output$`All observations`$SHAP_feat_imp
  if (length(list_output) == 1){list_output = list_output[[1]]}   # remove "All observations" level if it is the only available
  
  # evaluate input for plot_SHAP_summary()
  scaled_features = dataSample %>%    # scale input features in [0,1]
    as.data.frame() %>%
    mutate_all(~scale_range(., a=0, b=1)) %>%
    mutate(obs_index = 1:n()) %>%
    gather(key = "feature", value = "value_color", -obs_index)
  
  summary_plot_data = c()
  for (tr_model in names(trained_model_prediction_function)){
    
    summary_plot_data = summary_plot_data %>%
      bind_rows(
        local_SHAP %>%
          filter(model_name == tr_model) %>%
          left_join(scaled_features, by = c("feature", "obs_index")) %>%
          mutate(feature = factor(feature, levels = features_level %>% filter(model_name == tr_model) %>% pull(feature) %>% unique()))
      )
  } # tr_model
  
  list_output = c(list(type = "SHAP",
                       local_SHAP = local_SHAP,
                       summary_plot_data = summary_plot_data),
                  list_output)
  
  return(list_output)
}


vv = evaluate_SHAP(dataSample, sample_size = sample_size, trained_model_prediction_function = trained_model_prediction_function, obs_index_to_evaluate = obs_index_to_evaluate,
                   obs_index_to_sample = obs_index_to_sample, obs_index_subset = obs_index_subset, adjust_shapley = adjust_shapley, n_batch = n_batch,
                   verbose = verbose, n_workers = n_workers, seed = seed)

# saveRDS(vv, "ss_class.rds")
# saveRDS(vv, "ss_num_multi.rds")
# saveRDS(vv, "ss_num.rds")

vv = readRDS("ss_class.rds")
vv = readRDS("ss_num_multi.rds")
vv = readRDS("ss_num.rds")




list_input = vv
sina_method = "counts"
sina_bins = 20
sina_size = 2
sina_alpha = 0.7
plot_model_set = NULL
plot_subset_set = NULL
SHAP_axis_lower_limit = 0
magnify_text = 1.4
color_range = c("red", "blue")
plot_width = 12
plot_height = 8
plot_res = 300
squeeze_rows = T
save_path = paste0('./aa_', toString(squeeze_rows))

# Plot SHAP summary
plot_SHAP_summary = function(list_input, sina_method = "counts", sina_bins = 20, sina_size = 2, sina_alpha = 0.7, plot_model_set = NULL, plot_subset_set = NULL,
                             SHAP_axis_lower_limit = 0, magnify_text = 1.4, squeeze_rows = F, color_range = c("red", "blue"),
                             save_path = '', plot_width = 12, plot_height = 12, plot_res = 300){
  
  # Plot Shapley summary. Returns plots for all trained model splitting results by 'All observations' and 'subset ...' subsets if any.
  # list_input: output of evaluate_SHAP()
  # plot_model_set: if not NULL plot only provided trained model results.
  # plot_subset_set: if not NULL and if 'All observations', 'subset ...' available, plot only provided subset.
  # sina_ : options of geom_sina (sina plot)
  # SHAP_axis_lower_limit: value to add to minimum value of x-axis (SHAP). Also moves SHAP's magnitude column. For classification task, provide integer number only.
  # magnify_text: magnify all text font in plot
  # squeeze_rows: if TRUE reduce space between rows (features)
  # color_range: legend color of Low and High value of features
  # save_path: if not '', save all plots. Only modelName_subset.png will be added. "_" at the end of save_path is automatically added if not provided.
  # plot_width, plot_height, plot_res: width, height (inches) and resolution of saved plot
  
  # check last character of save_path
  if (substr(save_path, nchar(save_path), nchar(save_path)) != "_" & save_path != ''){save_path = paste0(save_path, "_")}
  
  # check prediction type (numeric or class)
  prediction_type = ifelse(is.numeric(list_input$summary_plot_data$phi), "numeric", "class")
  
  # check if subset plots are available
  summary_plot_data = list_input$summary_plot_data
  if ("All observations" %in% names(list_input)){
    subset_set = setdiff(names(list_input), c("local_SHAP", "summary_plot_data", "type"))
  } else {
    subset_set = c('No subset')
  }
  if (is.null(plot_subset_set)){
    plot_subset_set = subset_set
  } else if (!is.null(plot_subset_set) & subset_set == 'No subset'){
    plot_subset_set = 'No subset'
  }
  
  # get all available classes for classification task and add factor values for vertical lines - classes are ordered by alphabetical order
  if (prediction_type == "class"){
    all_classes = unique(c(unique(summary_plot_data$phi), unique(summary_plot_data$predicted_val))) %>% setdiff(., NA)
    class_change_list = expand.grid(all_classes, all_classes, stringsAsFactors = F) %>%
      mutate(change = paste0(Var1, "->", Var2)) %>%
      arrange(Var1, Var2) %>%
      mutate(order = rep(1:length(all_classes), each = length(all_classes))) %>%
      group_by(Var1) %>%
      mutate(column_title = ifelse(row_number() == 1, paste0("Predicted value\nfrom ", Var1), NA)) %>%
      ungroup() %>%
      bind_rows(data.frame(change = paste0("xxx_vline_", 1:(length(all_classes) - 1)), order = 1:(length(all_classes) - 1)+0.5, stringsAsFactors = F)) %>%
      arrange(order)
  }
  
  plot_list = list()
  for (subset_i in plot_subset_set){
    
    # extract data
    if (prediction_type == "numeric"){
      
      if (subset_i == 'No subset'){
        data_plot = summary_plot_data %>%
          left_join(list_input$SHAP_feat_imp %>% rename(abs.phi = phi), by = c("feature", "class_lab", "model_name"))
      } else if (subset_i == 'All observations'){
        data_plot = summary_plot_data %>%
          left_join(list_input[[subset_i]]$SHAP_feat_imp %>% rename(abs.phi = phi), by = c("feature", "class_lab", "model_name"))
      } else {
        data_plot = summary_plot_data %>%
          filter(subset == subset_i %>% gsub("subset ", "", .)) %>%
          left_join(list_input[[subset_i]]$SHAP_feat_imp %>% rename(abs.phi = phi), by = c("feature", "class_lab", "model_name"))
      }
      
    } else if (prediction_type == "class"){
      
      if (subset_i == 'No subset'){
        data_plot = summary_plot_data %>%
          left_join(list_input$SHAP_feat_imp %>% filter(class_lab == "class_change") %>% rename(abs.phi = phi) %>% select(-class_lab), by = c("feature", "model_name"))
      } else if (subset_i == 'All observations'){
        data_plot = summary_plot_data %>%
          left_join(list_input[[subset_i]]$SHAP_feat_imp %>% filter(class_lab == "class_change") %>% rename(abs.phi = phi) %>% select(-class_lab), by = c("feature", "model_name"))
      } else {
        data_plot = summary_plot_data %>%
          filter(subset == subset_i %>% gsub("subset ", "", .)) %>%
          left_join(list_input[[subset_i]]$SHAP_feat_imp %>% filter(class_lab == "class_change") %>% rename(abs.phi = phi) %>% select(-class_lab), by = c("feature", "model_name"))
      }
    }
    
    # loop models
    if (is.null(plot_model_set)){plot_model_set_work = unique(data_plot$model_name)}
    for (tr_model in plot_model_set_work){
      
      data_plot_tt = data_plot %>%
        filter(model_name == tr_model)
      if (prediction_type == "numeric"){
        y_lim = range(data_plot_tt$phi)
      } else if (prediction_type == "class"){
        data_plot_tt = data_plot_tt %>%
          mutate(phi = ifelse(is.na(phi), predicted_val, phi))
      }
      num_output_class = uniqueN(data_plot_tt$class_lab)
      main_title = paste0("SHAP summary plot for ",
                          ifelse(subset_i %in% c('No subset', 'All observations'), 'all observations', subset_i))
      
      # loop for multiple target (if many) and stack in single row
      row_fig = list()
      for (c_lab in unique(data_plot_tt$class_lab)){
        
        data_plot_tt_cl = data_plot_tt %>%
          filter(class_lab == c_lab)
        plot_title = ifelse(num_output_class == 1, main_title, c_lab)
        
        # create feature order to be plotted on y axis, adding space between rows
        additional_top_rows = 3   # adds fake features to increase y-axis (features) limits
        feature_order = data_plot_tt_cl %>%
          select(feature, abs.phi) %>%
          unique() %>%
          mutate(abs.phi.perc = abs.phi / sum(abs.phi),
                 lab = paste0(' ', round(abs.phi, 3), ' (', round(abs.phi.perc * 100, 2), '%)')) %>%
          arrange(desc(abs.phi)) %>%
          mutate(feature_ref = c(1:n())*2) %>%
          bind_rows(data.frame(feature = paste0("xxx_", 1:nrow(.))) %>%
                      mutate(feature_ref = c(1:n())*2-1),
                    data.frame(feature = paste0("xxx_", c(-additional_top_rows:0))) %>%
                      mutate(feature_ref = c(-additional_top_rows:0))) %>%
          arrange(feature_ref)
        if (squeeze_rows){
          feature_order = feature_order %>%
            bind_rows(data.frame(feature = "xxx_bottom", feature_ref = 2000))
        }
        
        # reduce space between rows (if number of features is small)
        if (squeeze_rows){
          feature_order = feature_order %>%
            filter((feature_ref > 0 & feature_ref %% 2 == 0) | (feature_ref < -2))
        }
        
        if (prediction_type == "numeric"){
          
          # ticks adjustment
          SHAP_limits = c(y_lim[1] * (1 - sign(y_lim[1]) * 1.5) + SHAP_axis_lower_limit, # lower x-axis value (added to make room for SHAP magnitude)
                          y_lim[2] * (1 + sign(y_lim[2]) * 0.1))
          SHAP_breaks = c(0, seq(y_lim[1], y_lim[2], length.out = 6)) %>% unique() %>% sort()  # ticks from data
          ticks_spacing = diff(SHAP_breaks)
          # if tick before/after 0 is too close, shift it a bit left by 50%
          tick_before_0_spacing = ticks_spacing[which(SHAP_breaks == 0) - 1]
          if (tick_before_0_spacing < max(ticks_spacing) * 0.3){SHAP_breaks[which(SHAP_breaks == 0) - 1] = SHAP_breaks[which(SHAP_breaks == 0) - 1] * 1.5}
          tick_before_1_spacing = ticks_spacing[which(SHAP_breaks == 0)]
          if (tick_before_1_spacing < max(ticks_spacing) * 0.3){SHAP_breaks[which(SHAP_breaks == 0) + 1] = SHAP_breaks[which(SHAP_breaks == 0) + 1] * 3}
          SHAP_labels = as.character(SHAP_breaks %>% round(2))
          
          # append fake points to add space between features
          add_fake = paste0("xxx_", min(feature_order$feature_ref):uniqueN(data_plot_tt_cl$feature))
          if (squeeze_rows){add_fake = c(add_fake, "xxx_bottom")}
          data_plot_tt_cl = data_plot_tt_cl %>%
            bind_rows(data.frame(feature = add_fake) %>%
                        mutate(phi = min(data_plot_tt_cl$phi)))
          out_plot = suppressWarnings(
            ggplot(data_plot_tt_cl %>%
                              group_by(feature) %>%
                              mutate(feature = factor(feature, levels = rev(feature_order$feature))),
                            aes(x = feature, y = phi, color = value_color)) +
            # scale_y_continuous(expand = c(0, 0), limits = c(SHAP_position_work * SHAP_axis_lower_limit, y_lim[2])) +    # SHAP    expand = (expansion between ticks, expansion outside limits)
            scale_y_continuous(expand = c(0, 0), limits = SHAP_limits, breaks = SHAP_breaks, labels = SHAP_labels) +    # SHAP    expand = (expansion between ticks, expansion outside limits)
            scale_x_discrete(expand=c(0.02, ifelse(squeeze_rows, 0, 0.5)), labels = feature_order %>%  # 0.11
                               mutate(feature = ifelse(grepl("xxx_", feature), "", feature)) %>%
                               pull(feature) %>%
                               rev(), limits = feature_order %>%  # 0.11
                               pull(feature) %>%
                               rev(),
                             breaks = feature_order %>%  # 0.11
                               pull(feature) %>%
                               rev()) +     # features
            labs(title = plot_title,
                 x = "Feature", y = "SHAP value (impact on model predictions)", color = "Feature value") +
            coord_flip() + 
            geom_sina(size = sina_size, bins = sina_bins, method = sina_method, alpha = sina_alpha) +
            geom_hline(yintercept = 0) +
            geom_text(data = feature_order %>%
                        filter(!grepl("xxx_", feature)), aes(x = feature, y=-Inf, label = lab),
                      size = 4 * magnify_text, hjust = 0, color = "black") +
            scale_color_gradient(low=color_range[1], high=color_range[2], 
                                 breaks=c(0,1), labels=c("Low","High"), na.value="white") +
            theme_bw() +
            theme(axis.line.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_text(size = 13 * magnify_text, vjust = 0.5),   # features
                  axis.text.x = element_text(size = 16),   # SHAP
                  axis.title = element_text(size = 20),
                  plot.title = element_text(size=ifelse(num_output_class == 1, 30, 25)),
                  plot.subtitle = element_text(size=25),
                  legend.title=element_text(size=18),
                  legend.text=element_text(size=15),
                  legend.position="bottom") +
            guides(colour = guide_colourbar(title.position="left", title.vjust = 1))
          )
          
          # add SHAP magnitude on the left
          if (squeeze_rows){
            out_plot = suppressWarnings(
              out_plot +
                annotate("text", x = feature_order$feature[1], y = -Inf, label = "SHAP abs\nmagnitude", size = 5 * magnify_text, hjust = 0, fontface = "bold")
            )
          } else {
            out_plot = out_plot +
              annotate("text", x = feature_order$feature[1], y = -Inf, label = " SHAP abs", size = 5 * magnify_text, hjust = 0, fontface = "bold") +
              annotate("text", x = feature_order$feature[3], y = -Inf, label = " magnitude", size = 5 * magnify_text, hjust = 0, fontface = "bold")
          }
          
        } else if (prediction_type == "class"){
          
          SHAP_magnitude_factors = paste0("xxx_SHAP_magn_", c(1:(3 + SHAP_axis_lower_limit))) # add space for SHAP magnitude on the left
          # append fake points to add space between features
          data_plot_tt_cl = data_plot_tt_cl %>%
            mutate(phi_orig = phi) %>%
            mutate(phi = paste0(predicted_val, "->", phi))  %>%
            # bind_rows(data.frame(feature = c(paste0("xxx_", min(feature_order$feature_ref):uniqueN(data_plot_tt_cl$feature)), "xxx_bottom")) %>%
                        bind_rows(data.frame(feature = paste0("xxx_", min(feature_order$feature_ref):uniqueN(data_plot_tt_cl$feature))) %>%
                        mutate(phi_orig = class_change_list$Var1[1],
                               phi = class_change_list$change[1]),
                      data.frame(feature = data_plot_tt_cl$feature[1], phi = SHAP_magnitude_factors)) %>%  
            mutate(phi = factor(phi, levels = c(SHAP_magnitude_factors, class_change_list$change)))
          
          # add missing class changes (if any)
          missing_changes = setdiff(class_change_list$change,data_plot_tt_cl$phi %>% unique() %>% as.character())
          if (length(missing_changes) > 0){
            data_plot_tt_cl = data_plot_tt_cl %>%
              bind_rows(data.frame(feature = data_plot_tt_cl$feature[1], phi = missing_changes))
          }
          
          # define SHAP axis labels, by adding vertical spacing for each "->class" (so to avoid vertical rotation and overlapping)
          SHAP_breaks = data_plot_tt_cl$phi %>% levels
          SHAP_labels = SHAP_breaks %>% gsub(".*->", "to ", .) # gsub("->", "->\n", .)
          SHAP_labels[grepl("xxx_vline_|SHAP_magn_", SHAP_labels)] = ""
          for (s in 2:length(SHAP_labels)){
            if (substr(SHAP_labels[s], 1, 3) == "to " & substr(SHAP_labels[s-1], 1, 3) == "to "){SHAP_labels[s] = paste0("\n", SHAP_labels[s])}
          }
          
          out_plot = suppressWarnings(
            ggplot(data_plot_tt_cl %>%
                              group_by(feature) %>%
                              mutate(feature = factor(feature, levels = rev(feature_order$feature))),
                            aes(x = feature, y = phi, color = value_color)) +
            scale_y_discrete(expand = c(0, 0.5), breaks = SHAP_breaks[-c(1:3)], labels = SHAP_labels[-c(1:3)]) +    # SHAP    expand = (expansion between ticks, expansion outside limits)
            scale_x_discrete(expand=c(0.02, ifelse(squeeze_rows, 0, 0.5)), labels = feature_order %>%  # 0.11
                               mutate(feature = ifelse(grepl("xxx_", feature), "", feature)) %>%
                               pull(feature) %>%
                               rev(), limits = feature_order %>%  # 0.11
                               pull(feature) %>%
                               rev(),
                             breaks = feature_order %>%  # 0.11
                               pull(feature) %>%
                               rev()) +     # features
            labs(title = plot_title,
                 x = "Feature", y = "SHAP value (impact on class change)", color = "Feature value") +
            coord_flip() + 
            geom_sina(size = sina_size, bins = sina_bins, method = sina_method, alpha = sina_alpha) +
            geom_hline(yintercept = class_change_list %>% filter(str_detect(change, "xxx_vline_")) %>% pull(change)) +
            geom_text(data = feature_order %>%
                        filter(!grepl("xxx_", feature)), aes(x = feature, y="xxx_SHAP_magn_1", label = lab),
                      size = 4 * magnify_text, hjust = 0, color = "black") +
            annotate("text", x = feature_order$feature[ifelse(squeeze_rows, 1, 2)], y = class_change_list %>% filter(!is.na(column_title)) %>% pull(change),
                     label = class_change_list %>% filter(!is.na(column_title)) %>% pull(column_title), size = 4 * magnify_text, hjust = 0) +   # prediction "from" title
            scale_color_gradient(low=color_range[1], high=color_range[2], 
                                 breaks=c(0,1), labels=c("Low","High"), na.value="white") +
            theme_bw() +
            theme(axis.line.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_text(size = 13 * magnify_text, vjust = 0.5),   # features
                  axis.text.x = element_text(size = 16, angle = 0),   # SHAP
                  axis.title = element_text(size = 20),
                  plot.title = element_text(size=ifelse(num_output_class == 1, 30, 25)),
                  plot.subtitle = element_text(size=25),
                  legend.title=element_text(size=18),
                  legend.text=element_text(size=15),
                  legend.position="bottom") +
            guides(colour = guide_colourbar(title.position="left", title.vjust = 1))
          )
          
          # add SHAP magnitude on the left
          if (squeeze_rows){
            out_plot = suppressWarnings(
              out_plot +
                annotate("text", x = feature_order$feature[1], y = "xxx_SHAP_magn_1", label = "SHAP abs\nmagnitude", size = 5 * magnify_text, hjust = 0, fontface = "bold")
            )
          } else {
            out_plot = out_plot +
              annotate("text", x = feature_order$feature[1], y = "xxx_SHAP_magn_1", label = " SHAP abs", size = 5 * magnify_text, hjust = 0, fontface = "bold") +
              annotate("text", x = feature_order$feature[3], y = "xxx_SHAP_magn_1", label = " magnitude", size = 5 * magnify_text, hjust = 0, fontface = "bold")
          }
        } # prediction_type
        
        
        # todo: rimuovi
        # {
        # png("./Distance_to_Default/Results/000p.png",
        #     width = plot_width, height = plot_height, units = 'in', res=300)
        # plot(out_plot)
        # dev.off()
        #   }
        
        if (num_output_class > 1){
          out_plot_magick = image_graph(width = plot_width * plot_res, height = plot_height * plot_res, res = plot_res)
          suppressWarnings(plot(out_plot))
          dev.off()
          row_fig[[toString(c_lab)]] = out_plot_magick
        } else {
          row_fig[[toString(c_lab)]] = out_plot
        }
      } # c_lab
      
      # assemble row
      if (num_output_class > 1){
        
        eval(parse(text=paste0('final_plot = image_append(c(', paste0('row_fig[[', 1:length(row_fig), ']]', collapse = ','), '), stack = F)')))
        
        # add main title and subtitle
        title_lab = image_graph(res = 100, width = image_info(final_plot)$width, height = 200, clip = F)
        plot(
          ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank() + xlim(0, 1) + ylim(0, 5) +
            annotate(geom = "text", x = 0, y = 3, label = main_title, cex = 40, hjust = 0, vjust = 0.5) +
            theme_bw() +
            theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                   axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
                   plot.margin=unit(c(0,0.4,0,0.4),"cm"))
        )
        dev.off()
        final_plot = image_append(c(title_lab, final_plot), stack = T)  # used to save plot on disk
        final_plot_output = image_ggplot(final_plot %>% image_flip())  # convert to ggplot file to export results
        
      } else {
        final_plot = row_fig[[1]]  # used to save plot on disk
        final_plot_output = final_plot  # export results (already ggplot file)
      }
      
      plot_list[[subset_i]][[tr_model]] = final_plot_output
      
      # save plot
      if (save_path != ''){
        png(paste0(save_path, tr_model, ifelse(subset_i == "No subset", "", paste0("_", gsub(" ", "_", subset_i))), ".png"),
            width = plot_width * num_output_class, height = plot_height, units = 'in', res=plot_res)
        par(mar=c(0,0,0,0))
        par(oma=c(0,0,0,0))
        suppressWarnings(plot(final_plot))
        dev.off()
      }
      
    } # tr_model
  } # subset_i
  if (subset_i == "No subset"){plot_list = plot_list[[1]]}
  
  return(plot_list)
}



oo = plot_SHAP_summary(list_input, sina_method = sina_method, sina_bins = sina_bins, sina_size = sina_size, sina_alpha = sina_alpha, plot_model_set = plot_model_set,
                  plot_subset_set = plot_subset_set, SHAP_axis_lower_limit = SHAP_axis_lower_limit, magnify_text = magnify_text, squeeze_rows = squeeze_rows,
                  color_range = color_range, save_path = save_path, plot_width = plot_width, plot_height = plot_height, plot_res = plot_res)



vv = readRDS("ss_class.rds")
vv = readRDS("ss_num_multi.rds")
vv = readRDS("ss_num.rds")

list_input = vv
normalize = F
color_pos = "blue"
color_neg = "red"
plot_model_set = NULL
plot_subset_set = NULL
plot_shape = "single"
cmap = NULL
magnify_text = 1.5
bar_label_size = 6
save_path = paste0('./aa_')
plot_width = 12
plot_height = 12
plot_res = 300

# Plot feature importance
plot_feat_imp = function(list_input, normalize = F, color_pos = "blue", color_neg = "red", plot_model_set = NULL, plot_subset_set = NULL,
                         plot_shape = "single", cmap = NULL, magnify_text = 1, bar_label_size = 6,
                         save_path = '', plot_width = 12, plot_height = 12, plot_res = 300){
  
  # Plot feature importance. Returns plots for all trained model and 'All observations' and 'subset ...' if any. If input is from evaluate_SHAP() plots
  # global (signed) features effects and global SHAP features importance. If input is from evaluate_Perm_Feat_Imp() simply plots feature importance.
  # All input elements in list_input must contain data.frame with "model_name", "feature" "class_lab" and "phi"/"importance"
  
  # list_input: output of evaluate_SHAP() or evaluate_Perm_Feat_Imp()
  # normalize: if TRUE normalize importance in 0-100%
  # color_pos, color_neg: color for positive and negative (if any) bars
  # plot_model_set: if not NULL plot only provided trained model results.
  # plot_subset_set: if not NULL and if 'All observations', 'subset ...' available, plot only provided subset.
  # plot_shape: if "single" and multi-class/multi-output, all classes/outputs bar plot are reported separately. If "stacked" or "grouped", all bars referring
  #             to the same feature will be stacked or put aside, respectively.
  # cmap: array of strings with color names for stacked/grouped bars. If NULL some default colors are used.
  # magnify_text: magnify all text font in plot
  # bar_label_size: size of bar labels
  # save_path: if not '', save all plots. Only modelName_subset.png will be added. "_" at the end of save_path is automatically added if not provided.
  # plot_width, plot_height, plot_res: width, height (inches) and resolution of saved plot
  
  # todo: aggiungi le barre di errore nel caso della PFI e capisci cosa viene fuori se la PFI è negativa
  
  # check prediction type (numeric or class)
  prediction_type = ifelse(is.numeric(list_input$summary_plot_data$phi), "numeric", "class")
  
  # check for plot_shape
  num_output_class = uniqueN(list_input$summary_plot_data$class_lab)   # todo: bisogna includere i casi per il PFI
  if (prediction_type == "class"){   # single output and class prediction. SHAP always with "single", Global Effects with plot_shape
    plot_options = list(SHAP = list(SHAP_feat_imp = "single",
                                    global_features_effect = plot_shape),
                        PFI = list(feature_importance = plot_shape)
    )
  }
  if (prediction_type == "numeric" & num_output_class == 1){   # single output and numeric prediction. SHAP and Global Effects always with "single"
    plot_options = list(SHAP = list(SHAP_feat_imp = "single",
                                    global_features_effect = "single"),
                        PFI = list(feature_importance = plot_shape)
    )
    if (plot_shape != "single"){
      warning("For single numeric output only plot_shape = \"single\" available. Forcing to \"single\".")
    }
  }
  if (prediction_type == "numeric" & num_output_class > 1){   # multiple output and numeric prediction. SHAP and Global Effects with plot_shape
    plot_options = list(SHAP = list(SHAP_feat_imp = plot_shape,
                                    global_features_effect = ifelse(plot_shape == "grouped", "stacked", plot_shape)),
                                    PFI = list(feature_importance = plot_shape)
    )
  }
  
  # check colormap
  if (is.null(cmap)){cmap = c('dodgerblue3', 'firebrick2', 'chartreuse3', 'cadetblue2', 'gold1', 'darkorange', 'slategray4', 'violet', 'yellow1')}
  
  # check last character of save_path
  if (substr(save_path, nchar(save_path), nchar(save_path)) != "_" & save_path != ''){save_path = paste0(save_path, "_")}
  
  # check if subset plots are available and select importance column name
  if (list_input$type == "SHAP"){
    importance_column = "phi"
    normalize = F
  } else if (list_input$type == "PFI"){
    importance_column = "importance"
  }
  if ("All observations" %in% names(list_input)){
    subset_set = setdiff(names(list_input), c("local_SHAP", "summary_plot_data", "type"))
  } else {
    subset_set = c('No subset')
  }
  if (is.null(plot_subset_set)){
    plot_subset_set = subset_set
  } else if (!is.null(plot_subset_set) & subset_set == 'No subset'){
    plot_subset_set = 'No subset'
  }
  
  plot_list = list()
  plot_loop = plot_options[[list_input$type]]
  for (subset_i in plot_subset_set){
    
    for (plot_type in names(plot_loop)){  # "global_features_effect", "SHAP_feat_imp", "feature_importance"
      
      # set importance axis label
      normalize_work = normalize
      
      if (plot_type == "global_features_effect"){
        if (prediction_type == "numeric"){
          imp_axis_label = "Average signed SHAP value\n(impact on model predictions)"
          plot_title = "Average signed SHAP"
        } else if (prediction_type == "class"){
          imp_axis_label = "Percentage of each class change\n(impact on class change)"
          plot_title = "Percentage of each class changes"
        }
        
      } else if (plot_type == "SHAP_feat_imp"){
        if (prediction_type == "numeric"){
          imp_axis_label = "Average absolute SHAP value\n(impact on model predictions)"
          plot_title = "Average absolute SHAP"
        } else if (prediction_type == "class"){
          imp_axis_label = "Percentage of all classes change\n(impact on class change)"
          plot_title = "Percentage of all classes changes"
        }
        
      } else if (plot_type == "Permutation_feat_imp"){
        if (prediction_type == "numeric"){
          imp_axis_label = "Feature importance"
          if (normalize_work){imp_axis_label = paste0(imp_axis_label, " (normalized)")}
          plot_title = "Permutation Feature Importance"
        } else if (prediction_type == "class"){
          # todo: finisci per permutation feature importance, se c'è da fare qualcosa
        }
      }
      
      # extract data to plot and set up 
      if (subset_i == 'No subset'){
        data_plot = list_input[[plot_type]]
      } else {
        data_plot = list_input[[subset_i]][[plot_type]]
      }
      
      if (prediction_type == "class"){
        if (plot_type == "global_features_effect"){
          data_plot = data_plot %>%
            filter(class_lab != "") %>%
            arrange(model_name, feature, desc(phi))
          normalize_work = FALSE
        } else if (plot_type == "SHAP_feat_imp"){
          data_plot = data_plot %>%
            filter(class_lab == "class_change") %>%
            arrange(model_name, class_lab, desc(phi))
          normalize_work = FALSE
        } else if (plot_type == "Permutation_feat_imp"){
          # todo: finisci per permutation feature importance, se c'è da fare qualcosa
        }
      }
      
      # loop models
      if (is.null(plot_model_set)){plot_model_set_work = unique(data_plot$model_name)}
      for (tr_model in plot_model_set_work){
        
        data_plot_tt = data_plot %>%
          rename(importance = !!sym(importance_column)) %>%
          filter(model_name == tr_model) %>%
          mutate(value_color = ifelse(importance >= 0, "pos", "neg"),
                 importance = signif(importance, 2)) %>%
          rowwise() %>%
          mutate(max_digits = importance  %>% as.character() %>% strsplit("\\.") %>% .[[1]] %>% .[2] %>% nchar()) %>%
          mutate(importance_lab = importance) %>%
          as.data.frame()
        
        # evaluate space for text number on bars
        max_digits = max(data_plot_tt$max_digits, na.rm = T)
        max_span = max(abs(data_plot_tt$importance)) * (1 + max_digits / 10)  # space for text number on top of bars
        
        if (prediction_type == "class"){
          data_plot_tt = data_plot_tt %>%
            mutate(importance_lab = paste0(round(importance * 100, 2), "%"))
        }
        
        num_output_class = uniqueN(data_plot_tt$class_lab)
        main_title = paste0(plot_title, " for ",
                            ifelse(subset_i %in% c('No subset', 'All observations'), 'all observations', subset_i))
        
        #### loop for multiple target (if many) and stack in single row for plot_shape == "single"
        
        if (plot_loop[[plot_type]] == "single"){
          row_fig = list()
          for (c_lab in unique(data_plot_tt$class_lab)){
            
            data_plot_tt_cl = data_plot_tt %>%
              filter(class_lab == c_lab)
            plot_title_work = ifelse(num_output_class == 1, main_title, c_lab)
            
            if (normalize_work){
              data_plot_tt_cl = data_plot_tt_cl %>%
                mutate(importance = round(importance / sum(abs(importance)) * 100, 1))
              max_span = max(abs(data_plot_tt_cl$importance)) * 1.3
            }
            y_lim = c(min(c(min(data_plot_tt_cl$importance) - max_span, 0)), max(data_plot_tt_cl$importance) + max_span)
            if (min(data_plot_tt_cl$importance) >= 0){y_lim[1] = 0}
            
            out_plot = ggplot(data_plot_tt_cl %>%
                                mutate(feature = factor(feature, levels = rev(data_plot_tt_cl$feature))),
                              aes(x = feature, y = importance)) +
              geom_bar(stat = "identity", aes(fill = value_color), width=0.9, position = position_dodge(width=0.5)) +
              scale_fill_manual(values = c("pos" = color_pos, "neg" = color_neg)) +
              labs(title = plot_title_work, x = "Feature", y = imp_axis_label) +
              scale_y_continuous(limits = y_lim) +
              geom_hline(yintercept = 0) +
              coord_flip() +
              theme_bw() +
              theme(axis.line.y = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text.y = element_text(size = 13 * magnify_text, vjust = 0.5),   # features
                    axis.text.x = element_blank(),   # SHAP
                    axis.title = element_text(size = 20 * magnify_text),
                    plot.title = element_text(size=30),
                    plot.subtitle = element_text(size=25),
                    legend.title=element_text(size=18 * magnify_text),
                    legend.text=element_text(size=15 * magnify_text),
                    legend.position="none")
            if (normalize_work){
              out_plot = out_plot +
                geom_text(aes(label = ifelse(importance == 0, "", paste0(" ", importance, "%")),
                              vjust = 0.5, hjust = ifelse(importance >= 0, 0, 1)), size = bar_label_size * magnify_text)
            } else {
              out_plot = out_plot +
                geom_text(aes(label = ifelse(importance == 0, "", ifelse(importance > 0, paste0(" ", importance_lab), paste0(importance_lab, " "))),
                              vjust = 0.5, hjust = ifelse(importance >= 0, 0, 1)), size = bar_label_size * magnify_text)
            }
            
            if (num_output_class > 1){
              out_plot_magick = image_graph(width = plot_width * plot_res, height = plot_height * plot_res, res = plot_res)
              suppressWarnings(plot(out_plot))
              dev.off()
              row_fig[[toString(c_lab)]] = out_plot_magick
            } else {
              row_fig[[toString(c_lab)]] = out_plot
            }
          } # c_lab
          
          # assemble row
          if (num_output_class > 1){
            
            eval(parse(text=paste0('final_plot = image_append(c(', paste0('row_fig[[', 1:length(row_fig), ']]', collapse = ','), '), stack = F)')))
            
            # add main title and subtitle
            title_lab = image_graph(res = 100, width = image_info(final_plot)$width, height = 200, clip = F)
            plot(
              ggplot(mtcars, aes(x = wt, y = mpg)) + geom_blank() + xlim(0, 1) + ylim(0, 5) +
                annotate(geom = "text", x = 0, y = 3, label = main_title, cex = 40, hjust = 0, vjust = 0.5) +
                theme_bw() +
                theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(),
                       axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
                       plot.margin=unit(c(0,0.4,0,0.4),"cm"))
            )
            dev.off()
            final_plot = image_append(c(title_lab, final_plot), stack = T)  # used to save plot on disk
            final_plot_output = image_ggplot(final_plot %>% image_flip())  # convert to ggplot file to export results
            
          } else {
            final_plot = row_fig[[1]]  # used to save plot on disk
            final_plot_output = final_plot  # export results (already ggplot file)
          }
          
        } # plot_shape == "single"
        
        
        #### plot_shape %in% c("stacked", "grouped")
        if (plot_loop[[plot_type]] %in% c("stacked", "grouped")){
          
          if (normalize_work){
            data_plot_tt = data_plot_tt %>%
              mutate(importance = round(importance / sum(abs(importance)) * 100, 1))
          }
          
          # get feature order summing by feature
          feature_order = data_plot_tt %>%
            group_by(feature) %>%
            summarise(total_importance = sum(importance)) %>%
            arrange(desc(total_importance))
          
          # get all available class_lab and add missing (if any for some features)
          if (plot_loop[[plot_type]] == "grouped"){
            data_plot_tt = data_plot_tt %>%
              complete(feature, class_lab)
            y_lim = range(data_plot_tt$importance)
          } else {
            y_lim = data_plot_tt %>% group_by(feature, value_color) %>% summarize(ss = sum(importance), .groups = "drop") %>% pull(ss) %>% range()
          }
          
          # add factor levels
          data_plot_tt = data_plot_tt %>%
            mutate(feature = factor(feature, levels = rev(feature_order$feature)),
                   class_lab = factor(class_lab),
                   vertical_space = FALSE)
          
          # add vertical space to text label so to avoid overlapping
          if (plot_loop[[plot_type]] == "stacked"){
            class_lab_order = levels(data_plot_tt$class_lab)
            
            for (ff in unique(data_plot_tt$feature)){
              ind = which(data_plot_tt$feature == ff)
              order_ind = match(class_lab_order, data_plot_tt$class_lab[ind] %>% as.character())
              matched_ordered_ind = ind[order_ind]
              matched_ordered_ind = matched_ordered_ind[!is.na(matched_ordered_ind)]
              # split ordering in negative/positive values
              pos_ind = which(data_plot_tt$value_color[matched_ordered_ind] == "pos")
              neg_ind = which(data_plot_tt$value_color[matched_ordered_ind] == "neg")
              matched_ordered_ind = c(matched_ordered_ind[neg_ind], rev(matched_ordered_ind[pos_ind]))
              # add \n to even indices
              replace_ind = matched_ordered_ind[lapply(1:length(matched_ordered_ind), "%%", 2) == 0]
              data_plot_tt$vertical_space[replace_ind] = TRUE
            }
            
          }
          
          # set bar label
          if (normalize_work){
            data_plot_tt = data_plot_tt %>%
              mutate(label = ifelse(importance == 0, "", paste0(" ", importance, "%"))) %>%
              mutate(label = ifelse(vertical_space, paste0("\n", label), label))
          } else {
            data_plot_tt = data_plot_tt %>%
              mutate(label = ifelse(importance == 0, "", ifelse(importance > 0, paste0(" ", importance_lab), paste0(importance_lab, " ")))) %>%
              mutate(label = ifelse(vertical_space, paste0("\n", label), label))
          }
          
          out_plot = ggplot(data_plot_tt,
                            aes(x = feature, y = importance)) +
            geom_bar(stat = "identity", aes(fill = class_lab), width=0.9, position = ifelse(plot_loop[[plot_type]] == "stacked", "stack", "dodge")) +
            scale_fill_manual(values = cmap) +
            labs(title = main_title, x = "Feature", y = imp_axis_label, fill = "") +
            scale_y_continuous(expand = c(0, 0.05), limits = c(min(c(y_lim[1],0)), y_lim[2])) +
            geom_hline(yintercept = 0) +
            coord_flip() +
            theme_bw() +
            theme(axis.line.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.y = element_text(size = 13 * magnify_text, vjust = 0.5),   # features
                  axis.text.x = element_blank(),   # SHAP
                  axis.title = element_text(size = 20 * magnify_text),
                  plot.title = element_text(size=30),
                  plot.subtitle = element_text(size=25),
                  legend.title=element_text(size=18 * magnify_text),
                  legend.text=element_text(size=15 * magnify_text),
                  legend.position="bottom") +
            guides(fill=guide_legend(nrow=ifelse(uniqueN(data_plot_tt$class_lab) >= 4, 2, 1),byrow=TRUE))
          # add bar label
          if (plot_loop[[plot_type]] == "grouped"){
            out_plot = suppressWarnings(out_plot +
                                          geom_text(aes(label = label, fill = class_lab, vjust = 0.5, hjust = ifelse(importance >= 0, 0, 1)),
                                                    size = bar_label_size * magnify_text, position=position_dodge(width=1)))
          } else if (plot_loop[[plot_type]] == "stacked"){
            out_plot = out_plot +
              geom_text(aes(label = label, group = class_lab), position = position_stack(vjust = 0.5), size = bar_label_size * magnify_text)
          }
          
          final_plot = out_plot  # used to save plot on disk
          final_plot_output = final_plot  # export results (already ggplot file)
          
        } # plot_shape %in% c("stacked", "grouped")
        
        plot_list[[subset_i]][[tr_model]] = final_plot_output
        
        # save plot
        if (save_path != ''){
          png(paste0(save_path, plot_type, "_", tr_model, ifelse(subset_i == "No subset", "", paste0("_", gsub(" ", "_", subset_i))), ".png"),
              width = plot_width * num_output_class, height = plot_height, units = 'in', res=plot_res)
          par(mar=c(0,0,0,0))
          par(oma=c(0,0,0,0))
          suppressWarnings(plot(final_plot))
          dev.off()
        }
        
      } # tr_model
    } # plot_type
  } # subset_i
  if (subset_i == "No subset"){plot_list = plot_list[[1]]}
  
  return(plot_list)
}



for (file in c("ss_class", "ss_num_multi", "ss_num")){
  
  list_input = readRDS(paste0(file, ".rds"))
  
  shape_set = c("single", "stacked", "grouped")
  if (file == "ss_num"){shape_set = "single"}
  
  for (plot_shape in shape_set){
    
    save_path = paste0('./a1a_', plot_shape, "_", file)
    
    cat('\n evaluating:', file, plot_shape)
    oo = plot_feat_imp(list_input = list_input, normalize = normalize, color_pos = color_pos, color_neg = color_neg, plot_model_set = plot_model_set,
                       plot_subset_set = plot_subset_set, plot_shape = plot_shape, cmap = cmap, magnify_text = magnify_text, bar_label_size = bar_label_size,
                       save_path = save_path, plot_width = plot_width, plot_height = plot_height, plot_res = plot_res)
    
    
  }
}




# evaluate Shapley values
dataSample = X %>% select(-medv) #df_predictors  # only predictor
sample_size = 30
seed = 22
obs_index_to_evaluate = c(1:100)#c(1:200, 1500:2000)
obs_index_subset = data.frame(obs_index = obs_index_to_evaluate, class = sample(c("c0", "c1"), length(obs_index_to_evaluate), replace = T))
n_workers = 5
n_batch=1
verbose = 1

evaluate_SHAP(dataSample, sample_size = sample_size, trained_model_prediction_function = trained_model_prediction_function, obs_index_to_evaluate = obs_index_to_evaluate,
                         obs_index_subset = obs_index_subset, n_batch = n_batch, verbose = verbose, n_workers = n_workers, seed = seed)
  
  
  
  



# evaluate PFI

dataSample = df_work %>% mutate(y = as.character(y))  # 
seed = 22
# trained_model_prediction_function = list(mmod1 = ff, Random_Forest = ff1)
obs_index_to_evaluate = c(1:15000)#c(1:200, 1500:2000)
obs_index_to_shuffle = NULL
obs_index_subset = NULL#df_work[obs_index_to_evaluate,] %>% mutate(obs_index = obs_index_to_evaluate, class = as.character(y)) %>% select(obs_index, class)
n_workers = 5
verbose = 1
perf_metric = MLmetrics::F1_Score
perf_metric_minimize = F   # If optimal value of perf_metric should be minimized or not. E.g. TRUE for RMSE, FALSE for Accuracy or F1. Used to set right order "compare" for Perf_permutation - Perf_original (or "ratio")
perf_metric_add_pars = list(positive = "1")    # named list of additional parameters to be passed to perf_metric
prediction_name = "y_pred"  # expected column name for predicted values in perf_metric
true_val_name = "y_true"   # expected column name for true values in perf_metric
n_repetitions = 5
compare = "difference"   # "ratio"

vv = evaluate_Perm_Feat_Imp(dataSample, trained_model_prediction_function = trained_model_prediction_function, n_repetitions = n_repetitions, compare = compare,
                            obs_index_to_evaluate = obs_index_to_evaluate, obs_index_to_shuffle = obs_index_to_shuffle, obs_index_subset = obs_index_subset,
                            perf_metric = perf_metric, perf_metric_add_pars = perf_metric_add_pars, true_val_name = true_val_name,
                            prediction_name = prediction_name, perf_metric_minimize = perf_metric_minimize,
                            package_to_load = NULL, verbose = verbose, n_workers = n_workers, seed = seed)
aa = vv$Permutation_feat_imp %>%
  arrange(model_name, desc(abs(importance)))
aa1 = vv1$Permutation_feat_imp
  
  
# plot SHAP summary
  list_input = readRDS("bb1_class.rds") # "bb1_class.rds"
save_path = './Distance_to_Default/Results/06_Feature_importance_SHAP_summary_'  # 

plot_SHAP_summary(list_input, sina_method = "counts", sina_bins = 20, sina_size = 2, sina_alpha = 0.7, plot_model_set = NULL, plot_class_set = NULL,
                  SHAP_axis_lower_limit = 0, magnify_text = 1.4, color_range = c("blue", "red"),
                  save_path = '', plot_width = 12, plot_height = 12)



# Plot feature importance
list_input = readRDS("bb1_noclass.rds") # "bb1_class.rds"
normalize = F
save_path = './Distance_to_Default/Results/06_'  # 

plot_feat_imp(list_input, normalize = F, color_pos = "blue", color_neg = "red", plot_model_set = NULL, plot_class_set = NULL,
              magnify_text = 1.4, save_path = save_path, plot_width = 12, plot_height = 12)








# obs_index_to_evaluate = c(1:2000)#c(1:200, 1500:2000)
# obs_index_subset = data.frame(obs_index = obs_index_to_evaluate, class = sample(c("c0", "c1"), length(obs_index_to_evaluate), replace = T))
# bb1_class = evaluate_SHAP(dataSample, sample_size, trained_model_prediction_function, obs_index_to_evaluate,
#                          obs_index_subset, n_batch = 5)
# saveRDS(bb1_class, "bb1_class.rds")
# 
# obs_index_to_evaluate = c(1:2000)#c(1:200, 1500:2000)
# obs_index_subset = data.frame(obs_index = obs_index_to_evaluate, class = sample(c("c0", "c1"), length(obs_index_to_evaluate), replace = T))
# bb1_noclass = evaluate_SHAP(dataSample, sample_size, trained_model_prediction_function, obs_index_to_evaluate,
#                        obs_index_subset = NULL, n_batch = 5)
# saveRDS(bb1_noclass, "bb1_noclass.rds")



# obs_index_to_evaluate = c(1:1000)#c(1:200, 1500:2000)
# obs_index_subset = data.frame(obs_index = obs_index_to_evaluate, class = sample(c("c0", "c1"), length(obs_index_to_evaluate), replace = T))
# {
#   cat('\n-----With batch\n')
#   bb1 = evaluate_SHAP(dataSample, sample_size, trained_model_prediction_function, obs_index_to_evaluate,
#                          obs_index_subset, n_batch = 5)
#   saveRDS(bb1, "bb1.rds")
#   
#   cat('\n-----No batch\n')
#   bb = evaluate_SHAP(dataSample, sample_size, trained_model_prediction_function, obs_index_to_evaluate,
#                         obs_index_subset, n_batch = 1)
#   saveRDS(bb, "bb.rds")
# }



# todo: capisci cosa succede se non specifichi i workers























{
plan(multisession, workers = 10)
start_time = Sys.time()
cat('\n10...', end = '')

list_generated_coal <- future_lapply(1:100, generate_coalitions, future.packages = c("data.table"), future.seed = NULL)

future:::ClusterRegistry("stop")
tot_diff=seconds_to_period(difftime(Sys.time(), start_time, units='secs'))
cat('Done in', paste0(lubridate::hour(tot_diff), 'h:', lubridate::minute(tot_diff), 'm:', round(lubridate::second(tot_diff))), ' ', as.character(Sys.time()))


plan(multisession, workers = 50)
start_time = Sys.time()
cat('\n50...', end = '')

list_generated_coal <- future_lapply(1:100, generate_coalitions, future.packages = c("data.table"), future.seed = NULL)

future:::ClusterRegistry("stop")
tot_diff=seconds_to_period(difftime(Sys.time(), start_time, units='secs'))
cat('Done in', paste0(lubridate::hour(tot_diff), 'h:', lubridate::minute(tot_diff), 'm:', round(lubridate::second(tot_diff))), ' ', as.character(Sys.time()))


plan(multisession, workers = 100)
start_time = Sys.time()
cat('\n100...', end = '')

list_generated_coal <- future_lapply(1:100, generate_coalitions, future.packages = c("data.table"), future.seed = NULL)

future:::ClusterRegistry("stop")
tot_diff=seconds_to_period(difftime(Sys.time(), start_time, units='secs'))
cat('Done in', paste0(lubridate::hour(tot_diff), 'h:', lubridate::minute(tot_diff), 'm:', round(lubridate::second(tot_diff))), ' ', as.character(Sys.time()))
}




# 400  -  0h:5m:13
# 400_chunk  -  0h:6m:14    with future.chunk.size=100
# 400_chunk  -  0h:5m:36    with custom chunks of 100

obs_index_to_evaluate = c(1:400)
{
# generate all sample to be used for all trained models
plan(multisession, workers = 5)
start_time = Sys.time()
cat('\n5 - Generating coalitions...', end = '')

list_generated_coal <- future_lapply(obs_index_to_evaluate, generate_coalitions, future.packages = c("data.table"), future.seed = NULL)

future:::ClusterRegistry("stop")
tot_diff=seconds_to_period(difftime(Sys.time(), start_time, units='secs'))
cat('Done in', paste0(lubridate::hour(tot_diff), 'h:', lubridate::minute(tot_diff), 'm:', round(lubridate::second(tot_diff))), ' ', as.character(Sys.time()))
}

aa_400 = data.table::rbindlist(list_generated_coal)


# {
#   # generate all sample to be used for all trained models
#   plan(multisession, workers = 5)
#   start_time = Sys.time()
#   cat('\n5 - Generating coalitions...', end = '')
#   
#   list_generated_coal <- future_lapply(obs_index_to_evaluate, generate_coalitions, future.packages = c("data.table"), future.seed = NULL, future.chunk.size = 100)
#   
#   future:::ClusterRegistry("stop")
#   tot_diff=seconds_to_period(difftime(Sys.time(), start_time, units='secs'))
#   cat('Done in', paste0(lubridate::hour(tot_diff), 'h:', lubridate::minute(tot_diff), 'm:', round(lubridate::second(tot_diff))), ' ', as.character(Sys.time()))
# }
# 
# aa_400_chunk = data.table::rbindlist(list_generated_coal)



n_chunk = 4
{
  split_list = split(obs_index_to_evaluate, sort(obs_index_to_evaluate%%n_chunk))
  plan(multisession, workers = 5)
  start_time = Sys.time()
  cat('\n5 - Generating coalitions...', end = '')
  final_list = list()
  for (ss in names(split_list)){
    list_tt <- future_lapply(split_list[[ss]], generate_coalitions, future.packages = c("data.table"), future.seed = NULL)
    final_list[[ss]] = data.table::rbindlist(list_tt)
  }
  future:::ClusterRegistry("stop")
  tot_diff=seconds_to_period(difftime(Sys.time(), start_time, units='secs'))
  cat('Done in', paste0(lubridate::hour(tot_diff), 'h:', lubridate::minute(tot_diff), 'm:', round(lubridate::second(tot_diff))), ' ', as.character(Sys.time()))
  
}
aa_400_chunk = data.table::rbindlist(final_list)
