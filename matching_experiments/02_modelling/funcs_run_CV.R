

run_cross_validation <- function(model_data, results_dir, output_suffix, 
                                 target_variable = 'label', positive_class = 1, 
                                 train_indices = NULL, test_indices = NULL,
                                 pr_curve_bins = 100, learner_type = 'XGB', 
                                 learner_params = list(
                                   nrounds = 100,
                                   verbose = TRUE,
                                   objective = 'binary:logistic'
                                 ), 
                                 perf_measures = palabmod::perf_make_pr_measure(10, "pr10"), 
                                 outputs_to_save = list(
                                   pr_curve = TRUE,
                                   resample = TRUE,
                                   VI = TRUE,
                                   plot_pr_curve = TRUE
                                 ), 
                                 K = 5, predict = 'test',
                                 matched_ids = NULL){
  
  #CREATE RESULTS DIR
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  
  
  if (substr(output_suffix, start = 1, stop = 1) == '_') {
    output_suffix = substr(output_suffix, start = 2, nchar(output_suffix))
  }
  
  # MLR pipeline ------------------------------------------------------------
  if (is.null(matched_ids)){  
    dataset <- mlr::makeClassifTask(id = output_suffix, 
                             data = model_data,
                             target = target_variable, 
                             positive = positive_class)
  } else {
     dataset <- mlr::makeClassifTask(id = output_suffix, 
                             data = model_data,
                             target = target_variable, 
                             positive = positive_class, 
                            blocking = matched_ids  ) 
  }
  
  if (learner_type == 'XGB') {
    # make xgboost learner:
    lrn_xgb <- mlr::makeLearner(cl = "classif.xgboost", predict.type = "prob")
    lrn_xgb$par.vals <- learner_params
  }
  
  # create resampling description and instance:
  rdesc <- mlr::makeResampleDesc(method = "CV", iters = K, predict = predict)
  # create resampling instance:
  rin <- mlr::makeResampleInstance(desc = rdesc, task = dataset)

  #Add training/test indices if specified
  if (!is.null(train_indices)) {
    rin$train.inds <- train_indices
  } 
  
  if (!is.null(test_indices)) {
    rin$test.inds <- test_indices
  }
  
  # run CV
  resample_params = list(learner = lrn_xgb, task = dataset, resampling = rin, 
                         models = TRUE)
  if (!is.null(perf_measures)) {
    resample_params$measures = perf_measures
  }
  
  res <- do.call(mlr::resample, resample_params)
  
  #Save resample object
  if (outputs_to_save$resample) {
    readr::write_rds(res, paste0(results_dir, learner_type, '_', output_suffix, '.rds' ))
  }
  # PR Curve:
  if (outputs_to_save$pr_curve) {
    pr_curve <- palabmod::perf_binned_perf_curve(res$pred, bin_num = pr_curve_bins)
    readr::write_csv(pr_curve$curve, paste0(results_dir, 'PRCurve_', learner_type,
                                     '_', output_suffix, '.csv'))
  }
  
  if (outputs_to_save$plot_pr_curve) {
    palabmod::perf_plot_pr_curve(res$pred)
    ggplot2::ggsave(paste0(results_dir, 'PRCurve_plotted_', output_suffix, '.png'))
  }
  
  if (outputs_to_save$VI) {
    importance_dir <- paste0(results_dir, 'variable_importance/')
    dir.create(importance_dir, recursive = TRUE, FALSE)
    #train_numeric <- as.data.frame(sapply(dataset$env$data, function(x) { as.numeric(as.character(x)) }))
    
    # generate variable importance for each fold of the CV:
    for (i in 1:length(res$models)) {
      importance_fold <- xgboost::xgb.importance(feature_names = res$models[[i]]$features,
                                      model = res$models[[i]]$learner.model)
      readr::write_csv(importance_fold, paste0(importance_dir, 'VI_', learner_type, 
                                        '_freq_fold_', i, '_', output_suffix, 
                                        '.csv'))
      
      # convert to numeric in order to use in detailed xgb.importance:
      
      #detailed_imp <- xgb.importance(feature_names = res$models[[i]]$features,
      #                               model = res$models[[i]]$learner.model, 
      #                               data = as.matrix(train_numeric[rin$train.inds[[i]],]),
      #                               label = train_numeric$label)
      
      # write out:
      #write_csv(detailed_imp, paste(importance_dir, paste0(importance_dir, 'VI_detailed_', learner_type, 
      #                                                     '_freq_fold_', i, '_', output_suffix, 
      #                                                     '.csv')))
    }
  }
  

  res
  
}



