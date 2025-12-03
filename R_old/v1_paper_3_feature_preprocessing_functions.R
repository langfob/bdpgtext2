#===============================================================================
#
#               v1_paper_3_feature_preprocessing_functions.R
#
#===============================================================================

#  History

#  2021 02 19 - BTL
#      - Created by extracting existing functions from 
#            bdpgtext/Analysis_scripts/
#            v14_bdpg_analysis_scripts_function_defns.paper_3.R
#        and moving them into this new R file.

#===============================================================================

#  Load necessary libraries

#===============================================================================

do_BoxCox_for_given_rs <- function (cur_rs_name, x_train, x_test)
    {
        #  Select the rows for just the current reserve selector and 
        #  remove its method name from the resulting selection since 
        #  BoxCox will choke on any non-numeric column.
        
    x_train %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (-c(rs_method_name, rsr_UUID)) -> cur_rs_x_train

    x_test %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (-c(rs_method_name, rsr_UUID)) -> cur_rs_x_test
        
    x_train %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (rsr_UUID) -> rsr_UUID_train_df 
    rsr_UUID_train_vec = as.vector (rsr_UUID_train_df$rsr_UUID)
    
    x_test %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (rsr_UUID) -> rsr_UUID_test_df 
    rsr_UUID_test_vec = as.vector (rsr_UUID_test_df$rsr_UUID)

    bct_results_list = list (x_train = NULL, 
                             x_test = NULL)
  
    if (nrow (cur_rs_x_train) > 0)
        {
        bct_results_list = 
                  do_BoxCox_transforms (as.data.frame (cur_rs_x_train), 
                                        as.data.frame (cur_rs_x_test))
                                    
            #  Put the rs method name back into the data sets to be returned.

        bct_results_list$x_train %>% 
                mutate (rs_method_name = cur_rs_name, 
                        rsr_UUID = rsr_UUID_train_vec) -> bct_train    #bct_results_list$x_train
        
        bct_results_list$x_test %>% 
                mutate (rs_method_name = cur_rs_name, 
                        rsr_UUID = rsr_UUID_test_vec) -> bct_test    #bct_results_list$x_test
        
        bct_results_list$x_train = bct_train
        bct_results_list$x_test = bct_test        
        }

    return (bct_results_list)
    }

#-------------------------------------------------------------------------------

do_BoxCox_on_all_rs <- function (rs_names_vec, x_train, x_test)
    {
        #  Run BoxCox on the first reserve selector and use the results 
        #  to create the initial full transformed train and test sets that  
        #  subsequent reserve selectors' results will be appended to.
      
    cur_rs_name = rs_names_vec [1]
    bct_results_list         = do_BoxCox_for_given_rs (cur_rs_name, 
                                                       x_train, 
                                                       x_test)
    full_transformed_x_train = bct_results_list$x_train
    full_transformed_x_test  = bct_results_list$x_test
            
        #  Run BoxCox on the rest of the reserve selectors and append the 
        #  results to the full train and test sets.
            
    if (length (rs_names_vec) > 1)
        {
        for (cur_rs_name in rs_names_vec [2:length (rs_names_vec)])
            {
            bct_results_list = do_BoxCox_for_given_rs (cur_rs_name, x_train, x_test)

            full_transformed_x_train = 
                dplyr::bind_rows (full_transformed_x_train, bct_results_list$x_train)
            
            full_transformed_x_test = 
                dplyr::bind_rows (full_transformed_x_test, bct_results_list$x_test)
            }
        }

    return (list (x_train = full_transformed_x_train, 
                  x_test  = full_transformed_x_test))
    }

#-------------------------------------------------------------------------------

   #  BoxCoxTrans() function from caret doesn't seem to work correctly with my 
    #  tibble inputs either when called alone or as an argument inside the 
    #  preProcess() function.  Looks like this may be a more general problem:
    #       Does caret not play well with tibbles? #611
    #           https://github.com/topepo/caret/issues/611
    #     which points to:
    #       Error calling gbm #145
    #           https://github.com/topepo/caret/issues/145
    #  As a result of these problems, I'm just doing a manual looping over the 
    #  columns of the tibble and copying the results back into the output since 
    #  I can't seem to get anything else to work properly.  I'm sure there must 
    #  be a better way, but this works and I'm not willing to spend any more time 
    #  on it now.  Here are the failed things that I tried:
    
    #--------------------------------------------
    
    #  Failure 1:
    # preprocessing_obj <- preProcess (train_df_to_build_preprocess_from, 
    #                       method=c("BoxCox", "center", "scale"))
    #     This failure is odd because the following line that doesn't use the 
    #     BoxCox argument works fine:
    # #                      method=c("center", "scale"))
    #     That may be because these caret functions are just wrappers around other 
    #     functions from other packages and the flaw is probably in the BoxCox 
    #     function that caret calls or in some function that that function calls.
    #
    #  Fails with message:
          # Error in predict.BoxCoxTrans(bc[[i]], x[, i]) : newdata should be a numeric vector
          #
          # 5. stop("newdata should be a numeric vector")
          # 4. predict.BoxCoxTrans(bc[[i]], x[, i])
          # 3. predict(bc[[i]], x[, i])
          # 2. preProcess.default(train_df_to_build_preprocess_from, method = c("BoxCox", "center", "scale"))
          # 1. preProcess(train_df_to_build_preprocess_from, method = c("BoxCox", "center", "scale"))
    
    #--------------------------------------------
    
    # Failure 2 (where I cast to data frame, as recommended in the web sites 
    #            mentioned above):
    #
    # preprocessing_obj <- preProcess (as.data.frame (train_df_to_build_preprocess_from), 
    #                       method=c("BoxCox", "center", "scale"))
    #
    # preprocessed_train_df_cols <- predict (preprocessing_obj, 
    #                                        train_df_to_build_preprocess_from)
    #
    #  Fails with message:
          # Error: Must use a vector in `[`, not an object of class matrix.
          # 7. stop(cnd)
          # 6. abort(error_dim_column_index(j))
          # 5. check_names_df(i, x)
          # 4. `[.tbl_df`(tt, !is.na(tt))
          # 3. tt[!is.na(tt)]
          # 2. predict.preProcess(preprocessing_obj, train_df_to_build_preprocess_from)
          # 1. predict(preprocessing_obj, train_df_to_build_preprocess_from)
    
    
    # y = BoxCoxTrans (as.data.frame (train_df_to_build_preprocess_from), 
    #                  na.rm = TRUE)
    #boxCox_y = predict (y, train_df_to_build_preprocess_from, na.remove = TRUE)
    
    #--------------------------------------------
    
    #  The looping scheme used here isn't pretty, but it does work, 
    #  unlike the stuff above.
    
    # library (e1071)
    # skewValues <- apply (x, 2, skewness)
    
do_BoxCox_transforms <- function (x_train, 
                                  x_test)
    {
    num_cols = ncol (x_train)
    bct_lambdas_etc = data.frame (name      = rep(NA,num_cols), 
                                  lambda    = rep(NA,num_cols), 
                                  n         = rep(NA,num_cols), 
                                  ratio     = rep(NA,num_cols), 
                                  skewness  = rep(NA,num_cols), 
    
                                  sm_train  = rep(NA,num_cols), 
                                  sm_test   = rep(NA,num_cols), 
                                  shift     = rep(NA,num_cols), 
                                  test_min  = rep(NA,num_cols), 
                                
                                  in_min    = rep(NA,num_cols), 
                                  in_Q1     = rep(NA,num_cols), 
                                  in_median = rep(NA,num_cols), 
                                  in_mean   = rep(NA,num_cols), 
                                  in_Q3     = rep(NA,num_cols), 
                                  in_max    = rep(NA,num_cols))
    
    cat ("\n")
    for (idx in 1:ncol(x_train))
        {
        cat ("idx = ", idx, ", colname = ", colnames(x_train)[idx], "\n")
        
          #  BoxCox requires positive values, so if the smallest training 
          #  value is not positive, shift all values so that they are positive.
          #  Note that we can't know what the range of values is in the real 
          #  world, so we shouldn't test the test values range to see if its 
          #  min is even lower than the train min.  Instead, we'll just add 
          #  1 to the train min and hope that's enough to cover the test min 
          #  too.
          
        smallest_train_value = min (x_train [,idx])
        smallest_test_value  = min (x_test [, idx])
        shift = 0
        
        if (smallest_train_value <= 0)
            {
            shift = abs (smallest_train_value) + 1
            x_train [, idx] = x_train [, idx] + shift
            x_test [, idx]  = x_test [, idx] + shift
            }
        
        bct_obj = BoxCoxTrans (x_train [,idx])
        
                bct_lambdas_etc$name [idx]      = colnames(x_train)[idx]
                
                bct_lambdas_etc$lambda [idx]    = bct_obj$lambda
                bct_lambdas_etc$n [idx]         = bct_obj$n
                
                bct_lambdas_etc$ratio [idx]     = bct_obj$ratio
                bct_lambdas_etc$skewness [idx]  = bct_obj$skewness
                
                bct_lambdas_etc$sm_train [idx]  = smallest_train_value
                bct_lambdas_etc$sm_test [idx]   = smallest_test_value
                bct_lambdas_etc$shift [idx]     = shift
                bct_lambdas_etc$test_min [idx]  = min (x_test [,idx])
        
                bct_lambdas_etc$in_min [idx]    = bct_obj$summary [1]
                bct_lambdas_etc$in_Q1 [idx]     = bct_obj$summary [2]
                bct_lambdas_etc$in_median [idx] = bct_obj$summary [3]
                bct_lambdas_etc$in_mean [idx]   = bct_obj$summary [4]
                bct_lambdas_etc$in_Q3 [idx]     = bct_obj$summary [5]
                bct_lambdas_etc$in_max [idx]    = bct_obj$summary [6]
        
        x_train [, idx] = predict (bct_obj, x_train [,idx])
        x_test [, idx]  = predict (bct_obj, x_test [,idx])
        }
    
    return (list (x_train = x_train, 
                  x_test  = x_test))
    }

#===============================================================================

###  Center and scale the data

center_and_scale_all_rs <- function (rs_names_vec, x_train, x_test)
    {
        #  Center and scale the first reserve selector and use the results 
        #  to create the initial full transformed train and test sets that  
        #  subsequent reserve selectors' results will be appended to.
       
    cur_rs_name              = rs_names_vec [1]
    cs_results_list          = center_and_scale_given_rs (cur_rs_name, 
                                                          x_train, 
                                                          x_test)
    full_transformed_x_train = cs_results_list$preprocessed_train_df_cols
    full_transformed_x_test  = cs_results_list$preprocessed_test_df_cols
            
        #  Center and scale the rest of the reserve selectors and append the 
        #  results to the full train and test sets.
            
    if (length (rs_names_vec) > 1)
        {
        for (cur_rs_name in rs_names_vec [2:length (rs_names_vec)])
            {
            cs_results_list = center_and_scale_given_rs (cur_rs_name, x_train, x_test)

            full_transformed_x_train = 
                dplyr::bind_rows (full_transformed_x_train, 
                                  cs_results_list$preprocessed_train_df_cols)
            
            full_transformed_x_test = 
                dplyr::bind_rows (full_transformed_x_test, 
                                  cs_results_list$preprocessed_test_df_cols)
            }
        }

    return (list (preprocessed_train_df_cols = full_transformed_x_train, 
                  preprocessed_test_df_cols  = full_transformed_x_test))
    }

#-------------------------------------------------------------------------------

do_centeringAndScaling_transforms <- function (cur_rs_x_train, 
                                               cur_rs_x_test)
    {
    centeringAndScaling_obj <- preProcess (cur_rs_x_train, 
                                           method=c("center", "scale"))
    
        #  Apply the mean and sd to scale the data.
    preprocessed_train_df_cols <- predict (centeringAndScaling_obj, 
                                           cur_rs_x_train)
    preprocessed_test_df_cols <- predict (centeringAndScaling_obj, 
                                          cur_rs_x_test)
    
    return (list (preprocessed_train_df_cols = preprocessed_train_df_cols, 
                  preprocessed_test_df_cols  = preprocessed_test_df_cols))
    }

#-------------------------------------------------------------------------------

center_and_scale_given_rs <- function (cur_rs_name, x_train, x_test)
    {
        #  Select the rows for just the current reserve selector and 
        #  remove its method name from the resulting selection in case 
        #  standardizing will choke on a non-numeric column.
    x_train %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (-c(rs_method_name, rsr_UUID)) -> cur_rs_x_train
    x_test %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (-c(rs_method_name, rsr_UUID)) -> cur_rs_x_test

    x_train %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (rsr_UUID) -> rsr_UUID_train_df 
    rsr_UUID_train_vec = as.vector (rsr_UUID_train_df$rsr_UUID)

    x_test %>%
        filter (rs_method_name == cur_rs_name) %>% 
        select (rsr_UUID) -> rsr_UUID_test_df 
    rsr_UUID_test_vec = as.vector (rsr_UUID_test_df$rsr_UUID)

    preprocessed_train_df_cols = NULL
    preprocessed_test_df_cols = NULL
    
    if (nrow (cur_rs_x_train) > 0)
        {
    centeringAndScaling_obj <- preProcess (cur_rs_x_train, 
                                           method=c("center", "scale"))
    
            #  Apply the mean and sd to scale the data.
    preprocessed_train_df_cols <- predict (centeringAndScaling_obj, 
                                           cur_rs_x_train)

    preprocessed_test_df_cols <- predict (centeringAndScaling_obj, 
                                          cur_rs_x_test)

        #  Put the rs method name back into the data sets to be returned.
        
    preprocessed_train_df_cols %>% 
            mutate (rs_method_name = cur_rs_name, 
                    rsr_UUID = rsr_UUID_train_vec) -> preprocessed_train_df_cols
            
    preprocessed_test_df_cols %>% 
            mutate (rs_method_name = cur_rs_name, 
                    rsr_UUID = rsr_UUID_test_vec) -> preprocessed_test_df_cols
                                                        
        }  #  end if - nrow (cur_rs_x_train) > 0
    
    return (list (preprocessed_train_df_cols = preprocessed_train_df_cols, 
                  preprocessed_test_df_cols  = preprocessed_test_df_cols))
    }

#===============================================================================
#===============================================================================
#===============================================================================

##  Function to check for (and remove) highly correlated variables

check_for_highly_correlated_vars <- function (train_x_df, 
                                              test_x_df, 
                                              high_cor_thresh = 0.99)
  {
  temp_dt = train_x_df
            # select (train_x_df, 
            #         -c(err_mag, 
            #            rsr_COR_euc_out_err_frac))
  
  descrCor <- cor (temp_dt)
  highCorr <- sum (abs (descrCor [upper.tri(descrCor)]) > 0.99)
  summary (descrCor [upper.tri (descrCor)])
  
  highlyCorDescr <- findCorrelation (descrCor, cutoff = .99)
  cat ("\n\nHighly correlated variables to remove:\n")
  colnames (temp_dt) [highlyCorDescr]
  cat ("\n\nRemaining, less correlated variables:\n")
  colnames (temp_dt) [-highlyCorDescr]
  
  filteredDescr <- temp_dt [,-highlyCorDescr]
  descrCor2 <- cor (filteredDescr)
  summary (descrCor2 [upper.tri(descrCor2)])
  
      #  Save the smaller set of predictor variables as the train and test set.
      
  return (list (train_x_df = train_x_df [,-highlyCorDescr], 
                test_x_df  = test_x_df [,-highlyCorDescr]))
  }

#===============================================================================

