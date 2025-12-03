#===============================================================================
#
#  v1a_paper_3_fitting_functions__returning_fitting_plots_instead_of_displaying_them.R
#
#===============================================================================

#  History

#  2024 02 03 - BTL
#     - Modified the end of fit_and_predict_output_error_using_feature_set() 
#       so that instead of showing the plots of fitted values, I return the 
#       plot object so that its display can be managed at a higher level.  
#       I wanted this because I was trying to combine the rep shortfall and 
#       the solution cost error panels into a single plot in p8 v11 using the 
#       patchwork package.  Unfortunately, I couldn't get a decent version 
#       of the combined plot using patchwork.  It could be that I just don't 
#       know how to use patchwork well enough, but for the moment, I'm 
#       abandoning trying to do that.  I'm leaving this version of the R file 
#       though so that I can find how to return the plot objects if I change 
#       my mind in the future.
#  
#  2021 02 19 - BTL
#      - Created by extracting existing functions from 
#            bdpgtext/Analysis_scripts/
#            v14_bdpg_analysis_scripts_function_defns.paper_3.R
#        and moving them into this new R file.

#===============================================================================

#  Load necessary libraries

#===============================================================================

fit_one_rs <- function (rs_name, 
                        
                        train_x_df, test_x_df,                #  e.g., p3_test_x_df_probSize (3 cols: rs_method_name, rsp_num_occupied_PUs, rsp_num_spp)
                        working_train_df, working_test_df,    #  e.g., p3_working_test_df    (all 90 cols, including lots of label data like rsr_UUID) 

                        train_aux_df, test_aux_df,            #  e.g., p3_test_aux_df        (2 cols: rs_method_name, dom_err_type)
                        vars_used_str,                        #  e.g., "probSize"
                        params, 
                        
                            #  Shouldn't these default to NULL or 
                            #  just not have a default at all?
                        target_col_name = "rsr_COR_spp_rep_shortfall", 
                        pred_value_name_display_str = "Rep Shortfall"
                       )
    {
        #  Reduce data down to just the current RS and remove the rs name 
        #  from the reduced data set.
    train_x_df %>%
        ungroup () %>% 
        filter (rs_method_name == rs_name) %>% 
        select (-rs_method_name) -> train_x_df
#        select (-c(rs_method_name, rsr_UUID)) -> train_x_df

    test_x_df %>%
        ungroup () %>% 
        filter (rs_method_name == rs_name) %>% 
        select (-rs_method_name) -> test_x_df
#        select (-c(rs_method_name, rsr_UUID)) -> test_x_df

    working_train_df %>%
        ungroup () %>% 
        filter (rs_method_name == rs_name) -> working_train_df

    working_test_df %>%
        ungroup () %>% 
        filter (rs_method_name == rs_name) -> working_test_df

    train_aux_df %>%
        ungroup () %>% 
        filter (rs_method_name == rs_name) %>% 
        select (-rs_method_name) -> train_aux_df
#        select (-c(rs_method_name, rsr_UUID)) -> train_aux_df

    test_aux_df %>%
        ungroup () %>% 
        filter (rs_method_name == rs_name) %>% 
        select (-rs_method_name) -> test_aux_df
#        select (-c(rs_method_name, rsr_UUID)) -> test_aux_df

        #  Set target vector values.  
        #
        #  Doing the same thing two different ways to remind myself that 
        #  both of these exist and do the same thing, i.e., 
        #  extract a data frame column as a vector, not as a one column 
        #  data frame.

    train_y_vec = working_train_df [[target_col_name]]
    test_y_vec  = dplyr::pull (working_test_df, target_col_name)

        #  Fit models

        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
#    test_eval_list = 
    train_and_test_eval_lists = 

        fit_and_plot (rs_name, 
             
                      pred_value_name_display_str  = pred_value_name_display_str,
                      vars_used_str                = vars_used_str,
                      params                       = params,
            
                      train_y_vec                  = train_y_vec,
                      test_y_vec                   = test_y_vec,
            
                      train_x_df                   = train_x_df,
                      test_x_df                    = test_x_df,
            
                      train_aux_df                 = train_aux_df,
                      test_aux_df                  = test_aux_df)

        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #return (test_eval_list)
    return (train_and_test_eval_lists)
    }

#===============================================================================

    ##  Fit specified model(s)

fit_and_plot <- function (rs_name, 
                       
                          pred_value_name_display_str, 
                          vars_used_str, 
                          params, 
                         
                          train_y_vec, 
                          test_y_vec, 
                         
                          train_x_df, 
                          test_x_df, 
                         
               train_aux_df, 
               test_aux_df
                         )
    
               

    {
        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #test_eval_list = NULL
    train_and_test_eval_lists = NULL
    
                #-------------------------------------
                #  Fit simple linear model using LM.
                #-------------------------------------
    
    if (params$do_lm)
        {
            #  2024 01 31 - BTL
            #  If you change the code here for calling f_lm(), be sure to make 
            #  the same change to the call to f_lm() in the glmnet section  
            #  below.  That f_lm() section is only called when you need to avoid  
            #  an error when there is just one column of predictor variables  
            #  since the glmnet code crashes on purpose when there is only one 
            #  predictor (though I'm not sure why they do that).
            #  
            #  2023 01 01 - BTL
            #  New code to return both test and train instead of just test.
#        test_eval_list = 
        train_and_test_eval_lists = 
            f_lm (rs_name, 
                  pred_value_name_display_str  = pred_value_name_display_str, 
                  vars_used_str                = vars_used_str, 
                  model_name_str_suffix        = "ALL",  
                  train_y_vec                  = train_y_vec, 
                  train_x_df                   = train_x_df, 
                  test_y_vec                   = test_y_vec, 
                  test_x_df                    = test_x_df, 
                  train_aux_df                 = train_aux_df, 
                  test_aux_df                  = test_aux_df, 
                  params                       = params)
        }
   
                #----------------------------------
                #  Fit LM using cross-validation. 
                #----------------------------------
    
    if (params$do_lm_cv)
      {
      set.seed(seed3)
      
      num_folds = params$num_folds
      folds = create_cv_folds (num_folds, 1:dim(train_x_df)[1])
      
      for (cur_fold_ID in 1:num_folds)
          {
          cur_fold_rows = folds$fold_row_nums[[cur_fold_ID]]
          
          cur_fold_train_x_df  = train_x_df  [ - cur_fold_rows, ]
          cur_fold_train_y_vec = train_y_vec [ - cur_fold_rows]

          cur_fold_test_x_df  = train_x_df  [cur_fold_rows, ]
          cur_fold_test_y_vec = train_y_vec [cur_fold_rows]


          f_lm (rs_name, 
                pred_value_name_display_str  = pred_value_name_display_str, 
                vars_used_str                = vars_used_str, 
                model_name_str_suffix        = paste0 ("ALL-fold ",
                                                       cur_fold_ID),  
                train_y_vec                  = cur_fold_train_y_vec, 
                train_x_df                   = cur_fold_train_x_df, 
                test_y_vec                   = cur_fold_test_y_vec, 
                test_x_df                    = cur_fold_test_x_df, 
                
                train_aux_df                 = train_aux_df, 
                test_aux_df                  = test_aux_df)
          }
      }
   
        #-----------------------------
        #  Fit RANDOM FOREST models.
        #-----------------------------

    if (params$do_rf)
        {
        if (params$use_party_pkg_for_rf)
            {
                #--------------------------------------
                #  Fit random forest using party pkg.
                #--------------------------------------
            
            #  2023 01 01 - BTL
            #  New code to return both test and train instead of just test.
#        test_eval_list = 
            train_and_test_eval_lists = 
                f_rf_party (rs_name, 
                            pred_value_name_display_str  = pred_value_name_display_str, 
                            vars_used_str                = vars_used_str, 
                            model_name_str_suffix        = "ALL",  
                            train_y_vec                  = train_y_vec, 
                            train_x_df                   = train_x_df, 
                            test_y_vec                   = test_y_vec, 
                            test_x_df                    = test_x_df, 
                        train_aux_df                 = train_aux_df, 
                        test_aux_df                  = test_aux_df,
                            params                       = params)
            } else 
            {
                #---------------------------------------
                #  Fit random forest using ranger pkg.
                #---------------------------------------
                
            #  2023 01 01 - BTL
            #  New code to return both test and train instead of just test.
#        test_eval_list = 
            train_and_test_eval_lists = 
                f_rf_ranger (rs_name, 
                             pred_value_name_display_str  = pred_value_name_display_str, 
                             vars_used_str                = vars_used_str, 
                             model_name_str_suffix        = "ALL",  
                             train_y_vec                  = train_y_vec, 
                             train_x_df                   = train_x_df, 
                             test_y_vec                   = test_y_vec, 
                             test_x_df                    = test_x_df,
                        train_aux_df                 = train_aux_df, 
                        test_aux_df                  = test_aux_df,
                             params                       = params)
            }
        }
  
    #----------------------
    #  Fit GLMNET models.
    #----------------------

        #  2024 01 31 - BTL
        #  Adding a test for the case where there is just one column of 
        #  predictor variables.  For some reason, the glmnet code 
        #  crashes on purpose if there is only one column of predictors.  
        #  Not sure why.  However, there are numerous stackoverflow and 
        #  other web pages that mention this.  
        #  So, here, if there is only one predictor column, I'll just use 
        #  lm instead of glmnet.  

    if (dim (train_x_df) [2] < 2)
        {
#        test_eval_list = 
        train_and_test_eval_lists = 
            f_lm (rs_name, 
                  pred_value_name_display_str  = pred_value_name_display_str, 
                  vars_used_str                = vars_used_str, 
                  model_name_str_suffix        = "ALL",  
                  train_y_vec                  = train_y_vec, 
                  train_x_df                   = train_x_df, 
                  test_y_vec                   = test_y_vec, 
                  test_x_df                    = test_x_df, 
                  train_aux_df                 = train_aux_df, 
                  test_aux_df                  = test_aux_df, 
                  params                       = params)
        
        } else    #  There is more than one input predictor variable.
        {
                #------------------------------
                #  Fit GLMNET using caret pkg.
                #------------------------------
    
        if (params$do_glmnet_caret)
            {
            set.seed (seed2)
            
            train_x_mat = as.matrix (train_x_df)
            test_x_mat = as.matrix (test_x_df)
            
            train_control <- trainControl(method = "cv", number = 10)
            
            caret_mod <- train(x = train_x_mat,    #ames_train_x,
                             y = train_y_vec,    #ames_train_y,
                             method = "glmnet",
                            #preProc = c("center", "scale", "zv", "nzv"),
                             trControl = train_control,
                             tuneLength = 10)
            
            cat ("\n\n>>>>> glment model:\n")
            print (caret_mod)
            
              # Model coefficients
            print (coef(caret_mod$finalModel, caret_mod$bestTune$lambda))
            
            predictions <- predict (caret_mod, newdata=test_x_mat)
            
            rmse = RMSE (predictions, test_y_vec)
            cat ("\n\nglmnet rmse = ", rmse)
            
            rsquared = R2 (predictions, test_y_vec)
            cat ("\nglmnet  R^2 = ", rsquared, "\n\n")
            }
    
                #----------------------------------------------
                #  Fit GLMNET using UC web page instructions.
                #----------------------------------------------
    
        if (params$do_glmnet_UC)
            {
                     #  2023 01 01 - BTL
                    #  New code to return both test and train instead of just test.
        #        test_eval_list = 
                train_and_test_eval_lists = 
                    f_glmnet_UC (rs_name, 
                                 pred_value_name_display_str  = pred_value_name_display_str, 
                                 vars_used_str                = vars_used_str, 
                                 model_name_str_suffix        = "ALL",  
                                 train_y_vec                  = train_y_vec, 
                                 train_x_df                   = train_x_df, 
                                 test_y_vec                   = test_y_vec, 
                                 test_x_df                    = test_x_df, 
                                 train_aux_df                 = train_aux_df, 
                                 test_aux_df                  = test_aux_df, 
                                 params                       = params, 
                                 VERBOSE_GLMNET_UC            = params$VERBOSE_GLMNET_UC)
            }
        }  #  end else - do some kind of glmnet
    
        #  NOTE that this will return the last version of test_eval_list 
        #  that is defined in this function if more than one fitting method 
        #  was called.  
        #  I don't want to clean this up at the moment because it will make 
        #  for a messy return value and messy decoding logic in the calling 
        #  routine of this routine.  Plus, the allowance for more than one 
        #  fitting in a run is mostly vestigial.  For quite a while now, 
        #  I've only been running them one at a time and some of them not 
        #  at all (like the glm caret).
        
            #  2023 01 01 - BTL
            #  New code to return both test and train instead of just test.
    #return (test_eval_list)
    return (train_and_test_eval_lists)
    }

#===============================================================================

gen_full_fits <- function (dummy_full_true_vs_pred_df, 
                           dummy_full_measured_values_df, 
                           rs_names_vec, 
                           rs_names_vec_fac, 
                           model_name_str, 
                           len, 
                           R2_x_loc, R2_y_loc, 
                           rmse_x_loc, rmse_y_loc, 
                           num_facet_wrap_rows, 
                           pred_value_name_display_str, 
                           params, 
                           
                           som_model, 
                           num_som_cells, 
                           test_or_train_str, 
                           
                  x_min_on_plot = NA,      
                  x_max_on_plot = NA, y_min_on_plot = NA, y_max_on_plot = NA
                           )
    {
    dummy_R2_strings = round (dummy_full_measured_values_df$adj_R2, digits = 2)

    dummy_locs_R2 <- tibble (x = rep (R2_x_loc, len), 
                            y = rep (R2_y_loc, len), 
                            rs_method_name_fac = rs_names_vec_fac,    #  Must have this element and its name must match the facetting variable for the facetted plot
                            R2_label = paste ("adj~R^2 ==", dummy_R2_strings))
     
    dummy_rmse_strings = round (dummy_full_measured_values_df$rmse, digits = 2)
    dummy_locs_rmse <- data.frame (x = rep (rmse_x_loc, len), 
                                  y = rep (rmse_y_loc, len), 
                                  rs_method_name_fac = rs_names_vec_fac,    #  Must have this element and its name must match the facetting variable for the facetted plot
                                  rmse_label = paste ("rmse ==", dummy_rmse_strings))
    
        #----------

    dummy_full_true_vs_pred_df %>% 
            group_by (rs_method_name) %>% 
            summarize (min = min (resid_pred_minus_true), 
                       max = max (resid_pred_minus_true)) %>% 
            ungroup ()  -> dummy_resid_summary_for_SOM
    
#x#    cat ("\n\n>>>>>  dummy_resid_summary_for_SOM  <<<<<\n")
#x#    show (dummy_resid_summary_for_SOM)
#x#    cat ("\n------------------------------------\n")

        #----------
    
        #  Generate self-organizing maps if desired.
    # if (! is.null (dummy_som_model))  run_SOM_resids (dummy_som_model, 
    #                                                  dummy_full_true_vs_pred_df, 
    #                                                  rs_names_vec, 
    #                                                  dummy_num_som_cells)
    if (! is.null (som_model))  run_SOM_resids (som_model, 
                                                     dummy_full_true_vs_pred_df, 
                                                     rs_names_vec, 
                                                     num_som_cells)

        #----------
    
        #  Add method name as factor so that ggplot facet order can be controlled.
    dummy_full_true_vs_pred_df %>% 
            mutate (rs_method_name_fac = 
                    convert_rs_method_name_to_ordered_factor (rs_method_name)) -> 
        dummy_full_true_vs_pred_df
  
        #----------

    dummy_full_fits_plot = plot_full_fits (rs_names_vec, 
                                          params, 
                                          dummy_full_true_vs_pred_df, 
                                          num_facet_wrap_rows, 
                                          pred_value_name_display_str, 
                                          model_name_str, 
                                          dummy_locs_R2, 
                                          dummy_locs_rmse, 
                                          test_or_train_str, 
                                          
                                          x_min_on_plot,      
                                          x_max_on_plot, y_min_on_plot, y_max_on_plot)
        
    dummy_full_fits_plot_and_data = list (full_fits_plot = dummy_full_fits_plot, 
                                          full_true_vs_pred_df = dummy_full_true_vs_pred_df, 
                          full_measured_values_df = dummy_full_measured_values_df)
    
    return (dummy_full_fits_plot_and_data)
    }

#===============================================================================

fit_to_target_var <- function (rs_names_vec, 
                               train_x_df, test_x_df,                #  e.g., p3_test_x_df_probSize (3 cols: rs_method_name, rsp_num_occupied_PUs, rsp_num_spp)
                               working_train_df, working_test_df,    #  e.g., p3_working_test_df    (all 90 cols, including lots of label data like rsr_UUID) 

                               train_aux_df, test_aux_df,            #  e.g., p3_test_aux_df        (2 cols: rs_method_name, dom_err_type)
                               vars_used_str,                        #  e.g., "probSize"
                               params,
                               
                               pred_value_name_display_str,

                               R2_x_loc, 
                               R2_y_loc, 
                              
                               rmse_x_loc, 
                               rmse_y_loc, 
                              
                               target_col_name,  
                               
                  x_min_on_plot = NA,      
                  x_max_on_plot = NA, y_min_on_plot = NA, y_max_on_plot = NA,      
                                   
                  som_model = NULL, 
                  num_som_cells = NULL, 
                  SOM_cost_err_frac = FALSE
                               )
    {
        #  Values specific to either plotting all RS or just the non-greedy ones.
    if (params$exclude_greedy_rs_in_fit_plots)
        {
        num_facet_wrap_rows = 1
#        rs_names_vec = c("Gurobi", "Marxan_SA", "Marxan_SA_SS")
        rs_names_vec = c("ILP", "SA", "SA_SS")
        
        } else
        {
        num_facet_wrap_rows = 2        
        }
    
        #----------

    test_eval_lists = vector (mode = "list", length = length (rs_names_vec))
    train_eval_lists = vector (mode = "list", length = length (rs_names_vec))
    
    for (cur_rs_name in rs_names_vec)
        {
            #  2023 01 01 - BTL
            #  New code to return both test and train instead of just test.
        #cur_test_eval_list = 
        cur_train_and_test_eval_lists = 
            fit_one_rs (cur_rs_name, 
                        train_x_df, test_x_df, 
                        working_train_df, working_test_df, 
                        train_aux_df, test_aux_df, 
                        vars_used_str, params, 
                        target_col_name, 
                        pred_value_name_display_str
                                  )
        cur_train_eval_list = cur_train_and_test_eval_lists$train_eval_list
        cur_test_eval_list = cur_train_and_test_eval_lists$test_eval_list
        
        #-----------------------------------------------------------------------
        #-----------------------------------------------------------------------

            #  DO test...
            #  
            #  Label the the current reserve selector's true vs pred values 
            #  and append that set to the full set for all reserve selectors.
            
        cur_test_eval_list$true_vs_pred_df %>% 
            mutate (rs_method_name = cur_rs_name, 
                    resid_pred_minus_true = pred_values - true_values) -> 
            test_cur_true_vs_pred_df
        
        if (exists ("test_full_true_vs_pred_df"))
            {
            test_full_true_vs_pred_df = rbind (test_full_true_vs_pred_df, 
                                               test_cur_true_vs_pred_df)
            } else
            {
            test_full_true_vs_pred_df = test_cur_true_vs_pred_df
            }
        
            #  Collect the the current reserve selector's measured values 
            #  and append that set to the full set for all reserve selectors.
        if (exists ("test_full_measured_values_df"))
            {
            test_cur_measured_values = 
                list (rs_method_name = cur_rs_name, 
#                      model_name_str = cur_test_eval_list$model_name_str, 
                      rmse           = cur_test_eval_list$rmse_value, 
                      R2             = cur_test_eval_list$R2, 
                      adj_R2         = cur_test_eval_list$adj_R2)
                            
            test_full_measured_values_df = rbind (test_full_measured_values_df, 
                                                  test_cur_measured_values)
            } else
            {
            test_full_measured_values_df = 
                data.frame (rs_method_name = cur_rs_name, 
#                            model_name_str = cur_test_eval_list$model_name_str, 
                            rmse           = cur_test_eval_list$rmse_value, 
                            R2             = cur_test_eval_list$R2, 
                            adj_R2         = cur_test_eval_list$adj_R2)
            }
        cur_test_eval_list$rs_method_name = cur_rs_name
        test_eval_lists [[cur_rs_name]] = cur_test_eval_list
        
        #-----------------------------------------------------------------------

            #  DO train....
            #  
            #  Label the the current reserve selector's true vs pred values 
            #  and append that set to the full set for all reserve selectors.
            
        cur_train_eval_list$true_vs_pred_df %>% 
            mutate (rs_method_name = cur_rs_name, 
                    resid_pred_minus_true = pred_values - true_values) -> 
            train_cur_true_vs_pred_df
        
        if (exists ("train_full_true_vs_pred_df"))
            {
            train_full_true_vs_pred_df = rbind (train_full_true_vs_pred_df, 
                                               train_cur_true_vs_pred_df)
            } else
            {
            train_full_true_vs_pred_df = train_cur_true_vs_pred_df
            }
        
            #  Collect the the current reserve selector's measured values 
            #  and append that set to the full set for all reserve selectors.
        if (exists ("train_full_measured_values_df"))
            {
            train_cur_measured_values = 
                list (rs_method_name = cur_rs_name, 
#                      model_name_str = cur_train_eval_list$model_name_str, 
                      rmse           = cur_train_eval_list$rmse_value, 
                      R2             = cur_train_eval_list$R2, 
                      adj_R2         = cur_train_eval_list$adj_R2)
                            
            train_full_measured_values_df = rbind (train_full_measured_values_df, 
                                                  train_cur_measured_values)
            } else
            {
            train_full_measured_values_df = 
                data.frame (rs_method_name = cur_rs_name, 
#                            model_name_str = cur_train_eval_list$model_name_str, 
                            rmse           = cur_train_eval_list$rmse_value, 
                            R2             = cur_train_eval_list$R2, 
                            adj_R2         = cur_train_eval_list$adj_R2)
            }
        
        cur_train_eval_list$rs_method_name = cur_rs_name
        train_eval_lists [[cur_rs_name]] = cur_train_eval_list
        
        #-----------------------------------------------------------------------
        #-----------------------------------------------------------------------
        }

        #----------

    model_name_str = cur_test_eval_list$model_name_str
    rs_names_vec_fac = convert_rs_method_name_to_ordered_factor (rs_names_vec)
    len <- length (rs_names_vec)
    
    train_full_fits_plot_and_data = gen_full_fits (train_full_true_vs_pred_df, 
                                                  train_full_measured_values_df, 
                                                  rs_names_vec, 
                                                  rs_names_vec_fac, 
                                                  model_name_str, 
                                                  len, 
                                                  R2_x_loc, R2_y_loc, 
                                                  rmse_x_loc, rmse_y_loc, 
                                                  num_facet_wrap_rows, 
                                                  pred_value_name_display_str, 
                                                  params, 
                                                  som_model, 
                                                  num_som_cells, 
                                                  "TRAIN", 
                                                  
                                                  x_min_on_plot, 
                                                  x_max_on_plot, y_min_on_plot, y_max_on_plot)
    
    test_full_fits_plot_and_data = gen_full_fits (test_full_true_vs_pred_df, 
                                                  test_full_measured_values_df, 
                                                  rs_names_vec, 
                                                  rs_names_vec_fac, 
                                                  model_name_str, 
                                                  len, 
                                                  R2_x_loc, R2_y_loc, 
                                                  rmse_x_loc, rmse_y_loc, 
                                                  num_facet_wrap_rows, 
                                                  pred_value_name_display_str, 
                                                  params, 
                                                  som_model, 
                                                  num_som_cells, 
                                                  "TEST", 
                                                  
                                                  x_min_on_plot, 
                                                  x_max_on_plot, y_min_on_plot, y_max_on_plot)
    
    train_and_test_full_fits_plot_and_data = 
        list (train_full_fits_plot_and_data = train_full_fits_plot_and_data, 
              test_full_fits_plot_and_data  = test_full_fits_plot_and_data)

    return (train_and_test_full_fits_plot_and_data)
#    return (full_fits_plot_and_data)
    }
    
#===============================================================================

plot_full_fits <- function (rs_names_vec, 
                            params, 
                            full_true_vs_pred_df, 
                            num_facet_wrap_rows, 
                            pred_value_name_display_str, 
                            model_name_str, 
                            locs_R2, 
                            locs_rmse, 
                            test_or_train_str, 
                              
                  x_min_on_plot = NA,      
                  x_max_on_plot = NA, y_min_on_plot = NA, y_max_on_plot = NA      
                                   
                            )
    {
    #num_facets = length (unique (sorted_msa_tib [[facet_var]]))
    num_facets = length (rs_names_vec)    #####4    #####5
    alpha_level = 0.5 
    #force_colors = TRUE
    force_colors = params$force_colors
    
            #----------
        
    if (params$use_FN_dominant & params$VERBOSE_LM)
        {
        FN_only_true_vs_pred_df = filter (full_true_vs_pred_df, dom_err_type == "FN")
        color_breaks_and_values = force_dom_err_type_colors (FN_only_true_vs_pred_df$dom_err_type)
        scale_color_breaks = color_breaks_and_values$breaks
        scale_color_values = color_breaks_and_values$values
        
        FN_only_true_vs_pred_df %>% 
            ggplot () +
                geom_point (aes (x     = true_values, 
                                 y     = pred_values, 
                                 color = dom_err_type), 
                            
                            shape = 15,    #".", 
                            size=0.5, 
                            alpha = alpha_level    #1
                            # shape="."
                            ) +  
                facet_wrap (~ rs_method_name_fac, nrow = num_facet_wrap_rows) + 
                geom_abline (intercept=0, slope=1) +     #, linetype, color, size
                ggtitle (paste0 ("FN only TEST pred vs. true ", pred_value_name_display_str, 
                                 " for ", model_name_str, " using ", vars_used_str)) + 
                theme (plot.title = element_text (hjust = 0.5)) +
            
                    #  If forcing colors, then manually scale the colors.  
                    #  Otherwise, use the default behavior.
                    #  To use an "if" statement inside these sets of statements 
                    #  separated by "+" signs, you have to put curly brackets around 
                    #  your test and result.
                { if (force_colors) scale_color_manual (breaks = scale_color_breaks, 
                                                        values = scale_color_values) } +
                
                { if (num_facets == 5) theme (legend.position = c(0.85, 0.25),
                                              legend.direction = "vertical") } + 
                
{ if (!is.na (x_min_on_plot) | !is.na(x_max_on_plot)) xlim (x_min_on_plot, x_max_on_plot) } + 
{ if (!is.na (y_min_on_plot) | !is.na(y_max_on_plot)) ylim (y_min_on_plot, y_max_on_plot) } + 
            
                    #  Symbols on the plot itself are small and semi-transparent, 
                    #  but in the legend that makes them nearly invisible.
                    #  Override the symbol size and alpha values in the legend to 
                    #  make the symbols much larger and not transparent at all.  
                    #  Note that the fill=NA is trying to get rid of the grey background 
                    #  around the symbols in the legend, but doesn't seem to be working.
                    #  Leaving it in for now to remind me to try to find some other 
                    #  way to get this to work.
                    
                guides (color = 
                    guide_legend (override.aes = list (size = 4, 
                                                       alpha = 1 
                                                       , fill = NA
                                                       ))) -> 
                        FN_fits_plot
                            
                show (FN_fits_plot)
                        
        }  #  end if - params$use_FN_dominant & params$VERBOSE_LM
             
            #----------
    
    if (params$use_FP_dominant & params$VERBOSE_LM)
        {
        FP_only_true_vs_pred_df = filter (full_true_vs_pred_df, dom_err_type == "FP")
        color_breaks_and_values = force_dom_err_type_colors (FP_only_true_vs_pred_df$dom_err_type)
        scale_color_breaks = color_breaks_and_values$breaks
        scale_color_values = color_breaks_and_values$values
        
        FP_only_true_vs_pred_df %>% 
            ggplot () + 
                geom_point (aes (x     = true_values, 
                                 y     = pred_values, 
                                 color = dom_err_type), 
    
                            shape = 15,    #".", 
                            size=0.5, 
                            alpha = alpha_level    #1
                            # shape="."
                            ) +  
                    facet_wrap (~ rs_method_name_fac, nrow = num_facet_wrap_rows) + 
                    geom_abline (intercept=0, slope=1) +     #, linetype, color, size
                    ggtitle (paste0 ("FP only TEST pred vs. true ", pred_value_name_display_str, 
                                     " for ", model_name_str, " using ", vars_used_str)) + 
                    theme (plot.title = element_text (hjust = 0.5)) +
            
                    #  If forcing colors, then manually scale the colors.  
                    #  Otherwise, use the default behavior.
                    #  To use an "if" statement inside these sets of statements 
                    #  separated by "+" signs, you have to put curly brackets around 
                    #  your test and result.
                { if (force_colors) scale_color_manual (breaks = scale_color_breaks, 
                                                        values = scale_color_values) } +
                
                { if (num_facets == 5) theme (legend.position = c(0.85, 0.25),
                                              legend.direction = "vertical") } + 
            
{ if (!is.na (x_min_on_plot) | !is.na(x_max_on_plot)) xlim (x_min_on_plot, x_max_on_plot) } + 
{ if (!is.na (y_min_on_plot) | !is.na(y_max_on_plot)) ylim (y_min_on_plot, y_max_on_plot) } + 
           
        #  Symbols on the plot itself are small and semi-transparent, 
        #  but in the legend that makes them nearly invisible.
        #  Override the symbol size and alpha values in the legend to 
        #  make the symbols much larger and not transparent at all.  
        #  Note that the fill=NA is trying to get rid of the grey background 
        #  around the symbols in the legend, but doesn't seem to be working.
        #  Leaving it in for now to remind me to try to find some other 
        #  way to get this to work.
        
                guides (color = 
                    guide_legend (override.aes = list (size = 4, 
                                                       alpha = 1 
                                                       , fill = NA
                                                       ))) -> 
                        FP_fits_plot
        
                    show (FP_fits_plot)
                        
                } #  end if - params$use_FP_dominant & params$VERBOSE_LM    
    
        #----------

        #  If coloring points by dominant error type, make sure that the 
        #  same color is always used for each type across all graphs.  
        #  If you don't do this and only one error type is appearing in the 
        #  current graph, it will take on whatever color is first in the 
        #  default list of colors.  This makes it harder to see at a glance 
        #  what kinds of points are being plotted in a set of graphs.  
        #  In particular, FNs show up as 2 different colors while FPs 
        #  are always the same color if you don't do this fix.
    
    color_breaks_and_values = force_dom_err_type_colors (full_true_vs_pred_df$dom_err_type)
    
    scale_color_breaks = color_breaks_and_values$breaks
    scale_color_values = color_breaks_and_values$values

    full_fits_plot = 
        ggplot (data = full_true_vs_pred_df) +  
                geom_point (aes (
                                 # x     = pred_values, 
                                 # y     = true_values, 
                                 x     = true_values, 
                                 y     = pred_values, 
                                 
                                 color = dom_err_type), 
                            shape = 15,    #".", 
                            size=0.5, 
                            alpha = alpha_level    #1
                            # shape="."
                            ) +  
                facet_wrap (~ rs_method_name_fac, nrow = num_facet_wrap_rows) + 
                geom_abline (intercept=0, slope=1) +     #, linetype, color, size
                ggtitle (paste0 (test_or_train_str, 
                                 " pred vs. true ", pred_value_name_display_str, 
                                 " for ", model_name_str, " using ", vars_used_str)) + 
                theme (plot.title = element_text (hjust = 0.5)) + 
    
                    #  If forcing colors, then manually scale the colors.  
                    #  Otherwise, use the default behavior.
                    #  To use an "if" statement inside these sets of statements 
                    #  separated by "+" signs, you have to put curly brackets around 
                    #  your test and result.
                { if (force_colors) scale_color_manual (breaks = scale_color_breaks, 
                                                        values = scale_color_values) } +
    
                { if (num_facets == 5) theme (legend.position = c(0.85, 0.25),
                                              legend.direction = "vertical") } + 
                        
                { if (num_facets == 4) theme (legend.position = "right",
                                              legend.direction = "vertical") } + 
      
{ if (!is.na (x_min_on_plot) | !is.na(x_max_on_plot)) xlim (x_min_on_plot, x_max_on_plot) } + 
{ if (!is.na (y_min_on_plot) | !is.na(y_max_on_plot)) ylim (y_min_on_plot, y_max_on_plot) } + 

                    #  Symbols on the plot itself are small and semi-transparent, 
                    #  but in the legend that makes them nearly invisible.
                    #  Override the symbol size and alpha values in the legend to 
                    #  make the symbols much larger and not transparent at all.  
                    #  Note that the fill=NA is trying to get rid of the grey background 
                    #  around the symbols in the legend, but doesn't seem to be working.
                    #  Leaving it in for now to remind me to try to find some other 
                    #  way to get this to work.
        
                guides (color = 
                      guide_legend (override.aes = list (size = 4, 
                                                         alpha = 1 
                                                         , fill = NA
                                                         ))) +

                geom_text (aes (x, y, label = R2_label), 
                           data=locs_R2, 
                           family="Times", fontface="italic", #lineheight=.03, 
                           size=3, hjust = 0, 
                           parse = T) + 

                geom_text (aes (x, y, label = rmse_label), 
                           data=locs_rmse, 
                           family="Times", fontface="italic", #lineheight=.03, 
                           size=3, hjust = 0, 
                           parse = T) + 
      
                { if (pred_value_name_display_str == "Cost Error") ylim (NA, 1.5) }
          
  
    cur_plot_name = paste0 ("full_fits.", test_or_train_str, ".predVsTrue.using.", 
                            pred_value_name_display_str, 
                            ".for.", model_name_str, 
                            ".Using.", vars_used_str)
    
    save_this_ggplot (full_fits_plot, cur_plot_name, params)
    
    return (full_fits_plot)
    }

#===============================================================================

fit_cost_err_frac <- function (rs_names_vec, 
                               train_x_df, test_x_df,                #  e.g., p3_test_x_df_probSize 
                               working_train_df, working_test_df,    #  e.g., p3_working_test_df 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params, 
                              
                  x_min_on_plot = NA,      
                  x_max_on_plot = NA, y_min_on_plot = NA, y_max_on_plot = NA,      
                                   
                  som_model = NULL, 
                  num_som_cells = NULL
                               )
    {
        #  Values specific to cost error plotting
        
    target_col_name = "rs_solution_cost_err_frac"
    pred_value_name_display_str = "Cost Error"

        #----------

        #  Values specific to either plotting all RS or just the non-greedy ones.
        
    if (params$exclude_greedy_rs_in_fit_plots)
        {
        R2_x_loc = -0.8 
        R2_y_loc = 2
        
        rmse_x_loc = -0.8 
        rmse_y_loc = 1.8
        
        } else
        {
        R2_x_loc = -0.8 
        R2_y_loc = 1.4    #####2    #####4.5    #10
        
        rmse_x_loc = -0.8 
        rmse_y_loc = 1.2    #####1.8    #####4    #9
        }
        
        #----------
    
train_and_test_full_fits_plot_and_data = 
    fit_to_target_var (rs_names_vec, 
                               train_x_df, test_x_df, 
                               working_train_df, working_test_df, 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params, 
                               
                              pred_value_name_display_str = pred_value_name_display_str,

                              R2_x_loc, 
                              R2_y_loc, 
                              
                              rmse_x_loc, 
                              rmse_y_loc, 
                              
                              target_col_name = target_col_name,  
                               
                  x_min_on_plot,      
                  x_max_on_plot, y_min_on_plot, y_max_on_plot, 
                                   
                  som_model = som_model, 
                  num_som_cells = num_som_cells, 
                  SOM_cost_err_frac = FALSE
                               )
    
return (train_and_test_full_fits_plot_and_data)
    }

#===============================================================================

fit_cost_err_mag <- function (rs_names_vec, 
                               train_x_df, test_x_df,                #  e.g., p3_test_x_df_probSize 
                               working_train_df, working_test_df,    #  e.g., p3_working_test_df 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params, 
                              
                  x_min_on_plot = NA,      
                  x_max_on_plot = NA, y_min_on_plot = NA, y_max_on_plot = NA,      
                                   
                  som_model = NULL, 
                  num_som_cells = NULL
                               )
    {
        #  Values specific to cost error plotting
        
    target_col_name = "signed_cost_err_mag"
    pred_value_name_display_str = "Signed Cost Error Mag"

        #----------

        #  Values specific to either plotting all RS or just the non-greedy ones.
        
    if (params$exclude_greedy_rs_in_fit_plots)
        {
        R2_x_loc = 0    #-0.8 
        R2_y_loc = 2400    #-100    #2
        
        rmse_x_loc = 0    #-0.8 
        rmse_y_loc = 2100    #-350    #1.8
        
        } else
        {
        R2_x_loc = 0    #-0.8 
        R2_y_loc = 2400    #-100    #1.4    #####2    #####4.5    #10
        
        rmse_x_loc = 0    #-0.8 
        rmse_y_loc = 2100    #-350    #1.2    #####1.8    #####4    #9
        }
        
        #----------
    
train_and_test_full_fits_plot_and_data = 
    fit_to_target_var (rs_names_vec, 
                               train_x_df, test_x_df, 
                               working_train_df, working_test_df, 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params, 
                               
                              pred_value_name_display_str = pred_value_name_display_str,

                              R2_x_loc, 
                              R2_y_loc, 
                              
                              rmse_x_loc, 
                              rmse_y_loc, 
                              
                              target_col_name = target_col_name,  
                               
                  x_min_on_plot,      
                  x_max_on_plot, y_min_on_plot, y_max_on_plot, 
                                   
                  som_model = som_model, 
                  num_som_cells = num_som_cells, 
                  SOM_cost_err_frac = FALSE
                               )
    
return (train_and_test_full_fits_plot_and_data)
    }

#===============================================================================

fit_rep_shortfall <- function (rs_names_vec, 
                               train_x_df, test_x_df, 
                               working_train_df, working_test_df, 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params,  
                               
                  x_min_on_plot = NA,      
                  x_max_on_plot = NA, y_min_on_plot = NA, y_max_on_plot = NA,      
                                   
                  som_model = NULL, 
                  num_som_cells = NULL
                               )
    {
        #  Values specific to rep shortfall plotting
        
    target_col_name = "rsr_COR_spp_rep_shortfall"
    pred_value_name_display_str = "Rep Shortfall"

    R2_x_loc = -0.1 
    R2_y_loc = 1.1
    
    rmse_x_loc = -0.1 
    rmse_y_loc = 0.95    #  0.78

        #----------
        
train_and_test_full_fits_plot_and_data = 
    fit_to_target_var (rs_names_vec, 
                               train_x_df, test_x_df, 
                               working_train_df, working_test_df, 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params, 
                               
                              pred_value_name_display_str = pred_value_name_display_str,

                              R2_x_loc, 
                              R2_y_loc, 
                              
                              rmse_x_loc, 
                              rmse_y_loc, 
                              
                              target_col_name = target_col_name,  
                               
                  x_min_on_plot,      
                  x_max_on_plot, y_min_on_plot, y_max_on_plot, 
                                   
                  som_model = som_model, 
                  num_som_cells = num_som_cells, 
                  SOM_cost_err_frac = FALSE
                               )
    
return (train_and_test_full_fits_plot_and_data)
    }

#===============================================================================

fit_rep_shortfall_mag <- function (rs_names_vec, 
                               train_x_df, test_x_df, 
                               working_train_df, working_test_df, 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params,  
                               
                  x_min_on_plot = NA,      
                  x_max_on_plot = NA, y_min_on_plot = NA, y_max_on_plot = NA,      
                                   
                  som_model = NULL, 
                  num_som_cells = NULL
                               )
    {
        #  Values specific to rep shortfall plotting
        
    target_col_name = "rep_shortfall_mag"
    pred_value_name_display_str = "Rep Shortfall Mag"

    R2_x_loc = 0    #-0.1 
    R2_y_loc = 18    #0.87
    
    rmse_x_loc = 0    # -0.1 
    rmse_y_loc = 16    #0.82    #  0.78

        #----------
      
train_and_test_full_fits_plot_and_data = 
    fit_to_target_var (rs_names_vec, 
                               train_x_df, test_x_df, 
                               working_train_df, working_test_df, 
                               train_aux_df, test_aux_df, 
                               vars_used_str, params, 
                               
                              pred_value_name_display_str = pred_value_name_display_str,

                              R2_x_loc, 
                              R2_y_loc, 
                              
                              rmse_x_loc, 
                              rmse_y_loc, 
                              
                              target_col_name = target_col_name,  
                               
                  x_min_on_plot,      
                  x_max_on_plot, y_min_on_plot, y_max_on_plot, 
                                   
                  som_model = som_model, 
                  num_som_cells = num_som_cells, 
                  SOM_cost_err_frac = FALSE
                               )
    
return (train_and_test_full_fits_plot_and_data)
    }

#===============================================================================

f_lm <- function (rs_name, 
                  pred_value_name_display_str, 
                  vars_used_str, 
                  model_name_str_suffix, 
                  train_y_vec, train_x_df, 
                  test_y_vec, test_x_df, 
                  train_aux_df, test_aux_df, 
                  params)
    {
    train_lm_model = lm (train_y_vec ~ ., 
                       data = train_x_df)
    
    if (params$show_lm_model_fit_coefficients_and_summary)
        {
        cat ("\n\n============start lm results for", rs_name, "===================\n\n")
        cat ("\nlm coef() for ", rs_name, "\n")
        print (coef (train_lm_model))    #x#    
        cat ("\n\nlm summary() for ", rs_name, "\n")
        print (summary (train_lm_model))    #x#    
        cat ("\n\n============end lm results for", rs_name, "===================\n\n")
        }
    
#    plot (train_lm_model, which=1:2)
if (params$VERBOSE_LM)    plot (train_lm_model, which=1)
### record the previous plot
#lm_resid_plot <- recordPlot()  

    #  2020 02 02 - BTL
    #  Removing this plotting of the qqplot of the lm() fit.
    #  It sometimes causes the code to crash saying something about no y values.
    #  For example, this is currently happening for ZL_Backwards on probSize 
    #  data for rep shortfall.
    #  Not sure why this happens, but I don't really need the plot, so I'm 
    #  getting rid of it.
    #      Just did another run with FN only and ZL_Backward says adj R2 is 
    #      NaN, so maybe that's what this is all about.  Maybe this is just 
    #      about having very small rep shortfall values when you have 
    #      FN-dominant input error.
#####  2020 02 02  #####    plot (train_lm_model, which=2)
### record the previous plot
#lm_qqplot <- recordPlot()

# lindia_scale.factor = 0.5
# #gg_diagnose (train_lm_model)
# show (gg_reshist (train_lm_model))
# show (gg_resfitted (train_lm_model, scale.factor = lindia_scale.factor))
# show (gg_qqplot (train_lm_model, scale.factor = lindia_scale.factor))

        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
#    test_eval_list = 
    train_and_test_eval_lists =

        ppe_for_train_and_test_for_one_RS (rs_name, 
                                
                                fitted_model                    = train_lm_model, 
                                model_name_str                  = "LM", 
                                model_name_str_suffix           = model_name_str_suffix,  ###  e.g., "PCA" 
                                train_true_values               = train_y_vec, 
                                test_true_values                = test_y_vec, 
                                pred_value_name_display_str     = pred_value_name_display_str,    #  e.g., "RS out err"
                                train_df                        = train_x_df, 
                                test_df                         = test_x_df, 
                                
                                train_aux_df                    = train_aux_df, 
                                test_aux_df                     = test_aux_df, 
                                
                                params                          = params, 
                                
                                vars_used_str                   = vars_used_str, 
                                must_specify_predictions_vector = FALSE)
    
        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
#    return (test_eval_list)
    return (train_and_test_eval_lists)
    }

#===============================================================================

##  Function to learn a random forest and plot feature importance.

#  The following web page provided the variable importance and cforest 
#  code and explains why you need to use particular methods for determining 
#  feature importance:
#      Be Aware of Bias in RF Variable Importance Metrics
#      Caleb Scheidel
#      Posted on Jun 20, 2018 
#      https://blog.methodsconsultants.com/posts/be-aware-of-bias-in-rf-variable-importance-metrics/

library (party)

create_crfplot <- function (rf, conditional = TRUE, var_names)
  {
  imp <- rf %>%
    varimp (conditional = conditional) %>% 
    as_tibble () %>% 
    rownames_to_column ("Feature") %>% 
    rename (Importance = value)
  
  p <- ggplot (imp, aes(x = reorder (Feature, Importance), y = Importance)) +
       geom_bar (stat = "identity", fill = "#53cfff", width = 0.65) +
       coord_flip () + 
       theme_light (base_size = 20) +
       theme(axis.title.x = element_text(size = 15, color = "black"),
             axis.title.y = element_blank(),
             axis.text.x  = element_text(size = 15, color = "black"),
             axis.text.y  = element_text(size = 15, color = "black")) 
  
  cat ("\n\nFeature IDs:\n")
  feature_names_and_IDs = data.frame (ID=imp$Feature, 
                                      var_name=var_names, 
                                      importance=imp$Importance)
  print (feature_names_and_IDs)
  cat ("\n\nSORTED feature IDs:\n")
  print (arrange (feature_names_and_IDs, desc(importance)))
  cat ("\n")
  
  return(p)
  }

#===============================================================================

  #  Function to fit random forest model using the party package.

  #  2019 10 28 - This no longer works for some unknown reason.  
  #  It works if you move the code up to the chunk level and have it inline, 
  #  but if it's down here in a function, the cforest() call fails 
  #  with a message that says:
  #     object 'train_y_vec' not found
  #  This happens even though you can put a write statement immediately 
  #  before the cforest() call and print out values of train_y_vec.  

  #  At the moment, I'm not using the random forests, so I'm going to just 
  #  move on to other things, but leave the code here in case I need it later.

f_rf_party <- function (rs_name, 
                        pred_value_name_display_str, 
                  vars_used_str, 
                  model_name_str_suffix, 
                  train_y_vec, train_x_df, 
                  test_y_vec, test_x_df, 
               train_aux_df, test_aux_df, 
                  params)
  {
  train_rf_model <- cforest (train_y_vec ~ .,
                             data = train_x_df,
                             control = cforest_unbiased (mtry = 2, ntree = 500))

      # not conditional
    show (create_crfplot(train_rf_model, conditional = FALSE, 
                         var_names = colnames (train_x_df)))
  
    summary (train_rf_model)
#plot (train_rf_model)
    
        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #test_eval_list = 
    train_and_test_eval_lists = 
        ppe_for_train_and_test_for_one_RS (rs_name, 
                                
                                fitted_model                    = train_rf_model, 
                                model_name_str                  = "RF party", 
                                model_name_str_suffix           = model_name_str_suffix,  ###  e.g., "PCA" 
                                train_true_values               = train_y_vec, 
                                test_true_values                = test_y_vec, 
                                pred_value_name_display_str     = pred_value_name_display_str,    #  e.g., "RS out err"
                                train_df                        = train_x_df, 
                                test_df                         = test_x_df, 
                                
                                train_aux_df                    = train_aux_df, 
                                test_aux_df                     = test_aux_df, 
                                
                                params                          = params, 
                                
                                vars_used_str                   = vars_used_str, 
                                must_specify_predictions_vector = FALSE, 
                      
                      rf_from_party = TRUE)
        
        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #return (test_eval_list)
    return (train_and_test_eval_lists)
    }

#===============================================================================

##  Function to learn random forest using ranger instead of cforest

f_rf_ranger <- function (rs_name, 
                         pred_value_name_display_str, 
                         vars_used_str, 
                         model_name_str_suffix, 
                         train_y_vec, train_x_df, 
                         test_y_vec, test_x_df, 
                         train_aux_df, test_aux_df, 
                         params)
    {
    train_rf_model = ranger (train_y_vec ~ ., 
                           data = train_x_df, 
                    importance = "impurity", 
                           write.forest = TRUE)
    
    summary (train_rf_model)
#plot (train_rf_model)
    
#importance (train_rf_model)

        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #test_eval_list = 
    train_and_test_eval_lists = 
        ppe_for_train_and_test_for_one_RS (rs_name, 
                                
                                fitted_model                    = train_rf_model, 
                                model_name_str                  = "RF ranger",  
                                model_name_str_suffix           = model_name_str_suffix,  ###  e.g., "PCA" 
                                train_true_values               = train_y_vec, 
                                test_true_values                = test_y_vec, 
                                pred_value_name_display_str     = pred_value_name_display_str,    #  e.g., "RS out err"
                                train_df                        = train_x_df, 
                                test_df                         = test_x_df, 
                                
                                train_aux_df                    = train_aux_df, 
                                test_aux_df                     = test_aux_df, 
                                
                                params                          = params, 
                                
                                vars_used_str                   = vars_used_str, 
                                must_specify_predictions_vector = TRUE, 
                      
                      rf_from_party = FALSE)
    
        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #return (test_eval_list)
    return (train_and_test_eval_lists)
    }

#===============================================================================

f_glmnet_caret <- function (pred_value_name_display_str, 
                      vars_used_str, 
                      model_name_str_suffix, 
                      train_y_vec, train_x_df, 
                      test_y_vec, test_x_df, 
                      params)
    {
    set.seed (seed2)
    
    train_x_mat = as.matrix (train_x_df)
    test_x_mat  = as.matrix (test_x_df)
    
    train_control <- trainControl (method = "cv", number = 10)
    
    caret_mod <- train (x = train_x_mat,    #ames_train_x,
                        y = train_y_vec,    #ames_train_y,
                        method = "glmnet",
                       #preProc = c("center", "scale", "zv", "nzv"),
                        trControl = train_control,
                        tuneLength = 10)
    
    cat ("\n\n>>>>> glment model:\n")
    print (caret_mod)
    
        # Model coefficients
    print (coef(caret_mod$finalModel, caret_mod$bestTune$lambda))
    
    predictions <- predict (caret_mod, newdata=test_x_mat)
    
    rmse = RMSE (predictions, test_y_vec)
    cat ("\n\nglmnet rmse = ", rmse)
    
    rsquared = R2 (predictions, test_y_vec)
    cat ("\nglmnet  R^2 = ", rsquared, "\n\n")
    }

#===============================================================================

##  UC Business web page version of glm stuff

#  From:  http://uc-r.github.io/regularized_regression#elastic

    #  The web page's code calls tidy(), but it doesn't work, 
    #  so I've made a little routine to replace what tidy() appears to have 
    #  been doing for them by:
    #       - Adding a column called "row" containing row names since 
    #         row names are lost in conversion to tibble and the next 
    #         line in this chain of commands expects to have a column 
    #         called "row" containing the names of the rows
    #       - Making sure that the column containing coefficients is 
    #         names "value", since that too is expected downstream
    #       - Converting the ridge regression output to a tibble
    #tidy() %>%

replacement_for_tidy <- function (ridge_reg)
    {
    ridge_reg %>% 
        as.matrix() %>%    
        as.data.frame() -> coeff_df

    coeff_df$row       = rownames(coeff_df)
    colnames(coeff_df) = c("value", "row")
    
    return (as_tibble (coeff_df))
    }

#-------------------------------------------------------------------------------

    #  The UC in this name is just a very short way to identify this as the 
    #  code that was copied/derived from a UC web page:
    #      http://uc-r.github.io/regularized_regression#elastic

f_glmnet_UC <- function (rs_name,
                         pred_value_name_display_str, 
                         vars_used_str, 
                         model_name_str_suffix, 
                         train_y_vec, train_x_df, 
                         test_y_vec, test_x_df, 
               train_aux_df, test_aux_df, 
                         params, 
                         VERBOSE_GLMNET_UC = FALSE, 
                         SHOW_ALL_GLMNET_UC_PLOTS = FALSE)
    {
  #library (rsample)  # data splitting 
  #library (glmnet)   # implementing regularized regression approaches
  #library (dplyr)    # basic data manipulation procedures
  #library (ggplot2)  # plotting
  
  set.seed (seed2)

      #  glmnet requires the input data to be a matrix instead of data frame.
  train_x_mat = as.matrix (train_x_df)
  test_x_mat  = as.matrix (test_x_df)
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before A\n")  
      # Apply Ridge regression to data WITHOUT cross-validation
  cur_ridge <- glmnet (x = train_x_mat, 
                       y = train_y_vec, 
                       alpha = 0    #  0 => ridge regression only
                       , standardize = FALSE
                       )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after A\n")  
  
if (SHOW_ALL_GLMNET_UC_PLOTS)    plot (cur_ridge, xvar = "lambda")
  
      # lambdas applied to penalty parameter
          #  $lambda lists all lambdas applied by glmnet in its search 
          #  for the best one
  cur_ridge$lambda %>% head ()
  
      # coefficients for the largest and smallest lambda parameters
          #  Ignoring for the moment since the column names are not for my data.
  #coef(cur_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 100]
  #coef(cur_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 1] 
  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before B\n")  
      # Apply Ridge regression to cur data WITH cross-validation
  cur_ridge <- cv.glmnet (x = train_x_mat,
                          y = train_y_vec, 
                          alpha = 0    #  0 => ridge regression only
                          , standardize = FALSE
                          )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after B\n")  
  
      # plot results
if (SHOW_ALL_GLMNET_UC_PLOTS)   plot (cur_ridge)
  
  min (cur_ridge$cvm)       # minimum MSE
  cur_ridge$lambda.min      # lambda for this min MSE
  cur_ridge$cvm [cur_ridge$lambda == cur_ridge$lambda.1se]  # 1 st.error of min MSE
  cur_ridge$lambda.1se      # lambda for this MSE
  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before C\n")  
      #  Repeat original ridge regression WITHOUT cross-validation (NOT SURE WHY)
  cur_ridge_min <- glmnet (x = train_x_mat, 
                           y = train_y_vec, 
                           alpha = 0    #  0 => ridge regression only
                          , standardize = FALSE
                          )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after C\n")  

if (SHOW_ALL_GLMNET_UC_PLOTS) 
{
  plot (cur_ridge_min, xvar = "lambda")
  abline (v = log (cur_ridge$lambda.1se), col = "red", lty = "dashed")
}

if (SHOW_ALL_GLMNET_UC_PLOTS) 
{
show (
  coef (cur_ridge, s = "lambda.1se") %>%
      replacement_for_tidy () %>% 
      filter (row != "(Intercept)") %>%
      top_n (25, wt = abs (value)) %>%
        
      ggplot (aes (value, reorder (row, value))) +
      geom_point () +
      ggtitle ("Top 25 influential variables") +
      xlab ("Coefficient") +
      ylab (NULL)
    )
}

  #----------
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before D\n")  
      ## Apply lasso regression to cur data WITHOUT cross-validation
  cur_lasso <- glmnet (x = train_x_mat, 
                       y = train_y_vec, 
                       alpha = 1    #  1 => lasso regression only
                        , standardize = FALSE
                       )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after D\n")  

if (SHOW_ALL_GLMNET_UC_PLOTS)   plot (cur_lasso, xvar = "lambda")
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before E\n")  
  
      # Redo Lasso regression to cur data WITH cross-validation
  cur_lasso <- cv.glmnet (x = train_x_mat,
                          y = train_y_vec,
                          alpha = 1    #  1 => lasso regression only
                           , standardize = FALSE
                          )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after E\n")  

      # plot results
if (SHOW_ALL_GLMNET_UC_PLOTS)   plot (cur_lasso)
  
  min (cur_lasso$cvm)       # minimum MSE
  cur_lasso$lambda.min      # lambda for this min MSE
  cur_lasso$cvm[cur_lasso$lambda == cur_lasso$lambda.1se]  # 1 st.error of min MSE
  cur_lasso$lambda.1se      # lambda for this MSE
  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before F\n")  
      #  Repeat original lasso regression WITHOUT cross-validation (NOT SURE WHY)
  cur_lasso_min <- glmnet (x = train_x_mat,
                           y = train_y_vec,
                           alpha = 1    #  1 => lasso regression only
                            , standardize = FALSE
                           )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after F\n")  
  
if (SHOW_ALL_GLMNET_UC_PLOTS) 
{
  plot (cur_lasso_min, xvar = "lambda")
  abline (v = log(cur_lasso$lambda.min), col = "red", lty = "dashed")
  abline (v = log(cur_lasso$lambda.1se), col = "red", lty = "dashed")

show (
  coef (cur_lasso, s = "lambda.1se") %>%
      replacement_for_tidy () %>% 
      filter (row != "(Intercept)") %>%
      ggplot (aes (value, reorder (row, value), color = value > 0)) +
      geom_point (show.legend = FALSE) +
      ggtitle ("All var coeffs for non-cv lasso") +
      xlab ("Coefficient") +
      ylab (NULL)
)

show (
  coef (cur_lasso, s = "lambda.1se") %>%
      replacement_for_tidy () %>% 
      filter (row != "(Intercept)") %>%
    filter (abs(value) > 0.005) %>%
      ggplot (aes (value, reorder (row, value), color = value > 0)) +
      geom_point (show.legend = FALSE) +
      ggtitle ("Coeffs of most influential vars (abs (value) > 0.005) for non-cv lasso") +
      xlab ("Coefficient") +
      ylab (NULL)
)
}


      # minimum Ridge MSE
  min (cur_ridge$cvm)
  
      # minimum Lasso MSE
  min (cur_lasso$cvm)
  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before G\n")  
  lasso    <- glmnet (train_x_mat, train_y_vec, alpha = 1.0 
                      , standardize = FALSE
                      ) 
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after G\n")  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before H\n")  
  elastic1 <- glmnet (train_x_mat, train_y_vec, alpha = 0.25                       ,
                      , standardize = FALSE
                      ) 
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after H\n")  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before I\n")  

  elastic2 <- glmnet (train_x_mat, train_y_vec, alpha = 0.75                       ,
                      , standardize = FALSE
                      ) 
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after I\n")  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before J\n")  
  ridge    <- glmnet (train_x_mat, train_y_vec, alpha = 0.0
                      , standardize = FALSE
                      )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after J\n")  

if (SHOW_ALL_GLMNET_UC_PLOTS) 
{
  par (mfrow = c(2, 2))    #, mar = c(6, 4, 6, 2) + 0.1)
  plot (lasso, xvar = "lambda", main = "Lasso (Alpha = 1)\n\n\n")
  plot (elastic1, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
  plot (elastic2, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")
  plot (ridge, xvar = "lambda", main = "Ridge (Alpha = 0)\n\n\n")
}
  
      # maintain the same folds across all models
  fold_id <- sample (1:10, size = length (train_y_vec), replace=TRUE)
  
  # search across a range of alphas
  tuning_grid <- tibble::tibble (alpha      = seq (0, 1, by = .1),
                                 mse_min    = NA,
                                 mse_1se    = NA,
                                 lambda_min = NA,
                                 lambda_1se = NA)
  
  for(i in seq_along (tuning_grid$alpha)) 
      {
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before K, i = ", i, "\n")  
      
          # fit CV model for each alpha value
      fit <- cv.glmnet (train_x_mat, train_y_vec, 
                        alpha = tuning_grid$alpha[i], 
                        foldid = fold_id
                        , standardize = FALSE
                       )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after K, i = ", i, "\n")  
      
          # extract MSE and lambda values
      tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
      tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
      tuning_grid$lambda_min[i] <- fit$lambda.min
      tuning_grid$lambda_1se[i] <- fit$lambda.1se
      }

if (SHOW_ALL_GLMNET_UC_PLOTS) 
{
  cat ("\n\ntuning_grid = \n")
  print (tuning_grid)
}

  tuning_grid %>%
      mutate (se = mse_1se - mse_min) %>%
      ggplot( aes (alpha, mse_min)) +
      geom_line (size = 2) +
      geom_ribbon (aes (ymax = mse_min + se, ymin = mse_min - se), alpha = .25) +
      ggtitle ("MSE ± one standard error")
  
      #  Predicting
      #  Once you have identified your preferred model, you can simply use 
      #  predict to predict the same model on a new data set. 
      #  The only caveat is you need to supply predict an s parameter with 
      #  the preferred models lambda value. 
      #  For example, here we create a lasso model, which provides me a 
      #  minimum MSE of 0.022. I use the minimum lambda value to predict 
      #  on the unseen test set and obtain a slightly lower MSE of 0.015.
  
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> before L\n")  
      # some best model
  cv_lasso   <- cv.glmnet (train_x_mat, train_y_vec, alpha = 1.0
                           , standardize = FALSE
                           )
if (VERBOSE_GLMNET_UC) cat ("\n>>>>>>>>>> after L\n")  
  min (cv_lasso$cvm)
  
      # predict
  
  pred_train <- predict (cv_lasso, s = cv_lasso$lambda.min, train_x_mat)
if (VERBOSE_GLMNET_UC) 
{
  train_mse_value = mean ((train_y_vec - pred_train)^2)
  cat ("\nmse for train prediction = ", train_mse_value, "\n")
  cat ("\nRMSE for train prediction = ", sqrt (train_mse_value), "\n")
}
  
  pred_test <-  predict (cv_lasso, s = cv_lasso$lambda.min, test_x_mat)
if (VERBOSE_GLMNET_UC) 
{
  test_mse_value = mean ((test_y_vec - pred_test)^2)
  cat ("\nmse for test prediction = ", test_mse_value, "\n")
  cat ("\nRMSE for test prediction = ", sqrt (test_mse_value), "\n")
}

  
show (
  coef (cv_lasso, s = "lambda.1se") %>%
      replacement_for_tidy () %>% 
      filter (row != "(Intercept)") %>%
      ggplot (aes (value, reorder (row, value), color = value > 0)) +
      geom_point (show.legend = FALSE) +
      ggtitle ("All var coeffs for cv elastic net") +
      xlab ("Coefficient") +
      ylab (NULL)
)

show (
  coef (cv_lasso, s = "lambda.1se") %>%
      replacement_for_tidy () %>% 
      filter (row != "(Intercept)") %>%
    filter (abs(value) > 0.005) %>%
      ggplot (aes (value, reorder (row, value), color = value > 0)) +
      geom_point (show.legend = FALSE) +
      ggtitle ("Coeffs of most influential vars (abs (value) > 0.005) for cv elastic net") +
      xlab ("Coefficient") +
      ylab (NULL)
)


        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #test_eval_list = 
    train_and_test_eval_lists = 
        ppe_for_train_and_test_given_preds_for_one_RS (
                    rs_name,
                                                
                    model_name_str = "Glmnet",                 #  e.g., "LM" 
                    model_name_str_suffix,          #  e.g., "PCA" 
                    
                    train_true_values = train_y_vec,              #  e.g., "rsr_COR_euc_out_err_frac" 
                    test_true_values  = test_y_vec, 
              
                    pred_value_name_display_str,    #  e.g., "RS out err"
                    num_predictors = ncol (train_x_mat),                 #  num cols in x data frames
                    vars_used_str = vars_used_str, 
              
                    predicted_trains = pred_train, 
                    predicted_tests = pred_test, 
                    
                train_aux_df, 
                test_aux_df, 
                    
                    params, 
              
                    must_specify_predictions_vector = FALSE
                  )
    
        #  2023 01 01 - BTL
        #  New code to return both test and train instead of just test.
    #return (test_eval_list)
    return (train_and_test_eval_lists)
    }

#===============================================================================

build_feature_set_specific_test_and_train <- function (working_train_df, 
                                                              working_test_df, 
                                                              inVars, 
                                                              params, 
                                                              include_median_redundancies=FALSE)
    {
    ## Define data sets according to the given input feature set
    
    reduced_test_and_train_dfs_list =
        build_specific_test_and_train_via_col_name_strings (working_train_df,
                                                            working_test_df,
                                                            inVars, 
                                                            include_median_redundancies = FALSE)
    
    #----------
    
    p3_train_x_df = reduced_test_and_train_dfs_list$train_x_df
    p3_test_x_df  = reduced_test_and_train_dfs_list$test_x_df
    
    if (params$write_tibs_to_csv) 
        {
        write_a_tib_to_csv_file_using_params (p3_train_x_df, 
                                              "p3_train_x_df", 
                                              params, params$file_type_to_write)
        
        write_a_tib_to_csv_file_using_params (p3_test_x_df, 
                                              "p3_test_x_df", 
                                              params, params$file_type_to_write)
      }
    
    if (params$VERBOSE_LM_CATS) cat ("\n\np3_test_x_df column names:\n")
    if (params$VERBOSE_LM_CATS)     names (p3_test_x_df)
    #cat ("\n\nworking_test_df column names:\n")
    #names (working_test_df)

    return (list (p3_train_x_df = p3_train_x_df, 
                  p3_test_x_df = p3_test_x_df))
    }

#===============================================================================

## Function to build test and train data frames via col name strings
## 
## Same as build_specific_test_and_train() except that it passes in a vector 
## of column name strings instead of the name of a function to call.
## Added 2022 12 29 - BTL.

#  Use this function for building any of the test and train data frame pairs, 
#  since the logic is the same for all of them.  The only difference between 
#  them is what columns are selected for inclusion. 
#  
#  The include_median_redundancies argument is required to call the 
#  add_median_redundancies() function if the resulting data sets need to 
#  contain those two columns as well.  It defaults to not calling that 
#  function.
#  This is necessary to handle issues with zero variance in the redundancies 
#  when looking only at FN data.  This is explained where the 
#  include_median_redundancies flag is defined earlier in the code.

build_specific_test_and_train_via_col_name_strings <- 
            function (working_train_df, 
                      working_test_df, 
                  col_name_strings,    #func_for_col_selection, 
                      include_median_redundancies = FALSE)
    {
        #  Call the function passed in here to select only the columns 
        #  specific to the data set you want for the specific fitting test, 
        #  e.g., only columns related to problem size or only related to 
        #  graph variables.
  
    # train_x_df  = func_for_col_selection (working_train_df, 
    #                                      include_median_redundancies)
    train_x_df = select_cols_for_learning (working_train_df, 
                                           col_name_strings, 
                                           include_median_redundancies)
    
    # test_x_df  = func_for_col_selection (working_test_df, 
    #                                      include_median_redundancies)
    test_x_df  = select_cols_for_learning (working_test_df, 
                                           col_name_strings, 
                                           include_median_redundancies)
    
    # cat ("\n\nis_grouped_df (train_x_df) = ", 
    #      is_grouped_df (train_x_df), sep='')
    # 
    # cat ("\n\nis_grouped_df (test_x_df) = ", 
    #      is_grouped_df (test_x_df), sep='')

    return (list (train_x_df = train_x_df, 
                  test_x_df  = test_x_df))
    }

#===============================================================================

select_cols_for_learning <- function (working_df, 
                                      col_name_strings, 
                                      include_median_redundancies=NULL)
    {
    reduced_df <- select (working_df, 
                          rs_method_name,    #  All learners need to know RS
                    #rsr_UUID,    #  2023 01 02 - BTL testing
                          all_of (col_name_strings))
    
    if (include_median_redundancies)
        reduced_df = add_median_redundancies (working_df, reduced_df)

    return (reduced_df)
    }

#===============================================================================
#
#  2022 12 31 - BTL
#  I think that this function is no longer used.  I think that it has been 
#  replaced by:  build_specific_test_and_train_via_col_name_strings().  
#  I think that this function was a first cut before I abstracted out the 
#  calling of specific functions that selected particular sets of features.  
#  Now I just hand in a vector of column name strings instead of handing in 
#  the name of the custom function that selected the particular set of features.
#  
#-------------------------------------------------------------------------------

## Function to build test and train data frames

#  Use this function for building any of the test and train data frame pairs, 
#  since the logic is the same for all of them.  The only difference between 
#  them is what columns are selected for inclusion. 
#
#  To use this function, you just hand it a function that selects the 
#  specific columns of interest for the desired data set.  These functions 
#  have been named with a consistent form of:  select_*_df_cols(), where 
#  the "*" refers to the data items of interest, e.g., prob_size or graph.
#  That selection routine needs to do two things:  
#      1)  select the appropriate columns
#      2)  call the add_median_redundancies() function if the resulting 
#          data sets need to contain those two columns as well
#          (This is necessary to handle issues with zero variance in the 
#           redundancies when looking only at FN data.  This is explained 
#           where the include_median_redundancies flag is defined 
#           earlier in the code.)

build_specific_test_and_train <- function (working_train_df, 
                                           working_test_df, 
                                      func_for_col_selection, 
                                           include_median_redundancies = FALSE)
    {
        #  Call the function passed in here to select only the columns 
        #  specific to the data set you want for the specific fitting test, 
        #  e.g., only columns related to problem size or only related to 
        #  graph variables.
  
    train_x_df = func_for_col_selection (working_train_df, 
                                         include_median_redundancies)
    test_x_df  = func_for_col_selection (working_test_df, 
                                         include_median_redundancies)
    
    # cat ("\n\nis_grouped_df (train_x_df) = ", 
    #      is_grouped_df (train_x_df), sep='')
    # 
    # cat ("\n\nis_grouped_df (test_x_df) = ", 
    #      is_grouped_df (test_x_df), sep='')

    return (list (train_x_df = train_x_df, 
                  test_x_df  = test_x_df))
    }

#===============================================================================

add_to_full_fitting_scores <- function (all_fitting_scores_df, 
                                        full_measured_values_df, 
                                        train_or_test, 
                                        vars_used_str, 
                                        fitting_model_str, 
                                        measure_name_str) 
    {
#                 list (rs_method_name = cur_rs_name, 
# #                      model_name_str = cur_test_eval_list$model_name_str, 
#                       rmse           = cur_test_eval_list$rmse_value, 
#                       R2             = cur_test_eval_list$R2, 
#                       adj_R2         = cur_test_eval_list$adj_R2)

    cur_scores_df = 
        data.frame (train_or_test     = train_or_test, 
                    fitting_model_str = fitting_model_str, 
                    vars_used_str     = vars_used_str, 
                    measure_name_str  = measure_name_str, 
                    rs_method_name    = full_measured_values_df$rs_method_name, 
                    rmse              = full_measured_values_df$rmse, 
                    R2                = full_measured_values_df$R2, 
                    adj_R2            = full_measured_values_df$adj_R2)
    
    
    all_fitting_scores_df = rbind (all_fitting_scores_df, cur_scores_df)

    return (all_fitting_scores_df)
    }    
  
#===============================================================================

fit_and_predict_output_error_using_feature_set <- 
    function (rs_method_names_list, 
              train_x_df,    # p3_train_x_df_probSize,
              test_x_df,    # p3_test_x_df_probSize, 
              working_train_df,    # p3_working_train_df, 
              working_test_df,    # p3_working_test_df, 
              train_aux_df,    # p3_train_aux_df, 
              test_aux_df,    # p3_test_aux_df, 
              train_df__before_any_preprocessing,    # working_train_df__before_any_preprocessing
              test_df__before_any_preprocessing,    # working_test_df__before_any_preprocessing
              params, 
              
              all_fitting_scores_df, 
                               
                  x_min_on_plot,      
                  x_max_on_plot, y_min_on_plot, y_max_on_plot,      
                                   
                  fitting_func,    # fit_rep_shortfall
              perf_metric_name_for_file_name_str,    # "abs_rep_shortfall_resid"
             
              vars_used_str,    #  "probSize" 
              inVars,    # probSize_inVars = c("rsp_num_occupied_PUs", "rsp_num_spp")
              source_string,     # "probSize_lm", 
              display_train_as_final_pred_using_plot_as_final_pred_using_plot
              )
    {
        #--------------------------------------------------------------
        #  Fit training data and evaluate fit on both train and test.  
        #--------------------------------------------------------------

    train_and_test_full_fits_plot_and_data = 
#        fit_rep_shortfall (rs_method_names_list, 
        fitting_func (rs_method_names_list, 
                      train_x_df, test_x_df, 
                      working_train_df, working_test_df, 
                      train_aux_df, test_aux_df, 
                              #test_df__before_any_preprocessing, 
                      vars_used_str, 
                      params,  
                      
                  x_min_on_plot,      
                  x_max_on_plot, y_min_on_plot, y_max_on_plot
 
                              # ,            
                              # som_model = som_model, 
                              # num_som_cells = num_som_cells
                     )
## 2024 02 03  ##browser()
    train_full_fits_plot_and_data = 
        train_and_test_full_fits_plot_and_data$train_full_fits_plot_and_data
    
    test_full_fits_plot_and_data = 
        train_and_test_full_fits_plot_and_data$test_full_fits_plot_and_data
    
        #-----------------------------------------------------------
        #  Add fitting scores for both train and test to full set.
        #-----------------------------------------------------------

    all_fitting_scores_df = 
        add_to_full_fitting_scores (all_fitting_scores_df, 
                                    train_full_fits_plot_and_data$full_measured_values_df, 
                                    "TRAIN", 
                                    vars_used_str, 
                                    params$fitting_model_str, 
                                    perf_metric_name_for_file_name_str)

    all_fitting_scores_df = 
        add_to_full_fitting_scores (all_fitting_scores_df, 
                                    test_full_fits_plot_and_data$full_measured_values_df, 
                                    "TEST", 
                                    vars_used_str, 
                                    params$fitting_model_str, 
                                    perf_metric_name_for_file_name_str)

        #------------------------------------------------
        #  Write MATILDA files for both train and test.
        #------------------------------------------------

            #  2023 12 31 - BTL
            #  Adding flag to ignore writing matilda files because when I 
            #  added code to include pca variables in the data the matilda 
            #  code crashed.  Not sure why at the moment.  Since I'm not 
            #  using the matilda output lately, I'm just going to set this 
            #  to FALSE to get things working again.  Since the pca variables 
            #  don't help much, that code will probably disappear at some 
            #  point and this can be set back to TRUE to get the matilda 
            #  code executing again.  

    write_matilda_files = FALSE
    if (write_matilda_files)
        {
        write_matilda_file_for_abs_residuals (train_df__before_any_preprocessing,
                                              inVars,
                                              train_full_fits_plot_and_data,
                                              params,
                                              source_string,
                                              perf_metric_name_for_file_name_str,
                                              test_train_string = "TRAIN"
                                             )
    
        write_matilda_file_for_abs_residuals (test_df__before_any_preprocessing,
                                              inVars,
                                              test_full_fits_plot_and_data,
                                              params,
                                              source_string,
                                              perf_metric_name_for_file_name_str,
                                              test_train_string = "TEST"
                                             )
        }
    
        #--------------------------------------------------------------------
        #  Display specified results for all reserve selectors in one plot.
        #--------------------------------------------------------------------

## 2024 02 03  ##    if (display_train_as_final_pred_using_plot)
## 2024 02 03  ##        {
## 2024 02 03  ##        show (train_full_fits_plot_and_data$full_fits_plot)
## 2024 02 03  ##        } else
## 2024 02 03  ##       {
## 2024 02 03  ##        show (test_full_fits_plot_and_data$full_fits_plot)
## 2024 02 03  ##        }
    
        #--------------------------------------------------------------------
        #  Bundle results and corresponding plot
        #  ## 2024 02 03  ##
        #--------------------------------------------------------------------

    if (display_train_as_final_pred_using_plot)
        {
        full_fits_plot = train_full_fits_plot_and_data$full_fits_plot
        } else
        {
        full_fits_plot = test_full_fits_plot_and_data$full_fits_plot
        }
    
   all_fitting_scores_and_full_fits_plot = 
      list (all_fitting_scores_df = all_fitting_scores_df, 
            full_fits_plot = full_fits_plot)
    
## 2024 02 03  ##    return (all_fitting_scores_df)
    return (all_fitting_scores_and_full_fits_plot)
    }

#===============================================================================

