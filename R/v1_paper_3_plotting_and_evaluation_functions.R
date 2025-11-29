#===============================================================================
#
#               v1_paper_3_plotting_and_evaluation_functions.R
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

#  Define functions to plot model fit results against test and train data

#===============================================================================

    #  Wrapper function for ppe_for_train_and_test_given_preds() to simplify 
    #  use for commonly used models like lm().  If the model you're using 
    #  only requires the 2 common arguments for predict(), i.e., the model and 
    #  the data set, then you can call this function, which will call predict() 
    #  for you and hand the results to the main function that this wraps.
    #
    #  Note that "ppe" is short for "plot, predict, and evaluate".

ppe_for_train_and_test <- 
    function (rs_name, 
              
              fitted_model, 
                model_name_str,                 #  e.g., "LM" 
                model_name_str_suffix,          #  e.g., "PCA" 
              
        train_true_values,    ###  pred_col_name_str,              #  e.g., "rsr_COR_euc_out_err_frac" 
        test_true_values, 
        
                pred_value_name_display_str,    #  e.g., "RS out err"
                train_df, 
                test_df, 
        
                train_aux_df, 
                test_aux_df, 

                params, 
        
                vars_used_str, 
        
              must_specify_predictions_vector, 
                  
    lm_resid_plot = lm_resid_plot, 
    lm_qqplot = lm_qqplot, 
        
        rf_from_party = TRUE 
              )
    {
        #  Get predicted values from the rf.
        #  When running predict() from the 'party' package, I was getting 
        #  the following error for no apparent reason:
        #       Error in OOB && is.null(newdata) : invalid 'x' type in 'x && y'
        #  Google led me to the following web page, where it said that you need 
        #  to add the argument "OOB=TRUE":
        #       https://groups.google.com/forum/#!topic/rattle-users/33AHXWrP5Vc
      
    if (rf_from_party)
      {
      predicted_trains = predict (fitted_model, train_df, OOB=TRUE)
      predicted_tests  = predict (fitted_model, test_df, OOB=TRUE)
      } else
      {
      predicted_trains = predict (fitted_model, train_df)
      predicted_tests  = predict (fitted_model, test_df)
      }

    test_eval_list = 
        ppe_for_train_and_test_given_preds (rs_name, 
          #fitted_model, 
                                            model_name_str,                 #  e.g., "LM" 
                                            model_name_str_suffix,          #  e.g., "PCA" 
                                          
                                    train_true_values,  
                                    test_true_values, 
                                    
                                            pred_value_name_display_str,    #  e.g., "RS out err"
    num_predictors = ncol (train_df),                                 
          #train_df, 
          #test_df, 
                                            vars_used_str, 
                                    predicted_trains, 
                                    predicted_tests, 
    
                    train_aux_df, 
                    test_aux_df, 
    
                    params, 
    
                                            must_specify_predictions_vector, 
    
    lm_resid_plot = lm_resid_plot, 
    lm_qqplot = lm_qqplot 

                                    
                )
    
    return (test_eval_list)
    }

#===============================================================================

compute_r.squared <- function (actual_vec, predicted_vec)
  {
  r.squared <- 1 - (sum ((actual_vec - predicted_vec) ^ 2) / 
                    sum ((actual_vec - mean (actual_vec)) ^ 2))
  }

#===============================================================================

compute_adj.r.squared <- function (R2, 
                                   num_observations, 
                                   num_predictors, 
                                   df.int=1)
  {
  
  #rdf = n - p - 1
  residual_deg_of_freedom = num_observations - num_predictors - 1
  
  #adj.r.squared <- 1 - (1 - R2) * ((n - df.int)/rdf)
  adj.r.squared <- 1 - (1 - R2) * ((num_observations - df.int)/residual_deg_of_freedom)
  }

#===============================================================================

eval_model_on_train_or_test_data <- function (test_or_train_string, #  "TEST" or "TRAIN"
                                              true_values_vec, 
                                              predicted_vec, 
                                              num_predictors, 
                                              
                                  dom_err_type, 

#  TEMPORARY
                                              must_specify_predictions_vector
                                             )
    {
#  TEMPORARY - START
    predicted = predicted_vec
    
    if (must_specify_predictions_vector) 
        predicted_vec = predicted$predictions else 
        predicted_vec = as.vector (predicted)
#  TEMPORARY - END

    true_vs_pred_df = 
        data.frame (ds_label = rep (test_or_train_string, 
                                    length (true_values_vec)), 
                    true_values  = true_values_vec, 
                    pred_values  = predicted_vec, 
                    dom_err_type = dom_err_type)

    
    #cat ("---------------\n")
    #cat ("Evaluate results of testing on ", test_or_train_string, " data:\n")
    regrEvalResults = regr.eval (true_values_vec, 
                       predicted_vec, 
                       #stats = c ("mae", "mse"),    #, "rmse", "mape"))
                       train.y = true_values_vec)
    rmse_value = regrEvalResults ["rmse"]    #####round (regrEvalResults ["rmse"], digits = 3)
#x#print (regrEvalResults)

    #----------

    actual = true_values_vec
    predicted = predicted_vec
    R2 <- compute_r.squared (actual, predicted)
    adj_R2 <- compute_adj.r.squared (R2, 
                                     num_observations = length (actual), 
                                     num_predictors = num_predictors)   #ncol (train_df))
#x#cat ("\n>>>>>  for ", test_or_train_string, " data: R2 = ", R2, ", 
#x#     adj_R2 = ", adj_R2, "\n")
    
#x#cat ("====================\n")

#x#cat ("\n>\n>\n>\n>")    #  Need some white space between regrEvalResults and plots

    #----------
    
    return (list (true_vs_pred_df = true_vs_pred_df, 
                  rmse_value      = rmse_value, 
                  R2              = R2, 
                  adj_R2          = adj_R2))
    }

#===============================================================================

ppe_for_train_and_test_given_preds <- 
    function (rs_name, 
              
              model_name_str,                 #  e.g., "LM" 
              model_name_str_suffix,          #  e.g., "PCA" 
              
              train_true_values,              #  e.g., "rsr_COR_euc_out_err_frac" 
              test_true_values, 
        
              pred_value_name_display_str,    #  e.g., "RS out err"
              num_predictors,                 #  num cols in x data frames
              vars_used_str, 
        
              predicted_trains, 
              predicted_tests, 
              
              train_aux_df, 
              test_aux_df, 
              
              params, 
        
              must_specify_predictions_vector , 
                      
    lm_resid_plot, 
    lm_qqplot
            )
    {
    train_dom_err_type = train_aux_df$dom_err_type
    test_dom_err_type  = test_aux_df$dom_err_type

            #----------------------------------------
            #  New general code...
            #----------------------------------------

    train_eval_list = 
        eval_model_on_train_or_test_data ("TRAIN", 
                                          train_true_values, 
                                          predicted_trains, 
                                          num_predictors, 
                                          
                                  train_dom_err_type, 

                                          must_specify_predictions_vector) 
                                                  
    test_eval_list = 
        eval_model_on_train_or_test_data ("TEST", 
                                          test_true_values, 
                                          predicted_tests, 
                                          num_predictors, 
                                          
                                  test_dom_err_type, 
                                          
                                          must_specify_predictions_vector)
                                               
    plot_train_and_test_stuff (rs_name, 
                               
                               train_eval_list$true_vs_pred_df, 
                               test_eval_list$true_vs_pred_df, 
                               
                               train_aux_df, 
                               test_aux_df, 
                                
                                pred_value_name_display_str, 
                                model_name_str, 
                                vars_used_str, 
                                model_name_str_suffix, 
                                
                                train_eval_list$rmse_value, 
                                test_eval_list$rmse_value, 
                               
                                train_eval_list$adj_R2, 
                                test_eval_list$adj_R2, 
                               
                                params = params
                                )

    test_eval_list$model_name_str = model_name_str
    
    return (test_eval_list)
    }

#===============================================================================

plot_train_and_test_stuff <- function (rs_name, 
                                                   
                                        train_pred_true_df, 
                                        test_pred_true_df, 
                                        
                                        train_aux_df, 
                                        test_aux_df, 
                                        
                                        pred_value_name_display_str, 
                                        model_name_str, 
                                        vars_used_str, 
                                        model_name_str_suffix, 
                                        
                                        train_rmse, 
                                        test_rmse, 
                                        
                                        train_adj_R2, 
                                        test_adj_R2, 
                                        
                                        params
                                        )
    {
            #----------------------------------------------
            #  Plot true vs predicted values in 2 panels, 
            #  1 for testing on TRAINING data and the 
            #  other for testing on TEST data.
            #----------------------------------------------

    combined_pred_true_df = rbind (train_pred_true_df, 
                                   test_pred_true_df)
    
#    combined_aux_df       = rbind (train_aux_df, 
#                                   test_aux_df)
#    dom_err_type          = combined_aux_df$dom_err_type
#    
#    combined_pred_true_df = cbind (combined_pred_true_df, 
#                                   dom_err_type)
    
    num_digits_to_show = 3

    if (!params$show_only_test_results)
        {
            #------------------------------
            #  Show TRAINing data results
            #------------------------------
            
            #  Plot histogram of residuals.
    
        train_residuals = train_pred_true_df$pred_values - train_pred_true_df$true_values
        show (histogram (train_residuals, nint=100))
        show (densityplot (train_residuals))
        train_residuals = NULL
  
                #  Plot predicted values vs true values
        
#        show (ggplot (data = combined_pred_true_df) + 
        show (ggplot (data = train_pred_true_df) + 
                
                  geom_point (aes (
                                   # x = pred_values, 
                                   # y = true_values, 
                                   x = true_values, 
                                   y = pred_values, 
                                   color = dom_err_type), 
                                   ###color = ds_label), 
                             shape=".") + 
                  ###scale_color_manual (breaks = c ("Train", "Test"), values=c("blue", "red")) + 
                  ggtitle (paste0 (rs_name, " - ", pred_value_name_display_str, 
                                   " for ", model_name_str, 
                                   " Using ", vars_used_str, 
                                   ###" ", model_name_str_suffix, 
                                   "\nrmse: ", round (train_rmse, digits=num_digits_to_show), 
                                   ",  adjR2: ", round (train_adj_R2, digits=num_digits_to_show), 
                                   "             rmse: ", 
                                   round (test_rmse, digits=num_digits_to_show), 
                                   ",  adjR2: ", round (test_adj_R2, digits=num_digits_to_show))) + 
                  theme (plot.title = element_text (hjust = 0.5)) +    #  To center the title
                  facet_wrap (~ ds_label, nrow = 1) +
                  geom_abline (intercept=0, slope=1)   #, linetype, color, size
              )
        }

          #--------------------------
          #  Show TEST data results
          #--------------------------
        
              #  Plot histogram of residuals.
      test_residuals = test_pred_true_df$pred_values - test_pred_true_df$true_values
      show (histogram (test_residuals, nint=100))
      show (densityplot (test_residuals))
      test_residuals = NULL

              #  Plot predicted values vs true values
      
##      show (ggplot (data = combined_pred_true_df) + 
#      show (ggplot (data = test_pred_true_df) + 

###FP_only_true_vs_pred_df = filter (full_true_vs_pred_df, dom_err_type == "FP")
color_breaks_and_values = force_dom_err_type_colors (test_pred_true_df$dom_err_type)
scale_color_breaks = color_breaks_and_values$breaks
scale_color_values = color_breaks_and_values$values
alpha_level = 0.5
#force_colors = TRUE
force_colors = params$force_colors

###FP_only_true_vs_pred_df %>% 
      test_pred_true_plot = ggplot (data = test_pred_true_df) + 
              
                  geom_point (aes (
                                   # x = pred_values, 
                                   # y = true_values, 
                                   x = true_values, 
                                   y = pred_values, 
#xxxxx                                   color = dom_err_type),    #ds_label), 
color = dom_err_type), 

shape = 15,    #".", 
size=0.5, 
alpha = alpha_level    #1
# shape="."
                            ) +  

        
                  ###scale_color_manual (breaks = c ("Train", "Test"), values=c("blue", "red")) + 
                  ggtitle (paste0 (rs_name, " - ", pred_value_name_display_str, 
                                   " for ", model_name_str, 
                                   " Using ", vars_used_str, 
                                   ###" ", model_name_str_suffix, 
                                   "\nrmse: ", round (test_rmse, digits=num_digits_to_show), 
                                   ",  adjR2: ", round (test_adj_R2, digits=num_digits_to_show))) + 
                  theme (plot.title = element_text (hjust = 0.5)) +    #  To center the title
        
            #  If forcing colors, then manually scale the colors.  
            #  Otherwise, use the default behavior.
            #  To use an "if" statement inside these sets of statements 
            #  separated by "+" signs, you have to put curly brackets around 
            #  your test and result.
{ if (force_colors) scale_color_manual (breaks = scale_color_breaks, 
                                        values = scale_color_values) } +
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
      
                  ###facet_wrap (~ ds_label, nrow = 1) +
                  geom_abline (intercept=0, slope=1)   #, linetype, color, size
#            )

      show (test_pred_true_plot)
  }

#===============================================================================
#===============================================================================
#===============================================================================

#  Force the coloring of points by dominant error type to always use the 
#  same color for FN and the same color for FP.  
#  If you don't do this forcing and you do a plot that is only FN_dominant or 
#  FP_dominant, then the points will always just get the first color in the 
#  ordering and so sometimes FN will be one color and other times the other 
#  color.  Here, we check to see if there is just one type of error appearing 
#  in the plot and if so, force it to be consistent in coloring FNs and FPs 
#  in different plots.

force_dom_err_type_colors <- function (dom_err_type,      #sorted_msa_tib, 
                                       gg_verbose = FALSE)
    {
        #  Choose the colors that will be used for FN and FP when manual 
        #  coloring is done in calls to ggplot.
  
    scale_color_breaks = c("FN", "FP")
    scale_color_values = c("blue", "red")
    
        #  Check to see whether there's only one type of point in the current 
        #  plot and if so, choose which color is to be used according to 
        #  whether the points are FN_dominant or FP_dominant.
    
    unique_values = unique (dom_err_type)    #sorted_msa_tib$dom_err_type)
    num_unique_values = length (unique_values)
    
    if (gg_verbose) cat ("\nIn force_dom_err_type_colors(), num_unique_values = ", 
                         num_unique_values, "\n")
    if (num_unique_values == 1)
        {
        if (unique_values == "FN")    #dom_err_type [1] == "FN")
            {
            scale_color_breaks = c("FN")
            scale_color_values = c("blue")
            
            } else if (unique_values == "FP")    #dom_err_type [1] == "FP")
            {
            scale_color_breaks = c("FP")
            scale_color_values = c("red")
            
            } else
            {
            err_string = paste0 ("\nBad break value in force_dom_err_type_colors() = '", 
                                 unique_values, "'.\n")
            stop (err_string)
            }
        }
        
    return (list (breaks = scale_color_breaks, 
                  values = scale_color_values))
    }

#===============================================================================

##  Function to show representation shortfall CDFs

show_rep_shortfall_cdfs <- function (working_train_df, 
                                     working_test_df, 
                                     rs_method_name)
  {
       #  Training data
  
          #  Add method name as factor so that ggplot facet order can be controlled.
  working_train_df %>% 
      mutate (rs_method_name_fac = 
              convert_rs_method_name_to_ordered_factor (rs_method_name)) -> working_train_df
  
  train_cdf_rep_shortfall <- ggplot (working_train_df, 
                                     aes(x=rsr_COR_spp_rep_shortfall)) + 
                                     stat_ecdf (aes (colour=rs_method_name)) + 
# facet_wrap (~ rs_method_name, nrow = 2) + 
facet_wrap (~ rs_method_name_fac, nrow = 2) + 
                                     ggtitle ("Train") + 
                                     theme (plot.title = element_text (hjust = 0.5))  #  Center title
  print (train_cdf_rep_shortfall)
  
        #  Test data
  
          #  Add method name as factor so that ggplot facet order can be controlled.
  working_test_df %>% 
      mutate (rs_method_name_fac = 
              convert_rs_method_name_to_ordered_factor (rs_method_name)) -> working_test_df
  
  test_cdf_rep_shortfall <- ggplot (working_test_df, 
                                    aes(x=rsr_COR_spp_rep_shortfall)) + 
                                    stat_ecdf (aes (colour=rs_method_name)) + 
# facet_wrap (~ rs_method_name, nrow = 2) + 
facet_wrap (~ rs_method_name_fac, nrow = 2) + 
                                    ggtitle ("Test") + 
                                    theme (plot.title = element_text (hjust = 0.5))  #  Center title
  print (test_cdf_rep_shortfall)
  }

#===============================================================================

##  Function to show cost error CDFs

show_cost_err_cdfs <- function (working_train_df, 
                                working_test_df, 
                                rs_method_name)
    {
        #  Training data

            #  Add method name as factor so that ggplot facet order 
            #  can be controlled.
    working_train_df %>% 
        mutate (rs_method_name_fac = 
                convert_rs_method_name_to_ordered_factor (rs_method_name)) -> working_train_df
  
    train_cdf_cost_err <- ggplot (working_train_df, 
                                  aes(x=rs_solution_cost_err_frac)) + 
                                  stat_ecdf (aes (colour=rs_method_name)) + 
# facet_wrap (~ rs_method_name, nrow = 2) + 
facet_wrap (~ rs_method_name_fac, nrow = 2) + 
                                  ggtitle ("Train") + 
                                  theme (plot.title = element_text (hjust = 0.5))  #  Center title
    print (train_cdf_cost_err)
    
        #  Test data
    
            #  Add method name as factor so that ggplot facet order 
            #  can be controlled.
    working_test_df %>% 
        mutate (rs_method_name_fac = 
                convert_rs_method_name_to_ordered_factor (rs_method_name)) -> working_test_df
  
    test_cdf_cost_err <- ggplot (working_test_df, 
                                 aes(x=rs_solution_cost_err_frac)) + 
                                 stat_ecdf (aes (colour=rs_method_name)) + 
# facet_wrap (~ rs_method_name, nrow = 2) + 
facet_wrap (~ rs_method_name_fac, nrow = 2) + 
                                 ggtitle ("Test") + 
                                 theme (plot.title = element_text (hjust = 0.5))  #  Center title
    print (test_cdf_cost_err)
    }

#===============================================================================

save_this_ggplot <- function (a_ggplot, cur_plot_name, extension = "pdf")
    {
    proj_dir = here()
#    cat ("\n\nproj_dir = here() = ", proj_dir, "\n", sep='')
    cur_plot_name_and_path = 
        file.path (proj_dir, 
                   "Analysis_scripts/Paper_3_learning_to_predict_error/Saved_plots", 
                   cur_plot_name) 
    cur_plot_name_and_path = paste0 (cur_plot_name_and_path, ".", extension)
#    cat ("\ncur_plot_name_and_path = '", cur_plot_name_and_path, "'\n")
    
    ggsave (a_ggplot, file = cur_plot_name_and_path)
    }

#===============================================================================

