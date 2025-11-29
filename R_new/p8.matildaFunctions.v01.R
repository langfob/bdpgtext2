#===============================================================================
#
#                       p8.matildaFunctions.v01.R
#
#  These functions support the creation of input files for Melbourne Uni's 
#  MATILDA program for instance space analysis.  They use information from 
#  the matilda website to define the format of the files:
#          https://matilda.unimelb.edu.au/matilda/submission-guidelines
#  
#===============================================================================

#  History

#  v01 December 29,2022
#  
#  - Created by moving functions and comments from p8_v01_all_combined.Rmd.

#===============================================================================

#  To use MATILDA on reserve selection algorithms and predictors of their error:
#  
#  Need one file for each score type, i.e.,
#    - rep_shortfall, cost_err_frac,
#    - residuals by learned model
#        - lm_resid_of_pred_rep_shortfall, lm_resid_of_pred_cost_err_frac
#        
#  All of these scores have to be done for each reserve selector, but
#  because they are labelled in the Matilda file as "algo_Gurobi", etc,
#  you have to have a SEPARATE FILE FOR every one of the different score
#  types combined with every different set of input features.  So, there
#  are 4 (or 6 if using combined error and not just separate rep/cost error)
#  files for each combination:
#
#  6 RESERVE SELECTOR SETS PER INPUT VARIABLE SET
#      Basic errors:
#        1) rep_shortfall_probSize (analysis of reserve selectors, not learned err pred)
#        2) cost_err_frac_probSize (analysis of reserve selectors, not learned err pred)
#        3) tot_err_probSize (analysis of reserve selectors, not learned err pred)
#      Basic error magnifications
#        4) rep_shortfall_MAG_probSize (analysis of reserve selectors, not learned err pred)
#        5) cost_err_frac_MAG_probSize (analysis of reserve selectors, not learned err pred)
#        6) tot_err_MAG_probSize (analysis of reserve selectors, not learned err pred)
#
#  6 RESERVE SELECTOR SETS PER [INPUT VARIABLE SET, LEARNER] PAIR
#      Basic errors:
#       1a) rep_shortfall_probSize_LM (analysis of reserve selectors, not learned err pred)
#       2a) cost_err_frac_probSize_LM (analysis of reserve selectors, not learned err pred)
#       3a) tot_err_probSize_LM (analysis of reserve selectors, not learned err pred)
#      Basic error magnifications
#       4a) rep_shortfall_MAG_probSize_LM (analysis of reserve selectors, not learned err pred)
#       5a) cost_err_frac_MAG_probSize_LM (analysis of reserve selectors, not learned err pred)
#       6a) tot_err_MAG_probSize_LM (analysis of reserve selectors, not learned err pred)

#===============================================================================

    #  Helper functions for building single column data frames holding just 
    #  the output one score (e.g., rep_shortfall or absolute value of learning 
    #  residuals).  The main matilda file builder function sets up everything 
    #  else and binds this column to the rest of the data before widening it.
    #  builder ne
    #  This function builds the single column df only for absolute value of 
    #  residuals from learning to predict some kind of error.  
    #  Need at least one other function for extracting other kinds of errors 
    #  for the reserve selectors.  This function is a little different because 
    #  it not only extracts a performance measure but it also computes its 
    #  absolute value,  I think that the other measures won't need their 
    #  absolute value taken.  Hmmm, that's actually wrong since solution cost 
    #  error can be negative even though rep shortfall can't.

#-------------------------------------------------------------------------------

    #  This function builds the single column df only for absolute value of 
    #  residuals from learning to predict some kind of error.  
    #  This function is a little different because it not only extracts a 
    #  performance measure but it also computes its absolute value since 
    #  matilda can't do its analysis if the optimum is not at one end or the 
    #  other of the data.  For any signed score like the residual, the 
    #  zero optimum is in the middle instead of the end.  The same is true 
    #  for bdpg's solution cost error and its magnification.  However, the absolute 
    #  value of the error has already been computed and stored as a variable 
    #  in the main data set so no function to load it and compute the absolute 
    #  value is necessary.  However, the magnification of the solution cost 
    #  error doesn't already have its absolute value computed, so it will need 
    #  a function like this if I decide to analyze it.  If that happens, I 
    #  should probably make a more general version of this function so that it 
    #  can handle both residuals and solution cost error magnification.
    #  
    #  This function is almost general enough to use to return the absolute 
    #  value of any column, however, at the moment I can't remember how to 
    #  pass in the column name to set as the result of the mutation and 
    #  selection.  I know that I need to do some kind of game with quotation, 
    #  etc but it's not worth the effort at this moment.  I think that the 
    #  ggplot with rays function may do what I need to do here.  Can look at 
    #  that later if necessary.

build_abs_perfMeasure_one_col_df_for_matilda <- function (perfMeasure_source_df, 
                                                          params)
    {
        #  Build residual column and add it to the other columns
    abs_perfMeasure_one_col_df = 
        perfMeasure_source_df %>%  
            mutate (abs_resid_pred_minus_true = abs (resid_pred_minus_true)) %>% 
            select (abs_resid_pred_minus_true)
    if (params$VERBOSE_LM_CATS) glimpse (abs_perfMeasure_one_col_df)
    
    return (abs_perfMeasure_one_col_df)
    }
    
#-------------------------------------------------------------------------------

build_perf_metric_col_df_for_matilda <- function (working_df, 
                                                  perf_metric_col_name, 
                                                  params)
    {
        #  Build residual column and add it to the other columns
    perf_metric_one_col_df = select (working_df, all_of (perf_metric_col_name))
    if (params$VERBOSE_LM_CATS) glimpse (perf_metric_one_col_df)
    
    return (perf_metric_one_col_df)
    }
    
#===============================================================================

build_matilda_metadata_df <- 
    function (working_df,                 #  A full data set such as the test or train set 
              inVars,                     #  Vector of char strings containing names of cols to 
                                          #  be used as matilda input features
              perfMeasure_one_col_df,     #  1 col data frame containing performance 
                                          #  measure to be plotted in matilda
              source_string,              #  String to use to fill matilda's Source column
              perf_metric_col_name_str,   #  Performance metric's name to echo to Rmd output
              params)
    {
        #  Build ID columns
    if (params$VERBOSE_LM_CATS) cat ("\n=====> working_df_with_perfMeasure after adding ID columns\n\n", sep="")
    working_df_with_perfMeasure = 
        working_df %>% 
            select (rsp_UUID, rs_method_name) %>% 
            rename (Instances = rsp_UUID) %>%
            mutate (Source = source_string) 
    if (params$VERBOSE_LM_CATS) glimpse (working_df_with_perfMeasure)
    
    #----------

        #  Build feature columns and add them to the ID columns  
    if (params$VERBOSE_LM_CATS) cat ("\n=====> working_df_with_perfMeasure after adding feature columns\n\n", sep="")
    feature_cols_df = 
        working_df %>% 
            select (all_of (inVars)) %>%    #  Have to use all_of() when passing 
                                            #  a list of strings to select()
            rename_with (~ paste0 ("feature_", .x))
    if (params$VERBOSE_LM_CATS) glimpse (feature_cols_df)   
    
    working_df_with_perfMeasure = cbind (working_df_with_perfMeasure, 
                                            feature_cols_df)
    if (params$VERBOSE_LM_CATS) glimpse (working_df_with_perfMeasure)
    
    #----------

        #  Add output var column to the other columns
    if (params$VERBOSE_LM_CATS) cat ("\n=====> working_df_with_perfMeasure after adding perfMeasure column\n\n", sep="")
    working_df_with_perfMeasure = cbind (working_df_with_perfMeasure, 
                                          perfMeasure_one_col_df)
    if (params$VERBOSE_LM_CATS) glimpse (working_df_with_perfMeasure)
    
    #----------

        #  Widen the file to get the algo_ columns.
    if (params$VERBOSE_LM_CATS) cat ("\n=====> working_df_with_perfMeasure after adding algo_ columns\n\n", sep="")
    working_df_with_perfMeasure = 
        pivot_wider (working_df_with_perfMeasure, 
                     names_from   = rs_method_name, 
                     names_prefix = "algo_", 
                     values_from  = all_of(names (perfMeasure_one_col_df)))
    if (params$VERBOSE_LM_CATS) glimpse (working_df_with_perfMeasure)
    
    #----------

    return (working_df_with_perfMeasure)
    }

#===============================================================================

write_matilda_file <- function (
                  working_df, 
                  perf_metric_col_name_str,    #  "abs_resid_pred_minus_true"
                  inVars, 
                  source_string, 
                  test_train_string, 
                  params)
    {
    cat ("\n\n===============================================", 
         "\n\nBUILDING MATILDA FILE FOR ", perf_metric_col_name_str, 
         " and ", source_string, "\n\n", sep="")
        
    #----------

    perfMeasure_one_col_df = 
        build_perf_metric_col_df_for_matilda (working_df, 
                                              perf_metric_col_name_str, 
                                              params)
    
    csv_tib = 
        build_matilda_metadata_df (working_df, 
                                   inVars = inVars, 
                                   perfMeasure_one_col_df, 
                                   source_string, 
                                   perf_metric_col_name_str, 
                                   params)
    
        #  Build file name
        #  Example:  "matilda__TEST__probSize_lm__log10_euc_out_err_mag__input"

    file_name = paste0 ("matilda__", test_train_string, 
                        "__", source_string, 
                        "__", perf_metric_col_name_str,   #perf_metric_name_for_file_name_str, 
                        "__", "input")
    
    write_a_tib_to_csv_file_using_params (csv_tib, file_name, params, "csv")
    
    #----------

    return (csv_tib)
    }

#----------

#===============================================================================

    #  This needs a bit of generalization since it will crash if the algorithms 
    #  used are different from these four, e.g., if Zonation is used.  
    #  For the moment (2022 12 29), it doesn't matter.

show_distributions_of_matilda_perf_measure_inputs <- function (csv_tib, perf_measure_name_str)
    {
    cat ("\n\ndistribution for algo_Gurobi absolute value of ", 
         perf_measure_name_str, ":\n", sep='')
    fivenum (csv_tib$algo_Gurobi)
    show (densityplot (csv_tib$algo_Gurobi))
    
    cat ("\n\ndistribution for algo_Marxan_SA absolute value of ", 
         perf_measure_name_str, ":\n", sep='')
    fivenum (csv_tib$algo_Marxan_SA)
    show (densityplot (csv_tib$algo_Marxan_SA))
    
    cat ("\n\ndistribution for algo_UR_Forward absolute value of ", 
         perf_measure_name_str, ":\n", sep='')
    fivenum (csv_tib$algo_UR_Forward)
    show (densityplot (csv_tib$algo_UR_Forward))
    
    cat ("\n\ndistribution for algo_Marxan_SA_SS absolute value of ", 
         perf_measure_name_str, ":\n", sep='')
    fivenum (csv_tib$algo_Marxan_SA_SS)
    show (densityplot (csv_tib$algo_Marxan_SA_SS))
    }

#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================

write_matilda_files_for_optimizers_only <- function (df__before_any_preprocessing, 
                                                     inVars, 
                                                     source_string, 
                                                     test_train_string, 
                                                     params)
    {
    #--------------------
    
    # [13] "rsr_COR_euc_out_err_frac"                       
    
    write_matilda_file (df__before_any_preprocessing,    #p3_working_test_df, 
                        "rsr_COR_euc_out_err_frac", 
                        inVars, 
                        source_string, 
                        test_train_string, 
                        params)
    
    #--------------------
    
    # [14] "rsr_COR_spp_rep_shortfall"                      
    
    write_matilda_file (df__before_any_preprocessing,    #p3_working_test_df, 
                        "rsr_COR_spp_rep_shortfall", inVars, 
                        source_string, test_train_string, params)
    
    #--------------------
    
    # [16] "abs_rs_solution_cost_err_frac"                  
    
    write_matilda_file (df__before_any_preprocessing,    #p3_working_test_df, 
                        "abs_rs_solution_cost_err_frac", inVars, 
                        source_string, test_train_string, params)
    
    #--------------------
    
    # [17] "err_mag"                                        
    
        #  err_mag in TEST set has no negative values because it's the magnification
        #  of the euclidean total error rather than the individual errors.  
        #  The euc value is always positive.  
        #  I don't think I ever calculate the magnification for the solution cost 
        #  error, which is the only one that can be negative.
    
    write_matilda_file (df__before_any_preprocessing,    #p3_working_test_df, 
                        "err_mag", inVars, 
                        source_string, test_train_string, params)
  
    #--------------------
    
    # "rep_shortfall_mag"                      
    
    write_matilda_file (df__before_any_preprocessing,    #p3_working_test_df, 
                        "rep_shortfall_mag", inVars, 
                        source_string, test_train_string, params)
    
    #--------------------
    
    # "abs_cost_err_mag"                  
    
    write_matilda_file (df__before_any_preprocessing,    #p3_working_test_df, 
                        "abs_cost_err_mag", inVars, 
                        source_string, test_train_string, params)
    
    }

#===============================================================================

write_matilda_file_for_abs_residuals <- 
        function (df__before_any_preprocessing,    # p3_working_test_df__before_any_preprocessing,
                  inVars, 
                  full_fits_plot_and_data, 
                  params, 
                  source_string, 
                       #  Usually perf_metric_name_for_file_name_str is 
                       #  the same as perf_metric_col_name_str, but they 
                       #  differ when perf metric is related to residuals 
                       #  from fitting.
                       #  There, the col name string is abstract and the 
                       #  same in all cases (because the residuals data is 
                       #  generated by the same code), i.e., 
                       #  "abs_resid_pred_minus_true".  
                       #  However, the perf metric name string used in naming 
                       #  the matilda file needs to be more specific.  
                       #  For example, it needs to show whether the residuals 
                       #  are related to predicting rep_shortfall or are 
                       #  they for predicting solution cost error.  
                       #  The file that's written out needs to 
                       #  differentiate between those so they 
                       #  don't overwrite each other and so that 
                       #  you can tell what the data in the files 
                       #  is about.
                  perf_metric_name_for_file_name_str,    # "abs_rep_shortfall_resid"
                  test_train_string = "TEST"
                 )
    {
    true_vs_pred_df = full_fits_plot_and_data$full_true_vs_pred_df
    #names (true_vs_pred_df)
    #dim (true_vs_pred_df)
    
    #names (p3_test_x_df_probSize)
    
        #  Matilda file prep for lm learning
    abs_residuals_one_col_df = 
        build_abs_perfMeasure_one_col_df_for_matilda (true_vs_pred_df, params)
    
    csv_tib = 
        build_matilda_metadata_df (df__before_any_preprocessing, 
                                   inVars, 
                                   perfMeasure_one_col_df = abs_residuals_one_col_df, 
                                   source_string, 
                                   perf_metric_col_name_str = "abs_resid_pred_minus_true", 
                                   params
                                   )
   
        #  "matilda__TEST__probSize_lm__abs_rep_shortfall_resid__input" 
    matilda_file_name = paste0 ("matilda__", test_train_string, 
                                "__", source_string, 
                                "__", perf_metric_name_for_file_name_str, 
                                "__input")
  
    write_a_tib_to_csv_file_using_params (csv_tib, matilda_file_name, 
                                          params, "csv")
    
    if (params$VERBOSE_LM)
        show_distributions_of_matilda_perf_measure_inputs (
            csv_tib, perf_measure_name_str = perf_metric_name_for_file_name_str)
    }

#===============================================================================
#===============================================================================



