#===============================================================================
#
#                       p8.preprocessingForLearning.v01.R
#
#===============================================================================

#  History

#  v01 December 26,2022
#  
#  - Created by copying some of the p3 preprocessing code in 
#    bdpgtext/R/v6_paper_3_learn_to_predict_output_error__body.

#===============================================================================

#  Set aside columns that will not be preprocessed (e.g., identifiers) 

#  These are columns that are either not numeric or they are numeric columns 
#  that are to be the response variable in the fittings.
#  This code is refactored from two chunks in the v6 p3 Rmd file and then  
#  modified to match renamed p8 v1 column names.

set_aside_cols_NOT_to_preprocess <- function (a_p3_working_df)
    {
    set_aside_cols = 
        select (a_p3_working_df, 
             c(
               batch_id,    #id, 
      rs_method_name, 
  rsr_UUID,             
               rsp_combined_err_label, 
               rsp_cor_or_app_str, 
               rsp_base_wrap_str,
  
               rsp_UUID,  
               rsp_UUID_of_COR_Wrap_problem_that_has_err_added,  
##2022 12 26 xxx##                                    rsp_UUID_of_base_problem_that_has_err_added, 
               rsp_UUID_of_COR_Base_problem_that_is_wrapped,
#2022 12 26 xxx##                            rsp_UUID_of_base_problem_that_is_wrapped,

               rsp_wrap_is_imperfect, 
               gurobi_status, 
               dom_err_type,

#--------------------

#  2023 12 24 - BTL 
#  Column for a variable based on dom_err_type to give a 
#  boolean indicator of whether the dominant error type is FP (i.e., 1) or 
#  FN (i.e., 0).  
#  Want to test whether perfectly knowing the dominant error type improves 
#  ability to predict.  If it does, then maybe it will pay to try to learn 
#  a probability that an example is FN or FP-dominant.  

dom_err_type_is_FP, 

#--------------------
              
               rsr_COR_euc_out_err_frac,
               rsr_COR_spp_rep_shortfall,
               rs_solution_cost_err_frac, 
               abs_rs_solution_cost_err_frac,
               err_mag, 
               log10_err_mag, 
signed_cost_err_mag, 
abs_cost_err_mag, 
log_abs_cost_err_mag, 
log_abs_cost_err, 
rep_shortfall_mag, 
log_rep_shortfall_mag, 
log_rep_shortfall, 
               rsp_correct_solution_cost, 
               # rs_over_opt_cost_err_frac_of_possible_overcost, 
               # rs_under_opt_cost_err_frac_of_possible_undercost, 
               rs_solution_cost, 
               rsr_COR_solution_FRAC_spp_covered, 
               is_train
               ))
    
    return (set_aside_cols)
    }

#===============================================================================

#  Set aside columns that WILL be preprocessed (e.g., identifiers) 

#  These are the columns that will be used to determine the preprocessing 
#  parameters such as the mean and std deviation used in centering and 
#  standardizing.

#  Note that this function lists exactly the same columns as the previous 
#  function, BUT with two small differences:
#      - a MINUS SIGN in the select so that it will add every column NOT 
#        included in the previous set, and 
#      - commenting out the rs_method_name and the rsr_UUID from the  
#        minus sign list so that the preprocessed and not preprocessed data 
#        can be matched up.

set_aside_cols_TO_preprocess <- function (a_p3_working_df)
    {
    set_aside_cols = 
        select (a_p3_working_df, 
                
             -c(            #  <----- Note the MINUS SIGN in front of the "c(".  
               
               batch_id,    #id, 
    #rs_method_name,      #  <----- Commented out of negation list, so left in set
    #rsr_UUID,            #  <----- Commented out of negation list, so left in set             
               rsp_combined_err_label, 
               rsp_cor_or_app_str, 
               rsp_base_wrap_str,
    
               rsp_UUID,  
               rsp_UUID_of_COR_Wrap_problem_that_has_err_added, 
##2022 12 26 xxx##                                    rsp_UUID_of_base_problem_that_has_err_added, 
               rsp_UUID_of_COR_Base_problem_that_is_wrapped,
#2022 12 26 xxx##                            rsp_UUID_of_base_problem_that_is_wrapped,

               rsp_wrap_is_imperfect, 
               gurobi_status, 
               dom_err_type,

#--------------------

#  2023 12 24 - BTL 
#  Column for a variable based on dom_err_type to give a 
#  boolean indicator of whether the dominant error type is FP (i.e., 1) or 
#  FN (i.e., 0).  
#  Want to test whether perfectly knowing the dominant error type improves 
#  ability to predict.  If it does, then maybe it will pay to try to learn 
#  a probability that an example is FN or FP-dominant.  

dom_err_type_is_FP, 

#--------------------

               rsr_COR_euc_out_err_frac,
               rsr_COR_spp_rep_shortfall,
               rs_solution_cost_err_frac, 
               abs_rs_solution_cost_err_frac,
               err_mag, 
               log10_err_mag, 
signed_cost_err_mag, 
abs_cost_err_mag, 
log_abs_cost_err_mag, 
log_abs_cost_err, 
rep_shortfall_mag, 
log_rep_shortfall_mag, 
log_rep_shortfall, 
               rsp_correct_solution_cost, 
               # rs_over_opt_cost_err_frac_of_possible_overcost, 
               # rs_under_opt_cost_err_frac_of_possible_undercost, 
               rs_solution_cost, 
               rsr_COR_solution_FRAC_spp_covered, 
               is_train
               ))
    
    return (set_aside_cols)
    }

#===============================================================================

