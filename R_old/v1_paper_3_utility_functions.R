#===============================================================================
#
#                       v1_paper_3_utility_functions.R
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

echo_rs_method_name_order_within_df <- function (a_df, label = "", num_of_rs = 5)
    {
    cat ("\n\n>>>>>>>>>>>>>>>>>>>>>>", label, "\n")
  
    order_of_rs = rep (NA, num_of_rs)
    cur_order_idx = 1
    
    cur_rs_name = a_df$rs_method_name [1]
    order_of_rs [cur_order_idx] = as.integer (convert_rs_method_name_to_ordered_factor (cur_rs_name))

    cat ("\n    rs_method_name [1] == '", a_df$rs_method_name [1], "'", sep='')
    
    for (idx in 2:length (a_df$rs_method_name))
      {
      if (a_df$rs_method_name [idx] != a_df$rs_method_name [idx - 1])
          {
          cur_rs_name = a_df$rs_method_name [idx]
          
          cur_order_idx = cur_order_idx + 1
          order_of_rs [cur_order_idx] = as.integer (convert_rs_method_name_to_ordered_factor (cur_rs_name))
        
          cat ("\n    rs_method_name [", idx, "] == '", a_df$rs_method_name [idx], "'", sep='')
          }
      }
    
    cat ("\n\n<<<<<<<<<<<<<<<<<<<<<<")
    
    cat ("\n\norder_of_rs = ")
    print (order_of_rs)

    return (order_of_rs)
    }

#===============================================================================

##  Function to print summary of counts for problem flags

print_summary_of_cts_for_prob_flags <- function (rs_name, working_tib)
    {
    cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    cat ("\nAfter filtering rows for '", rs_name, "':")
    cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
  
    cat ("\nbatch IDs remaining are: ")
    print (unique (working_tib$id))
    # cat ("\n")
    # print (count (working_tib, id))
    
    cat ("\nNumber of ig_median_bottom_bg_redundancy <= 0:  ", 
         length (which (working_tib$ig_median_bottom_bg_redundancy < 0)))

    cat ("\nNumber of gurobi_status == 'OPTIMAL':  ", 
         length (which (working_tib$gurobi_status == "OPTIMAL")))

    cat ("\nNumber of rsp_wrap_is_imperfect == FALSE:  ", 
         length (which (working_tib$rsp_wrap_is_imperfect == FALSE)))

    cat ("\nNumber of FN_dominant == TRUE:  ", 
         length (which (working_tib$dom_err_type == "FN")))

    cat ("\nNumber of FP_dominant == TRUE:  ", 
         length (which (working_tib$dom_err_type == "FP")))
  
    cat ("\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
    cat ("\n")
    }

#===============================================================================

##  Function to summarize errors on correct data

summarize_errors_on_COR_data = function (working_tib)
  {
  working_tib %>% 
      filter (rsp_cor_or_app_str == "COR", 
              rsr_COR_euc_out_err_frac > 0) %>%
      
      group_by (rsp_base_wrap_str) %>%
      summarize (n(), 
                 mean = mean (rsr_COR_euc_out_err_frac), 
                 median = median (rsr_COR_euc_out_err_frac), 
                 min = min (rsr_COR_euc_out_err_frac), 
                 max = max (rsr_COR_euc_out_err_frac)) %>% 
      ungroup() -> cor_errs_summary
  
  cat ("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n",
         "\nSummary of errors on COR data:",
         "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n", sep='')
  print (cor_errs_summary)
  cat ("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")

  }

#===============================================================================

convert_rs_method_name_to_ordered_factor <- function (rs_method_name)
    {
    rs_method_name_fac = factor (rs_method_name, 
                                 levels = c("Gurobi", "Marxan_SA", 
                                            "UR_Forward", "ZL_Backward", 
                                            "Marxan_SA_SS", "SR_Forward"), 
                                 ordered = TRUE)
    
    return (rs_method_name_fac)
    }

#===============================================================================

##  Function to quickly check for NAs

quick_check_for_NAs <- function (df_to_test, 
                                 name_of_df_for_display, 
                                 location_of_test_for_display)
  {
  num_cols = ncol (df_to_test)
  
  if (sum (is.na (df_to_test)) > 0)
      {
      for (iii in 1:num_cols) 
        {
        if (sum (is.na (df_to_test [,iii]) > 0)) 
            cat ("\n ", names (df_to_test)[iii], 
                 ", sum na = ", sum (is.na (df_to_test) [,iii]))
        }
      stop ("\n\nFound NAs in ", name_of_df_for_display, 
            " ", location_of_test_for_display, ".\n", sep='')
    
      } else
  
      {
      cat ("\n\nNo NAs found in ", name_of_df_for_display, 
             " ", location_of_test_for_display, ".\n", sep='')
      }
  }

#===============================================================================

