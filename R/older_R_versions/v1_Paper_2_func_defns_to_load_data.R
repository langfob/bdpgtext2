#===============================================================================
#
#                   v1_Paper_2_func_defns_to_load_data.R
#
#===============================================================================

#  History

#  2020 08 18 - BTL - v1
#     - Extracted from v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R 
#       as part of simplifying the paper 2 functions.

#===============================================================================

#  Data loading functions

#===============================================================================

##  Function to load csv file into a tibble

load_input_csv_into_tibble <- function (rs_name, base_path, suffix)
    {
    input_file_path = paste0 (base_path, rs_name, suffix)

    tib = read_csv (input_file_path)
    full_spec = spec_csv (input_file_path)
    print (full_spec)
     
    tib_col_names = colnames (tib)
    num_cols      = length (tib_col_names)
    
    cat ("\nnum ", rs_name, " cols = ", num_cols, sep='')
  
    return (tib)
    }

#===============================================================================
#  Functions that remove columns from the data
#===============================================================================

remove_irrelevant_cols <- function (full_tib)
    {
    working_tib = select (full_tib, 
                      
                        #  Problem and reserve selector labels                     
                            id,     #  Batch ID
                            
                            rs_method_name, 
                            rsp_combined_err_label, 
                            rsp_cor_or_app_str, 
                            rsp_base_wrap_str, 
                            
                            rsr_UUID,
                            rsp_UUID,
                            rsp_UUID_of_base_problem_that_has_err_added,
                            rsp_UUID_of_base_problem_that_is_wrapped,
                            
                            
                        #  Input descriptors
                            rsp_num_PUs, 
                            rsp_num_spp, 
                            rsp_num_spp_per_PU, 
                            rsp_correct_solution_cost, 
                            
                            sppPUsum,
                            sppPUprod, 
                            
                            rsp_realized_FP_rate, 
                            rsp_realized_FN_rate, 
                            rsp_realized_Ftot_rate, 
                            rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                            
                            rsp_wrap_is_imperfect,
                            
                        #  Results and their errors
                            rs_solution_cost, 
                            rs_solution_cost_err_frac, 
                            abs_rs_solution_cost_err_frac, 
                            
                            rsr_COR_euc_out_err_frac, 
                            
                            rsr_APP_spp_rep_shortfall, 
                            rsr_APP_solution_FRAC_spp_covered, 
                            
                            rsr_COR_spp_rep_shortfall, 
                            rsr_COR_solution_FRAC_spp_covered, 
                            
                            err_mag, 
                            
                            RS_system_time
                  )

    return (working_tib)
    }

#===============================================================================

get_gurobi_complete_vs_unfinished_inf <- function (full_Gurobi_tib, 
                                                   exp_tib, 
                                                   params)
    {
        #  For each reserve selector run, add the gurobi run information 
        #  for the same problem.
        #  These are extra columns that don't appear for any of the reserve 
        #  selectors other than gurobi, so you have to extract them from 
        #  the full gurobi data set that was saved expressly for this purpose 
        #  and then join them to all the corresponding runs in the exp_tib.

    gurobi_prob_UUIDs_and_gap_info = 
            select (full_Gurobi_tib, rsp_UUID, 
                                     gurobi_status, 
                                     gurobi_mipgap, 
                                     gurobi_objbound, 
                                     gurobi_itercount, 
                                     gurobi_runtime)
    
    exp_tib <- left_join (exp_tib, 
                          gurobi_prob_UUIDs_and_gap_info, 
                          by = "rsp_UUID")

    #-----------------------------------
    #-----------------------------------
    
#  2020 08 18
#  I think this whole section is probably no longer necessary, but not sure yet.
#  I think all this stuff is done on the fly in the Rmd file rather than using 
#  these completed and unfinished exp tibs.  
#  I think that I even wanted to have access in the Rmd to the gurobi_status 
#  column, but it's somehow getting deleted before getting there.  
#  Need to follow that out to see if there's a select() somewhere that's 
#  killing it.  I suspect that what's in the Rmd file can be greatly simplified 
#  if the gurobi_status field comes through.
#  Ach, just noticed that it's commented out in the select immediately 
#  preceding these comment lines.  Not sure why.  Should put it back?
    
            ##  Determine problems where Gurobi timed out
    
            #  Find UUIDs of all problems where gurobi did not run out of time.
        finished_gurobi_prob_UUIDs = 
            full_Gurobi_tib %>% 
                filter (gurobi_status == "OPTIMAL") %>% 
                select (rsp_UUID)
            
                #  Filter the full data to get problems that gurobi finished 
                #  solving.
        completed_exp_tib <- semi_join (exp_tib, 
                                  finished_gurobi_prob_UUIDs, 
                                  by = "rsp_UUID")
            
                #  Filter the full data to get problems that gurobi finished 
                #  solving.
        unfinished_exp_tib <- anti_join (exp_tib, 
                                  finished_gurobi_prob_UUIDs, 
                                  by = "rsp_UUID")
        
    #-----------------------------------

#browser()
    # gurobi_problem_filter = params$gurobi_problem_filter
    #
    # if (gurobi_problem_filter == "all")
    #     {
    #     cat ("\n\ngurobi_problem_filter == '", gurobi_problem_filter, "' so, ", 
    #          "using all problems instead of problems based on ", 
    #          "\nwhether gurobi finished.\n\n", sep='')
    #     
    #     } else
    #     {
    #         #  Find UUIDs of all problems where gurobi did not run out of time.
    #         
    #     finished_gurobi_prob_UUIDs = 
    #         full_Gurobi_tib %>% 
    #             filter (gurobi_status == "OPTIMAL") %>% 
    #             select (rsp_UUID)
    #         
    #     if (gurobi_problem_filter == "completed")
    #         {
    #             #  Filter the full data to get problems that gurobi finished 
    #             #  solving.
    #         exp_tib <- semi_join (exp_tib, 
    #                               finished_gurobi_prob_UUIDs, 
    #                               by = "rsp_UUID")
    #         
    #         } else if (gurobi_problem_filter == "unfinished")
    #         {
    #             #  Filter the full data to get problems that gurobi finished 
    #             #  solving.
    #         exp_tib <- anti_join (exp_tib, 
    #                               finished_gurobi_prob_UUIDs, 
    #                               by = "rsp_UUID")
    #         } else 
    #         {
    #         cat ("\n\nUnrecognized value '", gurobi_problem_filter, "' for ", 
    #              "gurobi_problem_filter.\n\n")
    #         browser()
    #         }
    #     }

    #---------------------------------------------------------------------------
  
    return (list (exp_tib = exp_tib, 
                  completed_exp_tib = completed_exp_tib, 
                  unfinished_exp_tib = unfinished_exp_tib))
    }

#===============================================================================

    ##  Load combined input data from multiple batches

load_all_data_for_paper_1_2 <- function (rs_method_names_list, base_path, suffix)
    {
    full_Gurobi_tib    = load_input_csv_into_tibble ("Gurobi", base_path, suffix)
    
    exp_tib = full_Gurobi_tib

    if ("Marxan_SA" %in% rs_method_names_list)
        {
        full_Marxan_SA_tib = 
            load_input_csv_into_tibble ("Marxan_SA", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_Marxan_SA_tib)
        }
    
    if ("Marxan_SA_SS" %in% rs_method_names_list)
        {
        full_Marxan_SA_SS_tib = 
            load_input_csv_into_tibble ("Marxan_SA_SS", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_Marxan_SA_SS_tib)
        }
    
    if ("ZL_Backward" %in% rs_method_names_list)
        {
        full_ZL_Backward_tib = 
            load_input_csv_into_tibble ("ZL_Backward", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_ZL_Backward_tib)
        }
    
    if ("SR_Forward" %in% rs_method_names_list)
        {
        full_SR_Forward_tib = 
            load_input_csv_into_tibble ("SR_Forward", base_path, suffix)
        
        exp_tib = bind_rows (exp_tib, 
                             full_SR_Forward_tib)
        }
    
    if ("UR_Forward" %in% rs_method_names_list)
        {
        full_UR_Forward_tib = 
            load_input_csv_into_tibble ("UR_Forward", base_path, suffix)
        
        exp_tib = bind_rows (exp_tib, 
                             full_UR_Forward_tib)
        }
    
    return (list (full_Gurobi_tib = full_Gurobi_tib,
                  exp_tib = exp_tib))
    }

#===============================================================================

add_new_derived_cols_to_FULL_data <- function (exp_tib, rs_method_names_list)
    {
        #  Add a column (at least temporarily - 2020 06 18) to help 
        #  calculate the maximum cost overrun and underrun values and their 
        #  magnifications in the full dataset.
        #  2020 08 18 - This is currently only used in a p2 appendix that 
        #  will probably disappear in the final version, but until then, 
        #  need to keep this variable.  The appendix is currently called 
        #  "Incommensurate errors".
    
     exp_tib %>% 
        mutate (rsp_max_overest_frac = 
                  (1 / (rsp_correct_solution_cost / rsp_num_PUs)) - 1) -> exp_tib
                
   #-----------------------------
    
        #  Add columns for the cost error magnification, 
        #  the log of the cost error fraction and 
        #  the log cost error magnification.
    
    exp_tib %>% 
        mutate (log_err_mag = log10 (err_mag), 
                log_out_err = log10 (rsr_COR_euc_out_err_frac), 
                
                    #  The log values have to be based on absolute value of  
                    #  cost err since cost error can be negative.
                    #  Magnification itself should also be done using positive values so that 
                    #  I don't make mistakes like I did with testing for error correction by 
                    #  saying magnification less than 1, which thereby included magnifications 
                    #  of -10, etc., which aren't error correcting.  
                    #  Since there's not much of interest in anything related to the sign of 
                    #  the magnification, I'm going to only use abs_cost_err_mag values instead, 
                    #  even when they're not logged.
                signed_cost_err_mag = rs_solution_cost_err_frac / 
                                      rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                abs_cost_err_mag = abs_rs_solution_cost_err_frac / 
                                   rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                log_abs_cost_err_mag = log10 (abs_cost_err_mag), 
                log_abs_cost_err = log10 (abs_rs_solution_cost_err_frac),    #rs_solution_cost_err_frac), 
                
                
                rep_shortfall_mag = rsr_COR_spp_rep_shortfall / 
                                    rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                log_rep_shortfall_mag = log10 (rsr_COR_spp_rep_shortfall), 
                log_rep_shortfall = log10 (rsr_COR_spp_rep_shortfall)

                ) -> exp_tib
    
        #-------------------
  
      #  Create a flag that tells whether the current example 
      #  is FN-dominated or FP-dominated, so that you can easily select or 
      #  color or facet on these very different kinds of problems.
  
                        #  Test version of err_labels.
                      # err_labels = c("05-FP_and_FN_not_matched_NO_cost_err", 
                      #                "junk", 
                      #                "morejunk", 
                      #                "03-FN_only_NO_cost_err", 
                      #                "03-FN_only_NO_cost_err", 
                      #                "04-FP_and_FN_matched_NO_cost_err", 
                      #                "02-FP_only_NO_cost_err", 
                      #                "junk", 
                      #                "04-FP_and_FN_matched_NO_cost_err", 
                      #                "junk")
                 
    err_labels = exp_tib$rsp_combined_err_label
    
    FN_dom_indices = which (err_labels == "03-FN_only_NO_cost_err" | 
                            err_labels == "04-FP_and_FN_matched_NO_cost_err")
                
    FP_dom_indices = which (err_labels == "02-FP_only_NO_cost_err" | 
                            err_labels == "05-FP_and_FN_not_matched_NO_cost_err")
                
    dominant_err_type = rep ("UNKNOWN_ERR_TYPE", length (err_labels))
    dominant_err_type [FN_dom_indices] = "FN"
    dominant_err_type [FP_dom_indices] = "FP"
    
    exp_tib %>% 
        mutate (dom_err_type = dominant_err_type) -> exp_tib
  
    #-----------------------------

      #  Create a version of the reserve selector names as an ordered set of 
      #  factors instead of strings.  
      #  This allows control of the order in which panels appear in the 
      #  facetted ggplot outputs.
      #  I might be able to convert the method name column directly without 
      #  creating a separate version of it, but I'm not sure if that would 
      #  end up causing problems somewhere else because something is 
      #  expecting a string instead of a factor.  Seems safer to just make 
      #  a separate factor column.
  
  exp_tib %>% 
    mutate (rs_method_name_fac = factor (rs_method_name, 
                                         levels = rs_method_names_list, 
                                         ordered = TRUE)
            ) -> exp_tib

    #-----------------------------
    
    return (exp_tib)
    }

#===============================================================================

filter_out_unwanted_rows_APP_and_or_COR_data <- function (bdpg_tib, params)
    {
        #  Will generally want to remove problems where the input error 
        #  was greater than 10% so that the explanations can just say 
        #  that all input errors were less than 10%.  
        #  Things that have more than 10% input error are rare and just 
        #  an artifact of the way I added errors using a coin flip at each 
        #  possible error location instead of specifying a number of locations 
        #  to flip and then flipping only those.  With the coin flip method, 
        #  you got something near the desired percentage error but often 
        #  not exactly.  This was what I wanted to do at first when I was 
        #  just doing easy and hard problems and wanted some jitter around 
        #  the exact values.  Once I switched to sampling everywhere instead 
        #  of just doing easy/hard, then this formerly desirable jitter gave 
        #  this little problem with occasionally greater 10% input error.

    if (is.null (params$remove_probs_with_gt_10_pct_input_err))
      params$remove_probs_with_gt_10_pct_input_err = FALSE
    
    if (params$remove_probs_with_gt_10_pct_input_err)
      {
      cat ("\nRemove problems with > 10% input error.\n")      
      bdpg_tib = filter (bdpg_tib, 
                        rsp_euc_realized_Ftot_and_cost_in_err_frac <= 0.1)
      }

    #-----------------------------
    
        ##  Filter out extra batches if desired
    
    if (is.null (params$do_all_batches)) params$do_all_batches = FALSE
    if (! as.logical (params$do_all_batches))
        bdpg_tib = filter (bdpg_tib, (id == "B3" | id =="B5"))
 
    #-----------------------------
    
        ##  Exclude imperfect wraps if desired
    
    if (is.null (params$exclude_imperfect_wraps)) 
        params$exclude_imperfect_wraps = FALSE
    
    if (as.logical (params$exclude_imperfect_wraps))
        bdpg_tib = 
            filter (bdpg_tib, 
                    is.na (rsp_wrap_is_imperfect) | ! rsp_wrap_is_imperfect) 

     #-----------------------------
    
       ##  Exclude APP problems with 0 input err if desired
    
    if (is.null (params$exclude_APP_0_inErr)) 
        params$exclude_APP_0_inErr = FALSE
    
     if (as.logical (params$exclude_APP_0_inErr))
         bdpg_tib = filter (bdpg_tib, 
                           rsp_cor_or_app_str == "COR" | 
                           rsp_euc_realized_Ftot_and_cost_in_err_frac != 0
                                   # is.infinite (err_mag)
                          ) 

    #---------------------------------------------------------------------------
  
    return (bdpg_tib)
    }

#===============================================================================

add_new_derived_cols_to_APP_WRAP_data <- function (app_wrap_tib)
    {
        #  Add magnifications to app data
        
    app_wrap_tib %>% 
      
        mutate (max_FN_FP_rate = pmax (rsp_realized_FP_rate, rsp_realized_FN_rate)) %>% 
      
        mutate (
                #max_FN_FP_rate                = pmax (rsp_realized_FP_rate, rsp_realized_FN_rate), 
                input_error_group_by_max_FN_FP = as.factor (ceiling (100*max_FN_FP_rate)), 
                input_error_group_by_tot_in_err = as.factor (ceiling (100*rsp_euc_realized_Ftot_and_cost_in_err_frac)), 

                #FN_tot_mag        = rsr_COR_euc_out_err_frac / rsp_realized_FN_rate, 
                #FP_tot_mag        = rsr_COR_euc_out_err_frac / rsp_realized_FP_rate, 
                max_FN_FP_tot_mag = rsr_COR_euc_out_err_frac / max_FN_FP_rate, 

                #FN_signed_cost_mag        = rs_solution_cost_err_frac / rsp_realized_FN_rate, 
                #FP_signed_cost_mag        = rs_solution_cost_err_frac / rsp_realized_FP_rate, 
                max_FN_FP_signed_cost_mag = rs_solution_cost_err_frac / max_FN_FP_rate, 
                                
                #FN_abs_cost_mag        = abs_rs_solution_cost_err_frac / rsp_realized_FN_rate, 
                #FP_abs_cost_mag        = abs_rs_solution_cost_err_frac / rsp_realized_FP_rate, 
                max_FN_FP_abs_cost_mag = abs_rs_solution_cost_err_frac / max_FN_FP_rate, 
                
                #FN_shortfall_mag        = rsr_COR_spp_rep_shortfall / rsp_realized_FN_rate, 
                #FP_shortfall_mag        = rsr_COR_spp_rep_shortfall / rsp_realized_FP_rate, 
                max_FN_FP_shortfall_mag = rsr_COR_spp_rep_shortfall / max_FN_FP_rate
                
                ) -> app_wrap_tib
    
    return (app_wrap_tib)
    }

#===============================================================================

filter_out_unwanted_rows_of_APP_data <- function (app_wrap_tib)
    {
        #  Also, make sure to remove any input errors larger than 10%.  
        #  FN errors larger than 10% were being allowed because they don't 
        #  lead to *total input error* larger than 10%.

    app_wrap_tib = 
        filter (app_wrap_tib, 
                    rsp_realized_FN_rate <= 0.1, 
                    rsp_realized_FP_rate <= 0.1)
      
    return (app_wrap_tib)
    }

#===============================================================================

set_up_for_paper_1_2 <- function (params, proj_dir, rs_method_names_list)
    {
        #  Load raw data for each separate reserve selector and 
        #  combine them all into one experiment set.
        #
        #  The singly loaded reserve selector sets can all be ignored 
        #  once the combined dataset except for gurobi's original data.
        #  Gurobi didn't finish running on some problems and we need to be 
        #  able to identify those problems as well as analyze both gurobi's 
        #  performance and the performance of other reserve selectors on 
        #  both kinds of problems.  This may help identify which problems 
        #  are intrinsically more difficult.  
        #  So, in addition to the full combined set of results across all 
        #  reserve selectors, we need to retain the full set of gurobi only 
        #  results even though we toss the full results of all other selectors.
  
    gur_and_exp_tibs = 
        load_all_data_for_paper_1_2 (
                rs_method_names_list, 
                base_path = file.path (proj_dir, "Data/Clean/All_batches/cln_exp."), 
                suffix = ".csv")
    
    #-----------------------------

#write_a_tib_to_csv_file_using_params (gur_and_exp_tibs$exp_tib, "exp_tib__untouched", params)
write_a_tib_to_csv_file_using_params (gur_and_exp_tibs$full_Gurobi_tib, "full_Gurobi_tib__untouched", params)

    unfiltered_exp_tib = remove_irrelevant_cols (gur_and_exp_tibs$exp_tib)
    
#write_a_tib_to_csv_file_using_params (unfiltered_exp_tib, "unfiltered_exp_tib__after_remove_irrelevant_cols", params)
    
    unfiltered_exp_tib = add_new_derived_cols_to_FULL_data (unfiltered_exp_tib, 
                                                            rs_method_names_list)

#write_a_tib_to_csv_file_using_params (unfiltered_exp_tib, "unfiltered_exp_tib__after_add_new_derived_cols_to_FULL_data", params)

     #-----------------------------

        #  Analyze the full data set in terms of which problems gurobi 
        #  did and didn't finish.
        #  Create two new reduced versions of the full set holding just 
        #  problems completed by gurobi and just ones not completed.  
        #  Add both sets to the list of all useful tibbles created so far.
    
    tib_list = 
        get_gurobi_complete_vs_unfinished_inf (gur_and_exp_tibs$full_Gurobi_tib, 
                                               unfiltered_exp_tib, 
                                               params)

    #-----------------------------

    unfiltered_exp_tib = tib_list$exp_tib 

        #  Create dataset containing apparent wrapped data only.

    unfiltered_app_wrap_tib = 
        filter (tib_list$exp_tib, 
                rsp_cor_or_app_str == "APP" & rsp_base_wrap_str  == "Wrap")

        #  All actions before this point include data for COR problems. 
        #  Actions related to errors and magnifications, etc, can only be  
        #  done on APP problems (e.g., to avoid 0 denominator in error 
        #  magnification calculations).  So, a sequence of actions 
        #  similar to the full dataset actions need to be done on the 
        #  APP restricted data.
    
    unfiltered_app_wrap_tib = 
        add_new_derived_cols_to_APP_WRAP_data (unfiltered_app_wrap_tib)
    
     #-----------------------------
  
        #  Filter out things that apply to both APP and COR data.
    filtered_exp_tib = 
        filter_out_unwanted_rows_APP_and_or_COR_data (unfiltered_exp_tib, 
                                                      params)
    filtered_app_wrap_tib = 
        filter_out_unwanted_rows_APP_and_or_COR_data (unfiltered_app_wrap_tib, 
                                                      params)
    
        #  There are a few extra things that need to be filtered out of APP 
        #  data only.
    filtered_app_wrap_tib = 
        filter_out_unwanted_rows_of_APP_data (filtered_app_wrap_tib)

     #-----------------------------
  
        #  Create datasets containing FN and FP-dominant apparent 
        #  wrapped data only.
    filtered_FN_dom_app_wrap_tib = filter (filtered_app_wrap_tib, dom_err_type == "FN")
    filtered_FP_dom_app_wrap_tib = filter (filtered_app_wrap_tib, dom_err_type == "FP")

    #-----------------------------
  
### 2021 04 13 ###  I think this also accidentally includes exp_tib from return of get_gurobi...() and p1 is wrongly using that instead of the filtered_exp_tib version
    tib_list$filtered_exp_tib             = filtered_exp_tib
    tib_list$unfiltered_exp_tib           = unfiltered_exp_tib
    tib_list$filtered_app_wrap_tib        = filtered_app_wrap_tib
    tib_list$unfiltered_app_wrap_tib      = unfiltered_app_wrap_tib
    tib_list$filtered_FN_dom_app_wrap_tib = filtered_FN_dom_app_wrap_tib
    tib_list$filtered_FP_dom_app_wrap_tib = filtered_FP_dom_app_wrap_tib

    #-----------------------------
  
    return (tib_list)
    }

#===============================================================================

