#===============================================================================
#
#                       p8.unifiedDataLoading.v01.R
#
#===============================================================================

#  History

#  v01 December 21,2022
#  
#  - Created by copying bdpgtext/v2_p6_unifiedDataLoading.R to 
#    bdpgtext/R_new/p8.unifiedDataLoading.v01.R.
#  - There are so many R files lying around from all the different earlier 
#    versions of the papers that it's hard to tell which things are actually 
#    being used anymore.  So, I've created the R_new directory and will move 
#    only those files and/or functions that are used by p8 into that directory.  
#    Later, I'll rename the R directory to something like R_old, but for right 
#    now, I want all the old stuff to still work when I have to go in and 
#    explore things I did there.

#===============================================================================

# 2022 12 22 - BTL
# This function isn't currently used.  It's here to remember how to do this 
# and what was done in the p6 unified loading code.  
# The code in this function was cut out of the load_p6_data() function there.  

compute_app_wrap_prob_counts_for_rs_and_FP_FN <- function (p2_app_wrap_tib, 
                                                           filtered_FN_dom_app_wrap_tib, 
                                                           filtered_FP_dom_app_wrap_tib)
{
        #  Compute counts of different numbers of problems so you can use them in 
        #  various tables.
    #  2020 12 11 - BTL
    #  Not sure if the values calculated in this chunk are used anymore.  
    #  They may be calculated now in the Shiny interface instead or 
    #  they may only be used in Appendices now.  
    #  I'm going to leave this chunk in for the moment because Appendix generation 
    #  is currently commented out (to save pdf generation time) while working on 
    #  the text of the paper.  When appendices are being generated again you 
    #  can take this out if these values are not being used there.
    
    #num_app_prob_per_rs = length (which (p2_app_wrap_tib$rs_method_name == "ILP"))
    
    # p2_app_wrap_tib %>% 
    #     group_by (rs_method_name) %>% 
    #     summarize (num_prob = n()) %>% 
    #     ungroup ()  ->  num_app_prob_per_rs
    
    num_app_prob_per_rs = count (p2_app_wrap_tib, rs_method_name)
   
    #----------
     
#    FN_dom_app_wrap_tib = tib_list$filtered_FN_dom_app_wrap_tib
    FN_dom_app_wrap_tib = filtered_FN_dom_app_wrap_tib
    
    # FN_dom_app_wrap_tib %>% 
    #     group_by (rs_method_name) %>% 
    #     summarize (num_prob = n()) %>% 
    #     ungroup ()  ->  num_FN_dom_app_prob_per_rs
    
    num_FN_dom_app_prob_per_rs = count (FN_dom_app_wrap_tib, rs_method_name)
    
    FN_dom_not_empty =(dim (FN_dom_app_wrap_tib)[1] > 0)
    
    #----------
     
#    FP_dom_app_wrap_tib = tib_list$filtered_FP_dom_app_wrap_tib
    FP_dom_app_wrap_tib = filtered_FP_dom_app_wrap_tib

         # FP_dom_app_wrap_tib %>% 
    #     group_by (rs_method_name) %>% 
    #     summarize (num_prob = n()) %>% 
    #     ungroup ()  ->  num_FP_dom_app_prob_per_rs
    
    num_FP_dom_app_prob_per_rs = count (FP_dom_app_wrap_tib, rs_method_name)
    
    #----------
     
    retVals = list (
                    num_app_prob_per_rs = num_app_prob_per_rs, 
                    num_FN_dom_app_prob_per_rs = num_FN_dom_app_prob_per_rs,  
                    num_FP_dom_app_prob_per_rs = num_FP_dom_app_prob_per_rs)
    
    return (retVals)
    }

#===============================================================================

#----------------------
#  Create p1 COR data  
#----------------------

#  Reduce full data set down to just COR problems and 
#  only the columns that are relevant to COR data

create_p1_COR_data <- function (full_initial_exp_tib, 
                                params 
                                )
    {
    cor_tib = filter (full_initial_exp_tib, rsp_cor_or_app_str == "COR")                                           
    cor_tib = select_paper_1_cols (cor_tib) 
        
    # if (params$write_tibs_to_csv  |  params$write_most_important_tibs_to_csv) 
    #     write_a_tib_to_csv_file_using_params (cor_tib, "p1_cor_tib", 
    #                                           params, params$file_type_to_write)
    # 
        #---------------------------------------------------------
        #  Compute number of problems for each reserve selector.      
        #---------------------------------------------------------
    
    num_cor_prob_per_rs = count (cor_tib, rs_method_name)
    
        #--------------------------------------------------------------------
        #  Compute min:max ranges for species, PUs, and solution fractions.    
        #--------------------------------------------------------------------

    cor_base_tib = filter (cor_tib, rsp_base_wrap_str == "Base")
    cor_wrap_tib = filter (cor_tib, rsp_base_wrap_str == "Wrap")
    
    min_base_spp = min (cor_base_tib$rsp_num_spp)
    max_base_spp = max (cor_base_tib$rsp_num_spp)
    min_base_PUs = min (cor_base_tib$rsp_num_occupied_PUs)
    max_base_PUs = max (cor_base_tib$rsp_num_occupied_PUs)
    
    min_wrap_spp = min (cor_wrap_tib$rsp_num_spp)
    max_wrap_spp = max (cor_wrap_tib$rsp_num_spp)
    min_wrap_PUs = min (cor_wrap_tib$rsp_num_occupied_PUs)
    max_wrap_PUs = max (cor_wrap_tib$rsp_num_occupied_PUs)
    
    min_base_sol_frac = min (cor_base_tib$actual_sol_frac_of_landscape)
    max_base_sol_frac = max (cor_base_tib$actual_sol_frac_of_landscape)
    min_wrap_sol_frac = min (cor_wrap_tib$actual_sol_frac_of_landscape)
    max_wrap_sol_frac = max (cor_wrap_tib$actual_sol_frac_of_landscape)
    
        #----------------------------------------------------
        #  Create tib and summary vars for UR_Forward only.  
        #----------------------------------------------------

    cor_tib %>% 
        filter (rs_method_name == "UR_Forward") %>% 
        select (rs_solution_cost_err_frac) -> UR_Forward_tib
    
    num_problems = dim (UR_Forward_tib)[1]
    UR_Forward_non_zero_cost_err_tib = filter (UR_Forward_tib, 
                                               rs_solution_cost_err_frac > 0)
    
    num_non_zero_UR_Forward_cost_errs = dim (UR_Forward_non_zero_cost_err_tib)[1]
    UR_Forward_non_zero_cost_err_pct = 
        round (100 * (num_non_zero_UR_Forward_cost_errs / num_problems), 0)
    
    UR_Forward_median_non_zero_cost_err_pct = 
        round (100 * median (UR_Forward_non_zero_cost_err_tib$rs_solution_cost_err_frac), 0)
    
    UR_Forward_max_non_zero_cost_err_pct = 
        round (100 * max (UR_Forward_non_zero_cost_err_tib$rs_solution_cost_err_frac), 0)
    
        #------------------------------------------------------
        #  Create tib and summary vars for SA_SS only.  
        #------------------------------------------------------

    cor_tib %>% 
        filter (rs_method_name == "SA_SS") %>% 
        select (rs_solution_cost_err_frac) -> SA_SS_tib
    
    num_problems = dim (SA_SS_tib)[1]
    SA_SS_non_zero_cost_err_tib = filter (SA_SS_tib, 
                                               rs_solution_cost_err_frac > 0)
    
    num_non_zero_SA_SS_cost_errs = dim (SA_SS_non_zero_cost_err_tib)[1]
    SA_SS_non_zero_cost_err_pct = 
        round (100 * (num_non_zero_SA_SS_cost_errs / num_problems), 0)
    
    SA_SS_median_non_zero_cost_err_pct = 
        round (100 * median (SA_SS_non_zero_cost_err_tib$rs_solution_cost_err_frac), 0)
    
    SA_SS_max_non_zero_cost_err_pct = 
        round (100 * max (SA_SS_non_zero_cost_err_tib$rs_solution_cost_err_frac), 0)
    
    #---------------------------------------------------------------------------
    
    #  Already created this column in gen_p4_p1_tib().
    
    cor_tib = mutate (cor_tib, log10_sppPUprod = log10 (sppPUprod))
    
    #---------------------------------------------------------------------------
    
    retVals = list (min_base_spp = min_base_spp 
                    , max_base_spp = max_base_spp 
                    
                    , min_wrap_spp = min_wrap_spp 
                    , max_wrap_spp = max_wrap_spp
                    
                    , min_base_PUs = min_base_PUs 
                    , max_base_PUs = max_base_PUs 
                    
                    , min_wrap_PUs = min_wrap_PUs 
                    , max_wrap_PUs = max_wrap_PUs 
                    
                    , cor_tib = cor_tib 
                    , cor_base_tib = cor_base_tib 
                    , cor_wrap_tib = cor_wrap_tib 
            # , p2_app_wrap_tib = p2_app_wrap_tib
                    
                    )

    return (retVals)
    }

#===============================================================================

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

#  A more generic version of load_input_csv_into_tibble() that just loads 
#  a csv file without paying any attention the reserve selector name.  
#  This was created to enable p8 v8 to load all the tibbles and dfs that 
#  are created in p8_v08_prep_data_for_p8_to_load_from_files.Rmd and 
#  written to disk there.
#  Cloned from load_input_csv_into_tibble().
# 2024 01 27 - BTL.

load_file_into_tibble <- function (file_name, base_path)
    {
    input_file_path = file.path (base_path, file_name)

    tib = read_csv (input_file_path)
    full_spec = spec_csv (input_file_path)
    print (full_spec)
     
    tib_col_names = colnames (tib)
    num_cols      = length (tib_col_names)
    
    cat ("\nnum ", file_name, " cols = ", num_cols, sep='')
   
    return (tib)
    }

#===============================================================================

build_joined_scores_tib_for_all_reserve_selectors <- 
    function (individual_rs_tib_list)
    {
    joined_scores_tib = NULL
    rs_names = names (individual_rs_tib_list)
    num_rs = length (rs_names)
    
    for (cur_idx in 1:num_rs)
        {
        cur_rs_name = rs_names [cur_idx]
        cat ("\n", cur_idx, ": cur_rs_name = '", cur_rs_name, sep='')
        
        cur_full_tib = individual_rs_tib_list [[cur_idx]]
        cur_small_tib = select (cur_full_tib, 
                              
                                #  Problem and reserve selector labels                     
                                    # batch_id,     #  Batch ID
                                    rsp_UUID, 
                                
                                    rs_solution_cost_err_frac,
                                    abs_rs_solution_cost_err_frac,

                                    rsr_COR_euc_out_err_frac,

                                    # rsr_APP_spp_rep_shortfall,
                                    # rsr_APP_solution_FRAC_spp_covered,

                                    rsr_COR_spp_rep_shortfall,
                                    rsr_COR_solution_FRAC_spp_covered,
                                    
                                    err_mag
                                ) 
      
        new_small_tib = 
            rename_score_cols_with_rs_name (cur_small_tib, names_to_replace, cur_rs_name)
        
        if (is.null (joined_scores_tib))
            {
            joined_scores_tib = new_small_tib
            
            } else
            {
            joined_scores_tib <- left_join (joined_scores_tib, 
                                            new_small_tib, 
                                            by = "rsp_UUID")
            }
    
        }  #  end for - all rs names
    
        #  This sorting was to make it easier to figure out whether test 
        #  results were correct, but isn't useful or necessary for real data, 
    # joined_scores_tib = arrange (joined_scores_tib, rsp_UUID)
    
    return (joined_scores_tib)
    }

#===============================================================================

rename_score_cols_with_rs_name <- function (cur_small_tib, names_to_replace, cur_rs_name)
    {
    cn = colnames (cur_small_tib)
    cn [which (cn == "rs_solution_cost_err_frac")] = glue ("rs_solution_cost_err_frac__", cur_rs_name)
    cn [which (cn == "abs_rs_solution_cost_err_frac")] = glue ("abs_rs_solution_cost_err_frac__", cur_rs_name)
    cn [which (cn == "rsr_COR_euc_out_err_frac")] = glue ("rsr_COR_euc_out_err_frac__", cur_rs_name)
    # cn [which (cn == "rsr_APP_spp_rep_shortfall")] = glue ("rsr_APP_spp_rep_shortfall__", cur_rs_name)
    # cn [which (cn == "rsr_APP_solution_FRAC_spp_covered")] = glue ("rsr_APP_solution_FRAC_spp_covered__", cur_rs_name)
    cn [which (cn == "rsr_COR_spp_rep_shortfall")] = glue ("rsr_COR_spp_rep_shortfall__", cur_rs_name)
    cn [which (cn == "rsr_COR_solution_FRAC_spp_covered")] = glue ("rsr_COR_solution_FRAC_spp_covered__", cur_rs_name)
    cn [which (cn == "err_mag")] = glue ("err_mag__", cur_rs_name)
    colnames (cur_small_tib) = cn
    
    return (cur_small_tib)
    }

#===============================================================================

build_score_diffs_tib <- function (sorted_joined_scores_tib)
    {
    score_diffs_tib = mutate (sorted_joined_scores_tib, 
                          
                            # #----------  xxx
                            # 
                            # diff__xxx__SA__ILP = xxx__SA - xxx__ILP, 
                            # diff__xxx__ILP__SA = -diff__xxx__SA__ILP, 
                            # 
                            # diff__xxx__SA_SS__ILP = xxx__SA_SS - xxx__ILP, 
                            # diff__xxx__ILP__SA_SS = -diff__xxx__SA_SS__ILP, 
                            # 
                            # diff__xxx__UR_Forward__ILP = xxx__UR_Forward - xxx__ILP, 
                            # diff__xxx__ILP__UR_Forward = -diff__xxx__UR_Forward__ILP, 
                            # 
                            # 
                            # diff__xxx__SA__SA_SS = xxx__SA - xxx__SA_SS, 
                            # diff__xxx__SA_SS__SA = -diff__xxx__SA__SA_SS, 
                            # 
                            # diff__xxx__SA__UR_Forward = xxx__SA - xxx__UR_Forward, 
                            # diff__xxx__UR_Forward__SA = -diff__xxx__SA__UR_Forward, 
                            # 
                            # 
                            # diff__xxx__SA_SS__UR_Forward = xxx__SA_SS - xxx__UR_Forward, 
                            # diff__xxx__UR_Forward__SA_SS = -diff__xxx__SA_SS__UR_Forward, 
                            
                            #----------  rs_solution_cost_err_frac
                            
                            diff__rs_solution_cost_err_frac__SA__ILP = rs_solution_cost_err_frac__SA - rs_solution_cost_err_frac__ILP, 
                            diff__rs_solution_cost_err_frac__ILP__SA = -diff__rs_solution_cost_err_frac__SA__ILP, 
                            
                            diff__rs_solution_cost_err_frac__SA_SS__ILP = rs_solution_cost_err_frac__SA_SS - rs_solution_cost_err_frac__ILP, 
                            diff__rs_solution_cost_err_frac__ILP__SA_SS = -diff__rs_solution_cost_err_frac__SA_SS__ILP, 
                            
                            diff__rs_solution_cost_err_frac__UR_Forward__ILP = rs_solution_cost_err_frac__UR_Forward - rs_solution_cost_err_frac__ILP, 
                            diff__rs_solution_cost_err_frac__ILP__UR_Forward = -diff__rs_solution_cost_err_frac__UR_Forward__ILP, 
                            
                            
                            diff__rs_solution_cost_err_frac__SA__SA_SS = rs_solution_cost_err_frac__SA - rs_solution_cost_err_frac__SA_SS, 
                            diff__rs_solution_cost_err_frac__SA_SS__SA = -diff__rs_solution_cost_err_frac__SA__SA_SS, 
                            
                            diff__rs_solution_cost_err_frac__SA__UR_Forward = rs_solution_cost_err_frac__SA - rs_solution_cost_err_frac__UR_Forward, 
                            diff__rs_solution_cost_err_frac__UR_Forward__SA = -diff__rs_solution_cost_err_frac__SA__UR_Forward, 
                            
                            
                            diff__rs_solution_cost_err_frac__SA_SS__UR_Forward = rs_solution_cost_err_frac__SA_SS - rs_solution_cost_err_frac__UR_Forward, 
                            diff__rs_solution_cost_err_frac__UR_Forward__SA_SS = -diff__rs_solution_cost_err_frac__SA_SS__UR_Forward, 
                            
                            #----------  abs_rs_solution_cost_err_frac
                            
                            diff__abs_rs_solution_cost_err_frac__SA__ILP = abs_rs_solution_cost_err_frac__SA - abs_rs_solution_cost_err_frac__ILP, 
                            diff__abs_rs_solution_cost_err_frac__ILP__SA = -diff__abs_rs_solution_cost_err_frac__SA__ILP, 
                            
                            diff__abs_rs_solution_cost_err_frac__SA_SS__ILP = abs_rs_solution_cost_err_frac__SA_SS - abs_rs_solution_cost_err_frac__ILP, 
                            diff__abs_rs_solution_cost_err_frac__ILP__SA_SS = -diff__abs_rs_solution_cost_err_frac__SA_SS__ILP, 
                            
                            diff__abs_rs_solution_cost_err_frac__UR_Forward__ILP = abs_rs_solution_cost_err_frac__UR_Forward - abs_rs_solution_cost_err_frac__ILP, 
                            diff__abs_rs_solution_cost_err_frac__ILP__UR_Forward = -diff__abs_rs_solution_cost_err_frac__UR_Forward__ILP, 
                            
                            
                            diff__abs_rs_solution_cost_err_frac__SA__SA_SS = abs_rs_solution_cost_err_frac__SA - abs_rs_solution_cost_err_frac__SA_SS, 
                            diff__abs_rs_solution_cost_err_frac__SA_SS__SA = -diff__abs_rs_solution_cost_err_frac__SA__SA_SS, 
                            
                            diff__abs_rs_solution_cost_err_frac__SA__UR_Forward = abs_rs_solution_cost_err_frac__SA - abs_rs_solution_cost_err_frac__UR_Forward, 
                            diff__abs_rs_solution_cost_err_frac__UR_Forward__SA = -diff__abs_rs_solution_cost_err_frac__SA__UR_Forward, 
                            
                            
                            diff__abs_rs_solution_cost_err_frac__SA_SS__UR_Forward = abs_rs_solution_cost_err_frac__SA_SS - abs_rs_solution_cost_err_frac__UR_Forward, 
                            diff__abs_rs_solution_cost_err_frac__UR_Forward__SA_SS = -diff__abs_rs_solution_cost_err_frac__SA_SS__UR_Forward, 
                            
                            #----------  rsr_COR_euc_out_err_frac
                            
                            diff__rsr_COR_euc_out_err_frac__SA__ILP = rsr_COR_euc_out_err_frac__SA - rsr_COR_euc_out_err_frac__ILP, 
                            diff__rsr_COR_euc_out_err_frac__ILP__SA = -diff__rsr_COR_euc_out_err_frac__SA__ILP, 
                            
                            diff__rsr_COR_euc_out_err_frac__SA_SS__ILP = rsr_COR_euc_out_err_frac__SA_SS - rsr_COR_euc_out_err_frac__ILP, 
                            diff__rsr_COR_euc_out_err_frac__ILP__SA_SS = -diff__rsr_COR_euc_out_err_frac__SA_SS__ILP, 
                            
                            diff__rsr_COR_euc_out_err_frac__UR_Forward__ILP = rsr_COR_euc_out_err_frac__UR_Forward - rsr_COR_euc_out_err_frac__ILP, 
                            diff__rsr_COR_euc_out_err_frac__ILP__UR_Forward = -diff__rsr_COR_euc_out_err_frac__UR_Forward__ILP, 
                            
                            
                            diff__rsr_COR_euc_out_err_frac__SA__SA_SS = rsr_COR_euc_out_err_frac__SA - rsr_COR_euc_out_err_frac__SA_SS, 
                            diff__rsr_COR_euc_out_err_frac__SA_SS__SA = -diff__rsr_COR_euc_out_err_frac__SA__SA_SS, 
                            
                            diff__rsr_COR_euc_out_err_frac__SA__UR_Forward = rsr_COR_euc_out_err_frac__SA - rsr_COR_euc_out_err_frac__UR_Forward, 
                            diff__rsr_COR_euc_out_err_frac__UR_Forward__SA = -diff__rsr_COR_euc_out_err_frac__SA__UR_Forward, 
                            
                            
                            diff__rsr_COR_euc_out_err_frac__SA_SS__UR_Forward = rsr_COR_euc_out_err_frac__SA_SS - rsr_COR_euc_out_err_frac__UR_Forward, 
                            diff__rsr_COR_euc_out_err_frac__UR_Forward__SA_SS = -diff__rsr_COR_euc_out_err_frac__SA_SS__UR_Forward, 
                            
                            # #----------  rsr_APP_spp_rep_shortfall

                            # diff__rsr_APP_spp_rep_shortfall__SA__ILP = rsr_APP_spp_rep_shortfall__SA - rsr_APP_spp_rep_shortfall__ILP,
                            # diff__rsr_APP_spp_rep_shortfall__ILP__SA = -diff__rsr_APP_spp_rep_shortfall__SA__ILP,
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__SA_SS__ILP = rsr_APP_spp_rep_shortfall__SA_SS - rsr_APP_spp_rep_shortfall__ILP,
                            # diff__rsr_APP_spp_rep_shortfall__ILP__SA_SS = -diff__rsr_APP_spp_rep_shortfall__SA_SS__ILP,
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__UR_Forward__ILP = rsr_APP_spp_rep_shortfall__UR_Forward - rsr_APP_spp_rep_shortfall__ILP,
                            # diff__rsr_APP_spp_rep_shortfall__ILP__UR_Forward = -diff__rsr_APP_spp_rep_shortfall__UR_Forward__ILP,
                            # 
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__SA__SA_SS = rsr_APP_spp_rep_shortfall__SA - rsr_APP_spp_rep_shortfall__SA_SS,
                            # diff__rsr_APP_spp_rep_shortfall__SA_SS__SA = -diff__rsr_APP_spp_rep_shortfall__SA__SA_SS,
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__SA__UR_Forward = rsr_APP_spp_rep_shortfall__SA - rsr_APP_spp_rep_shortfall__UR_Forward,
                            # diff__rsr_APP_spp_rep_shortfall__UR_Forward__SA = -diff__rsr_APP_spp_rep_shortfall__SA__UR_Forward,
                            # 
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__SA_SS__UR_Forward = rsr_APP_spp_rep_shortfall__SA_SS - rsr_APP_spp_rep_shortfall__UR_Forward,
                            # diff__rsr_APP_spp_rep_shortfall__UR_Forward__SA_SS = -diff__rsr_APP_spp_rep_shortfall__SA_SS__UR_Forward,

                            # #----------  rsr_APP_solution_FRAC_spp_covered
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__SA__ILP = rsr_APP_solution_FRAC_spp_covered__SA - rsr_APP_solution_FRAC_spp_covered__ILP, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__ILP__SA = -diff__rsr_APP_solution_FRAC_spp_covered__SA__ILP, 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__SA_SS__ILP = rsr_APP_solution_FRAC_spp_covered__SA_SS - rsr_APP_solution_FRAC_spp_covered__ILP, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__ILP__SA_SS = -diff__rsr_APP_solution_FRAC_spp_covered__SA_SS__ILP, 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__ILP = rsr_APP_solution_FRAC_spp_covered__UR_Forward - rsr_APP_solution_FRAC_spp_covered__ILP, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__ILP__UR_Forward = -diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__ILP, 
                            # 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__SA__SA_SS = rsr_APP_solution_FRAC_spp_covered__SA - rsr_APP_solution_FRAC_spp_covered__SA_SS, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__SA_SS__SA = -diff__rsr_APP_solution_FRAC_spp_covered__SA__SA_SS, 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__SA__UR_Forward = rsr_APP_solution_FRAC_spp_covered__SA - rsr_APP_solution_FRAC_spp_covered__UR_Forward, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__SA = -diff__rsr_APP_solution_FRAC_spp_covered__SA__UR_Forward, 
                            # 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__SA_SS__UR_Forward = rsr_APP_solution_FRAC_spp_covered__SA_SS - rsr_APP_solution_FRAC_spp_covered__UR_Forward, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__SA_SS = -diff__rsr_APP_solution_FRAC_spp_covered__SA_SS__UR_Forward, 
                            
                            #----------  rsr_COR_spp_rep_shortfall
                            
                            diff__rsr_COR_spp_rep_shortfall__SA__ILP = rsr_COR_spp_rep_shortfall__SA - rsr_COR_spp_rep_shortfall__ILP, 
                            diff__rsr_COR_spp_rep_shortfall__ILP__SA = -diff__rsr_COR_spp_rep_shortfall__SA__ILP, 
                            
                            diff__rsr_COR_spp_rep_shortfall__SA_SS__ILP = rsr_COR_spp_rep_shortfall__SA_SS - rsr_COR_spp_rep_shortfall__ILP, 
                            diff__rsr_COR_spp_rep_shortfall__ILP__SA_SS = -diff__rsr_COR_spp_rep_shortfall__SA_SS__ILP, 
                            
                            diff__rsr_COR_spp_rep_shortfall__UR_Forward__ILP = rsr_COR_spp_rep_shortfall__UR_Forward - rsr_COR_spp_rep_shortfall__ILP, 
                            diff__rsr_COR_spp_rep_shortfall__ILP__UR_Forward = -diff__rsr_COR_spp_rep_shortfall__UR_Forward__ILP, 
                            
                            
                            diff__rsr_COR_spp_rep_shortfall__SA__SA_SS = rsr_COR_spp_rep_shortfall__SA - rsr_COR_spp_rep_shortfall__SA_SS, 
                            diff__rsr_COR_spp_rep_shortfall__SA_SS__SA = -diff__rsr_COR_spp_rep_shortfall__SA__SA_SS, 
                            
                            diff__rsr_COR_spp_rep_shortfall__SA__UR_Forward = rsr_COR_spp_rep_shortfall__SA - rsr_COR_spp_rep_shortfall__UR_Forward, 
                            diff__rsr_COR_spp_rep_shortfall__UR_Forward__SA = -diff__rsr_COR_spp_rep_shortfall__SA__UR_Forward, 
                            
                            
                            diff__rsr_COR_spp_rep_shortfall__SA_SS__UR_Forward = rsr_COR_spp_rep_shortfall__SA_SS - rsr_COR_spp_rep_shortfall__UR_Forward, 
                            diff__rsr_COR_spp_rep_shortfall__UR_Forward__SA_SS = -diff__rsr_COR_spp_rep_shortfall__SA_SS__UR_Forward, 
                            
                            #----------  rsr_COR_solution_FRAC_spp_covered
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__SA__ILP = rsr_COR_solution_FRAC_spp_covered__SA - rsr_COR_solution_FRAC_spp_covered__ILP, 
                            diff__rsr_COR_solution_FRAC_spp_covered__ILP__SA = -diff__rsr_COR_solution_FRAC_spp_covered__SA__ILP, 
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__SA_SS__ILP = rsr_COR_solution_FRAC_spp_covered__SA_SS - rsr_COR_solution_FRAC_spp_covered__ILP, 
                            diff__rsr_COR_solution_FRAC_spp_covered__ILP__SA_SS = -diff__rsr_COR_solution_FRAC_spp_covered__SA_SS__ILP, 
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__ILP = rsr_COR_solution_FRAC_spp_covered__UR_Forward - rsr_COR_solution_FRAC_spp_covered__ILP, 
                            diff__rsr_COR_solution_FRAC_spp_covered__ILP__UR_Forward = -diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__ILP, 
                            
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__SA__SA_SS = rsr_COR_solution_FRAC_spp_covered__SA - rsr_COR_solution_FRAC_spp_covered__SA_SS, 
                            diff__rsr_COR_solution_FRAC_spp_covered__SA_SS__SA = -diff__rsr_COR_solution_FRAC_spp_covered__SA__SA_SS, 
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__SA__UR_Forward = rsr_COR_solution_FRAC_spp_covered__SA - rsr_COR_solution_FRAC_spp_covered__UR_Forward, 
                            diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__SA = -diff__rsr_COR_solution_FRAC_spp_covered__SA__UR_Forward, 
                            
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__SA_SS__UR_Forward = rsr_COR_solution_FRAC_spp_covered__SA_SS - rsr_COR_solution_FRAC_spp_covered__UR_Forward, 
                            diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__SA_SS = -diff__rsr_COR_solution_FRAC_spp_covered__SA_SS__UR_Forward, 
                            
                            #----------  err_mag
                            
                            diff__err_mag__SA__ILP = err_mag__SA - err_mag__ILP, 
                            diff__err_mag__ILP__SA = -diff__err_mag__SA__ILP, 
                            
                            diff__err_mag__SA_SS__ILP = err_mag__SA_SS - err_mag__ILP, 
                            diff__err_mag__ILP__SA_SS = -diff__err_mag__SA_SS__ILP, 
                            
                            diff__err_mag__UR_Forward__ILP = err_mag__UR_Forward - err_mag__ILP, 
                            diff__err_mag__ILP__UR_Forward = -diff__err_mag__UR_Forward__ILP, 
                            
                            
                            diff__err_mag__SA__SA_SS = err_mag__SA - err_mag__SA_SS, 
                            diff__err_mag__SA_SS__SA = -diff__err_mag__SA__SA_SS, 
                            
                            diff__err_mag__SA__UR_Forward = err_mag__SA - err_mag__UR_Forward, 
                            diff__err_mag__UR_Forward__SA = -diff__err_mag__SA__UR_Forward, 
                            
                            
                            diff__err_mag__SA_SS__UR_Forward = err_mag__SA_SS - err_mag__UR_Forward, 
                            diff__err_mag__UR_Forward__SA_SS = -diff__err_mag__SA_SS__UR_Forward
                            )
    
    return (score_diffs_tib)
    }

#===============================================================================

    #  In case anyone wants to analyze the data used here outside of this file, 
    #  allow them to set parameters in the header of this file to dump the 
    #  four main tibbles to csv files in a disk location of their choice.
    #  The "include=" option for this knitr chunk controls whether the writing 
    #  is noted in the output file.  
    #  
    #  To control whether these csv files are written or not, set the 
    #  write_tibs_to_csv flag in the header to TRUE or FALSE.
    #  
    #  To specify the directory to write to, set the data_out_loc flag 
    #  in the header to the desired directory.

write_a_tib_to_csv_file_using_params <- function (tib_to_write, 
                                                  tib_name, 
                                                  params, 
                                                  file_type_to_write = "both")
    {

    write_csv = TRUE
    write_rds = TRUE
    if (file_type_to_write == "csv") write_rds = FALSE
    if (file_type_to_write == "rds") write_csv = FALSE

    if (is.null (params$relative_path_to_data_out_loc)) 
        stop (paste0 ("In write_a_tib_to_csv_file_using_params(), \n",
                      "params$relative_path_to_data_out_loc is NULL.  ", 
                      "A relative path must be given in params list.\n"))
    data_out_loc = file.path (here(), params$relative_path_to_data_out_loc)
  
    # exclude_imperfect_wraps = vb (params$exclude_imperfect_wraps, 
    #                               def_on_empty=TRUE)
    exclude_imperfect_wraps = bdpg::vb (params$exclude_imperfect_wraps)
    
    add_gen_time_to_csv_name = bdpg::vb (params$add_gen_time_to_csv_name, 
                                         def_on_empty=TRUE)

        #  Make sure that gurobi_problem_filter has a value and it's legal.  
        #  I'm choosing to make it a fatal error to have no value, 
        #  but it probably wouldn't be terrible if it set the value to "all" 
        #  if there was a NULL value.  
        #  I'm choosing the error though to force someone to explicitly 
        #  choose which gurobi problems they want, since it's a more 
        #  important choice than many of the other options here that I've 
        #  given default values.

    gurobi_problem_filter = params$gurobi_problem_filter
    if (is.null (gurobi_problem_filter) |
                  ! ((gurobi_problem_filter == "all") | 
                     (gurobi_problem_filter == "completed") | 
                     (gurobi_problem_filter == "unfinished"))
        )
        stop (paste0 ("In write_a_tib_to_csv_file_using_params(), \n",
                      "params$gurobi_problem_filter is '", 
                      gurobi_problem_filter, "'.  ", 
                      "Must be 'all' or 'completed' or 'unfinished'.\n"))
                  

    write_a_tib_to_csv_file (tib_to_write, 
                             tib_name, 
                             data_out_loc, 
                             gurobi_problem_filter, 
                             exclude_imperfect_wraps, 
                             add_gen_time_to_csv_name, 
                             write_csv, 
                             write_rds
                             )
    }

##### Called from write_a_tib_to_csv_file_using_params()
write_a_tib_to_csv_file <- function (tib_to_write, 
                                     tib_name, 
                                     data_out_loc, 
                          gurobi_problem_filter, 
                          exclude_imperfect_wraps, 
                          add_gen_time_to_csv_name, 
                                     write_csv = TRUE, 
                                     write_rds = TRUE)
    {
        #---------------------------------------------------------------
        #  Build base csv file name including current option settings.
        #---------------------------------------------------------------
  
    file_name_including_options = 
      paste0 (tib_name, 
             ".gurobi__", gurobi_problem_filter, 
             ".exclude_imperfect_wraps__", exclude_imperfect_wraps)
    
        #----------------------------------------------------------
        #  Add date and time that file was generated, if desired.
        #----------------------------------------------------------
    
    if (add_gen_time_to_csv_name)
        {
        gen_at_time = paste0 (".written_at__", Sys.time())

            #  Sys.time string has spaces and colons that can mess up 
            #  legal file name on some systems so replace those.
        gen_at_time = str_replace_all (gen_at_time, " ", "_")
        gen_at_time = str_replace_all (gen_at_time, ":", "-")
        
        file_name_including_options = paste0 (file_name_including_options, 
                                              gen_at_time)
        }

        #--------------------------------------------------------------------
        #  Turn the base name into a fully specified csv file name and path 
        #  write the file.
        #--------------------------------------------------------------------
    
    VERBOSE_WRITING = FALSE  #  2022 12 31 - Don't want to use params for this 
                             #               at the moment, because it means 
                             #               adding params to every call to 
                             #               this function and it's meant to 
                             #               be a fairly general utility function.
    if (write_csv)
        {
        csv_file_name_including_options = paste0 (file_name_including_options, ".csv")
        csv_full_outfile_path = file.path (data_out_loc, csv_file_name_including_options)
        if (VERBOSE_WRITING) cat ("Writing tib '", tib_name, "' to '", csv_full_outfile_path, "'\n", sep='')
        write.csv (tib_to_write, csv_full_outfile_path, 
                   row.names=FALSE, quote=TRUE)
        }
 
    if (write_rds)
        {
        rds_file_name_including_options = paste0 (file_name_including_options, ".rds")
        rds_full_outfile_path = file.path (data_out_loc, rds_file_name_including_options)
        if (VERBOSE_WRITING) cat ("Writing RDS '", tib_name, "' to '", rds_full_outfile_path, "'\n", sep='')
        saveRDS (tib_to_write, rds_full_outfile_path)
        }
    }

#===============================================================================

    #  This function loads the full data set for all the reserve selectors 
    #  that are chosen for inclusion in this run (indicated by the 
    #  rs_method_names_list).  This includes both COR and APP data.
    #  
    #  It also joins gurobi performance information to each reserve selector's 
    #  output based on the corresponding gurobi problem output line.  
    #  This makes it easier to tell whether a given problem was difficult 
    #  for gurobi even when you're looking at the output for a different 
    #  reserve selector.  
    #  
    #  The function also adds some derived columns of use to all downstream 
    #  actions (e.g., rs_method_name_fac) and filters out rows that have been 
    #  specified for removal in the params list (e.g., imperfect wraps).
    #  
    #  Cloned from p1-2 set_up_for_paper_1_2().

load_and_build_FULL_data_for_all_reserve_selectors <- function (params, 
                                                                proj_dir, 
                                                                rs_method_names_list, 
                                                                relative_path_to_input_data, 
                                                                bdpg_p_needs_fixing)
    {
    unfiltered_exp_tib = 
        p4_load_full_unfiltered_data_with_joined_gurobi_perf_inf (params, 
                                                               proj_dir, 
                                                               rs_method_names_list, 
                                                               relative_path_to_input_data, 
                                                               bdpg_p_needs_fixing)
  
        #  Some columns have inappropriate names in that they are 
        #  meaningless/confusing or will be duplicated by downstream actions 
        #  (e.g., ).  
    unfiltered_exp_tib = rename (unfiltered_exp_tib, 
                                 
                                    #  id is a column name generated by 
                                    #  tidymodels, so rename bdpg's id to 
                                    #  specify the kind of id it is.
                                 batch_id = id, 
                                 
                                    #  From the current name, you might think 
                                    #  that it was pointing to a COR Base problem, 
                                    #  but it's not.  It points to the COR Wrap 
                                    #  problem that has error added to it.  
                                    #  In the future, you may need to also allow 
                                    #  for situations where you might want to 
                                    #  sequentially add different kinds of error 
                                    #  to a problem.  That would imply that the 
                                    #  problem getting error added to it could 
                                    #  be either a COR problem or another APP 
                                    #  problem that's earlier in the sequence.  
                                    #  Similarly, there's no reason you 
                                    #  couldn't add error to a COR Base problem; 
                                    #  I just haven't done that in these 
                                    #  experiments.  In all of these future 
                                    #  cases, you might want to have a variable 
                                    #  called something more like 
                                    #  rsp_UUID_of_UNDERLYING_problem_that_has_err_added.
                                    #  Here though, I'm going to just rename 
                                    #  this to exactly what it's point at now 
                                    #  (2022 12 26).
                                 rsp_UUID_of_COR_Wrap_problem_that_has_err_added = 
                                    rsp_UUID_of_base_problem_that_has_err_added, 
                                 
                                    #  Also renaming this to make it clearer 
                                    #  exactly what it's pointing at.
                                 rsp_UUID_of_COR_Base_problem_that_is_wrapped = 
                                   rsp_UUID_of_base_problem_that_is_wrapped, 
                                 
                                    #  The graph packages each return a different 
                                    #  number than bdpg for the number of PUs.  
                                    #  They return the number of occupied PUs 
                                    #  while bdpg returns the sum of both occupied
                                    #  and unoccupied PUs.  The igraph code 
                                    #  returns it in ig_bottom and the bipartite 
                                    #  package returns it in number.of.PUs.
                                    #  I'm going to change rsp_num_PUs so that 
                                    #  I have 2 columns related to the number 
                                    #  of PUs, i.e., one for the occupied PUs 
                                    #  count and one for the combined count of 
                                    #  occupied and unoccupied.  
                                    #  I'm purposely getting rid of the old 
                                    #  rsp_num_PUs column name by renaming it 
                                    #  to indicate it's the combined count for 
                                    #  occupied and unoccupied so that the old, 
                                    #  ambiguous name can't slip through 
                                    #  somewhere in cloned code.  
                                    #  I'm arbitrarily choosing the bipartite 
                                    #  value as the occupied count, just because 
                                    #  its name is more obvious than ig_bottom.
                                    #  Regardless, this line of code will break 
                                    #  if bipartite measures aren't run, but 
                                    #  they're always run in the current set of 
                                    #  experiments so I'm not going to worry 
                                    #  about the more general case right now 
                                    #  (2022 12 26).
                                 rsp_combined_num_occ_and_unocc_PUs = rsp_num_PUs, 
                                 rsp_num_occupied_PUs = number.of.PUs    #  value returned by bipartite pkg
                                )

        #  Derive some new columns that are computed from the other columns.
        #  
        #  Note that adding these new derived columns needs to come after 
        #  the renaming of columns above because it uses the new column 
        #  names in one of the calculations.
    unfiltered_exp_tib = 
        add_new_derived_cols_to_FULL_data (unfiltered_exp_tib, 
                                           rs_method_names_list)

    
    return (unfiltered_exp_tib)
    }

#===============================================================================

p4_load_full_unfiltered_data_with_joined_gurobi_perf_inf <- function (params, 
                                                                   proj_dir, 
                                                                   rs_method_names_list, 
                                                                   relative_path_to_input_data, 
                                                                   bdpg_p_needs_fixing)
    {
        #  Load raw data for each separate reserve selector into a tib and 
        #  then combine all of the tibs into one big experiment tib.  
        #  Ignore all the individual reserve selector tibs except for gurobi's.  
        #  
        #  ILP didn't finish running on some problems and we need to be 
        #  able to identify those problems as well as analyze both gurobi's 
        #  performance and the performance of other reserve selectors on 
        #  both kinds of problems.  This may help identify which problems 
        #  are intrinsically more difficult.  
        #  
        #  For each problem, copy gurobi's performance information (e.g., 
        #  run time, whether it completed, etc.) into extra columns of each 
        #  of the other reserve selectors for the corresponding problem.  
        #  These extra columns were already created by the row_bind() operation 
        #  that merged all the reserve selector sets since the gurobi data set 
        #  already had those columns and row_bind() matches columns when it 
        #  binds two sets but adds new columns if there are non-matching 
        #  columns. 
        #  
        #  Cloned from p1-2 new__load_all_data_for_paper_1_2() and 
        #  p3 read_data_for_all_reserve_selectors().

    gur_and_exp_tibs = 
                # # load_all_data_for_paper_1_2 (
                # new__load_all_data_for_paper_1_2 (
        load_data_for_all_reserve_selectors (
                rs_method_names_list, 
                base_path = file.path (proj_dir, relative_path_to_input_data), 
                suffix = ".csv")
    
    #-----------------------------
    
    unfiltered_full_exp_tib = 
        apply_corrections_to_FULL_data_if_requested (gur_and_exp_tibs$exp_tib, 
                                                     bdpg_p_needs_fixing)
    
    unfiltered_full_gurobi_tib = 
        apply_corrections_to_FULL_data_if_requested (gur_and_exp_tibs$full_ILP_tib, 
                                                     bdpg_p_needs_fixing)
    
    #-----------------------------

    if (params$write_tibs_to_csv) 
        {
        write_a_tib_to_csv_file_using_params (
                                      unfiltered_full_exp_tib, 
                                      "bdpg_indata_step_01__exp_tib__untouched", 
                                      params)
      
        write_a_tib_to_csv_file_using_params (
                              unfiltered_full_gurobi_tib, 
                              "bdpg_indata_step_01__full_ILP_tib__untouched", 
                              params)
        }
        
    #-----------------------------

        #  Remove gurobi performance information columns.  
        #  The values for all reserve selectors except gurobi are empty.  
        #  The columns were added automatically by the row_bind() that created  
        #  the full set since they were columns in the gurobi set.  
        #  They will interfere with the join in the next function call, 
        #  (in add_corresponding_gurobi_run_inf_to_each_rs_run()), because they 
        #  have values for the gurobi lines and the join will create a new 
        #  version of the columns (using a .x or .y suffix) so that it doesn't 
        #  write over the top of the existing values.  I think that there's 
        #  an argument to left.join() to deal with that, but I want to make 
        #  sure exactly what's going on here so I'm just deleting the existing 
        #  columns before the join happens.  The non-empty values in the 
        #  existing columns are only the values for gurobi and the joined  
        #  columns will duplicate those values as well as fill in the empty 
        #  values with the correct matching ones from gurobi.

    unfiltered_exp_tib_minus_empty_gurobi_perf_cols = 
        select (unfiltered_full_exp_tib,
                -gurobi_status,
                -gurobi_mipgap,
                -gurobi_objbound,
                -gurobi_itercount,
                -gurobi_runtime)

    if (params$write_tibs_to_csv) 
        {
        write_a_tib_to_csv_file_using_params (
                              unfiltered_exp_tib_minus_empty_gurobi_perf_cols, 
                              "unfiltered_exp_tib_minus_empty_gurobi_perf_cols", 
                              params)
        }

     #-----------------------------

        #  Add columns to the full set indicating for each row, information 
        #  about how gurobi did on the problem addressed in that row.  
        #  For example, regardless of what reserve selector/problem pair is 
        #  described by that row, how did gurobi do on that same problem, 
        #  e.g., did gurobi run to completion on that problem.  
        #  This will allows us to filter out or include problems that gurobi 
        #  found difficult.  

    unfiltered_exp_tib_joined_with_gurobi_performance_inf_by_problem = 
        add_corresponding_gurobi_run_inf_to_each_rs_run (
                                unfiltered_full_gurobi_tib, 
                                unfiltered_exp_tib_minus_empty_gurobi_perf_cols, 
                                params)

    if (params$write_tibs_to_csv) 
        {
        write_a_tib_to_csv_file_using_params (
            unfiltered_exp_tib_joined_with_gurobi_performance_inf_by_problem, 
            "unfiltered_exp_tib_joined_with_gurobi_performance_inf_by_problem", 
            params)
        }
    
    return (unfiltered_exp_tib_joined_with_gurobi_performance_inf_by_problem)
    }

#===============================================================================

    #  Load combined input data from multiple batches and reserve selectors.
    #  Cloned from new__load_all_data_for_paper_1_2().  

load_data_for_all_reserve_selectors <- function (rs_method_names_list, base_path, suffix)
    {
# #    full_ILP_tib    = load_input_csv_into_tibble ("ILP", base_path, suffix)
#     full_ILP_tib    = load_input_csv_into_tibble ("ILP", base_path, suffix)
    full_ILP_tib    = load_input_csv_into_tibble ("Gurobi", base_path, suffix)

    #---------------------------------------------
        #  Small bug fix - 2021 04 16 - BTL
        #  gurobi_runtime is added twice in gurobi input file that was just 
        #  read in, so second occurrence is gurobi_runtime.1.
        #  Not sure when the error was made.  Could have been in bdpg 
        #  or in data aggregation after runs.  
        #  If you fix this in a way that causes the gurobi input file to be 
        #  changed and no longer have the ".1" column, then you also need to 
        #  remove this select() call in load_all_...() that removes that column.
        #  Note that I did check after the file was loaded to see if the two 
        #  columns are identical, and they are, i.e., 
        #  gurobi_runtime == gurobi_runtime.1.
    full_ILP_tib = select (full_ILP_tib, -gurobi_runtime.1)    
    #---------------------------------------------
    
    exp_tib = full_ILP_tib

    individual_rs_tib_list = list (ILP = full_ILP_tib)
    
    if ("SA" %in% rs_method_names_list)
        {
        full_SA_tib = 
            # load_input_csv_into_tibble ("SA", base_path, suffix)
            load_input_csv_into_tibble ("Marxan_SA", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_SA_tib)
        
        individual_rs_tib_list [["SA"]] = full_SA_tib
        }
    
    if ("SA_SS" %in% rs_method_names_list)
        {
        full_SA_SS_tib = 
            # load_input_csv_into_tibble ("SA_SS", base_path, suffix)
            load_input_csv_into_tibble ("Marxan_SA_SS", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_SA_SS_tib)
        
        individual_rs_tib_list [["SA_SS"]] = full_SA_SS_tib
        }
    
    if ("ZL_Backward" %in% rs_method_names_list)
        {
        full_ZL_Backward_tib = 
            load_input_csv_into_tibble ("ZL_Backward", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_ZL_Backward_tib)
        
        individual_rs_tib_list [["ZL_Backward"]] = full_ZL_Backward_tib
        }
    
    if ("SR_Forward" %in% rs_method_names_list)
        {
        full_SR_Forward_tib = 
            load_input_csv_into_tibble ("SR_Forward", base_path, suffix)
        
        exp_tib = bind_rows (exp_tib, 
                             full_SR_Forward_tib)
        
        individual_rs_tib_list [["SR_Forward"]] = full_SR_Forward_tib
        }
    
    if ("UR_Forward" %in% rs_method_names_list)
        {
        full_UR_Forward_tib = 
            load_input_csv_into_tibble ("UR_Forward", base_path, suffix)
        
        exp_tib = bind_rows (exp_tib, 
                             full_UR_Forward_tib)
        
        individual_rs_tib_list [["UR_Forward"]] = full_UR_Forward_tib
        }

     #-----------------------------

        #  Build score diffs table and join it to full table.
    
    joined_scores_tib_list = 
        build_joined_scores_tib_for_all_reserve_selectors (individual_rs_tib_list)
    
    # joined_scores_tib_list = joined_scores_tib_list$sorted
   
    score_diffs_tib = build_score_diffs_tib (joined_scores_tib_list)
    
    exp_tib = left_join (exp_tib, score_diffs_tib, by = "rsp_UUID")
    
     #-----------------------------

    return (list (full_ILP_tib = full_ILP_tib,
                  exp_tib = exp_tib))
    }

#===============================================================================

apply_corrections_to_FULL_data_if_requested <- function (exp_tib, 
                                                         bdpg_p_needs_fixing)
    {
    exp_tib %>% 
        mutate (
                    #  Add column for d = clique size (aka group size).  
                rsp_d__number_of_nodes_per_group = 
                                round (rsp_n__num_groups ^ rsp_alpha__), 
                
                            #  An alternate way to compute d from the results.
                            #  I've decided not to use it, but want to 
                            #  remember it for the moment, in case I have a 
                            #  problem with the alpha-based method above.
                            #  2021 05 25 - BTL.
                        # (rsp_correct_solution_cost + rsp_n__num_groups) / 
                        # rsp_n__num_groups, 
      
                    #  Add column for the p value that was input as a 
                    #  model RB control parameter.  
                    #  If the p bug was active when the run was made, then 
                    #  this value of p is not the right p to report, but 
                    #  we want to know what was input.  
                    #  If the bug was not active during the run, then 
                    #  this nominal p will match the reported value for 
                    #  rsp_p__prop_of_links_between_groups.  
                    #  In the future, that should always be true, but 
                    #  for the initial bdpg papers, the bug was active 
                    #  and they won't match.  

                rsp_nominal_p__prop_of_links_between_groups = 
                                          rsp_p__prop_of_links_between_groups 
                ) -> exp_tib

    if (bdpg_p_needs_fixing)
        {
            #  The bug in using k__arity = 1 instead of 2 meant that the 
            #  effective value of p in the bdpg paper experiments was smaller 
            #  than the nominal value used as an input.  
            #  Now that we've saved the nominal value in its own column, 
            #  replace the original column of p values with the effective 
            #  value so that any predictive or explanatory analyses are 
            #  using the meaningful value of p.
        exp_tib %>% 
            mutate (rsp_p__prop_of_links_between_groups = 
                                  rsp_nominal_p__prop_of_links_between_groups / 
                                  rsp_d__number_of_nodes_per_group
                    ) -> exp_tib
        }

    return (exp_tib)
    }

#===============================================================================

add_corresponding_gurobi_run_inf_to_each_rs_run <- function (full_ILP_tib, 
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
            select (full_ILP_tib, rsp_UUID, 
                                     gurobi_status,
                                     gurobi_mipgap,
                                     gurobi_objbound,
                                     gurobi_itercount,
                                     gurobi_runtime)
    
    exp_tib <- left_join (exp_tib, 
                          gurobi_prob_UUIDs_and_gap_info, 
                          by = "rsp_UUID")

    return (exp_tib)
    }

#===============================================================================

add_new_derived_cols_to_FULL_data <- function (exp_tib, rs_method_names_list)
    {
  exp_tib %>% 
      mutate (log10_sppPUsum = log10 (sppPUsum), 
              log10_sppPUprod = log10 (sppPUprod), 

                  #  rsp_opt_solution_as_frac_of_tot_num_nodes exists in the 
                  #  data, but it's the fraction just for the Base problem
                  #  It can't be used for Wrap problems, so we need a 
                  #  generalized version of it here that works for both Base 
                  #  and Wrap.
                  #  The label here also includes "actual" because there is a 
                  #  desired value that is given in the input parameters for 
                  #  Wrap problems, but that value may not match the actual 
                  #  value because of a bug in earlier code (now fixed, 
                  #  2021-05-23).

                      #  2022 12 26 - BTL
                      #  I've just changed the initial loading of data to 
                      #  reflect the fact that igraph returns a value for the 
                      #  number of ocuppied PUs while rsp_num_PUs is the 
                      #  number of both occupied and unoccupied PUs.  
                      #  rsp_num_PUs no longer exists because of these changes.
                      #  It's been renamed to reflect that it's both occupied 
                      #  and unoccupied.  Since we don't really care about 
                      #  unoccupied PUs, I'm going to change this calculation 
                      #  to use the occupied value.
              actual_sol_frac_of_landscape = rsp_correct_solution_cost / 
                                             rsp_num_occupied_PUs,    #  value renamed from bipartite number.of.PUs
                                             #rsp_combined_num_occ_and_unocc_PUs,  #  original value renamed
                                             #rsp_num_PUs,    #  original value

                    #  Create a version of the reserve selector names as an 
                    #  ordered set of factors instead of strings.  
                    #  
                    #  This allows control of the order in which panels appear 
                    #  in the facetted ggplot outputs.
                    #  I might be able to convert the method name column 
                    #  directly without creating a separate version of it, but 
                    #  I'm not sure if that would end up causing problems 
                    #  somewhere else because something is expecting a string 
                    #  instead of a factor.  Seems safer to just make a 
                    #  separate factor column.
                
              rs_method_name_fac = factor (rs_method_name, 
#                                                 levels = rs_method_names_list, 
                                 levels = c("ILP", "SA", 
                                            "UR_Forward", 
                                            "SA_SS"), 
                                                 ordered = TRUE)
              
              ) -> exp_tib

    return (exp_tib)
    }

#===============================================================================
#===============================================================================
#===============================================================================

select_paper_1_cols <- function (full_tib)
    {
    working_tib = select (full_tib, 
                      
                            #id,     #  Batch ID
                            batch_id,     #  Batch ID
                            rs_method_name, 
                            rs_method_name_fac,
                            rsp_cor_or_app_str, 
                            rsp_base_wrap_str, 
                            rsp_wrap_is_imperfect, 
                          
rsr_UUID,
                          rsp_UUID, 
##2022 12 26 xxx##                          rsp_UUID_of_base_problem_that_is_wrapped, 
                          rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                  
                            rsp_combined_num_occ_and_unocc_PUs, 
rsp_num_occupied_PUs, 
                            rsp_num_spp, 
                            rsp_num_spp_per_PU, 
                            sppPUsum,
                            sppPUprod, 

log10_sppPUsum, 
log10_sppPUprod, 

                            rsp_correct_solution_cost, 

                            rs_solution_cost_err_frac, 
                
                            # RS_system_time, 
                            gurobi_status, 
                            # gurobi_mipgap, 
                            # gurobi_objbound, 
                            # gurobi_itercount, 
                            gurobi_runtime
                          
, rsp_n__num_groups
, rsp_alpha__
, rsp_d__number_of_nodes_per_group
, rsp_p__prop_of_links_between_groups
, rsp_nominal_p__prop_of_links_between_groups
, rsp_r__density
, actual_sol_frac_of_landscape
                          
                        )
    
    return (working_tib)
    }

#===============================================================================

#  Derived from:  create_app_wrap_tibs_from_full_exp_tib in 
#                 R/v2_Paper_2_func_defns_to_load_data.R
#  Note that the old function has an "s" on the end of the "tib" in the middle.
#  I've removed that "s" here, because it only returns one tib.  
#  I think that the old "s" is left from something that also returned tibs with 
#  the FP and FN versions.

    #  This function takes the fully loaded and filtered set of experiment 
    #  outputs for all chosen reserve selectors and whittles it down to just 
    #  the APP Wrap problems.  
    #  
    #  It then adds some columns that can't apply in COR data, i.e., that 
    #  are specific only to APP data (e.g., error magnification, etc.).  
    #  
    #  It also removes some rows if directed to by options in the params list 
    #  (e.g., remove_probs_with_gt_10_pct_input_err).
    #  
    #  It returns the augmented app wrap.

create_app_wrap_tib_from_full_exp_tib <- function (filtered_exp_tib, params)
    {
        #  Create dataset containing apparent wrapped data only.

    full_app_wrap_tib = 
        filter (filtered_exp_tib, 
                rsp_cor_or_app_str == "APP" & rsp_base_wrap_str  == "Wrap")

    #-----------------------------

        #  All actions before this point include data for COR problems. 
        #  Actions related to errors and magnifications, etc, can only be  
        #  done on APP problems (e.g., to avoid 0 denominator in error 
        #  magnification calculations).  So, a sequence of actions 
        #  similar to the full dataset actions need to be done on the 
        #  APP restricted data.
    
    augmented_full_app_wrap_tib = 
        add_new_derived_cols_to_APP_WRAP_data (full_app_wrap_tib)
    
     #-----------------------------
  
    return (augmented_full_app_wrap_tib)
    }

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

add_new_derived_cols_to_APP_WRAP_data <- function (app_wrap_tib)
    {
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
                 
    err_labels = app_wrap_tib$rsp_combined_err_label
    
    FN_dom_indices = which (err_labels == "03-FN_only_NO_cost_err" | 
                            err_labels == "04-FP_and_FN_matched_NO_cost_err")
                
    FP_dom_indices = which (err_labels == "02-FP_only_NO_cost_err" | 
                            err_labels == "05-FP_and_FN_not_matched_NO_cost_err")
                
    dominant_err_type = rep ("UNKNOWN_ERR_TYPE", length (err_labels))
    dominant_err_type [FN_dom_indices] = "FN"
    dominant_err_type [FP_dom_indices] = "FP"
    
    app_wrap_tib %>% 
        mutate (dom_err_type = dominant_err_type) -> app_wrap_tib
  
        #-------------------
  
        #  Add a column (at least temporarily - 2020 06 18) to help 
        #  calculate the maximum cost overrun and underrun values and their 
        #  magnifications in the full dataset.
        #  2020 08 18 - This is currently only used in a p2 appendix that 
        #  will probably disappear in the final version, but until then, 
        #  need to keep this variable.  The appendix is currently called 
        #  "Incommensurate errors".
    
     app_wrap_tib %>% 
        mutate (rsp_max_overest_frac = 
                  (1 / (rsp_correct_solution_cost / rsp_num_occupied_PUs)) - 1) -> app_wrap_tib
                
   #-----------------------------
    
        #  Add columns for the cost error magnification, 
        #  the log of the cost error fraction and 
        #  the log cost error magnification.
    
    app_wrap_tib %>% 
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

                ) -> app_wrap_tib
    
    #-----------------------------

        #  Add magnifications to app data
        
    app_wrap_tib %>% 
      
        mutate (max_TOT_FN_FP_rate = pmax (rsp_realized_FP_rate, 
                                       rsp_realized_FN_rate, 
                                       rsp_euc_realized_Ftot_and_cost_in_err_frac)) %>% 
      
        mutate (
                    #  Assign each problem to its 1% bin for the binned 
                    #  boxplots.
                    #  2022 01 30 - BTL
                    #  Have moved the creation of these factors for the 1% bins
                    #  to create them *after* the 0 and >10% error rows have 
                    #  been removed, so that they don't generate factor levels  
                    #  other than 1 through 10.
                # # input_error_group_by_max_TOT_FN_FP = as.factor (ceiling (100*max_TOT_FN_FP_rate)), 
                # # input_error_group_by_tot_in_err = as.factor (ceiling (100*rsp_euc_realized_Ftot_and_cost_in_err_frac)), 
                # input_error_group_by_max_TOT_FN_FP = as.ordered (ceiling (100*max_TOT_FN_FP_rate)), 
                # input_error_group_by_tot_in_err = as.ordered (ceiling (100*rsp_euc_realized_Ftot_and_cost_in_err_frac)), 

                #FN_tot_mag        = rsr_COR_euc_out_err_frac / rsp_realized_FN_rate, 
                #FP_tot_mag        = rsr_COR_euc_out_err_frac / rsp_realized_FP_rate, 
                max_TOT_FN_FP_tot_mag = rsr_COR_euc_out_err_frac / max_TOT_FN_FP_rate, 

                #FN_signed_cost_mag        = rs_solution_cost_err_frac / rsp_realized_FN_rate, 
                #FP_signed_cost_mag        = rs_solution_cost_err_frac / rsp_realized_FP_rate, 
                max_TOT_FN_FP_signed_cost_mag = rs_solution_cost_err_frac / max_TOT_FN_FP_rate, 
                                
                #FN_abs_cost_mag        = abs_rs_solution_cost_err_frac / rsp_realized_FN_rate, 
                #FP_abs_cost_mag        = abs_rs_solution_cost_err_frac / rsp_realized_FP_rate, 
                max_TOT_FN_FP_abs_cost_mag = abs_rs_solution_cost_err_frac / max_TOT_FN_FP_rate, 
                
                #FN_shortfall_mag        = rsr_COR_spp_rep_shortfall / rsp_realized_FN_rate, 
                #FP_shortfall_mag        = rsr_COR_spp_rep_shortfall / rsp_realized_FP_rate, 
                max_TOT_FN_FP_shortfall_mag = rsr_COR_spp_rep_shortfall / max_TOT_FN_FP_rate
                
                ) -> app_wrap_tib
    
    #-----------------------------

        #  From: create_new_features_paper_3().  
        #  Compute compound features capturing log10 of the error magnification 
        #  and what fraction of the number of possible edges occur in the 
        #  bipartite graph.
  app_wrap_tib %>% 
      mutate (log10_err_mag = log10 (1 + err_mag)) %>% 
      mutate (edge_frac_of_possible = ig_num_edges_m / sppPUprod) -> app_wrap_tib
    
    #-----------------------------

    return (app_wrap_tib)
    }

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

filter_out_unwanted_rows_of_APP_data <- function (app_wrap_tib, params)
    {
        #  Also, make sure to remove any INPUT errors larger than 10%.  
        #  FN errors larger than 10% were being allowed because they don't 
        #  lead to *total input error* larger than 10%.

    app_wrap_tib = 
        filter (app_wrap_tib, 
                    rsp_realized_FN_rate <= 0.1, 
                    rsp_realized_FP_rate <= 0.1)
      
     #-----------------------------
    
        #  Will generally want to remove problems where the input error 
        #  was greater than 10% so that the explanations can just say 
        #  that all input errors were less than 10%.  
        #  Things that have more than 10% input error are rare and just 
        #  an artifact of the way I originally added errors using a coin flip at  
        #  each possible error location instead of specifying a number of  
        #  locationsto flip and then flipping only those.  With the coin flip , 
        #  method you got something near the desired percentage error but often 
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
        mag_base_col_name_str = params$mag_base_col_name_str
        if (mag_base_col_name_str == "max_TOT_FN_FP_rate")
            {
            app_wrap_tib = filter (app_wrap_tib, max_TOT_FN_FP_rate <= 0.1)  #  bbb
            
            } else 
        if (mag_base_col_name_str == "rsp_euc_realized_Ftot_and_cost_in_err_frac")
            {
            app_wrap_tib = 
                filter (app_wrap_tib, 
                        rsp_euc_realized_Ftot_and_cost_in_err_frac <= 0.1)
            } else 
            {
            stop (paste0 ("\n\nIn v2_p6_unifiedDataLoading.R, ", 
                          "params$mag_base_col_name_str = '", 
                          mag_base_col_name_str, "'.\n", 
                          "        Must be 'max_TOT_FN_FP_rate' or ", 
                          "'rsp_euc_realized_Ftot_and_cost_in_err_frac'.\n\n"))
            }
        }

    #-----------------------------
    
       ##  Exclude APP problems with 0 input err if desired
    
    if (is.null (params$exclude_APP_0_inErr)) 
        params$exclude_APP_0_inErr = FALSE

     if (as.logical (params$exclude_APP_0_inErr))
         app_wrap_tib = filter (app_wrap_tib, 

#  2021 12 04 - ERROR?  You don't want COR included do you?  No harm though, since COR's were already filtered out by filtering to APP Wrap?
#                           rsp_cor_or_app_str == "COR" |     

                           rsp_euc_realized_Ftot_and_cost_in_err_frac != 0
                                   # is.infinite (err_mag)
                          ) 

    #-----------------------------

        #  Assign each problem to its 1% bin for the binned boxplots.
        #  2022 01 30 - BTL
        #  Have moved the creation of these factors out of 
        #  add_new_derived_cols_to_APP_WRAP_data(). 
        #  This needs to be done after the zero and large input errors have 
        #  already been removed or else you get too many factors, i.e., 
        #  factors for 0 and greater than or equal to 11 in addition to the 
        #  desired 1 through 10.
    app_wrap_tib %>% 
      
        mutate (
                    #  Assign each problem to its 1% bin for the binned 
                    #  boxplots.
                # input_error_group_by_max_TOT_FN_FP = factor (ceiling (100*max_TOT_FN_FP_rate), 
                #                                              levels=1:10, ordered=TRUE),
                # input_error_group_by_tot_in_err = factor (ceiling (100*rsp_euc_realized_Ftot_and_cost_in_err_frac), 
                #                                              levels=1:10, ordered=TRUE)
                input_error_group_by_max_TOT_FN_FP = ordered (ceiling (100*max_TOT_FN_FP_rate),
                                                              levels=1:10),
                input_error_group_by_tot_in_err = ordered (ceiling (100*rsp_euc_realized_Ftot_and_cost_in_err_frac),
                                                           levels=1:10),

                ) -> app_wrap_tib
    
    #-----------------------------

    return (app_wrap_tib)
    }

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

# remove_irrelevant_cols <- function (full_tib)
#####  Called from load_p6_data()
select_paper_2_cols <- function (full_tib)
    {
    working_tib = select (full_tib, 
                      
                        #  Problem and reserve selector labels                     
                            #id,     #  Batch ID
                            batch_id,     #  Batch ID
                            
                            rs_method_name, 
                            rs_method_name_fac, 
                            rsp_combined_err_label, 
                            rsp_cor_or_app_str, 
                            rsp_base_wrap_str, 
                            
                            rsr_UUID,
                            rsp_UUID,
                        
                            rsp_UUID_of_COR_Wrap_problem_that_has_err_added,  
##2022 12 26 xxx##                                    rsp_UUID_of_base_problem_that_has_err_added, 
                            rsp_UUID_of_COR_Base_problem_that_is_wrapped,
##2022 12 26 xxx##                            rsp_UUID_of_base_problem_that_is_wrapped,
                            
                        #  Input descriptors
                            rsp_combined_num_occ_and_unocc_PUs, 
rsp_num_occupied_PUs,                         
                            rsp_num_spp, 
                            rsp_num_spp_per_PU, 
                            rsp_correct_solution_cost, 
                            
                            sppPUsum,
                            sppPUprod, 
                            
log10_sppPUprod, 

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
                            
                            RS_system_time, 
                        
gurobi_status, 
gurobi_mipgap, 
gurobi_objbound, 
gurobi_itercount, 
gurobi_runtime
                          
, rsp_n__num_groups
, rsp_alpha__
, rsp_d__number_of_nodes_per_group
, rsp_p__prop_of_links_between_groups
, rsp_nominal_p__prop_of_links_between_groups
, rsp_r__density
, actual_sol_frac_of_landscape


, rsp_max_overest_frac
, log_err_mag
, log_out_err
, signed_cost_err_mag
, abs_cost_err_mag
, log_abs_cost_err_mag
, log_abs_cost_err
, rep_shortfall_mag
, log_rep_shortfall_mag
, log_rep_shortfall
, dom_err_type
, max_TOT_FN_FP_rate
, input_error_group_by_max_TOT_FN_FP
, input_error_group_by_tot_in_err
, max_TOT_FN_FP_tot_mag
, max_TOT_FN_FP_signed_cost_mag
, max_TOT_FN_FP_abs_cost_mag
, max_TOT_FN_FP_shortfall_mag



, diff__rsr_COR_euc_out_err_frac__SA__ILP
, diff__rsr_COR_euc_out_err_frac__SA_SS__ILP
, diff__rsr_COR_euc_out_err_frac__UR_Forward__ILP

, diff__rsr_COR_euc_out_err_frac__SA_SS__SA
, diff__rsr_COR_euc_out_err_frac__UR_Forward__SA
, diff__rsr_COR_euc_out_err_frac__UR_Forward__SA_SS
                          
, diff__rsr_COR_spp_rep_shortfall__SA__ILP
, diff__rsr_COR_spp_rep_shortfall__SA_SS__ILP
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__ILP

, diff__rsr_COR_spp_rep_shortfall__SA_SS__SA
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__SA
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__SA_SS

                  )

    return (working_tib)
    }

#===============================================================================

    #  Created by combining the code in a couple of chunks in the file 
    #  v13_Paper_6_p1p2_combined_body.Rmd (2022 12 22).
compute_mag_frac <- function (p2_app_wrap_tib, params)
    {
    mag_base_col_name_str = params$mag_base_col_name_str
    
    if (mag_base_col_name_str == "max_TOT_FN_FP_rate") 
        {
        mag_col_name_str = "max_TOT_FN_FP_tot_mag" 
        shortfall_mag_str = "max_TOT_FN_FP_shortfall_mag"
        mag_frac = round ((length (which (p2_app_wrap_tib$max_TOT_FN_FP_tot_mag > 1)) 
                              / 
                           length (p2_app_wrap_tib$max_TOT_FN_FP_tot_mag)) * 100, 0)  #90.308%
    
        } else  #  total input error
        {
        if (mag_base_col_name_str == "rsp_euc_realized_Ftot_and_cost_in_err_frac") 
            {
            mag_col_name_str = "max_TOT_FN_FP_tot_mag" 
            shortfall_mag_str = "rep_shortfall_mag"
            mag_frac = round ((length (which (p2_app_wrap_tib$err_mag > 1)) 
                                  / 
                               length (p2_app_wrap_tib$err_mag)) * 100, 0)  #90.381%
    
            } else  #  illegal mag base column name
            {
            stop (paste0 ("\n\nmag_base_col_name_str = '", mag_base_col_name_str, "'.\n", 
                          "    Must be 'max_TOT_FN_FP_tot_mag' or ", 
                          "'rsp_euc_realized_Ftot_and_cost_in_err_frac'.\n\n"))
            }
        }
    
    retvals = list (mag_col_name_str = mag_col_name_str, 
                    shortfall_mag_str = shortfall_mag_str, 
                    mag_frac = mag_frac)
    }

#===============================================================================

#####  2022 12 26 - BTL
#####  NOT USED at the moment, but good documentation of all the columns, 
#####  so leaving it in for now.

#  Created 2022 12 25 - BTL
#  Echoed all columns present in the full initial data set, then edited that 
#  list to remove columns not used in paper 3.  
#  Each line in the echoed data set looks something like:
#    [11] "rsp_num_spp_per_PU"
#  I edited the lines in two steps using regex in search and replace in BBedit.
#  First, dropped the trailing quote on each line using 
#      \"[ ]*$
#  as the search and nothing as the replace.
#  Second, I got rid of the leading line number and quote and replaced them with 
#  a leading comma using 
#      ^[ ]*\[[0-9]+\] \"
#  as the search and 
#      \t\t\t\t, 
#  as the replace.

select_paper_3_cols <- function (full_tib)
    {
    working_tib = select (full_tib, 
                      
                        #  Problem and reserve selector labels                     
                            #id,     #  Batch ID
                            batch_id,     #  Batch ID
                            
                            rs_method_name, 
                            rs_method_name_fac, 
                            rsp_combined_err_label, 
                            rsp_cor_or_app_str, 
                            rsp_base_wrap_str, 
                            
                            rsr_UUID,
                            rsp_UUID,
                        
                            rsp_UUID_of_COR_Wrap_problem_that_has_err_added,  
##2022 12 26 xxx##                                    rsp_UUID_of_base_problem_that_has_err_added, 
                            rsp_UUID_of_COR_Base_problem_that_is_wrapped,
##2022 12 26 xxx##                            rsp_UUID_of_base_problem_that_is_wrapped,
                            
                            
                        #  Input descriptors
                            rsp_combined_num_occ_and_unocc_PUs, 
rsp_num_occupied_PUs,                         
                            rsp_num_spp, 
                            rsp_num_spp_per_PU, 
                            rsp_correct_solution_cost, 
                            
                            sppPUsum,
                            sppPUprod, 
                            
log10_sppPUprod, 

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
signed_cost_err_mag, 
abs_cost_err_mag, 
log_abs_cost_err_mag, 
log_abs_cost_err, 

rep_shortfall_mag, 
log_rep_shortfall_mag, 
log_rep_shortfall, 
                            
                            RS_system_time, 
                        
, gurobi_status
, gurobi_mipgap
, gurobi_objbound
, gurobi_itercount
, gurobi_runtime
                          
, rsp_n__num_groups
, rsp_alpha__
, rsp_d__number_of_nodes_per_group
, rsp_p__prop_of_links_between_groups
, rsp_nominal_p__prop_of_links_between_groups
, rsp_r__density
, actual_sol_frac_of_landscape


, rsp_max_overest_frac
, log_err_mag
, log_out_err
, signed_cost_err_mag
, abs_cost_err_mag
, log_abs_cost_err_mag
, log_abs_cost_err
, rep_shortfall_mag
, log_rep_shortfall_mag
, log_rep_shortfall
, dom_err_type
, max_TOT_FN_FP_rate
, input_error_group_by_max_TOT_FN_FP
, input_error_group_by_tot_in_err
, max_TOT_FN_FP_tot_mag
, max_TOT_FN_FP_signed_cost_mag
, max_TOT_FN_FP_abs_cost_mag
, max_TOT_FN_FP_shortfall_mag



, diff__rsr_COR_euc_out_err_frac__SA__ILP
, diff__rsr_COR_euc_out_err_frac__SA_SS__ILP
, diff__rsr_COR_euc_out_err_frac__UR_Forward__ILP

, diff__rsr_COR_euc_out_err_frac__SA_SS__SA
, diff__rsr_COR_euc_out_err_frac__UR_Forward__SA
, diff__rsr_COR_euc_out_err_frac__UR_Forward__SA_SS
                          
, diff__rsr_COR_spp_rep_shortfall__SA__ILP
, diff__rsr_COR_spp_rep_shortfall__SA_SS__ILP
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__ILP

, diff__rsr_COR_spp_rep_shortfall__SA_SS__SA
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__SA
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__SA_SS

                  )

    return (working_tib)
    }

#===============================================================================

    #  Remove any rows specified in the params list as something to be 
    #  excluded in the current run.  
    #  2022 12 25 - BTL
    #  At the moment, it looks like excluded rows may all be related only 
    #  to APP Wrap problems, so I'll just return the original tib for now.  
    #  If you find no row filtering that applies to both COR and APP, then you 
    #  can delete this routine and the call to it in the p8 Rmd file.
    #  For the moment, it's just a placeholder.

remove_param_excluded_rows_from_full_data <- function (full_initial_exp_tib, 
                                                       params)
    {
    filtered_full_initial_exp_tib = full_initial_exp_tib
  
    return (filtered_full_initial_exp_tib)
    }

#===============================================================================

##  Filter down to just rows of interest

#  For example, remove all rows where gurobi didn't finish or remove all 
#  FN rows, etc.

remove_unused_rows_paper_3 <- function (working_tib, 
                                #rs_name, 
                                params, 
                                batch_ids_to_include
                                #, 
                                #separate_by_redundancy = FALSE
                                )
  {
  #--------------------
  
      #  Select just the rows that are APP WRAP problems and have no input 
      #  cost error.
  
  working_tib = 
    filter (working_tib, 
            
            rsp_cor_or_app_str == "APP" & 
            rsp_base_wrap_str == "Wrap" & 

            (rsp_combined_err_label == "02-FP_only_NO_cost_err" | 
             rsp_combined_err_label == "03-FN_only_NO_cost_err" | 
             rsp_combined_err_label == "04-FP_and_FN_matched_NO_cost_err" | 
             rsp_combined_err_label == "05-FP_and_FN_not_matched_NO_cost_err"))

  cat ("\nafter filtering down to APP WRAP, dim(working_tib) = \n")
  show (dim(working_tib))
  

  #--------------------
  
      #  B3 is the only data that I've looked at so far and 
      #  I want to reserve all the other batches for final analysis, 
      #  so remove everything but B3 in here for now.  
      #  However, some of the old code below was assuming 2 batches 
      #  (easy and hard), so until I get all this working again 
      #  on the outputs from cleanBdpg, I'll reset what's 
      #  considered the "full" set to be B3 and B5.
  
            #  Tried to do this using %in% and nothing matched even though 
            #  it works if you have a string directly instead of the id column.
            #  So, I'm going back to hard coding this for now since I have 
            #  no idea how do this correctly.

#########batch_ids_to_include = c("B3","B5")

##  working_tib = filter (working_tib, (id == "B3" | id =="B5")) 
#  working_tib = filter (working_tib, (id %in% params$batch_ids_to_include)) 
        #  2022 12 25 - BTL 
        #  id has been changed to batch_id in the data loading code.
##2022 12 25##  working_tib = filter (working_tib, (id %in% batch_ids_to_include)) 
  working_tib = filter (working_tib, (batch_id %in% batch_ids_to_include)) 

# #  keepers = which (working_tib$id %in% params$batch_ids_to_include)
#   keepers = which (working_tib$id %in% batch_ids_to_include)

#   working_tib = working_tib [keepers,]
  
  cat ("\nafter filtering B3, B5, dim(working_tib) = \n")
  show (dim(working_tib))
  
  #--------------------
  
      #  Learners seem to have trouble with predicting when there is 
      #  no output error at all, so just out of curiosity, what happens 
      #  if you don't include those (e.g., if you were able to learn 
      #  a classifier that separated into error/no error piles).

  if (params$remove_zero_output_errors)
    working_tib = filter (working_tib, rsr_COR_euc_out_err_frac > 0)

  cat ("\nafter remove_zero_errors, dim(working_tib) = \n")
  show (dim(working_tib))

  #--------------------
  
        #  Quick test of separating by redundancy
        #  Could eventually also do plotting of this stuff using the 
        #  group_by or facetting by this.
          #  One odd thing is that if you separate the data by bottom bg redundancy, 
          #  things show some trends when it's greater than 0 and look like random 
          #  noise when it equals 0.  Something to explore...
          #  This also suggests that there might be some things to look at in JMP, 
          #  where it's easy to select a group and look at its antecedents in 
          #  other variables.  For example, I've noticed that in the err_mag values, 
          #  there are 3 distinct groups of values and I wonder what their antecedents 
          #  are.

  if (params$separate_by_redundancy)
      #                ig_median_bottom_bg_redundancy == 0) %>% 
    working_tib = filter (working_tib, ig_median_bottom_bg_redundancy > 0)

  cat ("\nafter separate_by_redundancy, dim(working_tib) = \n")
  show (dim(working_tib))
  
  #--------------------
  
  if (params$use_gurobi_optimal_runs_only)
    working_tib = filter (working_tib, gurobi_status == "OPTIMAL")

  cat ("\nafter use_gurobi_optimal_runs_only, dim(working_tib) = \n")
  show (dim(working_tib))
  
  #--------------------
  
  if (params$use_perfect_wraps_only)
    working_tib = filter (working_tib, rsp_wrap_is_imperfect == FALSE)

  cat ("\nafter use_perfect_wraps_only, dim(working_tib) = \n")
  show (dim(working_tib))
  
  #--------------------
  
  use_FN_dominant = params$use_FN_dominant
  use_FP_dominant = params$use_FP_dominant
  
  if (use_FN_dominant != use_FP_dominant)
    {
    if (use_FN_dominant)
      working_tib = filter (working_tib, dom_err_type == "FN") else
      working_tib = filter (working_tib, dom_err_type == "FP") 
      
    } else
    {
        #  Nothing to do if both are FALSE.  Both TRUE is ok though.
      
    if (!use_FN_dominant & !use_FP_dominant)
      stop (paste0 ("\n\nCan't have both use_FN_dominant and ", 
                    "use_FP_dominant be FALSE.\n\n"))
    }

  cat ("\nafter use_FN_dominant:use_FP_dominant, dim(working_tib) = \n")
  show (dim(working_tib))
  
  #--------------------
  
#  cat ("\nAfter filtering rows for '", rs_name, "':")
  cat ("\nAfter filtering rows:")
  
  cat ("\n\n    batch IDs remaining are: ", unique (working_tib$batch_id), sep="")
  print (count (working_tib, batch_id))
  
  cat ("\n\nNumber of ig_median_bottom_bg_redundancy <= 0:\n")
  print (count (working_tib, ig_median_bottom_bg_redundancy <= 0))
  
  cat ("\n\nNumber of gurobi_status == 'OPTIMAL':\n")
  print (count (working_tib, gurobi_status == "OPTIMAL"))
  
  cat ("\n\nNumber of rsp_wrap_is_imperfect == FALSE:\n")
  print (count (working_tib, rsp_wrap_is_imperfect == FALSE))
  
  cat ("\n\nNumber of FN_dominant == TRUE:\n")
  print (count (working_tib, dom_err_type == "FN"))
  
  cat ("\n\nNumber of FP_dominant == TRUE:\n")
  print (count (working_tib, dom_err_type == "FP"))

  cat ("\n")

  #--------------------
  
  return (working_tib)
  }

#===============================================================================

##  Function to remove columns that won't be used at all

    #  2019 03 10 - BTL
    #  Variables commented out in the leftmost column below were in the easyHard_df 
    #  version but were removed in the data generated by cleanBdpg.Rmd.

remove_unused_cols_paper_3 <- function (full_tib)
    {
    working_tib = select (full_tib, #c(
                      
            #  Problem and reserve selector labels                     
                    batch_id,     #  Easy or Hard
            
                    rs_method_name, 
rsr_UUID,             
                    rsp_combined_err_label, 
                    rsp_cor_or_app_str, 
                    rsp_base_wrap_str, 
                    
                    # rsr_tzar_run_ID, 
                    # 
                    # rsp_file_name_prefix, 
                    # 
                    rsp_UUID, 
                    
                    rsp_UUID_of_COR_Wrap_problem_that_has_err_added,  
##2022 12 26 xxx##                                    rsp_UUID_of_base_problem_that_has_err_added, 
                    rsp_UUID_of_COR_Base_problem_that_is_wrapped,
#2022 12 26 xxx##                            rsp_UUID_of_base_problem_that_is_wrapped,
    
    
            #  Input descriptors
                    rsp_combined_num_occ_and_unocc_PUs, 
rsp_num_occupied_PUs, 
                    rsp_num_spp, 
                    rsp_num_spp_per_PU, 
                    rsp_correct_solution_cost, 
            
                  #  2023 12 18
                  #  Previously sppPUsum was commented out because it can be 
                  #  recreated in linear model that includes the sum and PU variables.
                  #  Reinstating it now because Dormann et al 2009 paper on bipartite 
                  #  indices uses it as their network dimension variable and in 
                  #  their analysis, many bipartite measures of their plant-pollinator 
                  #  networks are related to network dimension.  
                  #  Not sure if this will be the case in model RB derived networks 
                  #  but want to be able to test that when looking at correlations 
                  #  among variables to be used in learning to predict.  
                  #  Another variable they use in that way is "sampling intensity", 
                  #  which they define as "mean number of observed interactions 
                  #  per species" (defined in caption of their Figure 3 of that 
                  #  paper).  Not sure whether that is already measured somewhere 
                  #  in here, e.g., as a bipartite network variable.  If it's not, 
                  #  then you should compute it and add it to the data structure.  
              sppPUsum,  
                    sppPUprod, 
            
            #  model RB generator variables
rsp_alpha__,
rsp_n__num_groups,
rsp_p__prop_of_links_between_groups,
rsp_r__density,

rsp_d__number_of_nodes_per_group, 
rsp_nominal_p__prop_of_links_between_groups, 
rsp_r__density, 
actual_sol_frac_of_landscape, 


            
            #  Post-gen knowable problem descriptors
            
                #  Species and PU counts
# rsp_app_num_spp,          
# rsp_app_num_PUs, 
                    
                #  igraph package metrics
    #                ig_rsp_UUID, 
            
                    ig_top, 
                    ig_bottom, 
                    ig_num_edges_m, 
                    ig_ktop, 
                    ig_kbottom, 
                    ig_bidens, 
                    ig_lcctop, 
                    ig_lccbottom, 
                    ig_distop, 
                    ig_disbottom, 
                    ig_cctop, 
                    ig_ccbottom, 
                    ig_cclowdottop, 
                    ig_cclowdotbottom, 
                    ig_cctopdottop, 
                    ig_cctopdotbottom, 
                    ig_mean_bottom_bg_redundancy, 
                    ig_median_bottom_bg_redundancy, 
                    ig_mean_top_bg_redundancy, 
                    ig_median_top_bg_redundancy, 
                    
# ig_user_time, 
# ig_system_time, 
# ig_elapsed_time, 
# ig_user_child_time, 
# ig_sys_child_time, 
                                    
                #  bipartite package metrics
    #                bip_rsp_UUID, 

                    connectance, 
        
                    web_asymmetry,
                    links_per_PUsAndSpp,    #  essentially, mean links per node in a bipartite network
                    cluster_coefficient,
# weighted_NODF,
# interaction_strength_asymmetry,
                    specialisation_asymmetry,
                    linkage_density,
                    weighted_connectance,
                    Shannon_diversity,
                    interaction_evenness,
                    Alatalo_interaction_evenness,
    
                    #number.of.PUs,
                    number.of.Spp,
    
                    mean.number.of.shared.partners.PUs,
                    mean.number.of.shared.partners.Spp,
                    cluster.coefficient.PUs,
                    cluster.coefficient.Spp,
                    niche.overlap.PUs,
                    niche.overlap.Spp,
                    togetherness.PUs,
                    togetherness.Spp,
                    C.score.PUs,
                    C.score.Spp,
                    V.ratio.PUs,
                    V.ratio.Spp,
                    functional.complementarity.PUs,
                    functional.complementarity.Spp,
                    partner.diversity.PUs,
                    partner.diversity.Spp,
                    generality.PUs,
                    vulnerability.Spp,
                    
# bip_user_time, 
# bip_system_time, 
# bip_elapsed_time, 
# bip_user_child_time, 
# bip_sys_child_time, 
    

                #  Possibly knowable realized input error values
# rsp_realized_median_abs_cost_err_frac, 
# rsp_realized_mean_abs_cost_err_frac, 
# rsp_realized_sd_abs_cost_err_frac, 
# rsp_FP_const_rate, 
# rsp_FN_const_rate, 
                    
                    rsp_realized_FP_rate, 
                    rsp_realized_FN_rate, 
                    rsp_realized_Ftot_rate, 
                    rsp_euc_realized_FP_and_cost_in_err_frac, 
                    rsp_euc_realized_FN_and_cost_in_err_frac, 
                    rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                    
                    rsp_wrap_is_imperfect,
                            
    
                #  Results and their errors
                    rs_solution_cost, 
                    rs_solution_cost_err_frac, 
                    abs_rs_solution_cost_err_frac, 
                    
            # rs_over_opt_cost_err_frac_of_possible_overcost, 
            # rs_under_opt_cost_err_frac_of_possible_undercost, 
                    
                    rsr_COR_euc_out_err_frac, 
                    
                            # rsr_APP_spp_rep_shortfall, 
                            # rsr_APP_solution_NUM_spp_covered, 
                            # rsr_APP_solution_FRAC_spp_covered, 

                    rsr_COR_spp_rep_shortfall, 
# rsr_COR_solution_NUM_spp_covered, 
                    rsr_COR_solution_FRAC_spp_covered, 

        
                    err_mag, 

signed_cost_err_mag, 
abs_cost_err_mag, 
log_abs_cost_err_mag, 
log_abs_cost_err, 

rep_shortfall_mag, 
log_rep_shortfall_mag, 
log_rep_shortfall, 
                            
          #  Extra features created after loading data

gurobi_status,
    gurobi_mipgap, 
dom_err_type,    

        log10_err_mag, 
        edge_frac_of_possible 

                    
                #  Reserve selector run times
# RS_user_time, 
# RS_system_time    #, 
# RS_elapsed_time, 
# RS_user_child_time, 
# RS_sys_child_time
                      )
#    )
    
    return (working_tib)
    }

#===============================================================================

