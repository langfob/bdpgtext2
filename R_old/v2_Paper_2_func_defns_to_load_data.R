#===============================================================================
#
#                   v2_Paper_2_func_defns_to_load_data.R
#
#===============================================================================

#  History

#  2021 04 14 - BTL - v2
#     - Streamlining set_up_for_paper_1_2() and related functions to make it 
#       clearer which data stages should be saved to files for storage with 
#       papers on Zenodo or somewhere similar.  Also, making the logic a little 
#       easier to follow and avoiding some duplication and some vestigial, 
#       unused code.

#  2020 08 18 - BTL - v1
#     - Extracted from v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R 
#       as part of simplifying the paper 2 functions.

#===============================================================================

#  RDS data loading functions

#filename = file.path ("/var/folders/6f/qnts1_5d5gz4phpzwvwhtjh80000gp/T//RtmpEGWoFW/RSrun_-COR-Base-UR_Forward.50d09314-0e05-4956-8728-d9a1c895b3e0", 
#"saved.RSrun_-COR-Base-UR_Forward.50d09314-0e05-4956-8728-d9a1c895b3e0.rds")

get_rsRun_object_from_RDS_in_dir <- function (dir)
    {
    obj_name = basename (dir)
    filename = file.path (dir, 
                          paste0 ("saved.", obj_name, ".rds"))
    
    return (readRDS (filename))
    }

#===============================================================================

#  Tibble data loading functions

#===============================================================================

##  Function to load csv file into a tibble

load_input_csv_into_tibble <- function (rs_name, base_path, suffix)
    {
    input_file_path = paste0 (base_path, rs_name, suffix)

    tib = read_csv (input_file_path)
    
        #  Print out the names and data types of the columns that were loaded.
    full_spec = spec_csv (input_file_path)
    print (full_spec)
     
    num_cols      = length (colnames (tib))
    cat ("\nnum ", rs_name, " cols = ", num_cols, sep='')
  
    return (tib)
    }

#===============================================================================
#  Functions that remove columns from the data
#===============================================================================

select_paper_1_cols <- function (full_tib)
    {
    working_tib = select (full_tib, 
                      
                            id,     #  Batch ID
                            rs_method_name, 
                            rs_method_name_fac,
                            rsp_cor_or_app_str, 
                            rsp_base_wrap_str, 
                            rsp_wrap_is_imperfect, 
                          
                          rsp_UUID, 
                          rsp_UUID_of_base_problem_that_is_wrapped, 
                  
                            rsp_num_PUs, 
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

# remove_irrelevant_cols <- function (full_tib)
select_paper_2_cols <- function (full_tib)
    {
    working_tib = select (full_tib, 
                      
                        #  Problem and reserve selector labels                     
                            id,     #  Batch ID
                            
                            rs_method_name, 
                            rs_method_name_fac, 
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

, diff__rsr_COR_euc_out_err_frac__Marxan_SA__Gurobi
, diff__rsr_COR_euc_out_err_frac__Marxan_SA_SS__Gurobi
, diff__rsr_COR_euc_out_err_frac__UR_Forward__Gurobi

, diff__rsr_COR_euc_out_err_frac__Marxan_SA_SS__Marxan_SA
, diff__rsr_COR_euc_out_err_frac__UR_Forward__Marxan_SA
, diff__rsr_COR_euc_out_err_frac__UR_Forward__Marxan_SA_SS
                          
, diff__rsr_COR_spp_rep_shortfall__Marxan_SA__Gurobi
, diff__rsr_COR_spp_rep_shortfall__Marxan_SA_SS__Gurobi
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__Gurobi

, diff__rsr_COR_spp_rep_shortfall__Marxan_SA_SS__Marxan_SA
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__Marxan_SA
, diff__rsr_COR_spp_rep_shortfall__UR_Forward__Marxan_SA_SS

                  )

    return (working_tib)
    }

#===============================================================================

add_corresponding_gurobi_run_inf_to_each_rs_run <- function (full_Gurobi_tib, 
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

    return (exp_tib)
    }

#===============================================================================

    ##  Load combined input data from multiple batches

new__load_all_data_for_paper_1_2 <- function (rs_method_names_list, base_path, suffix)
    {
    full_Gurobi_tib    = load_input_csv_into_tibble ("Gurobi", base_path, suffix)

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
    full_Gurobi_tib = select (full_Gurobi_tib, -gurobi_runtime.1)    
    #---------------------------------------------
    
    exp_tib = full_Gurobi_tib

    individual_rs_tib_list = list (Gurobi = full_Gurobi_tib)
    
    if ("Marxan_SA" %in% rs_method_names_list)
        {
        full_Marxan_SA_tib = 
            load_input_csv_into_tibble ("Marxan_SA", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_Marxan_SA_tib)
        
        individual_rs_tib_list [["Marxan_SA"]] = full_Marxan_SA_tib
        }
    
    if ("Marxan_SA_SS" %in% rs_method_names_list)
        {
        full_Marxan_SA_SS_tib = 
            load_input_csv_into_tibble ("Marxan_SA_SS", base_path, suffix)

        exp_tib = bind_rows (exp_tib, 
                             full_Marxan_SA_SS_tib)
        
        individual_rs_tib_list [["Marxan_SA_SS"]] = full_Marxan_SA_SS_tib
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
        
        individual_rs_tib_list [["UR_Forward"]] = full_UR_Forward_tib
        }

    joined_scores_tib_list = 
        build_joined_scores_tib_for_all_reserve_selectors (individual_rs_tib_list)
    
    # joined_scores_tib_list = joined_scores_tib_list$sorted
    
    score_diffs_tib = build_score_diffs_tib (joined_scores_tib_list)
    
    exp_tib = left_join (exp_tib, score_diffs_tib, by = "rsp_UUID")
    
    return (list (full_Gurobi_tib = full_Gurobi_tib,
                  exp_tib = exp_tib))
    }

#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================

    ##  Load combined input data from multiple batches

load_all_data_for_paper_1_2 <- function (rs_method_names_list, base_path, suffix)
    {
    full_Gurobi_tib    = load_input_csv_into_tibble ("Gurobi", base_path, suffix)

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
    full_Gurobi_tib = select (full_Gurobi_tib, -gurobi_runtime.1)    
    #---------------------------------------------
    
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
                                    # id,     #  Batch ID
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

build_score_diffs_tib <- function (sorted_joined_scores_tib)
    {
    score_diffs_tib = mutate (sorted_joined_scores_tib, 
                          
                            # #----------  xxx
                            # 
                            # diff__xxx__Marxan_SA__Gurobi = xxx__Marxan_SA - xxx__Gurobi, 
                            # diff__xxx__Gurobi__Marxan_SA = -diff__xxx__Marxan_SA__Gurobi, 
                            # 
                            # diff__xxx__Marxan_SA_SS__Gurobi = xxx__Marxan_SA_SS - xxx__Gurobi, 
                            # diff__xxx__Gurobi__Marxan_SA_SS = -diff__xxx__Marxan_SA_SS__Gurobi, 
                            # 
                            # diff__xxx__UR_Forward__Gurobi = xxx__UR_Forward - xxx__Gurobi, 
                            # diff__xxx__Gurobi__UR_Forward = -diff__xxx__UR_Forward__Gurobi, 
                            # 
                            # 
                            # diff__xxx__Marxan_SA__Marxan_SA_SS = xxx__Marxan_SA - xxx__Marxan_SA_SS, 
                            # diff__xxx__Marxan_SA_SS__Marxan_SA = -diff__xxx__Marxan_SA__Marxan_SA_SS, 
                            # 
                            # diff__xxx__Marxan_SA__UR_Forward = xxx__Marxan_SA - xxx__UR_Forward, 
                            # diff__xxx__UR_Forward__Marxan_SA = -diff__xxx__Marxan_SA__UR_Forward, 
                            # 
                            # 
                            # diff__xxx__Marxan_SA_SS__UR_Forward = xxx__Marxan_SA_SS - xxx__UR_Forward, 
                            # diff__xxx__UR_Forward__Marxan_SA_SS = -diff__xxx__Marxan_SA_SS__UR_Forward, 
                            
                            #----------  rs_solution_cost_err_frac
                            
                            diff__rs_solution_cost_err_frac__Marxan_SA__Gurobi = rs_solution_cost_err_frac__Marxan_SA - rs_solution_cost_err_frac__Gurobi, 
                            diff__rs_solution_cost_err_frac__Gurobi__Marxan_SA = -diff__rs_solution_cost_err_frac__Marxan_SA__Gurobi, 
                            
                            diff__rs_solution_cost_err_frac__Marxan_SA_SS__Gurobi = rs_solution_cost_err_frac__Marxan_SA_SS - rs_solution_cost_err_frac__Gurobi, 
                            diff__rs_solution_cost_err_frac__Gurobi__Marxan_SA_SS = -diff__rs_solution_cost_err_frac__Marxan_SA_SS__Gurobi, 
                            
                            diff__rs_solution_cost_err_frac__UR_Forward__Gurobi = rs_solution_cost_err_frac__UR_Forward - rs_solution_cost_err_frac__Gurobi, 
                            diff__rs_solution_cost_err_frac__Gurobi__UR_Forward = -diff__rs_solution_cost_err_frac__UR_Forward__Gurobi, 
                            
                            
                            diff__rs_solution_cost_err_frac__Marxan_SA__Marxan_SA_SS = rs_solution_cost_err_frac__Marxan_SA - rs_solution_cost_err_frac__Marxan_SA_SS, 
                            diff__rs_solution_cost_err_frac__Marxan_SA_SS__Marxan_SA = -diff__rs_solution_cost_err_frac__Marxan_SA__Marxan_SA_SS, 
                            
                            diff__rs_solution_cost_err_frac__Marxan_SA__UR_Forward = rs_solution_cost_err_frac__Marxan_SA - rs_solution_cost_err_frac__UR_Forward, 
                            diff__rs_solution_cost_err_frac__UR_Forward__Marxan_SA = -diff__rs_solution_cost_err_frac__Marxan_SA__UR_Forward, 
                            
                            
                            diff__rs_solution_cost_err_frac__Marxan_SA_SS__UR_Forward = rs_solution_cost_err_frac__Marxan_SA_SS - rs_solution_cost_err_frac__UR_Forward, 
                            diff__rs_solution_cost_err_frac__UR_Forward__Marxan_SA_SS = -diff__rs_solution_cost_err_frac__Marxan_SA_SS__UR_Forward, 
                            
                            #----------  abs_rs_solution_cost_err_frac
                            
                            diff__abs_rs_solution_cost_err_frac__Marxan_SA__Gurobi = abs_rs_solution_cost_err_frac__Marxan_SA - abs_rs_solution_cost_err_frac__Gurobi, 
                            diff__abs_rs_solution_cost_err_frac__Gurobi__Marxan_SA = -diff__abs_rs_solution_cost_err_frac__Marxan_SA__Gurobi, 
                            
                            diff__abs_rs_solution_cost_err_frac__Marxan_SA_SS__Gurobi = abs_rs_solution_cost_err_frac__Marxan_SA_SS - abs_rs_solution_cost_err_frac__Gurobi, 
                            diff__abs_rs_solution_cost_err_frac__Gurobi__Marxan_SA_SS = -diff__abs_rs_solution_cost_err_frac__Marxan_SA_SS__Gurobi, 
                            
                            diff__abs_rs_solution_cost_err_frac__UR_Forward__Gurobi = abs_rs_solution_cost_err_frac__UR_Forward - abs_rs_solution_cost_err_frac__Gurobi, 
                            diff__abs_rs_solution_cost_err_frac__Gurobi__UR_Forward = -diff__abs_rs_solution_cost_err_frac__UR_Forward__Gurobi, 
                            
                            
                            diff__abs_rs_solution_cost_err_frac__Marxan_SA__Marxan_SA_SS = abs_rs_solution_cost_err_frac__Marxan_SA - abs_rs_solution_cost_err_frac__Marxan_SA_SS, 
                            diff__abs_rs_solution_cost_err_frac__Marxan_SA_SS__Marxan_SA = -diff__abs_rs_solution_cost_err_frac__Marxan_SA__Marxan_SA_SS, 
                            
                            diff__abs_rs_solution_cost_err_frac__Marxan_SA__UR_Forward = abs_rs_solution_cost_err_frac__Marxan_SA - abs_rs_solution_cost_err_frac__UR_Forward, 
                            diff__abs_rs_solution_cost_err_frac__UR_Forward__Marxan_SA = -diff__abs_rs_solution_cost_err_frac__Marxan_SA__UR_Forward, 
                            
                            
                            diff__abs_rs_solution_cost_err_frac__Marxan_SA_SS__UR_Forward = abs_rs_solution_cost_err_frac__Marxan_SA_SS - abs_rs_solution_cost_err_frac__UR_Forward, 
                            diff__abs_rs_solution_cost_err_frac__UR_Forward__Marxan_SA_SS = -diff__abs_rs_solution_cost_err_frac__Marxan_SA_SS__UR_Forward, 
                            
                            #----------  rsr_COR_euc_out_err_frac
                            
                            diff__rsr_COR_euc_out_err_frac__Marxan_SA__Gurobi = rsr_COR_euc_out_err_frac__Marxan_SA - rsr_COR_euc_out_err_frac__Gurobi, 
                            diff__rsr_COR_euc_out_err_frac__Gurobi__Marxan_SA = -diff__rsr_COR_euc_out_err_frac__Marxan_SA__Gurobi, 
                            
                            diff__rsr_COR_euc_out_err_frac__Marxan_SA_SS__Gurobi = rsr_COR_euc_out_err_frac__Marxan_SA_SS - rsr_COR_euc_out_err_frac__Gurobi, 
                            diff__rsr_COR_euc_out_err_frac__Gurobi__Marxan_SA_SS = -diff__rsr_COR_euc_out_err_frac__Marxan_SA_SS__Gurobi, 
                            
                            diff__rsr_COR_euc_out_err_frac__UR_Forward__Gurobi = rsr_COR_euc_out_err_frac__UR_Forward - rsr_COR_euc_out_err_frac__Gurobi, 
                            diff__rsr_COR_euc_out_err_frac__Gurobi__UR_Forward = -diff__rsr_COR_euc_out_err_frac__UR_Forward__Gurobi, 
                            
                            
                            diff__rsr_COR_euc_out_err_frac__Marxan_SA__Marxan_SA_SS = rsr_COR_euc_out_err_frac__Marxan_SA - rsr_COR_euc_out_err_frac__Marxan_SA_SS, 
                            diff__rsr_COR_euc_out_err_frac__Marxan_SA_SS__Marxan_SA = -diff__rsr_COR_euc_out_err_frac__Marxan_SA__Marxan_SA_SS, 
                            
                            diff__rsr_COR_euc_out_err_frac__Marxan_SA__UR_Forward = rsr_COR_euc_out_err_frac__Marxan_SA - rsr_COR_euc_out_err_frac__UR_Forward, 
                            diff__rsr_COR_euc_out_err_frac__UR_Forward__Marxan_SA = -diff__rsr_COR_euc_out_err_frac__Marxan_SA__UR_Forward, 
                            
                            
                            diff__rsr_COR_euc_out_err_frac__Marxan_SA_SS__UR_Forward = rsr_COR_euc_out_err_frac__Marxan_SA_SS - rsr_COR_euc_out_err_frac__UR_Forward, 
                            diff__rsr_COR_euc_out_err_frac__UR_Forward__Marxan_SA_SS = -diff__rsr_COR_euc_out_err_frac__Marxan_SA_SS__UR_Forward, 
                            
                            # #----------  rsr_APP_spp_rep_shortfall

                            # diff__rsr_APP_spp_rep_shortfall__Marxan_SA__Gurobi = rsr_APP_spp_rep_shortfall__Marxan_SA - rsr_APP_spp_rep_shortfall__Gurobi,
                            # diff__rsr_APP_spp_rep_shortfall__Gurobi__Marxan_SA = -diff__rsr_APP_spp_rep_shortfall__Marxan_SA__Gurobi,
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__Marxan_SA_SS__Gurobi = rsr_APP_spp_rep_shortfall__Marxan_SA_SS - rsr_APP_spp_rep_shortfall__Gurobi,
                            # diff__rsr_APP_spp_rep_shortfall__Gurobi__Marxan_SA_SS = -diff__rsr_APP_spp_rep_shortfall__Marxan_SA_SS__Gurobi,
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__UR_Forward__Gurobi = rsr_APP_spp_rep_shortfall__UR_Forward - rsr_APP_spp_rep_shortfall__Gurobi,
                            # diff__rsr_APP_spp_rep_shortfall__Gurobi__UR_Forward = -diff__rsr_APP_spp_rep_shortfall__UR_Forward__Gurobi,
                            # 
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__Marxan_SA__Marxan_SA_SS = rsr_APP_spp_rep_shortfall__Marxan_SA - rsr_APP_spp_rep_shortfall__Marxan_SA_SS,
                            # diff__rsr_APP_spp_rep_shortfall__Marxan_SA_SS__Marxan_SA = -diff__rsr_APP_spp_rep_shortfall__Marxan_SA__Marxan_SA_SS,
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__Marxan_SA__UR_Forward = rsr_APP_spp_rep_shortfall__Marxan_SA - rsr_APP_spp_rep_shortfall__UR_Forward,
                            # diff__rsr_APP_spp_rep_shortfall__UR_Forward__Marxan_SA = -diff__rsr_APP_spp_rep_shortfall__Marxan_SA__UR_Forward,
                            # 
                            # 
                            # diff__rsr_APP_spp_rep_shortfall__Marxan_SA_SS__UR_Forward = rsr_APP_spp_rep_shortfall__Marxan_SA_SS - rsr_APP_spp_rep_shortfall__UR_Forward,
                            # diff__rsr_APP_spp_rep_shortfall__UR_Forward__Marxan_SA_SS = -diff__rsr_APP_spp_rep_shortfall__Marxan_SA_SS__UR_Forward,

                            # #----------  rsr_APP_solution_FRAC_spp_covered
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA__Gurobi = rsr_APP_solution_FRAC_spp_covered__Marxan_SA - rsr_APP_solution_FRAC_spp_covered__Gurobi, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Gurobi__Marxan_SA = -diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA__Gurobi, 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS__Gurobi = rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS - rsr_APP_solution_FRAC_spp_covered__Gurobi, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Gurobi__Marxan_SA_SS = -diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS__Gurobi, 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__Gurobi = rsr_APP_solution_FRAC_spp_covered__UR_Forward - rsr_APP_solution_FRAC_spp_covered__Gurobi, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Gurobi__UR_Forward = -diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__Gurobi, 
                            # 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA__Marxan_SA_SS = rsr_APP_solution_FRAC_spp_covered__Marxan_SA - rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS__Marxan_SA = -diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA__Marxan_SA_SS, 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA__UR_Forward = rsr_APP_solution_FRAC_spp_covered__Marxan_SA - rsr_APP_solution_FRAC_spp_covered__UR_Forward, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__Marxan_SA = -diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA__UR_Forward, 
                            # 
                            # 
                            # diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS__UR_Forward = rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS - rsr_APP_solution_FRAC_spp_covered__UR_Forward, 
                            # diff__rsr_APP_solution_FRAC_spp_covered__UR_Forward__Marxan_SA_SS = -diff__rsr_APP_solution_FRAC_spp_covered__Marxan_SA_SS__UR_Forward, 
                            
                            #----------  rsr_COR_spp_rep_shortfall
                            
                            diff__rsr_COR_spp_rep_shortfall__Marxan_SA__Gurobi = rsr_COR_spp_rep_shortfall__Marxan_SA - rsr_COR_spp_rep_shortfall__Gurobi, 
                            diff__rsr_COR_spp_rep_shortfall__Gurobi__Marxan_SA = -diff__rsr_COR_spp_rep_shortfall__Marxan_SA__Gurobi, 
                            
                            diff__rsr_COR_spp_rep_shortfall__Marxan_SA_SS__Gurobi = rsr_COR_spp_rep_shortfall__Marxan_SA_SS - rsr_COR_spp_rep_shortfall__Gurobi, 
                            diff__rsr_COR_spp_rep_shortfall__Gurobi__Marxan_SA_SS = -diff__rsr_COR_spp_rep_shortfall__Marxan_SA_SS__Gurobi, 
                            
                            diff__rsr_COR_spp_rep_shortfall__UR_Forward__Gurobi = rsr_COR_spp_rep_shortfall__UR_Forward - rsr_COR_spp_rep_shortfall__Gurobi, 
                            diff__rsr_COR_spp_rep_shortfall__Gurobi__UR_Forward = -diff__rsr_COR_spp_rep_shortfall__UR_Forward__Gurobi, 
                            
                            
                            diff__rsr_COR_spp_rep_shortfall__Marxan_SA__Marxan_SA_SS = rsr_COR_spp_rep_shortfall__Marxan_SA - rsr_COR_spp_rep_shortfall__Marxan_SA_SS, 
                            diff__rsr_COR_spp_rep_shortfall__Marxan_SA_SS__Marxan_SA = -diff__rsr_COR_spp_rep_shortfall__Marxan_SA__Marxan_SA_SS, 
                            
                            diff__rsr_COR_spp_rep_shortfall__Marxan_SA__UR_Forward = rsr_COR_spp_rep_shortfall__Marxan_SA - rsr_COR_spp_rep_shortfall__UR_Forward, 
                            diff__rsr_COR_spp_rep_shortfall__UR_Forward__Marxan_SA = -diff__rsr_COR_spp_rep_shortfall__Marxan_SA__UR_Forward, 
                            
                            
                            diff__rsr_COR_spp_rep_shortfall__Marxan_SA_SS__UR_Forward = rsr_COR_spp_rep_shortfall__Marxan_SA_SS - rsr_COR_spp_rep_shortfall__UR_Forward, 
                            diff__rsr_COR_spp_rep_shortfall__UR_Forward__Marxan_SA_SS = -diff__rsr_COR_spp_rep_shortfall__Marxan_SA_SS__UR_Forward, 
                            
                            #----------  rsr_COR_solution_FRAC_spp_covered
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA__Gurobi = rsr_COR_solution_FRAC_spp_covered__Marxan_SA - rsr_COR_solution_FRAC_spp_covered__Gurobi, 
                            diff__rsr_COR_solution_FRAC_spp_covered__Gurobi__Marxan_SA = -diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA__Gurobi, 
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS__Gurobi = rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS - rsr_COR_solution_FRAC_spp_covered__Gurobi, 
                            diff__rsr_COR_solution_FRAC_spp_covered__Gurobi__Marxan_SA_SS = -diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS__Gurobi, 
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__Gurobi = rsr_COR_solution_FRAC_spp_covered__UR_Forward - rsr_COR_solution_FRAC_spp_covered__Gurobi, 
                            diff__rsr_COR_solution_FRAC_spp_covered__Gurobi__UR_Forward = -diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__Gurobi, 
                            
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA__Marxan_SA_SS = rsr_COR_solution_FRAC_spp_covered__Marxan_SA - rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS, 
                            diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS__Marxan_SA = -diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA__Marxan_SA_SS, 
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA__UR_Forward = rsr_COR_solution_FRAC_spp_covered__Marxan_SA - rsr_COR_solution_FRAC_spp_covered__UR_Forward, 
                            diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__Marxan_SA = -diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA__UR_Forward, 
                            
                            
                            diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS__UR_Forward = rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS - rsr_COR_solution_FRAC_spp_covered__UR_Forward, 
                            diff__rsr_COR_solution_FRAC_spp_covered__UR_Forward__Marxan_SA_SS = -diff__rsr_COR_solution_FRAC_spp_covered__Marxan_SA_SS__UR_Forward, 
                            
                            #----------  err_mag
                            
                            diff__err_mag__Marxan_SA__Gurobi = err_mag__Marxan_SA - err_mag__Gurobi, 
                            diff__err_mag__Gurobi__Marxan_SA = -diff__err_mag__Marxan_SA__Gurobi, 
                            
                            diff__err_mag__Marxan_SA_SS__Gurobi = err_mag__Marxan_SA_SS - err_mag__Gurobi, 
                            diff__err_mag__Gurobi__Marxan_SA_SS = -diff__err_mag__Marxan_SA_SS__Gurobi, 
                            
                            diff__err_mag__UR_Forward__Gurobi = err_mag__UR_Forward - err_mag__Gurobi, 
                            diff__err_mag__Gurobi__UR_Forward = -diff__err_mag__UR_Forward__Gurobi, 
                            
                            
                            diff__err_mag__Marxan_SA__Marxan_SA_SS = err_mag__Marxan_SA - err_mag__Marxan_SA_SS, 
                            diff__err_mag__Marxan_SA_SS__Marxan_SA = -diff__err_mag__Marxan_SA__Marxan_SA_SS, 
                            
                            diff__err_mag__Marxan_SA__UR_Forward = err_mag__Marxan_SA - err_mag__UR_Forward, 
                            diff__err_mag__UR_Forward__Marxan_SA = -diff__err_mag__Marxan_SA__UR_Forward, 
                            
                            
                            diff__err_mag__Marxan_SA_SS__UR_Forward = err_mag__Marxan_SA_SS - err_mag__UR_Forward, 
                            diff__err_mag__UR_Forward__Marxan_SA_SS = -diff__err_mag__Marxan_SA_SS__UR_Forward
                            )
    
    return (score_diffs_tib)
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

#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
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

              actual_sol_frac_of_landscape = rsp_correct_solution_cost / 
                                             rsp_num_PUs,

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
                                                 levels = rs_method_names_list, 
                                                 ordered = TRUE)
              
              ) -> exp_tib

    return (exp_tib)
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

filter_out_unwanted_rows_APP_and_or_COR_data <- function (bdpg_tib, params)
    {
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

    return (bdpg_tib)
    }

#===============================================================================

add_new_derived_cols_to_APP_WRAP_data <- function (app_wrap_tib)
    {
        #  Add a column (at least temporarily - 2020 06 18) to help 
        #  calculate the maximum cost overrun and underrun values and their 
        #  magnifications in the full dataset.
        #  2020 08 18 - This is currently only used in a p2 appendix that 
        #  will probably disappear in the final version, but until then, 
        #  need to keep this variable.  The appendix is currently called 
        #  "Incommensurate errors".
    
     app_wrap_tib %>% 
        mutate (rsp_max_overest_frac = 
                  (1 / (rsp_correct_solution_cost / rsp_num_PUs)) - 1) -> app_wrap_tib
                
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
  
    #-----------------------------

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

filter_out_unwanted_rows_of_APP_data <- function (app_wrap_tib, params)
    {
        #  Also, make sure to remove any input errors larger than 10%.  
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
      app_wrap_tib = filter (app_wrap_tib, 
                        rsp_euc_realized_Ftot_and_cost_in_err_frac <= 0.1)
      }

    #-----------------------------
    
       ##  Exclude APP problems with 0 input err if desired
    
    if (is.null (params$exclude_APP_0_inErr)) 
        params$exclude_APP_0_inErr = FALSE
    
     if (as.logical (params$exclude_APP_0_inErr))
         app_wrap_tib = filter (app_wrap_tib, 
                           rsp_cor_or_app_str == "COR" | 
                           rsp_euc_realized_Ftot_and_cost_in_err_frac != 0
                                   # is.infinite (err_mag)
                          ) 

    #-----------------------------

    return (app_wrap_tib)
    }

#===============================================================================

load_full_unfiltered_data_with_joined_gurobi_perf_inf <- 
                              function (params, proj_dir, rs_method_names_list, 
                                        bdpg_p_needs_fixing)
    {
        #  Load raw data for each separate reserve selector into a tib and 
        #  then combine all of the tibs into one big experiment tib.  
        #  Ignore all the individual reserve selector tibs except for gurobi's.  
        #  
        #  Gurobi didn't finish running on some problems and we need to be 
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

    gur_and_exp_tibs = 
        # load_all_data_for_paper_1_2 (
        new__load_all_data_for_paper_1_2 (
                rs_method_names_list, 
                base_path = file.path (proj_dir, "Data/Clean/All_batches/cln_exp."), 
                suffix = ".csv")
    
    #-----------------------------
    
    unfiltered_full_exp_tib = 
        apply_corrections_to_FULL_data_if_requested (gur_and_exp_tibs$exp_tib, 
                                                     bdpg_p_needs_fixing)
    
    unfiltered_full_gurobi_tib = 
        apply_corrections_to_FULL_data_if_requested (gur_and_exp_tibs$full_Gurobi_tib, 
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
                              "bdpg_indata_step_01__full_Gurobi_tib__untouched", 
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

        #  Analyze the full data set in terms of which problems gurobi 
        #  did and didn't finish.
        #  Create two new reduced versions of the full set holding just 
        #  problems completed by gurobi and just ones not completed.  
        #  Add both sets to the list of all useful tibbles created so far.
    
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

tsa <- function ()
#test_set_up_for_paper_1_2 <- function ()
    {
require (bdpg)
  
    params = list (do_all_batches                        = TRUE, 
                   exclude_imperfect_wraps               = TRUE, 
                   remove_probs_with_gt_10_pct_input_err = TRUE, 
                   exclude_APP_0_inErr                   = TRUE, 
                   add_gen_time_to_csv_name              = TRUE, 
                   gurobi_problem_filter                 = "all", 
                   data_out_loc                          = "~/Downloads", 
                   write_tibs_to_csv                     = FALSE)
    
    proj_dir = here()
    
    rs_method_names_list = c("Gurobi", "Marxan_SA", "UR_Forward"
                             #, "ZL_Backward"
                             , "Marxan_SA_SS" 
                             #, "SR_Forward"
                             )
    
    set_up_for_paper_1_2 (params, proj_dir, rs_method_names_list, 
                          bdpg_p_needs_fixing = TRUE)
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

set_up_for_paper_1_2 <- function (params, proj_dir, rs_method_names_list, 
                                  bdpg_p_needs_fixing)
    {
    unfiltered_exp_tib = 
        load_full_unfiltered_data_with_joined_gurobi_perf_inf (params, 
                                                               proj_dir, 
                                                               rs_method_names_list, 
                                                               bdpg_p_needs_fixing)
  
    # unfiltered_exp_tib = remove_irrelevant_cols (unfiltered_exp_tib)

    unfiltered_exp_tib = 
        add_new_derived_cols_to_FULL_data (unfiltered_exp_tib, 
                                           rs_method_names_list)

    filtered_exp_tib = 
        filter_out_unwanted_rows_APP_and_or_COR_data (unfiltered_exp_tib, 
                                                      params)
    
    if (params$write_tibs_to_csv)
        write_a_tib_to_csv_file_using_params (filtered_exp_tib, 
                                              "filtered_exp_tib__FULL", 
                                              params)
 
    return (filtered_exp_tib)
    }

#===============================================================================

    #  This function takes that fully loaded and filtered set of experiment 
    #  outputs for all chosen reserve selectors and whittles it down to just 
    #  the APP Wrap problems.  
    #  
    #  It then adds some columns specific only to APP data (e.g., 
    #  error magnification, etc.).  
    #  
    #  It also removes some rows if directed to by options in the params list 
    #  (e.g., remove_probs_with_gt_10_pct_input_err).
    #  
    #  It returns the filtered and augmented app wrap data as well as 
    #  that same data split into two sets, one for FP-dominated problems only 
    #  and one for FN-dominated only.

create_app_wrap_tibs_from_full_exp_tib <- function (filtered_exp_tib, params)
    {
        #  Create dataset containing apparent wrapped data only.

    partly_filtered_app_wrap_tib = 
        filter (filtered_exp_tib, 
                rsp_cor_or_app_str == "APP" & rsp_base_wrap_str  == "Wrap")

    #-----------------------------

        #  All actions before this point include data for COR problems. 
        #  Actions related to errors and magnifications, etc, can only be  
        #  done on APP problems (e.g., to avoid 0 denominator in error 
        #  magnification calculations).  So, a sequence of actions 
        #  similar to the full dataset actions need to be done on the 
        #  APP restricted data.
    
    partly_filtered_app_wrap_tib = 
        add_new_derived_cols_to_APP_WRAP_data (partly_filtered_app_wrap_tib)
    
     #-----------------------------
  
        #  There are a few extra things that need to be filtered out of APP 
        #  data only.
    filtered_app_wrap_tib = 
        filter_out_unwanted_rows_of_APP_data (partly_filtered_app_wrap_tib, params)

     #-----------------------------
  
        #  Create datasets containing FN and FP-dominant apparent 
        #  wrapped data only.
    filtered_FN_dom_app_wrap_tib = filter (filtered_app_wrap_tib, dom_err_type == "FN")
    filtered_FP_dom_app_wrap_tib = filter (filtered_app_wrap_tib, dom_err_type == "FP")

    #-----------------------------
  
    return (list (filtered_app_wrap_tib        = filtered_app_wrap_tib, 
                  filtered_FN_dom_app_wrap_tib = filtered_FN_dom_app_wrap_tib, 
                  filtered_FP_dom_app_wrap_tib = filtered_FP_dom_app_wrap_tib))
    }

#===============================================================================

