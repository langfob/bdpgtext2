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

init_and_add_one_rs <- function (cur_rs_name, multiplier, individual_rs_tib_list)
    {
    uuids <- sample (1:5,replace=FALSE)
    full_tib = tibble (mult = multiplier, rsp_UUID = uuids, 
                       
                       rs_solution_cost_err_frac = uuids * multiplier,
                       abs_rs_solution_cost_err_frac = uuids * multiplier,

                       rsr_COR_euc_out_err_frac = uuids * multiplier,

                       # rsr_APP_spp_rep_shortfall = uuids * multiplier,
                       # rsr_APP_solution_FRAC_spp_covered = uuids * multiplier,
                       # 
                       rsr_COR_spp_rep_shortfall = uuids * multiplier,
                       rsr_COR_solution_FRAC_spp_covered = uuids * multiplier,
                    
                       err_mag = uuids * multiplier
                      )
    individual_rs_tib_list [[cur_rs_name]] = full_tib
   
    return (individual_rs_tib_list)
    }

#===============================================================================

init_data <- function ()
    {
    individual_rs_tib_list = list()
    
    individual_rs_tib_list = 
        init_and_add_one_rs (cur_rs_name = "Gurobi", 
                             multiplier = 2, 
                             individual_rs_tib_list)

    individual_rs_tib_list = 
        init_and_add_one_rs (cur_rs_name = "Marxan_SA", 
                             multiplier = 3, 
                             individual_rs_tib_list)

    individual_rs_tib_list = 
        init_and_add_one_rs (cur_rs_name = "Marxan_SA_SS", 
                             multiplier = 5, 
                             individual_rs_tib_list)

    individual_rs_tib_list = 
        init_and_add_one_rs (cur_rs_name = "UR_Forward", 
                             multiplier = 7, 
                             individual_rs_tib_list)

    return (individual_rs_tib_list)
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
                            # 
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

test_build_score_diffs <- function ()
    {
    set.seed (101)
    individual_rs_tib_list = init_data ()
    
    joined_scores_tib = 
      build_joined_scores_tib_for_all_reserve_selectors (individual_rs_tib_list)
    
    sorted_joined_scores_tib = arrange (joined_scores_tib, rsp_UUID)
    
    #sorted_joined_scores_tib = joined_scores_tib_list$sorted
    
    score_diffs_tib = build_score_diffs_tib (sorted_joined_scores_tib)

    return (score_diffs_tib)    
    }

#===============================================================================

library (tidyverse)
library (glue)
library (patchwork) 

score_diffs_tib = test_build_score_diffs ()

p1 = ggplot (data = score_diffs_tib, 
             aes (x = rsp_UUID, y = diff__rs_solution_cost_err_frac__Marxan_SA__Gurobi)
            ) + 
        geom_point (aes (color = rsp_UUID)) + 
  theme_bw()

p1

p2 <- ggplot (data = score_diffs_tib, 
             aes (x = rsp_UUID, y = diff__rs_solution_cost_err_frac__Marxan_SA_SS__Gurobi)
            ) + 
        geom_point (aes (color = rsp_UUID)) + 
  theme_bw()

p2
patched <- p1 + p2

patched + plot_annotation (tag_levels = 'a') +
          plot_layout (guides="collect") #+
          #plot_layout (heights=c(3,2,2))

#===============================================================================


