#===============================================================================
#
#                         v1_p5_unifiedDataLoading.R
#
#===============================================================================

#  History

#  2021 12 05 - BTL - v1
#     - Cloned from v1_p4_unifiedDataLoading.R and by extracting various 
#       data loading functions from papers 1, 2, and 3, particularly:
#         - bdpgtext/R/v2_Paper_2_func_defns_to_load_data.R and 
#         - v1_paper_3_initial_data_loading_functions.R

#===============================================================================

#  Load necessary libraries

    #  Suppress startup message for tidyverse package because when it's loaded, 
    #  it puts out a message that includes some unicode that the normal latex  
    #  engine (used to produce the pdf) can't handle and it crashes with the  
    #  following message:
    #       ! Package inputenc Error: Unicode character [sqrt symbol goes here] (U+221A)
    #       (inputenc)                not set up for use with LaTeX.
    #  Note that I've also had to remove the sqrt symbol from the error message 
    #  when embedding it in the comment here, because even inside the comment, 
    #  latex tried to render that and crashed.
    #  More information about this can be found at:
    #   - https://community.rstudio.com/t/tidyverse-1-2-1-knitting-to-pdf-issue/2880/4
    #   - https://community.rstudio.com/t/cant-render-tidyverse-1-2-startup-message-in-latex/2811/5
    #   - https://chrisbeeley.net/?p=1037

suppressPackageStartupMessages (library (tidyverse))

library (here)  
library (glue)

#===============================================================================
#===============================================================================
#===============================================================================

load_full_data_from_individual_rs_files <- function (params, 
                                                     proj_dir, 
                                                     rs_method_names_list, 
                                                     relative_path_to_input_data, 
                                                     bdpg_p_needs_fixing)
    {
        #-----------------------------------------------------------------
        #  Read aggregated initial data input files into one big tibble.
        #-----------------------------------------------------------------

    full_initial_exp_tib = 
        load_and_build_FULL_data_for_all_reserve_selectors (params, 
                                                            proj_dir, 
                                                            rs_method_names_list, 
                                                            relative_path_to_input_data, 
                                                            bdpg_p_needs_fixing)

        #-----------------------------------------------------------------
        #  Add new computed/derived columns that apply to all items, i.e, 
        #  to COR, APP, Base, and Wrap.
        #-----------------------------------------------------------------
  
  
  
  
  
        #-----------------------------------------------------------------
        #  Split into COR and APP tibbles.
        #-----------------------------------------------------------------
  
  
  
  
        #-----------------------------------------------------------------
        #  Add new derived columns for COR.
        #-----------------------------------------------------------------
  
  
  
  
        #-----------------------------------------------------------------
        #  Add new derived columns for APP.
        #-----------------------------------------------------------------
  
  
  
  
  
        #-----------------------------------------------------------------
        #  Filter out unwanted COR rows based on params list.
        #-----------------------------------------------------------------
  
  
  
  
  
        #-----------------------------------------------------------------
        #  Create FN/FP dominant tibbles ???  
        #  ARE THESE USED ANYMORE?  I DON'T THINK SO...
        #-----------------------------------------------------------------
  
  
  
  
        #-----------------------------------------------------------------
        #  Select out unwanted COR columns by paper.
        #-----------------------------------------------------------------
  
  
  
  
  
        #-----------------------------------------------------------------
        #  Select out unwanted APP columns by paper.
        #-----------------------------------------------------------------
  
  
  
  
  
  
      return (full_initial_exp_tib)
      }

#===============================================================================
#===============================================================================
#===============================================================================

#  Load old functions used by p4 version but not rewritten for it.
#  These have been discovered by running the p4 version and seeing what 
#  R complains about not finding in this source file.

#===============================================================================

#   From:  Data_building/v1_paper_3_initial_data_loading_functions.R

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

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

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

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

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

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

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

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

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

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

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
  
#  NO LONGER USED???    
        #  Create datasets containing FN and FP-dominant apparent 
        #  wrapped data only.
    filtered_FN_dom_app_wrap_tib = filter (filtered_app_wrap_tib, dom_err_type == "FN")
    filtered_FP_dom_app_wrap_tib = filter (filtered_app_wrap_tib, dom_err_type == "FP")

    #-----------------------------
  
    return (list (filtered_app_wrap_tib        = filtered_app_wrap_tib, 

#  NO LONGER USED???    
                  filtered_FN_dom_app_wrap_tib = filtered_FN_dom_app_wrap_tib, 
                  filtered_FP_dom_app_wrap_tib = filtered_FP_dom_app_wrap_tib))
    }

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

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
                    #  Assign each problem to its 1% bin for the binned 
                    #  boxplots.
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

#  2021 12 04 - ERROR?  You don't want COR included do you?  No harm though, since COR's were already filtered out by filtering to APP Wrap?
                           rsp_cor_or_app_str == "COR" |     

                           rsp_euc_realized_Ftot_and_cost_in_err_frac != 0
                                   # is.infinite (err_mag)
                          ) 

    #-----------------------------

    return (app_wrap_tib)
    }

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

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
, max_FN_FP_rate
, input_error_group_by_max_FN_FP
, input_error_group_by_tot_in_err
, max_FN_FP_tot_mag
, max_FN_FP_signed_cost_mag
, max_FN_FP_abs_cost_mag
, max_FN_FP_shortfall_mag



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

#  From:  Data_building/v1_paper_3_initial_data_loading_functions.R

##  Function to remove columns that won't be used at all

    #  2019 03 10 - BTL
    #  Variables commented out in the leftmost column below were in the easyHard_df 
    #  version but were removed in the data generated by cleanBdpg.Rmd.

remove_unused_cols_paper_3 <- function (full_tib)
    {
    working_tib = select (full_tib, #c(
                      
            #  Problem and reserve selector labels                     
                    id,     #  Easy or Hard
            
                    rs_method_name, 
rsr_UUID,             
                    rsp_combined_err_label, 
                    rsp_cor_or_app_str, 
                    rsp_base_wrap_str, 
                    
                    # rsr_tzar_run_ID, 
                    # 
                    # rsp_file_name_prefix, 
                    # 
                    # rsp_UUID, 
rsp_UUID_of_base_problem_that_has_err_added, 
                    # rsp_UUID_of_base_problem_that_is_wrapped, 
    
    
            #  Input descriptors
                    rsp_num_PUs, 
                    rsp_num_spp, 
                    rsp_num_spp_per_PU, 
                    rsp_correct_solution_cost, 
            
                    # sppPUsum,
                    sppPUprod, 
            
            #  model RB generator variables
rsp_alpha__,
rsp_n__num_groups,
rsp_p__prop_of_links_between_groups,
rsp_r__density,
            
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
                    links_per_PUsAndSpp,
                    cluster_coefficient,
# weighted_NODF,
# interaction_strength_asymmetry,
                    specialisation_asymmetry,
                    linkage_density,
                    weighted_connectance,
                    Shannon_diversity,
                    interaction_evenness,
                    Alatalo_interaction_evenness,
    
                    number.of.PUs,
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

          #  Extra features created after loading data

gurobi_status,     
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

#  From:  Data_building/v1_paper_3_initial_data_loading_functions.R

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
  working_tib = filter (working_tib, (id %in% batch_ids_to_include)) 

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

  if (params$remove_zero_errors)
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
  cat ("\nAfter filtering rows for p3:")
  
  cat ("\n\n    batch IDs remaining are: ", unique (working_tib$id), sep="")
  print (count (working_tib, id))
  
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

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

#===============================================================================

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

#===============================================================================

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

#===============================================================================

#  From:  R/v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R

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

    write_a_tib_to_csv_file (tib_to_write, 
                             tib_name, 
                             params$data_out_loc, 
                             params$gurobi_problem_filter, 
                             params$exclude_imperfect_wraps, 
                             params$add_gen_time_to_csv_name, 
                             write_csv, 
                             write_rds
                             )
    }

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
    
    if (write_csv)
        {
        csv_file_name_including_options = paste0 (file_name_including_options, ".csv")
        csv_full_outfile_path = file.path (data_out_loc, csv_file_name_including_options)
        cat ("Writing tib '", tib_name, "' to '", csv_full_outfile_path, "'\n", sep='')
        write.csv (tib_to_write, csv_full_outfile_path, 
                   row.names=FALSE, quote=TRUE)
        }
    
    if (write_rds)
        {
        rds_file_name_including_options = paste0 (file_name_including_options, ".rds")
        rds_full_outfile_path = file.path (data_out_loc, rds_file_name_including_options)
        cat ("Writing RDS '", tib_name, "' to '", rds_full_outfile_path, "'\n", sep='')
        saveRDS (tib_to_write, rds_full_outfile_path)
        }
    }

#===============================================================================
#===============================================================================
#===============================================================================

#proj_dir = here()

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
  
    # unfiltered_exp_tib = remove_irrelevant_cols (unfiltered_exp_tib)

    unfiltered_exp_tib = 
        add_new_derived_cols_to_FULL_data (unfiltered_exp_tib, 
                                           rs_method_names_list)

    # filtered_exp_tib = 
    #     filter_out_unwanted_rows_APP_and_or_COR_data (unfiltered_exp_tib, 
    #                                                   params)
    
    if (params$write_tibs_to_csv)
        write_a_tib_to_csv_file_using_params (unfiltered_exp_tib, 
                                              "unfiltered_exp_tib__FULL", 
                                              params)
 
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

    return (list (full_Gurobi_tib = full_Gurobi_tib,
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
#===============================================================================
#===============================================================================

                #  Testing the functions in this file.

#===============================================================================
#===============================================================================
#===============================================================================

    #-----------------------------------------------
    #  Paper 1 Rmd file chunk that loads the data.
    #-----------------------------------------------

# ```{r load_data, include=FALSE}
# 
# #  Load data and filter down to what's required
# 
# filtered_exp_tib = set_up_for_paper_1_2 (params, proj_dir, rs_method_names_list, 
#                                          bdpg_p_needs_fixing = TRUE)
# 
# cor_tib = filter (filtered_exp_tib, rsp_cor_or_app_str == "COR")
# 
# cor_tib = select_paper_1_cols (cor_tib)
# 
# if (params$write_tibs_to_csv) 
#     write_a_tib_to_csv_file_using_params (cor_tib, "cor_tib", params)
# 
# ```

#-------------------------------------------------------------------------------

gen_p1_tib <- function (params, proj_dir, rs_method_names_list, 
                        bdpg_p_needs_fixing = TRUE)
    {
    full_initial_exp_tib = set_up_for_paper_1_2 (params, proj_dir, rs_method_names_list, 
                                                bdpg_p_needs_fixing = TRUE)
    
    p1_cor_tib = filter (full_initial_exp_tib, rsp_cor_or_app_str == "COR")                                           
                             
    p1_cor_tib = select_paper_1_cols (p1_cor_tib) 
    
    write_a_tib_to_csv_file_using_params (p1_cor_tib, "p1_cor_tib", params)
    
    return (p1_cor_tib)
    }

#-------------------------------------------------------------------------------

gen_p4_p1_tib <- function (full_initial_exp_tib, 
                           params
                           # , 
                           # proj_dir, rs_method_names_list, 
                           # relative_path_to_input_data
                           # # , 
                           # # bdpg_p_needs_fixing = TRUE
                           )
    {
    # full_initial_exp_tib = 
    #     load_and_build_FULL_data_for_all_reserve_selectors (params, 
    #                                                         proj_dir, 
    #                                                         rs_method_names_list, 
    #                                                         relative_path_to_input_data, 
    #                                                         bdpg_p_needs_fixing)
    # 
    # # p1_filtered_exp_tib = set_up_for_paper_1_2 (params, proj_dir, rs_method_names_list, 
    # #                                             bdpg_p_needs_fixing = TRUE)
    
    p4_p1_cor_tib = filter (full_initial_exp_tib, rsp_cor_or_app_str == "COR")                                           
                             
    p4_p1_cor_tib = select_paper_1_cols (p4_p1_cor_tib) 
    
    # write_a_tib_to_csv_file_using_params (p4_p1_cor_tib, "p4_p1_cor_tib", params)
    # 
    return (p4_p1_cor_tib)
    }

#-------------------------------------------------------------------------------

test_p1_tib_generator <- function ()
    {
    require (bdpg)
  
    params = list (do_all_batches                        = TRUE, 
                   exclude_imperfect_wraps               = FALSE, 
                   remove_probs_with_gt_10_pct_input_err = TRUE, 
                   exclude_APP_0_inErr                   = TRUE, 
                   add_gen_time_to_csv_name              = TRUE, 
                   gurobi_problem_filter                 = "all", 
                   data_out_loc                          = "~/Downloads", 
                   write_tibs_to_csv                     = FALSE, 
                   func_path_in_proj_dir                 = 
                     "R/v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R")
  
    
proj_dir = here()
cat ("\n\nproj_dir = here() = ", proj_dir, "\n", sep='')

  #  Previously tried to share some code between papers 2 and 3 since they 
  #  have many actions and locations in common, but it didn't work out.  
  #  The function definitions file name built below is related to the failed 
  #  files in the following path:
  #         "bdpgtext/
  #          Analysis_scripts/
  #          AbandonedAttemptAtSharingRcodeBetweenPapers2and3/
  #          R/
  #          bdpg_analysis_scripts_function_defns.shared.R"

func_defns_file_path = 
    file.path (proj_dir,params$func_path_in_proj_dir
               # "Paper_2_comparison_of_reserve_selectors",
               # "v2_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R"
               )
    
#-----

#  2020 08 20 - BTL
#  "Rmarkdown Cookbook" section	"16.1 Source external R scripts" says to 
#  include the "local" argument.
#       https://bookdown.org/yihui/rmarkdown-cookbook/source-script.html
#       "We recommend that you use the argument local in source() or envir in 
#        sys.source() explicitly to make sure the code is evaluated in the 
#        correct environment, i.e., knitr::knit_global(). The default values 
#        for them may not be the appropriate environment: you may end up 
#        creating variables in the wrong environment, and being surprised 
#        that certain objects are not found in later code chunks."
#  I haven't noticed a problem with this, but maybe it's been there and 
#  I just haven't had it affect something important enough to notice.

# source (func_defns_file_path, local = knitr::knit_global())
        
    rs_method_names_list = c("Gurobi", "Marxan_SA", "UR_Forward"
                             #, "ZL_Backward"
                             , "Marxan_SA_SS" 
                             #, "SR_Forward"
                             )

    relative_path_to_input_data = "Data/Clean/All_batches/cln_exp."
        
     #-----------------------------

    # original_p1_tib = gen_p1_tib (params, proj_dir, rs_method_names_list, 
    #                               bdpg_p_needs_fixing = TRUE)
    
     #-----------------------------

    full_initial_exp_tib = 
        load_and_build_FULL_data_for_all_reserve_selectors (params, 
                                                            proj_dir, 
                                                            rs_method_names_list, 
                                                            relative_path_to_input_data, 
                                                            bdpg_p_needs_fixing)

    p4_p1_tib = gen_p4_p1_tib (full_initial_exp_tib, 
                               params
                               # , proj_dir, rs_method_names_list, 
                               # relative_path_to_input_data, 
                               # bdpg_p_needs_fixing = params$bdpg_p_needs_fixing
                               )
    
     #-----------------------------

    # dae = dplyr::all_equal (original_p1_tib, p4_p1_tib)    
    
     #-----------------------------

    return (p4_p1_tib)
    }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

        #  Function calls to compare data frames.
        #  
        #  Learned about all these different functions for comparing 
        #  data frames via a blog post by someone named Sharla Gelfand:
        #     https://sharla.party/post/comparing-two-dfs/ 
        #  The first set of functions compare the structure of the 
        #  data frames rather than the cell values.  
        #  The second set compares the values and the structures.  
        #  Unfortunately, many of them don't play nicely inside a function 
        #  call (e.g., they generate a graphical display rather than an 
        #  easily interpreted return), so I've got them laid out here 
        #  in a way that I can just select them with the cursor and run 
        #  them interactively.  
        #  - The most immediately useful one is the dplyr::all_equal() call,  
        #    which returns TRUE if there are no structural differences between 
        #    the two data frames.
        #  - The diffdf::diffdf() call returns some simple, clear messages when 
        #    everything matches.
        #  - The arsenal::comparedf() function also returns a simple message 
        #    when there are no differences in either structure or values.
        #  So, I typically quickly run just those 3 functions.  
        #  All of the functions have something interesting to report and 
        #  are worth checking out, but they all also blow up into a huge 
        #  mess of output if there are any differences in structure or 
        #  values.  
        #  -The visdat::vis_compare() function is sometimes useful for quickly 
        #    spotting that something is odd and maybe the pattern of it because 
        #    it draws a solid block of color for everything that matches and 
        #    then has different colors/lines where things don't match.
        #  - skimr::skim() is useful for getting a quick overview of the 
        #    structure of a data frame plus some tiny histograms of the values 
        #    of each column.  It's less useful for comparing than just for 
        #    getting a quick feeling for the data.

if (FALSE)    #  This "if (FALSE)" is just to keep the code handy for   
              #  interactively running parts of it, but not run any of 
              #  it when this R file is loaded. 
    {
        #  Load tibs to compare.
        #  
        #  NOTE:  Have to be careful about params settings in each of the 
        #  Rmd files for papers 1, 2, and 3 when doing these compares.  
        #  p1 & p2 have bdpg_needs_fixing set to TRUE in the original
        #  version on the master branch, but p3 there has it set to FALSE.  
        #  So, need to set it accordingly in the generating the unified
        #  data loading versions.
        # 
        #  I've recorded each pair of files I used to test whether the 
        #  new unified data loading code generated the same results as 
        #  the old non-unified code.
        #  I've commented out each pair when I finished that test.
        #  All pairs matched.

    library (here)
    test_unifyDataLoading_dir = file.path (here(), "tests/unifyDataLoading")
    
        #  Paper 1 (passed 2021 12 08 around 5 pm)
#    orig_filename = file.path (test_unifyDataLoading_dir, "original_saved_data_loading_tibs/p1_full_initial_exp_tib.rds")
#    orig_filename = file.path (test_unifyDataLoading_dir, "original_saved_data_loading_tibs/p1_cor_tib.rds")    #  DON'T arrange() this one because it has no rsr_UUID column
        #  Paper 2 (passed 2021 12 08 around 5 pm)
#    orig_filename = file.path (test_unifyDataLoading_dir, "original_saved_data_loading_tibs/p2_full_initial_exp_tib.rds")
#    orig_filename = file.path (test_unifyDataLoading_dir, "original_saved_data_loading_tibs/p2_final_app_wrap_tib.rds")
        #  Paper 3 (passed 2021 12 08 around 5 pm)
    orig_filename = file.path (test_unifyDataLoading_dir, "original_saved_data_loading_tibs/p3_all_rs_working_tib.rds")
    orig_tib = readRDS (orig_filename)
    ref_tib = orig_tib
    
        #  Paper 1 (passed 2021 12 08 around 5 pm)
#    p5_filename = file.path (test_unifyDataLoading_dir, "p5_saved_data_loading_tibs/p1_full_initial_exp_tib.gurobi__all.exclude_imperfect_wraps__FALSE.written_at__2021-12-08_16-49-10.rds")
#    p5_filename = file.path (test_unifyDataLoading_dir, "p5_saved_data_loading_tibs/p1_cor_tib.gurobi__all.exclude_imperfect_wraps__FALSE.written_at__2021-12-08_16-49-16.rds")    #  DON'T arrange() this one because it has no rsr_UUID column
        #  Paper 2 (passed 2021 12 08 around 5 pm)
#    p5_filename = file.path (test_unifyDataLoading_dir, "p5_saved_data_loading_tibs/p2_full_initial_exp_tib.gurobi__all.exclude_imperfect_wraps__FALSE.written_at__2021-12-08_17-01-05.rds")
#    p5_filename = file.path (test_unifyDataLoading_dir, "p5_saved_data_loading_tibs/p2_final_app_wrap_tib.gurobi__all.exclude_imperfect_wraps__FALSE.written_at__2021-12-08_17-01-17.rds")
        #  Paper 3 (passed 2021 12 08 around 5 pm)
    p5_filename = file.path (test_unifyDataLoading_dir, "p5_saved_data_loading_tibs/p3_all_rs_working_tib.gurobi__.exclude_imperfect_wraps__.written_at__2021-12-08_17-12-22.rds")
    p5_tib = readRDS (p5_filename)
  
    #     #  Did the original p3 code forget to filter out errors > 10%?
    # ref_tib = 
    #     filter (ref_tib, 
    #                 rsp_realized_FN_rate <= 0.1, 
    #                 rsp_realized_FP_rate <= 0.1)
    
    library (dplyr)
    ref_tib = arrange (ref_tib, rsr_UUID)
    p5_tib = arrange (p5_tib, rsr_UUID)
    
#-------------------------------------------------------------------------------

        #  Compare tib structures rather than tib cell values.

    dplyr_result = dplyr::all_equal (p5_tib, ref_tib)
    cat ("\n\ndplyr_result = \n")
    print (dplyr_result)
    
    library (janitor)
    janitor_result = janitor::compare_df_cols (p5_tib, ref_tib)
    cat ("\n\njanitor_result = \n")
    print (janitor_result)
    
    library (vetr)
    vetr_result = vetr::alike (p5_tib, ref_tib)
    cat ("\n\nvetr_result = \n")
    print (vetr_result)
    
    library (diffdf)
    diffdf_result = diffdf::diffdf (p5_tib, ref_tib)
    cat ("\n\ndiffdf_result = \n")
    print (diffdf_result)
    
    #----------------------------------------------

        #  Compare tib structures rather than tib cell values.

    library (visdat)
    
    visdat::vis_compare (p5_tib, ref_tib)
    
    visdat::vis_dat (ref_tib)
    visdat::vis_miss (ref_tib)
    
    visdat::vis_dat (p5_tib)
    visdat::vis_miss (p5_tib)

    #----------------------------------------------

    library (skimr)
    
    skimr::skim (ref_tib)
    skimr::skim (p5_tib)

    #----------------------------------------------

    #  dataCompareR vignette
    #  https://cran.r-project.org/web/packages/dataCompareR/vignettes/dataCompareR.html
    #  Particularly see the final section of that vignette, "dataCompareR Workflow".
    
    library (dataCompareR)
    
    rCompare_result = dataCompareR::rCompare (p5_tib, ref_tib)
    summary (rCompare_result)
    
        #  This call fails with the following error message (possibly because 
        #  the tibbles are identical - it might work if they have some difference - 
        #  need to test that):
        #  > rCompare_on_UUID_result <- rCompare (p5_tib, ref_tib, keys = 'rsp_UUID')
            #  Running rCompare...
            #  Coercing input data to data.frame
            #  Error in matchSingleIndex(df_a, df_b, indices) : 
            #  The indices are not unique in the submitted dataframes. Please resubmit with unique indices.
    # rCompare_on_UUID_result <- dataCompareR::rCompare (p5_tib, ref_tib, keys = 'rsp_UUID')
    # print (rCompare_on_UUID_result)
    #     # use generateMismatchData to pull out the mismatching rows from each table
    # mismatches <- dataCompareR::generateMismatchData (rCompare_on_UUID_result, p5_tib, ref_tib)
    # mismatches

    #----------------------------------------------

    library (daff)
    
    daff_result = daff::diff_data (p5_tib, ref_tib)
    summary (daff_result)
    render_diff (daff_result)

    #----------------------------------------------

    library (arsenal)
    
    arsenal_result = arsenal::comparedf (p5_tib, ref_tib)
    summary (arsenal_result)

    #----------------------------------------------

        #  This function is not that useful except that it shows how to call 
        #  some of the other functions above and what to expect from them in 
        #  terms of output.  It was the function she built after learning 
        #  about all the other functions.

    sharla_diff <- function(df, expected_df) {
      data_as_expected <- dplyr::all_equal(expected_df, df)
    
      if (!isTRUE(data_as_expected)) {
        data_diffs <- janitor::compare_df_cols(expected_df, df)
    
        cols_mismatch <- dplyr::filter(data_diffs, is.na(expected_df) | is.na(df))
    
        extra_cols <- cols_mismatch %>%
          dplyr::filter(is.na(expected_df)) %>%
          dplyr::pull(column_name)
    
        missing_cols <- cols_mismatch %>%
          dplyr::filter(is.na(df)) %>%
          dplyr::pull(column_name)
    
        type_mismatch <- dplyr::filter(data_diffs, expected_df != df)
    
        if (length(extra_cols) > 0) {
          warning("`", deparse(substitute(df)), "`", " contains extra column(s): ",
            glue::glue_collapse(extra_cols, sep = ", "), ".",
            call. = FALSE
          )
        } 
        
        if (length(missing_cols) > 0) {
          stop("`", deparse(substitute(df)), "`", " is missing column(s): ", glue::glue_collapse(missing_cols, sep = ", "), ".",
            call. = FALSE
          )
        } 
        
        if (nrow(type_mismatch) > 0) {
          type_mismatch_errors <- type_mismatch %>%
            dplyr::mutate(error = glue::glue("{column_name} should be class {expected_df}, is {df}")) %>%
            dplyr::pull(error) %>%
            as.character() %>%
            paste0("\n")
    
          stop("`", deparse(substitute(df)), "`", " column types are not as expected. These columns have issues:\n", type_mismatch_errors,
            call. = FALSE
          )
        }
      }
    
      df
    }

    sharla_diff_result = sharla_diff (p5_tib, ref_tib)
    cat ("\n\nsharla_diff_result = \n")
    print (sharla_diff_result)
    
    }  #  end if - FALSE

#===============================================================================

    #-----------------------------------------------
    #  Paper 2 Rmd file chunk that loads the data.
    #-----------------------------------------------

# ```{r load_data, include=FALSE}
# 
# #  Load data and filter down to what's required
# 
# filtered_exp_tib = set_up_for_paper_1_2 (params, proj_dir, rs_method_names_list, 
#                                          bdpg_p_needs_fixing = TRUE)
# 
# filtered_exp_tib = select_paper_2_cols (filtered_exp_tib)
# 
# tib_list = create_app_wrap_tibs_from_full_exp_tib (filtered_exp_tib, params)
# 
# app_wrap_tib        = tib_list$filtered_app_wrap_tib
# 
# 
# FN_dom_app_wrap_tib = tib_list$filtered_FN_dom_app_wrap_tib
# FP_dom_app_wrap_tib = tib_list$filtered_FP_dom_app_wrap_tib
# 
#     #  Write spec showing data types of app wrap tib columns.
#     #  Note that this call returns NULL when the tibs are generated here 
#     #  rather than read in using a readr function.
# 
# library (readr)
# cat ("\n\n=========================================================")
# cat ("\nAbout to readr::spec (app_wrap_tib)")
# readr::spec (app_wrap_tib)
# cat ("\n\n=========================================================")
# ```
# 
# ```{r writeTibsToDisk, include=FALSE}
# 
# ###  Save created datasets to disk 
# 
# if (params$write_tibs_to_csv) 
#     {
#     write_a_tib_to_csv_file_using_params (app_wrap_tib, 
#                                           "app_wrap_tib", params)
#     write_a_tib_to_csv_file_using_params (FP_dom_app_wrap_tib,
#                                           "FP_dom_app_wrap_tib", params)
#     write_a_tib_to_csv_file_using_params (FN_dom_app_wrap_tib,
#                                           "FN_dom_app_wrap_tib", params)
#     }
# ```

#===============================================================================

    #-----------------------------------------------
    #  Paper 3 Rmd file chunk that loads the data.
    #-----------------------------------------------

# ```{r setTestAndTrainControls, include=FALSE}
# 
# ###  Choose test and training set controls.
# 
# #training_split_denom = 2    #  equal size test and train datasets
# #sample_training_data_with_replacement = FALSE    #  TRUE if bootstrapping
# ```
# ```{r chooseDataToWorkOn, include=FALSE}
# 
# ##  Load full data for each RS from multiple batches
# 
# ###  Choose which data set to work on
# 
# all_rs_working_tib = read_data_for_all_reserve_selectors (rs_names_vec, 
#                                                           base_path, 
#                                                           suffix, 
#                                                           batch_ids_to_include, 
#                                                           params
#                                                           #, 
#                                                           
#     #  2021 04 17 - BTL
#     #  Adding this argument as reminder that the effective p needs to be 
#     #  added.  See set_up_...() for papers 1 and 2 to see how to deal with 
#     #  this.  Should probably be using that set_up_...() call instead of 
#     #  read_data...() here anyway.                                                      
#                                  #bdpg_p_needs_fixing = TRUE
#                                  )
# ```

#===============================================================================

    #  Test p4 loading of full data set.
    #  Cloned from tsa().
tsp4 <- function ()
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

    relative_path_to_input_data = "Data/Clean/All_batches/cln_exp."
        
     #-----------------------------

    load_and_build_FULL_data_for_all_reserve_selectors (params, 
                                                        proj_dir, 
                                                        rs_method_names_list, 
                                                        relative_path_to_input_data, 
                                                        bdpg_p_needs_fixing = TRUE)
    }

#===============================================================================

