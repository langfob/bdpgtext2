#===============================================================================
#
#                       base_wrap_diffs.R ???
#                       Cloned from v2_p6_unifiedDataLoading.R ???
#
#  2022 02 16 - BTL 
#  Don't want to delete this yet, because I'm not sure what was going on here.  
#  Looks like it might be where I was starting to work out how to do something 
#  by cloning some other diffs code, but maybe went on and did it a different 
#  way somewhere else.  Not at all sure though, so I'm going to leave it alone 
#  until I've done the resubmission and know for sure that it's not used.  
#
#===============================================================================

#  History

#  2022-01-06 - BTL - v2
#   - Renamed from v06_Paper_6.R to v2_p6_unifiedDataLoading.R since the old 
#     old name was not informative and came from the fact that it was originally
#     just a quick, temporary, exploratory hack.  
#   - Have now merged many functions from v1_p5_unifiedDataLoading.R into here 
#     so that there is just one file of data loading functions for p6.

#  Created 2022-01-06 - BTL
#   - Named v06_Paper_6.R

#===============================================================================

##  Load R libraries  

# library (knitr)    #  For include_graphics()
# library (ggplot2)
# library (here)
# library (readr)

#===============================================================================

load_p6_data <- function (params, 
                          proj_dir, 
                          rs_method_names_list, 
                          file_type_to_write)
    {
    #---------------------------------------------------------------------
    #  Load full data set before filtering down to just what's required.
    #---------------------------------------------------------------------
    
    full_initial_exp_tib = 
        load_and_build_FULL_data_for_all_reserve_selectors (
                                  params, 
                                  proj_dir, 
                                  rs_method_names_list, 
                                  params$relative_path_to_input_data, 
                                  bdpg_p_needs_fixing = params$bdpg_p_needs_fixing)
    
    usethis::ui_done ("load_and_build_FULL_data_for_all_reserve_selectors")
    
    if (params$write_tibs_to_csv  |  params$write_most_important_tibs_to_csv) 
        write_a_tib_to_csv_file_using_params (full_initial_exp_tib, 
                                              "p2_full_initial_exp_tib", 
                                              params, file_type_to_write)
    
    
        #----------------------
        #  Create p1 COR data  
        #----------------------
        
        #  Reduce full data set down to just COR problems and 
        #  only the columns that are relevant to COR data
    cor_tib = filter (full_initial_exp_tib, rsp_cor_or_app_str == "COR")                                           
    cor_tib = select_paper_1_cols (cor_tib) 
        
    if (params$write_tibs_to_csv  |  params$write_most_important_tibs_to_csv) 
        write_a_tib_to_csv_file_using_params (cor_tib, "p1_cor_tib", 
                                              params, file_type_to_write)
    
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
    min_base_PUs = min (cor_base_tib$rsp_num_PUs)
    max_base_PUs = max (cor_base_tib$rsp_num_PUs)
    
    min_wrap_spp = min (cor_wrap_tib$rsp_num_spp)
    max_wrap_spp = max (cor_wrap_tib$rsp_num_spp)
    min_wrap_PUs = min (cor_wrap_tib$rsp_num_PUs)
    max_wrap_PUs = max (cor_wrap_tib$rsp_num_PUs)
    
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
        #  Create tib and summary vars for Marxan_SA_SS only.  
        #------------------------------------------------------

    cor_tib %>% 
        filter (rs_method_name == "Marxan_SA_SS") %>% 
        select (rs_solution_cost_err_frac) -> Marxan_SA_SS_tib
    
    num_problems = dim (Marxan_SA_SS_tib)[1]
    Marxan_SA_SS_non_zero_cost_err_tib = filter (Marxan_SA_SS_tib, 
                                               rs_solution_cost_err_frac > 0)
    
    num_non_zero_Marxan_SA_SS_cost_errs = dim (Marxan_SA_SS_non_zero_cost_err_tib)[1]
    Marxan_SA_SS_non_zero_cost_err_pct = 
        round (100 * (num_non_zero_Marxan_SA_SS_cost_errs / num_problems), 0)
    
    Marxan_SA_SS_median_non_zero_cost_err_pct = 
        round (100 * median (Marxan_SA_SS_non_zero_cost_err_tib$rs_solution_cost_err_frac), 0)
    
    Marxan_SA_SS_max_non_zero_cost_err_pct = 
        round (100 * max (Marxan_SA_SS_non_zero_cost_err_tib$rs_solution_cost_err_frac), 0)
    
    #---------------------------------------------------------------------------
    
    #  Already created this column in gen_p4_p1_tib().
    
    cor_tib = mutate (cor_tib, log10_sppPUprod = log10 (sppPUprod))
    
    #---------------------------------------------------------------------------
    
    ##  Create APP Wrap tibs for p2  
    
    #---------------------------------------------------------------------------
    
        #  Filter down to just APP Wrap problems.
    tib_list = create_app_wrap_tibs_from_full_exp_tib (full_initial_exp_tib, params)
    
    usethis::ui_done ("create_app_wrap_tibs_from_full_exp_tib")
    
    initial_app_wrap_tib        = tib_list$filtered_app_wrap_tib
    
    if (params$write_tibs_to_csv  |  params$write_most_important_tibs_to_csv) 
        write_a_tib_to_csv_file_using_params (initial_app_wrap_tib, 
                                              "p2_initial_app_wrap_tib", 
                                              params, file_type_to_write)
    
    #  NO LONGER USED???
    FN_dom_app_wrap_tib = tib_list$filtered_FN_dom_app_wrap_tib
    FP_dom_app_wrap_tib = tib_list$filtered_FP_dom_app_wrap_tib
    
    #---------------------------------------------------------------------------
    
    ##  Create p2 tib from APP Wrap tib
    
    #---------------------------------------------------------------------------
    
        #  Get rid of columns that aren't used in this paper.
        #  
    p2_app_wrap_tib = select_paper_2_cols (initial_app_wrap_tib)
    
    usethis::ui_done ("select_paper_2_cols")
    
    #---------------------------------------------------------------------------
    
        #-----------------------------------------------------------------
        #  Write spec showing data types of app wrap tib columns.
        #  Is this unnecessary?  Can you just use str() instead?
        #  
        #  *** NOTE:  readr::spec() returns NULL when the tibs are generated  
        #             here rather than read in using a readr function.
        #-----------------------------------------------------------------
    
    cat ("\n\n=========================================================")
    # cat ("\nAbout to readr::spec (p2_app_wrap_tib)")
    # readr::spec (p2_app_wrap_tib)
    cat ("\nAbout to str (p2_app_wrap_tib)")
    str (p2_app_wrap_tib)
    cat ("\n\n=========================================================")
    
    #---------------------------------------------------------------------------
    
    ###  Save created datasets to disk 
    
    if (params$write_tibs_to_csv  |  params$write_most_important_tibs_to_csv) 
        write_a_tib_to_csv_file_using_params (p2_app_wrap_tib, 
                                              "p2_final_app_wrap_tib", 
                                              params, file_type_to_write)
    
    if (params$write_tibs_to_csv) 
        {
        write_a_tib_to_csv_file_using_params (FP_dom_app_wrap_tib, 
                                              "FP_dom_app_wrap_tib", 
                                              params, file_type_to_write)
        write_a_tib_to_csv_file_using_params (FN_dom_app_wrap_tib,
                                              "FN_dom_app_wrap_tib", 
                                              params, file_type_to_write)
        }
    
    #---------------------------------------------------------------------------
    
    ##2021 04 26##  ###input_file_path_except_extension = "/Users/bill/Downloads/app_wrap_tib.gurobi__all.exclude_imperfect_wraps__FALSE.written_at__2020-10-25_17-32-44"
    ##2021 04 26##  input_file_path_except_extension = "/Users/bill/Downloads/bdpgRelated/app_wrap_tib.gurobi__all.exclude_imperfect_wraps__FALSE.written_at__2020-11-02_10-14-07"
    
    ##2021 04 26##  csv_input_file_path =  paste0 (input_file_path_except_extension, ".csv")
    ##2021 04 26##  readFromCSV_tib = read.csv (csv_input_file_path, stringsAsFactors=FALSE)
    
    ##2021 04 26##  rds_input_file_path =  paste0 (input_file_path_except_extension, ".rds")
    ##2021 04 26##  readFromRDS_tib = readRDS (rds_input_file_path)
    
    #---------------------------------------------------------------------------
    
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
    
    #num_app_prob_per_rs = length (which (p2_app_wrap_tib$rs_method_name == "Gurobi"))
    
    # p2_app_wrap_tib %>% 
    #     group_by (rs_method_name) %>% 
    #     summarize (num_prob = n()) %>% 
    #     ungroup ()  ->  num_app_prob_per_rs
    
    num_app_prob_per_rs = count (p2_app_wrap_tib, rs_method_name)
    
    # FN_dom_app_wrap_tib %>% 
    #     group_by (rs_method_name) %>% 
    #     summarize (num_prob = n()) %>% 
    #     ungroup ()  ->  num_FN_dom_app_prob_per_rs
    
    num_FN_dom_app_prob_per_rs = count (FN_dom_app_wrap_tib, rs_method_name)
    
    FN_dom_not_empty =(dim (FN_dom_app_wrap_tib)[1] > 0)
    
    # FP_dom_app_wrap_tib %>% 
    #     group_by (rs_method_name) %>% 
    #     summarize (num_prob = n()) %>% 
    #     ungroup ()  ->  num_FP_dom_app_prob_per_rs
    
    num_FP_dom_app_prob_per_rs = count (FP_dom_app_wrap_tib, rs_method_name)
    
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
                    , p2_app_wrap_tib = p2_app_wrap_tib
                    
                    )
    
    }
    
#===============================================================================
#===============================================================================
#    From here down, functions copied in from v1_p5_unifiedDataLoading.R
#    2022 01 24 - BTL
#===============================================================================
#===============================================================================

#####  Called from load_p6_data()
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

##### Called from load_data_for_all_reserve_selectors()
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

##### Called from load_data_for_all_reserve_selectors()
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

##### Called from build_joined_scores_tib_for_all_reserve_selectors()
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

##### Called from load_data_for_all_reserve_selectors()
build_score_diffs_tib <- function (sorted_joined_scores_tib)
    {
    score_diffs_tib = mutate (sorted_joined_scores_tib, 
                          
                              
#  For each Wrap problem, 
#  want/need to know what is the score for the same reserve selector on the Base 
#  problem that the Wrap was derived from.
#  
#  So, need to select just the score columns for all Base problems, 
#  then need to do a join of those columns with just the Wrap problem score columns  
#  where the join is done on the Base problem ID in each row.  
#  Do I need to rename those columns or maybe make a new column that shares the 
#  same name in both Base and Wrap subsets or is it already there, i.e., does 
#  the Base problem already have a column saying that its derived from itself?
#                              
                            # xxx = score_name
                            # yyy = reserve_selector_name
                            # 
                            # diff__rs_solution_cost_err_frac__Wrap__Base = rs_solution_cost_err_frac__Wrap - rs_solution_cost_err_frac__Base, 
                            # diff__abs_rs_solution_cost_err_frac__Wrap__Base = abs_rs_solution_cost_err_frac__Wrap - abs_rs_solution_cost_err_frac__Base, 
                            # diff__rsr_COR_euc_out_err_frac__Wrap__Base = rsr_COR_euc_out_err_frac__Wrap - rsr_COR_euc_out_err_frac__Base, 
                            # diff__rsr_COR_spp_rep_shortfall__Wrap__Base = rsr_COR_spp_rep_shortfall__Wrap - rsr_COR_spp_rep_shortfall__Base, 

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

##### Called from load_data_for_all_reserve_selectors(), etc.
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

#####  Called from load_and_build_FULL_data_for_all_reserve_selectors()
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

#####  Called from p4_load_full_unfiltered_data_with_joined_gurobi_perf_inf()
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

#####  Called from p4_load_full_unfiltered_data_with_joined_gurobi_perf_inf()
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

#####  Called from p4_load_full_unfiltered_data_with_joined_gurobi_perf_inf()
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

#####  Called from load_and_build_FULL_data_for_all_reserve_selectors()
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

#  From:  R/v2_Paper_2_func_defns_to_load_data.R

#####  Called from load_p6_data()
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

#####  Called from load_p6_data()
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

#####  Called from create_app_wrap_tibs_from_full_exp_tib()
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

#####  Called from create_app_wrap_tibs_from_full_exp_tib()
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
, max_TOT_FN_FP_rate
, input_error_group_by_max_TOT_FN_FP
, input_error_group_by_tot_in_err
, max_TOT_FN_FP_tot_mag
, max_TOT_FN_FP_signed_cost_mag
, max_TOT_FN_FP_abs_cost_mag
, max_TOT_FN_FP_shortfall_mag



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

