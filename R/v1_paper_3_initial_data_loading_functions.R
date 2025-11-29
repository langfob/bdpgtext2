#===============================================================================
#
#                 v1_paper_3_initial_data_loading_functions.R
#
#===============================================================================

#  History

#  2021 02 19 - BTL
#      - Created by extracting existing functions from 
#            bdpgtext/Analysis_scripts/
#            v14_bdpg_analysis_scripts_function_defns.paper_3.R
#        and moving them into this new R file.
#       - Renamed several functions by appending "_paper_3" because they're 
#         similar to functions for papers 1 and 2 but may not do exactly the 
#         same thing there.
#         - create_new_features_paper_3 ()
#         - remove_unused_rows_paper_3 ()
#         - remove_unused_cols_paper_3 ()

#===============================================================================

#  Load necessary libraries

##  Load data for all reserve selectors

#  Do this by loading each reserve selector one at a time from file, then 
#  append that reserve selector's data to the full set.

read_data_for_all_reserve_selectors <- function (rs_names_vec, 
                                                 base_path, 
                                                 suffix, 
                                                 batch_ids_to_include, 
                                                 params)
    {
        ##  Extract the column telling whether Gurobi found optimal or timed out
    
    gurobi_prob_status_tib = extract_gurobi_prob_status_tib (base_path, suffix)

        #  Initialize full data set with the data from the first 
        #  reserve selector in the list of reserve selector names.
    all_rs_working_tib = 
            build_working_tib (rs_names_vec [1], base_path, suffix, 
                               gurobi_prob_status_tib, batch_ids_to_include, 
                               params)

      #  Load and append the data for each of the rest of the reserve selectors 
      #  in turn.
    for (cur_rs_name in rs_names_vec[2:length (rs_names_vec)])
        {
        cur_tib = build_working_tib (cur_rs_name, 
                                     base_path, 
                                     suffix, 
                                     gurobi_prob_status_tib, 
                                     batch_ids_to_include, 
                                     params)
        
        all_rs_working_tib = dplyr::bind_rows (all_rs_working_tib, 
                                               cur_tib)
        }

    return (all_rs_working_tib)
    }

#===============================================================================

##  Function to build working data set for one reserve selector

#  This function reads the data set into a tibble and then creates some 
#  new features from the original ones.  After that, it removes columns 
#  and rows that aren't used in the analysis.

build_working_tib <- function (rs_name, base_path, suffix, 
                               gurobi_prob_status_tib, batch_ids_to_include, 
                               params)
  {
  cat ("\n\nbuild_working_tib() for ", rs_name, "\n\n", sep='')

  rs_tibble = load_input_csv_into_tibble (rs_name, base_path, suffix)
 
  summarize_errors_on_COR_data (rs_tibble)
 
  rs_tibble %>% 
      create_new_features_paper_3 (rs_name, gurobi_prob_status_tib) %>% 
#      remove_unused_rows_paper_3 (rs_name, params, batch_ids_to_include) %>% 
      remove_unused_rows_paper_3 (params, batch_ids_to_include) %>% 
      remove_unused_cols_paper_3 () -> working_tib

  return (working_tib)
  }

#===============================================================================

#  Data loading functions

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

##  Function to create new features

create_new_features_paper_3 <- function (working_tib, rs_name, gurobi_prob_status_tib)
  {
        #  Compute compound features capturing log10 of the error magnification 
        #  and what fraction of the number of possible edges occur in the 
        #  bipartite graph.
  working_tib %>% 
      mutate (log10_err_mag = log10 (1 + err_mag)) %>% 
      mutate (edge_frac_of_possible = ig_num_edges_m / sppPUprod) -> working_tib
  
        #-------------------
  
        #  Add a feature indicating whether gurobi ran to optimal completion 
        #  on the given problem.
        #  Want this because we may want to restrict analysis to just the 
        #  problems gurobi was able to solve in the allotted time, so that 
        #  comparisons between the reserve selectors are more fair (though 
        #  "fair" is a bit subjective here since marxan could do better if 
        #  you gave it more time).
        #  In the process of adding the column, do a sanity check to make 
        #  sure that the number of lines in the output 
  if (rs_name != "Gurobi") 
      {
      row_ct_working_tib_before_join = dim (working_tib) [1]
      
      working_tib = inner_join (working_tib, gurobi_prob_status_tib, 
                                by="rsp_UUID")
      
      row_ct_working_tib_after_join = dim (working_tib) [1]
      row_ct_gurobi_prob_status_tib = dim (gurobi_prob_status_tib) [1]
     
      if ((row_ct_working_tib_before_join != row_ct_gurobi_prob_status_tib) | 
              (row_ct_working_tib_after_join != row_ct_gurobi_prob_status_tib))
          {
          cat ("\n\nWhen adding gurobi completion status to working_tib for '", rs_name, 
             "',\nrow counts don't match after inner join with gurobi status values.",
             "\n    row_ct_working_tib_before_join = ", row_ct_working_tib_before_join, 
             "\n    row_ct_working_tib_after_join = ", row_ct_working_tib_after_join, 
             "\n    row_ct_gurobi_prob_status_tib = ", row_ct_gurobi_prob_status_tib, 
             "\n", sep="")
          }
      # mutate (working_tib,      VERSION BEFORE USING INNER JOIN
      #         gurobi_status = gurobi_prob_status_tib$gurobi_status)
      }
  
  
        #-------------------
  
      #  Finally, create a flag that tells whether the current example 
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
                 
  err_labels = working_tib$rsp_combined_err_label
  
  FN_dom_indices = which (err_labels == "03-FN_only_NO_cost_err" | 
                          err_labels == "04-FP_and_FN_matched_NO_cost_err")
              
  FP_dom_indices = which (err_labels == "02-FP_only_NO_cost_err" | 
                          err_labels == "05-FP_and_FN_not_matched_NO_cost_err")
              
  dominant_err_type = rep ("UNKNOWN_ERR_TYPE", length (err_labels))
  dominant_err_type [FN_dom_indices] = "FN"
  dominant_err_type [FP_dom_indices] = "FP"
  
  working_tib %>% 
      mutate (dom_err_type = dominant_err_type) -> working_tib
  
        #-------------------
  
  return (working_tib)
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
rsp_UUID_of_base_problem_that_is_wrapped, 
    
    
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

##  Function to extract col telling whether Gurobi found optimal or timed out

#  This column will be added to all the other RS tibbles so that we can 
#  select just the problems where Gurobi finished, if we want to.

#  This is somewhat inefficient because it reads the gurobi data in even 
#  though it will be read in again down below.  I'm doing it this way 
#  because the inefficiency isn't important and without it, I have to 
#  add various "if" statements and special handling for gurobi in what 
#  follows.  This way, the loading and building of all the reserve 
#  selector tibbles can be pretty much the same and therefore, easier 
#  to follow.

extract_gurobi_prob_status_tib <- function (base_path, suffix)
    {
        #  Load the gurobi data from disk and make a copy of the status column.
    
    gurobi_prob_status_tib = 
          load_input_csv_into_tibble ("Gurobi", base_path, suffix)
    
        #  Extract the problem UUID, gurobi completion status, and gurobi mipgap  
        #  so that they can be joined with outputs from other reserve selectors  
        #  that worked on the same problem.
    
    gurobi_prob_status_tib = 
          select (gurobi_prob_status_tib, 
                  rsp_UUID, 
                  gurobi_status, 
                  gurobi_mipgap)
    
        #  Show what unique values status takes on.
    
    cat ("\nUnique values in gurobi_status = ")
    print (unique (gurobi_prob_status_tib$gurobi_status))
    
        #  Print the counts of the different status values and 
        #  what fraction of the runs found the optimal solution in the 
        #  time allowed.
    
    num_gur_optimal = 
        length (which (gurobi_prob_status_tib$gurobi_status == "OPTIMAL"))
    cat ("\nnum OPTIMAL gurobi results = ", num_gur_optimal)
    
    num_gur_time_outs = 
        length (which (gurobi_prob_status_tib$gurobi_status == "TIME_LIMIT"))
    cat ("\nnum TIME LIMIT gurobi results = ", num_gur_time_outs)
    
    frac_gur_optimal = num_gur_optimal / (num_gur_optimal + num_gur_time_outs)
    cat ("\nfraction of runs where gurobi found optimal within time limit = ", 
         frac_gur_optimal)
    
    cat ("\n\n")
    
    return (gurobi_prob_status_tib)
    }

#===============================================================================

