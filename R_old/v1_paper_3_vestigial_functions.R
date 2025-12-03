#===============================================================================
#
#                      v1_paper_3_vestigial_functions.R
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
#                         NO LONGER USED IN PAPER 3?
#                         
#  Remove if commenting it out like this causes no problems.
##===============================================================================

##  Load data for one reserve selector

read_data_for_one_reserve_selector <- function (rs_name, 
                                                base_path, 
                                                suffix, 
                                                gurobi_prob_status_tib, 
                                                batch_ids_to_include, 
                                                params)
    {
    if (rs_name == "Gurobi")
        cur_tib = 
                build_working_tib ("Gurobi", base_path, suffix, 
                                   gurobi_prob_status_tib, batch_ids_to_include, 
                                   params)
    
    if (rs_name == "Marxan_SA")
        working_tib = 
                build_working_tib ("Marxan_SA", base_path, suffix, 
                                   gurobi_prob_status_tib, batch_ids_to_include, 
                                   params)
    
    if (rs_name == "Marxan_SA_SS")
        working_tib = 
                build_working_tib ("Marxan_SA_SS", base_path, suffix, 
                                   gurobi_prob_status_tib, batch_ids_to_include, 
                                   params)
    
    if (rs_name == "ZL_Backward")
        working_tib = 
                build_working_tib ("ZL_Backward", base_path, suffix, 
                                   gurobi_prob_status_tib, batch_ids_to_include, 
                                   params)
    
    if (rs_name == "UR_Forward")
        working_tib = 
                build_working_tib ("UR_Forward", base_path, suffix, 
                                   gurobi_prob_status_tib, batch_ids_to_include, 
                                   params)
    
    if (rs_name == "SR_Forward")
        working_tib = 
                build_working_tib ("SR_Forward", base_path, suffix, 
                                   gurobi_prob_status_tib, batch_ids_to_include, 
                                   params)
    
    return (working_tib)
    }

#===============================================================================

select_no_partial_redundancy_df_cols <- function (working_df, 
                                                 include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                    #  igraph package metrics
# dup var removal 2019 12 26 -BTL #  ig_top ,  
# dup var removal 2019 12 26 -BTL #  ig_bottom ,  
                                      
ig_num_edges_m ,  
# dup var removal 2019 12 26 -BTL #  edge_frac_of_possible, 
                                        # ig_ktop ,  
                                        # ig_kbottom ,  
                                        # ig_bidens ,  
                                        # ig_lcctop ,  
                                        # ig_lccbottom ,  
                                        # ig_distop ,  
                                        # ig_disbottom ,  
                                        # ig_cctop ,  
                                        # ig_ccbottom ,  
                                        # ig_cclowdottop ,  
                                        # ig_cclowdotbottom ,  
                                        # ig_cctopdottop ,  
                                        # ig_cctopdotbottom ,  
                                        
                                        ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_bottom_bg_redundancy ,  
                                        ig_mean_top_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
                    
                                    #  bipartite package metrics
                                        connectance ,  
                            
                                        web_asymmetry , 
                                        links_per_PUsAndSpp , 
                                        cluster_coefficient , 
# weighted_NODF , 
# interaction_strength_asymmetry , 
                                        specialisation_asymmetry , 
                                        linkage_density , 
                                        weighted_connectance , 
                                        Shannon_diversity , 
                                        interaction_evenness , 
                                        Alatalo_interaction_evenness , 
                        
                                        mean.number.of.shared.partners.PUs , 
                                        mean.number.of.shared.partners.Spp , 
                                        cluster.coefficient.PUs , 
                                        cluster.coefficient.Spp , 
                                        niche.overlap.PUs , 
                                        niche.overlap.Spp , 
                                        togetherness.PUs , 
                                        togetherness.Spp , 
                                        C.score.PUs , 
                                        C.score.Spp , 
                                        V.ratio.PUs , 
                                        V.ratio.Spp , 
                                        functional.complementarity.PUs , 
                                        functional.complementarity.Spp , 
                                        partner.diversity.PUs , 
                                        partner.diversity.Spp , 
                                        generality.PUs , 
                                        vulnerability.Spp
                         )
    
    if (include_median_redundancies)
        reduced_df <- add_median_redundancies (working_df, reduced_df)

    return (reduced_df)
    }

#===============================================================================

