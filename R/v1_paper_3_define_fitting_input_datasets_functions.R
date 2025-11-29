#===============================================================================
#
#            v1_paper_3_define_fitting_input_datasets_functions.R
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

add_median_redundancies <- function (working_df, reduced_df, 
                                     add_top = TRUE, add_bottom = TRUE)
    {
    if (add_top & add_bottom)
        {
        median_redundancies_df <- 
            select (working_df, 
                        ig_median_bottom_bg_redundancy ,  
                        ig_median_top_bg_redundancy)
        
        } else if (add_top)
        {
        median_redundancies_df <- 
            select (working_df, 
                        ig_median_top_bg_redundancy)
        
        } else    #  add_bottom
        {
        median_redundancies_df <- 
            select (working_df, 
                        ig_median_bottom_bg_redundancy)
        }
    
    reduced_df <- bind_cols (reduced_df, median_redundancies_df)
    
    return (reduced_df)
    }

#===============================================================================

## Define **PROB SIZE** data sets

### Build the **PROB SIZE** test and train data frames

select_prob_size_df_cols <- function (working_df, 
                                      include_median_redundancies=NULL)
    {
    reduced_df <- select (working_df, 
    
rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                            rsp_num_occupied_PUs,    #rsp_num_PUs, 
                            rsp_num_spp, 
# #                 # sppPUsum, 
# #                            sppPUprod
# # 
# #         #  2022 03 25 - BTL
# #         #  Not sure whether to lump these in with problem size variables or not.
# #         #  It's more like they're density variables that can be computed from simple 
# #         #  counts without any notion of a graph being involved, but they 
# #         #  don't fit with the notion of problems get harder as they include 
# #         #  more spp and PUs, which is what most papers report.
# #         #
# #         #  2022 04 16 - BTL
# #         #  Also, is this variable no longer quite correct?  
# #         #  That is, is it computed in bdpg using the rsp_num_PUs rather than 
# #         #  in bdpgtext using the new rsp_num_occupied_PUs?
# # 
# #                            , rsp_num_spp_per_PU  
# # #, ig_num_edges_m
# , edge_frac_of_possible

        #  2022 93 25 0 - BTL
        #  Adding these two variables to problem size variables 
        #  dramatically improves the scores for problem size based prediction 
        #  for FPs.  Not so much for FNs.
#, ig_ktop  # dup var removal 2019 12 26 -BTL #    #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL
#, ig_kbottom  # dup var removal 2019 12 26 -BTL #    #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL

                           )
    
    return (reduced_df)
    }

#===============================================================================

## Define **GRAPH** data sets

### Build the **GRAPH** test and train data frames

select_graph_df_cols <- function (working_df, include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #

#####  2020 02 02 - Knowing this is cheating  #####  dom_err_type, 


            
            #  igraph package metrics

#  2022 03 25 - BTL - Removing after overhaul of prob size vars & their duplicates
                                      #  ig_top ,  
                                      #  ig_bottom ,  
                                      
#  ig_num_edges_m is the total number of edges in the bg graph, which is 
#  essentially, the total number of occurrences of all species.
#  Is it essentially the total number of lines in one of the marxan input 
#  files (the one that lists each occurrence on a separate line)?
#
#  It's set at lines 498 and 594 of 
#     gscp_11b_network_measures_using_igraph_package.R
#         ...
#         497       # Number of edges
#         498    m <- ecount (bg)
#         ...
#         594    ig_num_edges_m = m,
#         ...

#ig_num_edges_m ,  #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL
#edge_frac_of_possible,  #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL

        #  2022 03 25 - BTL
        #  Reinstating these two variables.  Not sure why I considered them 
        #  duplicates and removed them before.  
        #  Might be that I was confusing them with ig_top and ig_bottom, 
        #  which ARE duplicates of size variables (number of spp and PUs).
#ig_ktop ,  # dup var removal 2019 12 26 -BTL #    #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL
#ig_kbottom ,  # dup var removal 2019 12 26 -BTL #    #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL

# dup var removal 2019 12 26 -BTL #  ig_bidens ,  #  This duplicates bipartite package's connectance measure.

                                        ig_lcctop ,  
                                        ig_lccbottom ,  
                                        ig_distop ,  
                                        ig_disbottom ,  
                                        ig_cctop ,  
                                        ig_ccbottom ,  
                                        ig_cclowdottop ,  
                                        ig_cclowdotbottom ,  
                                        ig_cctopdottop ,  
                                        ig_cctopdotbottom ,  
                                        
                                        ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_bottom_bg_redundancy ,  
                                        ig_mean_top_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_top_bg_redundancy ,  
                    
                                    #  bipartite package metrics
#2022 03 25  connectance ,  #  Same things as ig_bidens and edge_frac_of_possible
                            
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

## Define **PROB SIZE & GRAPH** data sets

### Build the **PROB SIZE & GRAPH** test and train data frames

select_graph_and_prob_size_df_cols <- function (working_df, 
                                                include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                         rsp_num_occupied_PUs,    #rsp_num_PUs, 
                         rsp_num_spp, 
                         rsp_num_spp_per_PU, 
                    # sppPUsum, 
                         sppPUprod
                    ,
                      
                                    #  igraph package metrics
# dup var removal 2019 12 26 -BTL #  ig_top ,  
# dup var removal 2019 12 26 -BTL #  ig_bottom ,  
                                      
ig_num_edges_m ,
edge_frac_of_possible,
# dup var removal 2019 12 26 -BTL #  edge_frac_of_possible,
                                        ig_ktop ,  
                                        ig_kbottom ,  
# dup var removal 2019 12 26 -BTL #  ig_bidens ,  
                                        ig_lcctop ,  
                                        ig_lccbottom ,  
                                        ig_distop ,  
                                        ig_disbottom ,  
                                        ig_cctop ,  
                                        ig_ccbottom ,  
                                        ig_cclowdottop ,  
                                        ig_cclowdotbottom ,  
                                        ig_cctopdottop ,  
                                        ig_cctopdotbottom ,  
                                        
                                        ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_bottom_bg_redundancy ,  
                                        ig_mean_top_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
                    
                                    #  bipartite package metrics
                #  2022 03 25 - same as edge_frac_of_possible                        connectance ,  
                            
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

## Define **INPUT ERR** data sets

### Build the **INPUT ERR** test and train data frames

select_input_err_df_cols <- function (working_df, include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                   rsp_realized_FP_rate, 
                                   rsp_realized_FN_rate, 
                                   rsp_realized_Ftot_rate, 
                                   rsp_euc_realized_FP_and_cost_in_err_frac, 
                                   rsp_euc_realized_FN_and_cost_in_err_frac, 
                                   rsp_euc_realized_Ftot_and_cost_in_err_frac
                         )
    
    return (reduced_df)
    }

#===============================================================================

## Define **INPUT ERR and PROB SIZE** data sets

### Build the **INPUT ERR and PROB SIZE** test and train data frames

select_input_err_and_prob_size_df_cols <- function (working_df, 
                                                    include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
                                   rsp_realized_FP_rate, 
                                   rsp_realized_FN_rate, 
                                   rsp_realized_Ftot_rate, 
                                   rsp_euc_realized_FP_and_cost_in_err_frac, 
                                   rsp_euc_realized_FN_and_cost_in_err_frac, 
                                   rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                      
                         rsp_num_occupied_PUs,    #rsp_num_PUs, 
                         rsp_num_spp, 
                         rsp_num_spp_per_PU, 
                    # sppPUsum, 
                         sppPUprod

, ig_num_edges_m
, edge_frac_of_possible

        #  2022 93 25 0 - BTL
        #  Adding these two variables to problem size variables 
        #  dramatically improves the scores for problem size based prediction 
        #  for FPs.  Not so much for FNs.
, ig_ktop  # dup var removal 2019 12 26 -BTL #    #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL
, ig_kbottom  # dup var removal 2019 12 26 -BTL #    #  Is this really a size variable rather than a graph variable?  2022 03 25 - BTL

                         )

    return (reduced_df)
    }

#===============================================================================

## Define **INPUT ERR & PROB SIZE & GRAPH** data sets

### Build the **INPUT ERR & PROB SIZE & GRAPH** test and train data frames

select_input_err_and_prob_size_and_graph_df_cols <- 
                            function (working_df, include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                   rsp_realized_FP_rate, 
                                   rsp_realized_FN_rate, 
                                   rsp_realized_Ftot_rate, 
                                   rsp_euc_realized_FP_and_cost_in_err_frac, 
                                   rsp_euc_realized_FN_and_cost_in_err_frac, 
                                   rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                      
                         rsp_num_occupied_PUs,    #rsp_num_PUs, 
                         rsp_num_spp, 
                         rsp_num_spp_per_PU, 
                    # sppPUsum, 
                         sppPUprod
                    ,

                                    #  igraph package metrics
# dup var removal 2019 12 26 -BTL #  ig_top ,  
# dup var removal 2019 12 26 -BTL #  ig_bottom ,  
                                      
ig_num_edges_m ,
edge_frac_of_possible,
# dup var removal 2019 12 26 -BTL #  edge_frac_of_possible,
                                        ig_ktop ,  
                                        ig_kbottom ,  
# dup var removal 2019 12 26 -BTL #  ig_bidens ,  
                                        ig_lcctop ,  
                                        ig_lccbottom ,  
                                        ig_distop ,  
                                        ig_disbottom ,  
                                        ig_cctop ,  
                                        ig_ccbottom ,  
                                        ig_cclowdottop ,  
                                        ig_cclowdotbottom ,  
                                        ig_cctopdottop ,  
                                        ig_cctopdotbottom ,  
                                        
                                        ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_bottom_bg_redundancy ,  
                                        ig_mean_top_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
                    
                                    #  bipartite package metrics
                      #  2022 03 25 - same as edge_frac_of_possible                  connectance ,  
                            
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

## Define **INPUT ERR & GRAPH** data sets

### Build the **INPUT ERR & GRAPH** test and train data frames

select_input_err_and_graph_df_cols <- 
                            function (working_df, include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                   rsp_realized_FP_rate, 
                                   rsp_realized_FN_rate, 
                                   rsp_realized_Ftot_rate, 
                                   rsp_euc_realized_FP_and_cost_in_err_frac, 
                                   rsp_euc_realized_FN_and_cost_in_err_frac, 
                                   rsp_euc_realized_Ftot_and_cost_in_err_frac, 
                      
                                    #  igraph package metrics
# dup var removal 2019 12 26 -BTL #  ig_top ,  
# dup var removal 2019 12 26 -BTL #  ig_bottom ,  
                                      
#  2022 03 25 - moved to prob size            ig_num_edges_m ,
# dup var removal 2019 12 26 -BTL #  edge_frac_of_possible,
                    #  2022 03 25 - moved to prob size            ig_ktop ,  
                    #  2022 03 25 - moved to prob size            ig_kbottom ,  
# dup var removal 2019 12 26 -BTL #  ig_bidens ,  
                                        ig_lcctop ,  
                                        ig_lccbottom ,  
                                        ig_distop ,  
                                        ig_disbottom ,  
                                        ig_cctop ,  
                                        ig_ccbottom ,  
                                        ig_cclowdottop ,  
                                        ig_cclowdotbottom ,  
                                        ig_cctopdottop ,  
                                        ig_cctopdotbottom ,  
                                        
                                        ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_bottom_bg_redundancy ,  
                                        ig_mean_top_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
                    
                                    #  bipartite package metrics
                    #  2022 03 25                     connectance ,  
                            
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

select_reduced_glmnet_graph_df_cols <- function (working_df, 
                                                  include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                    #  igraph package metrics
#                                         ig_top ,  
                    #  2022 03 25                     ig_bottom ,  

# ig_num_edges_m ,  
                    #  2022 03 25                     edge_frac_of_possible, 
#                                         ig_ktop ,  
                    #  2022 03 25                     ig_kbottom ,  
#                                         ig_bidens ,  
#                                         ig_lcctop ,  
                                        ig_lccbottom ,  
#                                         ig_distop ,  
                                        ig_disbottom ,  
#                                         ig_cctop ,  
                                        ig_ccbottom ,  
                                        ig_cclowdottop ,  
#                                         ig_cclowdotbottom ,  
                                        ig_cctopdottop ,  
#                                         ig_cctopdotbottom ,  
#                                         
#                                         ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs                                        ig_median_bottom_bg_redundancy ,  
#                                         ig_mean_top_bg_redundancy ,  
# # 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
#                     
#                                     #  bipartite package metrics
#                                         connectance ,  
#                             
                                        web_asymmetry , 
#                                         links_per_PUsAndSpp , 
                                        cluster_coefficient , 
# # weighted_NODF , 
# # interaction_strength_asymmetry , 
                                        specialisation_asymmetry , 
#                                         linkage_density , 
#                                         weighted_connectance , 
#                                         Shannon_diversity , 
#                                         interaction_evenness , 
#                                         Alatalo_interaction_evenness , 
#                         
#                                         mean.number.of.shared.partners.PUs , 
                                        mean.number.of.shared.partners.Spp , 
#                                         cluster.coefficient.PUs , 
#                                         cluster.coefficient.Spp , 
#                                         niche.overlap.PUs , 
#                                         niche.overlap.Spp , 
#                                         togetherness.PUs , 
                                        togetherness.Spp , 
                                        C.score.PUs , 
                                        C.score.Spp , 
                                        V.ratio.PUs , 
                                        V.ratio.Spp , 
                                        functional.complementarity.PUs , 
#                                         functional.complementarity.Spp , 
                                        partner.diversity.PUs , 
#                                         partner.diversity.Spp , 
                                        generality.PUs 
#, 
#                                         vulnerability.Spp
                         )
    
    if (include_median_redundancies)
        reduced_df <- add_median_redundancies (working_df, reduced_df, 
                                               add_top = FALSE, 
                                               add_bottom = TRUE)

    return (reduced_df)
    }

#===============================================================================

## Define **2var GRAPH** data sets

### Build the **2var GRAPH** test and train data frames

select_2var_graph_df_cols <- function (working_df, 
                                       include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                    #  igraph package metrics
# #                                         ig_top ,  
#                                         ig_bottom ,  
# #                                       
# # #  ig_num_edges_m is the total number of edges in the bg graph, which is 
# # #  essentially, the total number of occurrences of all species.
# # #  Is it essentially the total number of lines in one of the marxan input 
# # #  files (the one that lists each occurrence on a separate line)?
# # #
# # #  It's set at lines 498 and 594 of 
# # #     gscp_11b_network_measures_using_igraph_package.R
# # #         ...
# # #         497       # Number of edges
# # #         498    m <- ecount (bg)
# # #         ...
# # #         594    ig_num_edges_m = m,
# # #         ...
# # 
# # ig_num_edges_m ,  
# edge_frac_of_possible, 
# #                                         ig_ktop ,  
#                                         ig_kbottom ,  
# #                                         ig_bidens ,  
# #                                         ig_lcctop ,  
#                                         ig_lccbottom ,  
# #                                         ig_distop ,  
#                                         ig_disbottom ,  
# #                                         ig_cctop ,  
#                                         ig_ccbottom ,  
                                        ig_cclowdottop ,  
# #                                         ig_cclowdotbottom ,  
                                        ig_cctopdottop 
#,  
# #                                         ig_cctopdotbottom ,  
# #                                         
# #                                         ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs#                                         ig_median_bottom_bg_redundancy ,  
# #                                         ig_mean_top_bg_redundancy ,  
# # # 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
# #                     
# #                                     #  bipartite package metrics
# #                                         connectance ,  
# #                             
#                                         web_asymmetry , 
# #                                         links_per_PUsAndSpp , 
#                                         cluster_coefficient , 
# # # weighted_NODF , 
# # # interaction_strength_asymmetry , 
#                                         specialisation_asymmetry , 
# #                                         linkage_density , 
# #                                         weighted_connectance , 
# #                                         Shannon_diversity , 
# #                                         interaction_evenness , 
# #                                         Alatalo_interaction_evenness , 
# #                         
# #                                         mean.number.of.shared.partners.PUs , 
#                                         mean.number.of.shared.partners.Spp , 
# #                                         cluster.coefficient.PUs , 
# #                                         cluster.coefficient.Spp , 
# #                                         niche.overlap.PUs , 
# #                                         niche.overlap.Spp , 
# #                                         togetherness.PUs , 
#                                         togetherness.Spp , 
#                                         C.score.PUs , 
#                                         C.score.Spp , 
#                                         V.ratio.PUs , 
#                                         V.ratio.Spp , 
#                                         functional.complementarity.PUs , 
# #                                         functional.complementarity.Spp , 
#                                         partner.diversity.PUs , 
# #                                         partner.diversity.Spp , 
#                                         generality.PUs 
# #, 
# #                                         vulnerability.Spp
                         )

    return (reduced_df)
    }

#===============================================================================

## Define **4var GRAPH** data sets

### Build the **4var GRAPH** test and train data frames

select_4var_graph_df_cols <- function (working_df, 
                                       include_median_redundancies)
    {
    reduced_df <- select (working_df, 
                          
rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                    #  igraph package metrics
# #                                         ig_top ,  
#                                         ig_bottom ,  
# #                                       
# # ig_num_edges_m ,  
# edge_frac_of_possible, 
# #                                         ig_ktop ,  
#                                         ig_kbottom ,  
# #                                         ig_bidens ,  
# #                                         ig_lcctop ,  
#                                         ig_lccbottom ,  
# #                                         ig_distop ,  
#                                         ig_disbottom ,  
# #                                         ig_cctop ,  
#                                         ig_ccbottom ,  
                                        ig_cclowdottop ,  
# #                                         ig_cclowdotbottom ,  
                                        ig_cctopdottop ,  
# #                                         ig_cctopdotbottom ,  
# #                                         
# #                                         ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs#                                         ig_median_bottom_bg_redundancy ,  
# #                                         ig_mean_top_bg_redundancy ,  
# # # 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
# #                     
# #                                     #  bipartite package metrics
# #                                         connectance ,  
# #                             
#                                         web_asymmetry , 
# #                                         links_per_PUsAndSpp , 
#                                         cluster_coefficient , 
# # # weighted_NODF , 
# # # interaction_strength_asymmetry , 
#                                         specialisation_asymmetry , 
# #                                         linkage_density , 
# #                                         weighted_connectance , 
# #                                         Shannon_diversity , 
# #                                         interaction_evenness , 
# #                                         Alatalo_interaction_evenness , 
# #                         
# #                                        mean.number.of.shared.partners.PUs , 
                                         mean.number.of.shared.partners.Spp , 
# #                                         cluster.coefficient.PUs , 
# #                                         cluster.coefficient.Spp , 
# #                                         niche.overlap.PUs , 
# #                                         niche.overlap.Spp , 
# #                                         togetherness.PUs , 
#                                         togetherness.Spp , 
#                                         C.score.PUs , 
#                                         C.score.Spp , 
                                         V.ratio.PUs , 
#                                         V.ratio.Spp , 
#                                         functional.complementarity.PUs , 
# #                                         functional.complementarity.Spp , 
#                                         partner.diversity.PUs , 
# #                                         partner.diversity.Spp , 
#                                         generality.PUs 
# #, 
# #                                         vulnerability.Spp
                         )

    return (reduced_df)
    }                      

#===============================================================================

select_9var_graph_df_cols <- function (working_df, 
                                       include_median_redundancies)
    {
    reduced_df <- select (working_df, 

rs_method_name,    #  *****  #
#####  2020 02 02  #####  dom_err_type, 
            
                                    #  igraph package metrics
# #                                         ig_top ,  
                    #  2022 03 25                     ig_bottom ,  
# #                                       
# # ig_num_edges_m ,  
# edge_frac_of_possible, 
# #                                         ig_ktop ,  
#                                         ig_kbottom ,  
# #                                         ig_bidens ,  
# #                                         ig_lcctop ,  
#                                         ig_lccbottom ,  
# #                                         ig_distop ,  
#                                         ig_disbottom ,  
# #                                         ig_cctop ,  
#                                         ig_ccbottom ,  
                                        ig_cclowdottop ,  
# #                                         ig_cclowdotbottom ,  
                                        ig_cctopdottop ,  
# #                                         ig_cctopdotbottom ,  
# #                                         
# #                                         ig_mean_bottom_bg_redundancy ,  
# 0 variance for FNs                                         ig_median_bottom_bg_redundancy ,  
# #                                         ig_mean_top_bg_redundancy ,  
# # # 0 variance for FNs                                        ig_median_top_bg_redundancy ,   
# #                     
# #                                     #  bipartite package metrics
# #                                         connectance ,  
# #                             
#                                         web_asymmetry , 
# #                                         links_per_PUsAndSpp , 
#                                         cluster_coefficient , 
# # # weighted_NODF , 
# # # interaction_strength_asymmetry , 
#                                         specialisation_asymmetry , 
# #                                         linkage_density , 
# #                                         weighted_connectance , 
# #                                         Shannon_diversity , 
# #                                         interaction_evenness , 
# #                                         Alatalo_interaction_evenness , 
# #                         
# #                                        mean.number.of.shared.partners.PUs , 
                                         mean.number.of.shared.partners.Spp , 
# #                                         cluster.coefficient.PUs , 
# #                                         cluster.coefficient.Spp , 
# #                                         niche.overlap.PUs , 
# #                                         niche.overlap.Spp , 
# #                                         togetherness.PUs , 
#                                         togetherness.Spp , 
                                         C.score.PUs , 
#                                         C.score.Spp , 
                                         V.ratio.PUs , 
#                                         V.ratio.Spp , 
                                         functional.complementarity.PUs , 
# #                                         functional.complementarity.Spp , 
#                                         partner.diversity.PUs , 
# #                                         partner.diversity.Spp , 
                                         generality.PUs 
# #, 
# #                                         vulnerability.Spp
                         )

    if (include_median_redundancies)
        reduced_df <- add_median_redundancies (working_df, reduced_df, 
                                               add_top = FALSE, 
                                               add_bottom = TRUE)

    return (reduced_df)
    }

#===============================================================================

