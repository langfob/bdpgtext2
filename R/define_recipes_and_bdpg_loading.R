#===============================================================================

                  #  define_recipes_and_bdpg_loading.R

#  Originally called "define_coffee_recipe.R" as it was taken from an example 
#  on the web that was about analyzing some coffee data.  

#===============================================================================

load_bdpg_data_and_choose_subset <- function (rds_input_file_path, 
                                              seed, 
                                              save_result_to_disk=FALSE)
    {
    new_p3_tib = 
        load_bdpg_data_and_build_random_independent_subset (rds_input_file_path, 
                                                            save_result_to_disk) 
    
    
new_p3_tib <- mutate (new_p3_tib, log10_sppPUprod = log10 (sppPUprod))    
    

        ##  For the moment, only look at Gurobi results
    new_p3_tib = filter (new_p3_tib, rs_method_name == "Gurobi")

                        #  2022 04 16 - BTL - Note that this will break the current _targets.R use of the return value from this function.  
                        #                     I'm commenting it out here to get the building of different recipes for different data subsets working.
                    if(FALSE) {        ##  Create subsets of data to be used in different predictions  
                        prob_size_tib <- 
                            new_p3_tib %>% 
                                select (rs_solution_cost_err_frac
                                        #, rsr_COR_spp_rep_shortfall    #  Internal error: Multiple outcomes are not supported in `estimate_metrics()`.
                                        
                                    , dom_err_type
                                        
                                        , rsp_num_occupied_PUs
                                        , rsp_num_spp
                                        , sppPUprod
                                    , log10_sppPUprod
                        
                                            #  2022 03 25 - BTL
                                            #  Not sure whether to lump these in with problem size variables or not.
                                            #  It's more like they're density variables that can be computed from simple
                                            #  counts without any notion of a graph being involved, but they
                                            #  don't fit with the notion of problems get harder as they include
                                            #  more spp and PUs, which is what most papers report.
                                        
                    , rsp_num_spp_per_PU
                                        
                                        #ig_num_edges_m
                    , edge_frac_of_possible
                        
                                            #  2022 93 25 0 - BTL
                                            #  Adding these two variables to problem size variables 
                                            #  dramatically improves the scores for problem size based prediction 
                                            #  for FPs.  Not so much for FNs.
                                                #  Are these really size variables rather than  
                                                #  graph variables?  2022 03 25 - BTL
                    # , ig_ktop  # dup var removal 2019 12 26 -BTL #    
                    # , ig_kbottom  # dup var removal 2019 12 26 -BTL #    
                                        )
                    
                        new_p3_tib_reduced_var_set = prob_size_tib
                    } 
new_p3_tib_reduced_var_set = new_p3_tib

    return (new_p3_tib_reduced_var_set)
    }

#===============================================================================

define_bdpg_numeric_output_recipe <- function (reduced_p3_TRAIN)
    {
    numeric_outcomes_recipe <- 
      recipe (rs_solution_cost_err_frac ~ . , data = reduced_p3_TRAIN) %>% 
    
      #update_role (rsr_COR_spp_rep_shortfall, new_role = "outcome") %>%
      update_role (dom_err_type, new_role = "identifier") %>%
    
          ## For modeling, it is preferred to encode qualitative data as factors 
          ## (instead of character). 
    #step_string2factor (any_of("dom_err_type")) %>% 
      step_novel (all_nominal_predictors()) %>% 
      
          ## This model requires the predictors to be numeric. The most common 
          ## method to convert qualitative predictors to numeric is to create 
          ## binary indicator variables (aka dummy variables) from these 
          ## predictors. 
      step_dummy (all_nominal_predictors()) %>% 
          ## Regularization methods sum up functions of the model slope 
          ## coefficients. Because of this, the predictor variables should be on 
          ## the same scale. Before centering and scaling the numeric predictors, 
          ## any predictors with a single unique value are filtered out. 
      step_zv (all_predictors()) %>% 
      step_normalize (all_numeric_predictors())                  #%>% 
    
    # #  step_dummy (all_nominal(), -all_outcomes()) #%>% 
    #  step_dummy (all_nominal_predictors()) %>% 
    #  step_zv (all_predictors())  %>% 
    #  step_BoxCox (all_numeric_predictors()) %>% 
    #  step_normalize (all_predictors())
    
    summary (numeric_outcomes_recipe)

    return (numeric_outcomes_recipe)
    }

#===============================================================================
#===============================================================================

define_coffee_recipe <- function (coffee_train)
    {
    coffee_recipe <- recipe(coffee_train) %>%
      update_role (everything(), new_role = "support") %>% 
      update_role (cupper_points, new_role = "outcome") %>%
      update_role (
        variety, processing_method, country_of_origin,
        aroma, flavor, aftertaste, acidity, sweetness, altitude_mean_meters,
        new_role = "predictor"
      ) %>%
      step_string2factor (all_nominal(), -all_outcomes()) %>%
            #  `step_knnimpute()` was deprecated in recipes 0.1.16.
            #  Please use `step_impute_knn()` instead.
      # step_knnimpute (country_of_origin,
      step_impute_knn (country_of_origin,
                      impute_with = imp_vars(
                      in_country_partner, company, region, farm_name, certification_body
                      )
      ) %>%
            #  `step_knnimpute()` was deprecated in recipes 0.1.16.
            #  Please use `step_impute_knn()` instead.
      # step_knnimpute (altitude_mean_meters,
      step_impute_knn (altitude_mean_meters,
                       impute_with = imp_vars(
                       in_country_partner, company, region, farm_name, certification_body,
                       country_of_origin
                       )
      ) %>%
      step_unknown (variety, processing_method, new_level = "unknown") %>%
      step_other (country_of_origin, threshold = 0.01) %>%
      step_other (processing_method, variety, threshold = 0.10) %>% 
      step_normalize (all_numeric(), -all_outcomes())   
    
    return (coffee_recipe)
    }

#===============================================================================

