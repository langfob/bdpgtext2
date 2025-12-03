#===============================================================================
#
#                   v1_Paper_3_func_defns_for_tidymodels.R
#
#===============================================================================

#  History

#  2022 04 04 - BTL - v1
#     - Putting all existing functions that use tidymodels code into one file.

#===============================================================================

#  Define function to build the data frame of problems to draw train and 
#  test from  

build_tib_of_problems_to_draw_train_and_test_from <- function (sorted_gurobi_tib, 
                                                               uuid_cts)
    {
    max_idx = dim (sorted_gurobi_tib)[1]
    num_unique_uuids = dim (uuid_cts)[1]
    cur_total_idx = 1
    #browser()
    
    for (cur_uuid_idx in 1:num_unique_uuids)
        {
            #  Create a randomly ordered group of cv set IDs based on the number 
            #  of reserve selector runs in the current uuid group.
        num_of_entries_for_cur_uuid = uuid_cts$n [cur_uuid_idx]
        
        chosen_offset = safe_sample (0:(num_of_entries_for_cur_uuid-1), 1)
    
        sorted_gurobi_tib$cv_set_ID [cur_total_idx + chosen_offset] = 1
    
        cur_total_idx = cur_total_idx + num_of_entries_for_cur_uuid
        
        if (cur_total_idx > (max_idx+1)) 
            {
            stop_msg = paste0 ("\ncur_total_idx (", cur_total_idx, 
                               ") > max_idx (", max_idx, 
                               ") at cur_uuid_idx = ", cur_uuid_idx, 
                               "\n")
                               
            stop (stop_msg)
            }
        }
    return (sorted_gurobi_tib)
    }

#===============================================================================

strip_df_to_one_rs_and_minimal_cols <- function (p3_all_rs_working_tib, 
                                                 single_rs_name = "Gurobi")
    {
    p3_all_rs_working_tib %>%                 
        
            #  Start by reducing the full data to the minimum necessary to build test 
            #  and train, i.e., reduce the data to just one of the reserve selectors   
            #  since all reserve selectors run on the same problems.  
            #       ***  2024 01 13 - BTL  ***
            #       There may be an issue here in that gurobi didn't finish every 
            #       problem and I'm not sure whether the list of gurobi problems 
            #       will match the list for a non-gurobi reserve selector.  
            #       It might just be a subset.  Have to think about what I want 
            #       to do if that's the case.
            #  This makes it 
            #  easier to assign the cv set IDs because there's just one reserve 
            #  selector run for each problem instead of four (one from each reserve 
            #  selector) that need to be sorted out and duplicated.  
            #  Once the assignments are made to one reserve selector, it's the 
            #  same for every other reserve selector and that can be done using 
            #  a join.  
        filter (rs_method_name == "Gurobi") %>%    #  #  Using Gurobi, but could be any of the reserve selectors 
            
            #  We only need the uuid column and the 
            #  cv_set_ID column during the assignment phase, so get rid of all  
            #  the other columns to simplify and speed things up.  
        select (batch_id, 
                rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                rsp_combined_err_label) %>%
        
            #  Sort the data by the uuid of the problem that has error added 
            #  to it.
       arrange (rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                 rsp_combined_err_label) %>% 
        
            #  Add a new, empty column to hold the cross-validation set IDs 
            #  after they've been assigned.  
        mutate (cv_set_ID = 0) -> sorted_gurobi_tib
    
    glimpse (sorted_gurobi_tib)
    
    
    return (sorted_gurobi_tib)
    }

#-------------------------------------------------------------------------------

verify_that_num_uuids_is_ok <- function (sorted_gurobi_tib)
    {
        #  Count the number of reserve selector runs in each group of 
        #  uuids of the problem that has error added to it.
    sorted_gurobi_tib %>% 
        group_by (rsp_UUID_of_COR_Base_problem_that_is_wrapped) %>% 
        count() -> uuid_cts
    
    glimpse (uuid_cts)
    
    glimpse (unique (sorted_gurobi_tib$rsp_UUID_of_COR_Base_problem_that_is_wrapped))
    
        #  Do a quick check to make sure that there aren't any strange 
        #  uuid counts.  They should all be multiples of the number of 
        #  reserve selectors that were run.
    unique (uuid_cts$n)
    
        #  Also check to see what the distribution is of the number of 
        #  problems for each problem that is wrapped.  
    uuid_cts %>% 
        group_by(n) %>% 
        count() 
    
    return (uuid_cts)
    }
    
#-------------------------------------------------------------------------------

join_cv_set_IDs_back_into_main_data_for_all_rs <- function (p3_all_rs_working_tib, 
                                                            sorted_gurobi_tib)
    {
    new_p3 = 
        inner_join (p3_all_rs_working_tib, 
                    
                    select (sorted_gurobi_tib, 
                            -batch_id), 
                    
                    by = c("rsp_UUID_of_COR_Base_problem_that_is_wrapped", 
                           "rsp_combined_err_label"), 
                    
                    suffix = c(".DUPLICATE.X", ".DUPLICATE.Y")
                    )
    
    new_p3 %>% 
        
        select (rsp_UUID_of_COR_Base_problem_that_is_wrapped, rs_method_name, 
                rsp_combined_err_label, cv_set_ID) %>% 
        
        arrange (rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                 cv_set_ID, rs_method_name, rsp_combined_err_label) -> tempcols
    
    tempcols
    
    new_p3 %>% 
        filter (cv_set_ID == 1) -> new_p3
    
    new_p3 %>% 
        group_by (rsp_combined_err_label) %>% 
        count() -> err_label_cts
    err_label_cts
    
    #--------------------

    return (new_p3)
    }

#===============================================================================

#  Define function to build a random subset of bdpg data where all rows are 
#  independent  

        #  Since test set problems need to be independent of each other, 
        #  we can only choose one APP problem from each set derived from 
        #  a single Base problem.  However, the training set problems don't 
        #  have to be independent of each other, so the training set can 
        #  have all of the APP problems for each Base problem.  
        #  They just can't have any problems whose Base problem is the 
        #  same as one that's in the test set.  
        # 
        #  This makes building cv folds a little tricky, so for the moment, 
        #  I'm going to just take one APP problem from each Base problem in 
        #  the training set even though more are allowed.  
        #  I'll fix this later when I better understand the tidymodels 
        #  functions that build cv folds, etc.

build_random_independent_subset_of_bdpg_data <- function (p3_all_rs_working_tib, 
                                                          single_rs_name = "Gurobi")
    {
browser()    
    
    sorted_gurobi_tib = 
        strip_df_to_one_rs_and_minimal_cols (p3_all_rs_working_tib, 
                                             single_rs_name)
 
    uuid_cts = verify_that_num_uuids_is_ok (sorted_gurobi_tib)
   
    #--------------------

    sorted_gurobi_tib = 
        build_tib_of_problems_to_draw_train_and_test_from (sorted_gurobi_tib, 
                                                           uuid_cts)
    glimpse (sorted_gurobi_tib)
    
    #--------------------

        #  Join the cv_set_IDs back into the main data set for 
        #  all reserve selectors (since they're now only assigned for Gurobi).  

    new_p3 = join_cv_set_IDs_back_into_main_data_for_all_rs (p3_all_rs_working_tib, 
                                                             sorted_gurobi_tib)
    
    return (new_p3)
    }

#===============================================================================

load_bdpg_data_and_build_random_independent_subset <- 
        function (rds_input_file_path, save_result_to_disk=FALSE)
    {
        #  Read full APP Wrap data set from RDS file  
    p3_all_rs_working_tib = readRDS (rds_input_file_path)
    dim (p3_all_rs_working_tib)
    
        #  Some columns have inappropriate names in that they are 
        #  meaningless/confusing or will be duplicated by downstream actions 
        #  (e.g., id is a column name generated by tidymodels).  
        #  This should actually have been done before the file was written 
        #  out, but for now, we need to do it here.
    p3_all_rs_working_tib = rename (p3_all_rs_working_tib, batch_id = id)

        #  Need to select a subset of the full data so that the subset 
        #  meets certain constraints that make sure that each row is 
        #  independent.  The goal is to have two forms of independence 
        #  in the test set:  
        #   - Every problem in the test set (fold) is independent of 
        #     every other problem in the test set (fold), i.e., no problem 
        #     in that set is derived from the same Base problem.  
        #   - Every problem in the test set (fold) is independent of 
        #     every problem in the training set (folds).  

    new_p3 = build_random_independent_subset_of_bdpg_data (p3_all_rs_working_tib)
    
        #  Make sure that there are no duplicate problems in the set.
    num_rs = 4
##2024 01 12##    length (unique (new_p3$rsp_UUID_of_base_problem_that_is_wrapped)) == 
    length (unique (new_p3$rsp_UUID_of_COR_Base_problem_that_is_wrapped)) == 
        (dim (new_p3)[1]) / num_rs
    
    if (save_result_to_disk) 
        {
        outfile_name = "new_p3.rds"  
        rds_output_file_path =  file.path (here(), "Data/TempOutput", outfile_name)
        saveRDS (new_p3, rds_output_file_path)
        }
    
    return (new_p3)
    }

#===============================================================================

create_bdpg_splits_and_folds <- function (new_p3_tib, seed1 = NULL, seed2 = NULL)
    {
        #  Split into test/train  
    
    if (!is.null (seed1)) set.seed (seed1)
        
    splits = initial_split (new_p3_tib    
                            #, strata = dom_err_type
                            , prop = 3/4)
    
    new_p3_TRAIN = training (splits)
    new_p3_TEST  = testing (splits)
    
                # training set proportions by dom_err_type
            new_p3_TRAIN %>% 
              count (dom_err_type) %>% 
              mutate (prop = n/sum(n))
            
                # test set proportions by dom_err_type
            new_p3_TEST  %>% 
              count (dom_err_type) %>% 
              mutate (prop = n/sum(n))
    
        #  Split into cross-validation folds  
    
    if (!is.null (seed2)) set.seed (seed2)
        
    new_p3_folds = 
       vfold_cv (new_p3_TRAIN
                 , v = 10
                 #, strata = dom_err_type 
                 #, repeats = 5
                 )

    return (list (bdpg_splits = splits, 
                  bdpg_folds = new_p3_folds))
    }

#===============================================================================

