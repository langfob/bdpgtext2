#===============================================================================
#
#              v1_paper_3_cv_test_train_splitting_functions.R
#
#===============================================================================

#  History

#  2021 02 19 - BTL
#      - Created by extracting existing functions from 
#            bdpgtext/Analysis_scripts/
#            v14_bdpg_analysis_scripts_function_defns.paper_3.R
#        and moving them into this new R file.
#        
#  2024 01 15 - BTL
#      - Adding new functions to refactor and abstract the building of 
#        training and test sets that use problems that are independent of 
#        each other and therefore, don't lead to overestimation of the 
#        number of samples when evaluating lm and other fitted predictive 
#        functions.

#===============================================================================

#  Load necessary libraries

#===============================================================================

get_assigned_UUID_err_type_pairs_that_exist_in_all <- 
    function (original_df, 
              assign_by_unique_UUID_errtype_pair = FALSE, 
              training_split_denom = 2, 
              sample_training_data_with_replacement = FALSE
             )
    {
        #---------------------------------------------------------------
        #  Get rid of all columns other than the ones necessary to see
        #  if the UUID/error type occurs in all reserve selectors.
        #---------------------------------------------------------------
  
    original_df %>% 
        select (rs_method_name, 
                rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                rsp_combined_err_label
                # , is_train
                ) -> 
      df
  
        #---------------------------------------------------------------
        #  Determine the number of unique entries for each key column.
        #---------------------------------------------------------------
  
            #  Reserve selector method names
    unique_rs_method_names = unique (df$rs_method_name)
    num_unique_rs_method_names = length (unique_rs_method_names)
    
            #  Base problem UUIDs
    unique_prob_UUIDs = unique (df$rsp_UUID_of_COR_Base_problem_that_is_wrapped)
    num_unique_prob_UUIDs = length (unique_prob_UUIDs)
    
            #  Error types
    unique_err_types = unique (df$rsp_combined_err_label)
    num_ind_rand_prob_groups = length (unique_err_types)
    
        #-----------------------------------------------------------------------
        #  Identify which UUID/error type pairs occur for every reserve selector.
        #  We want to eliminate any that don't occur for every reserve selector 
        #  since you can't do a pairwise comparison between every pair of reserve 
        #  selector problem instances.  
        #  This could just be an option, since right now, I don't do these 
        #  comparisons at all and doing this filtering removes data points from 
        #  the full test and train sample.
        #-----------------------------------------------------------------------
    
    df %>%
      group_by (#rs_method_name, 
                rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                rsp_combined_err_label) %>%
      mutate (
        exists_in_ct = n(), 
        exists_in_all = (n() == num_unique_rs_method_names) 
        
      ) -> exists_df  #  Need to ungroup here too?
    
        #  Can't get ungroup() to work inside previous statement
    exists_df = ungroup (exists_df)  
    
    #--------------------------------------------------------------
    #  Sort by whether each line occurs in all reserve selectors.
    #  This is just for display in debugging/testing and can be 
    #  removed/commented out once things are working.
    #--------------------------------------------------------------
    
    if(TRUE){  
        fails        = filter (exists_df, !exists_in_all)
        sorted_fails = arrange (fails, rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                                       rsp_combined_err_label)
    }
    
        #--------------------------------------------------------------
        #  Collect all of the problem/error type pairs that occur in 
        #  every reserve selector.
        #--------------------------------------------------------------
  
    successful_df = filter (exists_df, exists_in_all)
    
    first_method_name = unique_rs_method_names [1]
    successful_df %>% 
        filter (rs_method_name == first_method_name) %>% 
        select (-rs_method_name) -> 
      successful_UUID_err_type
        
        #--------------------------------------------------------------
        #  Randomly assign each .
        #--------------------------------------------------------------

if (assign_by_unique_UUID_errtype_pair)
    {
    successful_UUID_err_type %>%
        group_by (rsp_UUID_of_COR_Base_problem_that_is_wrapped ) %>%
        mutate (
              #  Assign what may become a cross-validation set ID to each 
              #  problem.  Because there are 4 types of error used in the 
              #  bdpgtext study, there can be at most 4 different independent 
              #  sets of cross-validation problems chosen.  So, you need to 
              #  choose a random number between 1 and 4 for the cv set ID 
              #  assigned to each problem.  This value cannot be repeated 
              #  within the same set of problems derived from a given 
              #  COR Base problem.  There will usually be 4 derived (App Wrap) 
              #  problems for each COR Base problem, but something like a 
              #  failed run can reduce that number.  In those cases, you 
              #  still want to draw the random cv set ID (ind_rand_prob_group) 
              #  from the values 1:4 instead of 1 to the number of problems 
              #  in the group.  This is because you don't end up with group 4  
              #  being much smaller than all the other groups.  This way, 
              #  "missing" problems will left out of all of the groups 
              #  randomly rather than only being left out of the higher 
              #  numbered cv set IDs.  
              #  In the command below, we randomly choose n() numbers between 1 
              #  and the number of error types (generally 4), where n() is the 
              #  number of App Wrap problems derived from the current COR Base 
              #  problem
          ind_rand_prob_group = safe_sample (1:num_ind_rand_prob_groups, n(), 
                                             replace=FALSE)
          
                ) %>%
        ungroup (successful_UUID_err_type) ->
      assigned_df
  
    #   ) -> assigned_df  #  Need to ungroup here too?
    # assigned_df = ungroup (assigned_df)
  
    } else
        #  2024 01 15 - BTL
        #  Can't remember what this else branch is for.  
        #  However, I think something may also be wrong with the way it 
        #  uses training_split_denom.  The way it's used may only work when 
        #  the test and train split is 50/50.  For example, it looks like this 
        #  may cause the training set to be 1/3 of the whole set when the 
        #  training_split_denom is 3.
    {
    unique_UUIDs = unique (successful_UUID_err_type$rsp_UUID_of_COR_Base_problem_that_is_wrapped)
    num_unique_UUIDs = length (unique_UUIDs)
    num_UUIDs_in_training_set = round (num_unique_UUIDs / training_split_denom)
    
    training_UUID_indices = 
        safe_sample (1:num_unique_UUIDs, 
                num_UUIDs_in_training_set, 
                replace = sample_training_data_with_replacement)
    
    training_UUIDs = unique_UUIDs [training_UUID_indices]

    training_rows = which (
       successful_UUID_err_type$rsp_UUID_of_COR_Base_problem_that_is_wrapped 
          %in% 
       training_UUIDs)

    successful_UUID_err_type = mutate (successful_UUID_err_type, 
                                       is_train = FALSE)
    successful_UUID_err_type$is_train [training_rows] = TRUE
    
    assigned_df = successful_UUID_err_type
    }
    
        #------------------------------------------------------------------
        #  Get rid of the temporary columns that were just used to figure 
        #  out what group to assign things to.
        #------------------------------------------------------------------
        #  Should end up with a data frame that has just 3 columns 
        #  which identify an independent random problem group ID for each 
        #  unique UUID/error type pair:
        #     - rsp_UUID_of_COR_Base_problem_that_is_wrapped
        #     - rsp_combined_err_label
        #     - ind_rand_prob_group
        #------------------------------------------------------------------
    
    assigned_df = select (assigned_df, -c (exists_in_ct, exists_in_all))

        #------------------------------------------------------------------
        #  Finally, reduce the original df to just the rows that met all 
        #  the criteria here for UUID/error type occurring in all reserve 
        #  selectors.  Also include the new column for problem group ID.
        #------------------------------------------------------------------

    original_df %>% 
        inner_join (assigned_df) %>% 
        ungroup() ->
      working_tib

    if(TRUE){    
    #---------------------------------------------------
    #  Sort for display in debugging/testing. 
    #  Can remove/comment out once things are working.
    #---------------------------------------------------
    
        working_tib %>%
            arrange (rs_method_name, 
                     rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                     rsp_combined_err_label) -> 
          working_tib
    }
    
    return (working_tib)
    }

#===============================================================================

split_all_rs_into_independent_tests_and_trains <- function (all_rs_working_tib, 
                                                            training_split_denom)
    {
    # all_rs_working_tib %>%
    #     mutate (is_train = FALSE) -> 
    #   all_rs_working_tib

    echo_rs_method_name_order_within_df (all_rs_working_tib, "after mutate")
    
        all_rs_working_tib %>% 
            group_by (rs_method_name) -> 
          tempdf1
    
    echo_rs_method_name_order_within_df (tempdf1, "after group_by")
    
browser()    
    
    
    assign_by_unique_UUID_errtype_pair = FALSE
    all_rs_working_tib = 
    #    get_assigned_UUID_err_type_pairs_that_exist_in_all (all_rs_working_tib)
        get_assigned_UUID_err_type_pairs_that_exist_in_all (all_rs_working_tib, 
                                                            assign_by_unique_UUID_errtype_pair, 
                                                            training_split_denom, 
                                                            sample_training_data_with_replacement = FALSE
                                                            )

    if (assign_by_unique_UUID_errtype_pair)
        {
    stop()
        # train_ind_rand_prob_group_ID = 1
        # test_ind_rand_prob_group_ID  = 2
        # 
        # # working_tib [ind_rand_prob_group == train_ind_rand_prob_group_ID] = TRUE
        # # working_tib %>% 
        # #     filter (ind_rand_prob_group %in% c(train_ind_rand_prob_group_ID, 
        # #                                        test_ind_rand_prob_group_ID)) -> 
        # #   working_tib
        # 
        # ind_fold_1_tib = filter (working_tib, ind_rand_prob_group == 1)
        # ind_fold_2_tib = filter (working_tib, ind_rand_prob_group == 2)
        # ind_fold_3_tib = filter (working_tib, ind_rand_prob_group == 3)
        # ind_fold_4_tib = filter (working_tib, ind_rand_prob_group == 4)
        # 
        # train_part_tib = 
        #     switch (train_ind_rand_prob_group_ID, 
        #             
        #             1 = ind_fold_1_tib, 
        #             2 = ind_fold_2_tib, 
        #             3 = ind_fold_3_tib, 
        #             4 = ind_fold_4_tib, 
        #             
        #             stop (paste0 ("\n\nBad train_ind_rand_prob_group_ID = '", 
        #                           train_ind_rand_prob_group_ID, "'\n"))
        #             )
        #     
        # test_part_tib = 
        #     switch (test_ind_rand_prob_group_ID, 
        #             
        #             1 = ind_fold_1_tib, 
        #             2 = ind_fold_2_tib, 
        #             3 = ind_fold_3_tib, 
        #             4 = ind_fold_4_tib, 
        #             
        #             stop (paste0 ("\n\nBad test_ind_rand_prob_group_ID = '", 
        #                           test_ind_rand_prob_group_ID, "'\n"))
        #             )
        #     
        # all_rs_working_tib = rbind (train_part_tib, test_part_tib)
        
        } 
  
    # *****  THIS STEP REORDERS THE DATA FRAME ON OUTPUT  ***** # 
    # *****  EVEN THOUGH ALL IT'S DOING IS SETTING THE    ***** #
    # *****  VALUE OF ONE ALREADY EXISTING COLUMN.        ***** #
    #     tempdf1 %>% 
    #         group_modify (~ more_alt_split_one_rs_into_test_and_train (.x, 
    #                                                           training_split_denom, 
    #                                                           FALSE, 
    #                                                           training_UUIDs)) -> tempdf2
    # 
    # echo_rs_method_name_order_within_df (tempdf2, "after group_modify")
    # 
    #     tempdf2    %>% 
    #         ungroup() -> all_rs_working_tib
    #     
    # echo_rs_method_name_order_within_df (all_rs_working_tib, "after ungroup")
           
        #  Remove the base problem UUID because it's no longer needed and 
        #  BoxCox transform downstream chokes on any non-numeric column.
    all_rs_working_tib = select (all_rs_working_tib, 
                                 -rsp_UUID_of_COR_Base_problem_that_is_wrapped)
    
    print (colnames (all_rs_working_tib))

    return (all_rs_working_tib)
    }

#===============================================================================

test_split_all_rs_into_tests_and_trains <- function ()
    {
    library(dplyr)
    
    set.seed (124)
    
    training_split_denom = 3
    
    all_rs_working_tib = tibble (rs_method_name = c ('G', 'G', 'G', 'G', 
                                                     'M', 'M', 'M', 'M', 
                                                     'Z', 'Z', 'Z', 'Z'), 
                                 value = c ('ga','gb','gc','gd',
                                            'ma','mb','mc','md',
                                            'za','zb','zc','zd'))
    
    all_rs_working_tib = 
#        split_all_rs_into_tests_and_trains (all_rs_working_tib, 
        split_all_rs_into_independent_tests_and_trains (all_rs_working_tib, 
                                            training_split_denom)

    return (all_rs_working_tib)
    }

# test_split_all_rs_into_tests_and_trains ()

#===============================================================================

## Function to build test and train data frames

#  Use this function for building any of the test and train data frame pairs, 
#  since the logic is the same for all of them.  The only difference between 
#  them is what columns are selected for inclusion. 
#
#  To use this function, you just hand it a function that selects the 
#  specific columns of interest for the desired data set.  These functions 
#  have been named with a consistent form of:  select_*_df_cols(), where 
#  the "*" refers to the data items of interest, e.g., prob_size or graph.
#  That selection routine needs to do two things:  
#      1)  select the appropriate columns
#      2)  call the add_median_redundancies() function if the resulting 
#          data sets need to contain those two columns as well
#          (This is necessary to handle issues with zero variance in the 
#           redundancies when looking only at FN data.  This is explained 
#           where the include_median_redundancies flag is defined 
#           earlier in the code.)

build_specific_test_and_train <- function (working_train_df, 
                                           working_test_df, 
                                      func_for_col_selection, 
                                           include_median_redundancies = FALSE)
    {
        #  Call the function passed in here to select only the columns 
        #  specific to the data set you want for the specific fitting test, 
        #  e.g., only columns related to problem size or only related to 
        #  graph variables.
  
    train_x_df = func_for_col_selection (working_train_df, 
                                         include_median_redundancies)
    test_x_df  = func_for_col_selection (working_test_df, 
                                         include_median_redundancies)
    
    cat ("\n\nis_grouped_df (train_x_df) = ", 
         is_grouped_df (train_x_df), sep='')
    
    cat ("\n\nis_grouped_df (test_x_df) = ", 
         is_grouped_df (test_x_df), sep='')

    return (list (train_x_df = train_x_df, 
                  test_x_df  = test_x_df))
    }

#===============================================================================

      #  Create set of folds with each row having a fold ID and a vector of 
      #  row numbers that are included in that fold.

create_cv_folds <- function (num_folds, IDs_to_fold)
  {
  num_IDs_to_fold    = length (IDs_to_fold)
  fold_size          = floor (num_IDs_to_fold / num_folds)
  folds_list         = vector("list", num_folds)
  
  random_row_indices = sample.int (num_IDs_to_fold, 
                                   num_IDs_to_fold, 
                                   replace=FALSE)

      #  Pull randomized rows into folds by grabbing successive fold-size 
      #  chunks into separate vectors in a list of vectors. 
  
      #  Treat the last chunk differently from the rest.  
      #  If num_IDs_to_fold isn't an even multiple of fold_size, 
      #  dump any extra rows into the last fold.  
      #  The number of folds is usually 10 and the fold size for  
      #  this project will usually be in the hundreds at least, 
      #  so having up to 9 extra elements in one of the folds will 
      #  not cause a lot of extra weight on that fold.

  for (cur_fold_idx in 1:(num_folds-1))
      {
      cur_fold_start_idx = (cur_fold_idx - 1) * fold_size + 1
      cur_fold_end_idx   = cur_fold_idx * fold_size
      folds_list [[cur_fold_idx]] = 
          random_row_indices [cur_fold_start_idx : cur_fold_end_idx]
      }
  
          #  Handle last fold.
  
  cur_fold_start_idx = (num_folds - 1) * fold_size + 1
  folds_list [[num_folds]] = 
      random_row_indices [cur_fold_start_idx : num_IDs_to_fold]
  
      #  Create a data frame of folds with each row having 
      #  a fold ID and a vector of row numbers that are included in that fold.
          #  For some reason, doing it in the most straightforward way causes 
          #  an error that I don't understand, e.g.,:
          #     Error in data.frame(fold_id = 1:num_folds, 
          #                         fold_row_nums = folds_list) : 
          #     arguments imply differing number of rows: 5, 6
          #  Doing it by adding a new column seems to work instead.
# folds_df = data.frame (fold_id=1:num_folds, 
#                        fold_row_nums = folds_list)
  folds_df = data.frame (fold_id=1:num_folds)
  folds_df$fold_row_nums = folds_list

  return (folds_df)
  }

#===============================================================================
#===============================================================================
#===============================================================================
#
#  2024 01 15 - BTL
#  From here down is new stuff added starting in January, 2024.  
#  
#===============================================================================
#===============================================================================
#===============================================================================

#  This function is meant to provide a single, standard call for building 
#  a training set.  It will return either the same set that is fed to it 
#  or a random subset of that input set.  The random subset will contain 
#  all problems that are derived from the same COR Base problems that are 
#  randomly chosen to produce a subsets with no overlapping COR Base problems.  
#  Note that the subset_size refers to the number of COR Base problems in 
#  the subset, NOT the total number of problems in the subset.  The total 
#  number of problems will generally be much larger than the subset_size 
#  value since there are usually 4 different App Wrap problems derived from 
#  each COR Base problems in the bdpgtext study.  

build_training_set <- function (train_src_df, use_subset, subset_size = NA, 
                                verbose = FALSE)
    {
    train_set_df = train_src_df
  
    if (use_subset)
        train_set_df = build_non_independent_problem_subset (train_src_df, 
                                                             use_subset, 
                                                             subset_size, 
                                                             verbose)

    return (train_set_df)
    }

#-------------------------------------------------------------------------------

#  This function is the workhorse for building the subsetted data described 
#  in the comments for the function build_training_set() above.  
#  This function will stop if any errors are found in the process (e.g., 
#  a subset_size that is too small or too large).  If there are no errors, 
#  then it will return a subset of the input data frame train_src_df.   

build_non_independent_problem_subset <- function (train_src_df, 
                                                  use_subset, 
                                                  subset_size = NA, 
                                                  verbose = FALSE)
    {
    train_set_df = NULL
  
        #  Get a list of all the COR_Base problems to choose from.
    COR_Base_problem_uuids = 
        unique (train_src_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped)
    
    num_COR_Base_problems_in_train_src_df = length (COR_Base_problem_uuids)
    
        #  Given a specified number of problems for the subset, 
        #  randomly choose that many problems to use as the basis for 
        #  what to include in the subset.
    if (is.na (subset_size))
        {
            #  Fatal error if no subset size is provided.
        stop_bdpg (msg=paste0 ("In build_non_independent_problem_subset(): ", 
                               "told to use a subset so must be given a ", 
                               "subset size, but none given."),  
                   browser_on=TRUE)
      
        } else
        {
        if ((subset_size <= 0) | (subset_size > num_COR_Base_problems_in_train_src_df))
            {
                #  Fatal error if subset size is bigger than the input set or 
                #  if it's non-positive.
            stop_bdpg (msg=paste0 ("In build_non_independent_problem_subset(): ", 
                                   "subset_size (",
                                   subset_size, ") must be > 0 and <= to ",  
                                   "num_COR_Base_problems_in_train_src_df (",
                                   num_COR_Base_problems_in_train_src_df, 
                                   ")."), 
                       browser_on=TRUE)
          
            } else
            {
                #  Get a random subset of uuids whose derived APP Wrap problems 
                #  will be extracted from the full input set.
            subset_problem_uuids = safe_sample (COR_Base_problem_uuids, 
                                                subset_size)
            
            if (verbose)    #  Echo the chosen uuids only when debugging.
                {
                cat ("\n\nsubset_problem_uuids = \n")
                print (subset_problem_uuids)
                }

                #  Extract all problems derived from the randomly chosen 
                #  COR_Base problems.
            train_src_df %>% 
                filter (rsp_UUID_of_COR_Base_problem_that_is_wrapped %in% 
                        subset_problem_uuids) -> train_set_df
            
            if (verbose)    #  Echo the resulting uuids only when debugging.
                {
                cat ("\n\nResulting train_set_df uuids = \n")
                print (unique(train_set_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped))
                }
            }
        }

    return (train_set_df)
    }

#===============================================================================

#  Cloned from get_assigned_UUID_err_type_pairs_that_exist_in_all().  
 
get_assigned_UUID_err_type_pairs <- 
    function (original_df
              # , 
              # assign_by_unique_UUID_errtype_pair = FALSE, 
              # training_split_proportion = 0.5, 
              # sample_training_data_with_replacement = FALSE
             )
    {
        #---------------------------------------------------------------
        #  Get rid of all columns other than the ones necessary to see
        #  if the UUID/error type occurs in all reserve selectors.
        #---------------------------------------------------------------
  
    original_df %>% 
        select (rs_method_name, 
                rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                rsp_combined_err_label
                # , is_train
                ) -> 
      df
  
    num_unique_err_types = length (unique (df$rsp_combined_err_label))
    successful_UUID_err_type = 
        find_uuid_err_type_pairs_that_occur_in_all_reserve_selectors (df)

    successful_UUID_err_type %>%
        group_by (rsp_UUID_of_COR_Base_problem_that_is_wrapped ) %>%
        mutate (
              #  Assign what may become a cross-validation set ID to each 
              #  problem.  Because there are 4 types of error used in the 
              #  bdpgtext study, there can be at most 4 different independent 
              #  sets of cross-validation problems chosen.  So, you need to 
              #  choose a random number between 1 and 4 for the cv set ID 
              #  assigned to each problem.  This value cannot be repeated 
              #  within the same set of problems derived from a given 
              #  COR Base problem.  There will usually be 4 derived (App Wrap) 
              #  problems for each COR Base problem, but something like a 
              #  failed run can reduce that number.  In those cases, you 
              #  still want to draw the random cv set ID (ind_rand_prob_group) 
              #  from the values 1:4 instead of 1 to the number of problems 
              #  in the group.  This is because you don't end up with group 4  
              #  being much smaller than all the other groups.  This way, 
              #  "missing" problems will left out of all of the groups 
              #  randomly rather than only being left out of the higher 
              #  numbered cv set IDs.  
              #  In the command below, we randomly choose n() numbers between 1 
              #  and the number of error types (generally 4), where n() is the 
              #  number of App Wrap problems derived from the current COR Base 
              #  problem
                                            #  2024 01 15 - SHOULDN'T THIS BE 
                                            #  1:NUM_unique_err_types instead of 1:unique_err_types?
#          ind_rand_prob_group = safe_sample (1:unique_err_types, n(),    
          ind_rand_prob_group = safe_sample (1:num_unique_err_types, n(),    
                                             replace=FALSE)
          
                ) %>%
#        ungroup (successful_UUID_err_type) ->
        ungroup () ->
      assigned_df

        #------------------------------------------------------------------
        #  Get rid of the temporary columns that were just used to figure 
        #  out what group to assign things to.
        #------------------------------------------------------------------
        #  Should end up with a data frame that has just 3 columns 
        #  which identify an independent random problem group ID for each 
        #  unique UUID/error type pair:
        #     - rsp_UUID_of_COR_Base_problem_that_is_wrapped
        #     - rsp_combined_err_label
        #     - ind_rand_prob_group
        #------------------------------------------------------------------
    
    assigned_df = select (assigned_df, -c (exists_in_ct, exists_in_all))

        #------------------------------------------------------------------
        #  Finally, reduce the original df to just the rows that met all 
        #  the criteria here for UUID/error type occurring in all reserve 
        #  selectors.  Also include the new column for problem group ID.
        #------------------------------------------------------------------

    original_df %>% 
        inner_join (assigned_df) %>% 
        ungroup() ->
      working_tib

    if(TRUE){    
    #---------------------------------------------------
    #  Sort for display in debugging/testing. 
    #  Can remove/comment out once things are working.
    #---------------------------------------------------
    
        working_tib %>%
            arrange (rs_method_name, 
                     rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                     rsp_combined_err_label) %>%    #-> 
            select (rs_method_name, 
                    rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                    rsp_combined_err_label, 
                    ind_rand_prob_group) ->
          narrow_working_tib
    }
    
    return (working_tib)
    }

#-------------------------------------------------------------------------------
#
find_uuid_err_type_pairs_that_occur_in_all_reserve_selectors <- 
    function (df
              #, 
              #assign_by_unique_UUID_errtype_pair = FALSE, 
              #training_split_denom = 2, 
              #sample_training_data_with_replacement = FALSE
             )
    {
        #-------------------------------------------------------------
        #  Determine the number and names of unique RS method names.
        #-------------------------------------------------------------
  
    unique_rs_method_names = unique (df$rs_method_name)
    num_unique_rs_method_names = length (unique_rs_method_names)
    
        #-----------------------------------------------------------------------
        #  Identify which UUID/error type pairs occur for every reserve selector.
        #  We want to eliminate any that don't occur for every reserve selector 
        #  since you can't do a pairwise comparison between every pair of reserve 
        #  selector problem instances.  
        #  This could just be an option, since right now, I don't do these 
        #  comparisons at all and doing this filtering removes data points from 
        #  the full test and train sample.
        #-----------------------------------------------------------------------
    
    df %>%
      group_by (#rs_method_name, 
                rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                rsp_combined_err_label) %>%
      mutate (
        exists_in_ct = n(), 
        exists_in_all = (n() == num_unique_rs_method_names) 
        
      ) -> exists_df  #  Need to ungroup here too?
    
        #  Can't get ungroup() to work inside previous statement
    exists_df = ungroup (exists_df)  
    
                                          #--------------------------------------------------------------
                                          #  Sort by whether each line occurs in all reserve selectors.
                                          #  This is just for display in debugging/testing and can be 
                                          #  removed/commented out once things are working.
                                          #--------------------------------------------------------------
                                          
                                          if(TRUE){  
                                              fails        = filter (exists_df, !exists_in_all)
                                              sorted_fails = arrange (fails, rsp_UUID_of_COR_Base_problem_that_is_wrapped, 
                                                                             rsp_combined_err_label)
                                          }
                                          
        #--------------------------------------------------------------
        #  Collect all of the problem/error type pairs that occur in 
        #  every reserve selector.
        #--------------------------------------------------------------
  
    successful_df = filter (exists_df, exists_in_all)
    
    first_method_name = unique_rs_method_names [1]
    successful_df %>% 
        filter (rs_method_name == first_method_name) %>% 
        select (-rs_method_name) -> 
      successful_UUID_err_type
    
    return (successful_UUID_err_type)
    }
        
#===============================================================================
