#===============================================================================
#
#              v2_paper_3_cv_test_train_splitting_functions.R
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
#
#  2024 01 16 - BTL
#      - Stripping the v1 version of this file down to just the new code 
#        added in the last couple of days since most of what's in this file 
#        is no longer relevant.

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

#-------------------------------------------------------------------------------

split_into_train_test_non_ind_prob_subsets_by_prop <- function (
                                                            src_df, 
                                                            training_proportion, 
                                                            verbose = FALSE)
    {
    if ((training_proportion <= 0) | (training_proportion >= 1))
        {
            #  Fatal error if training_proportion is not in <0,1>.
        stop_bdpg (msg=paste0 ("In split_into_train_test_non_ind_prob_subsets_by_prop(): ", 
                               "training_proportion = ", training_proportion, 
                               ".  Must be in <0,1>."),  
                   browser_on=TRUE)
        } 
  
        #  Get a list of all the COR_Base problems to choose from.
    COR_Base_problem_uuids = 
        unique (src_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped)
    
    num_COR_Base_problems_in_src_df = length (COR_Base_problem_uuids)
    
    num_training_COR_Base_problems = 
        round (training_proportion * num_COR_Base_problems_in_src_df)
    
    num_test_COR_Base_problems = 
        num_COR_Base_problems_in_src_df - num_training_COR_Base_problems
   
        #  Get a random subset of uuids whose derived APP Wrap problems 
        #  will be extracted from the full input set.
    training_problem_uuids = safe_sample (COR_Base_problem_uuids, 
                                          num_training_COR_Base_problems)
    
    if (verbose)    #  Echo the chosen uuids only when debugging.
        {
        cat ("\n\ntraining_problem_uuids = \n")
        print (training_problem_uuids)
        }

        #  Extract all problems derived from the randomly chosen 
        #  COR_Base problems.
    src_df %>% 
        filter (rsp_UUID_of_COR_Base_problem_that_is_wrapped %in% 
                training_problem_uuids) -> train_set_df
    
        #  Remaining problems are all test set problems.
    src_df %>% 
        filter (!(rsp_UUID_of_COR_Base_problem_that_is_wrapped %in% 
                training_problem_uuids)) -> test_set_df

        #--------------------------------------------------------------------
        #  Check that training and test sets are independent of each other, 
        #  i.e., the intersection of their COR_Base problems is empty.  
        #--------------------------------------------------------------------

    training_test_intersection = 
        intersect (unique(train_set_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped), 
                   unique(test_set_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped))
    if (length (training_test_intersection) != 0)
        {
            #  Fatal error if training and test sets overlap.
        stop_bdpg (msg=paste0 ("In split_into_train_test_non_ind_prob_subsets_by_prop(): ", 
                               "\ntraining_test_intersection must be empty but it has length = ", 
                               length (training_test_intersection), "."),  
                   browser_on=TRUE)
        } else
        {
        cat ("\n\nVerified: training_test_intersection is empty.\n")
        }
    
        #--------------------------------------------------------------------
        #  Check that training and test sets contain all of the problems 
        #  from the original set of COR_Base problems, 
        #  i.e., the union of their COR_Base problems matches the source.  
        #--------------------------------------------------------------------

    if (verbose)    #  Echo the resulting uuids only when debugging.
        {
        cat ("\n\nResulting train_set_df uuids = \n")
        print (unique(train_set_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped))
        
        cat ("\n\nResulting test_set_df uuids = \n")
        print (unique(test_set_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped))
        }

    union_of_training_and_test_COR_Base_problems = 
        c(unique (train_set_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped), 
          unique (test_set_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped))

    if (!setequal (union_of_training_and_test_COR_Base_problems, 
                   COR_Base_problem_uuids))
        {
            #  Fatal error if the union of training and test sets does not 
            #  include all of the original COR_Base problems.
        stop_bdpg (msg=paste0 ("In split_into_train_test_non_ind_prob_subsets_by_prop(): ", 
                               "\nunion_of_training_and_test_COR_Base_problems does not ", 
                               "match the source COR_Base_problem_uuids."),  
                   browser_on=TRUE)
        } else
        {
        cat ("\n\nVerified: union_of_training_and_test_COR_Base_problems == ", 
             "COR_Base_problem_uuids")
        }
    
    return (list (train_set_df = train_set_df, 
                  test_set_df = test_set_df))
    }

#===============================================================================

#  Cloned from get_assigned_UUID_err_type_pairs_that_exist_in_all().  
 
get_assigned_UUID_err_type_pairs <- 
    function (original_df, 
              verbose = FALSE
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

#  Check that two problem sets are independent of each other, 
#  i.e., the intersection of their COR_Base problems is empty.  
#  Return TRUE if no COR_Base problem appears in both sets.  
#  Stop if any COR_Base problem appears in both sets. 

COR_base_sets_are_independent_of_each_other <- function (set_1_df, set_2_df)
    {
    COR_base_intersection = 
        intersect (unique(set_1_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped), 
                   unique(set_2_df$rsp_UUID_of_COR_Base_problem_that_is_wrapped))
    if (length (COR_base_intersection) != 0)
        {
            #  Fatal error if training and test sets overlap.
        stop_bdpg (msg=paste0 ("intersection of the 2 sets must be empty but it has non-zero length = ", 
                               length (COR_base_intersection), "."),  
                   browser_on=TRUE)
        } else
        {
        cat ("\n\nVerified: COR_base_sets_are_independent_of_each_other.\n")
        }
    
    return (TRUE)
    }

#-------------------------------------------------------------------------------

#  Check that all problems within a set are independent of each other, 
#  i.e., no problems in the set have the same COR_Base problem.  
#  Return TRUE if all problems in the are independent of each other.  
#  Stop if any problems share the same COR_Base problem. 

no_duplicate_COR_base_problems_within_set <- function (a_df)
    {
        #  If there are multiple reserve selectors in the set, 
        #  then they will probably have run on the same problems as each other, 
        #  so you need to break this test up and perform it separately for  
        #  each reserve selector.  
        #  You could probably get away with just choosing one reserve 
        #  selector and do the test only on it since each reserve selector 
        #  will probably have the identical set of problems.  
        #  However, it might be that a run failed on some reserve selector 
        #  but not on the others and so the problem count for each reserve 
        #  selector might be different from the other ones.  
        #  So, just to be safe, run the test on all of the reserve selectors 
        #  separately.  

    rs_names = unique (a_df$rs_method_name)
    for (cur_rs_name in rs_names)
        {
        df_for_cur_rs = filter (a_df, rs_method_name == cur_rs_name)
      
        num_COR_Base_problems_in_set = 
            length (df_for_cur_rs$rsp_UUID_of_COR_Base_problem_that_is_wrapped)
        
        num_unique_COR_Base_problems_in_set = 
            length (unique (df_for_cur_rs$rsp_UUID_of_COR_Base_problem_that_is_wrapped))
        
        if (num_COR_Base_problems_in_set > num_unique_COR_Base_problems_in_set)
            {
                #  Fatal error: duplicate COR_Base problems exist in the set.
            stop_bdpg (msg=paste0 ("duplicate COR_Base problems exist in set.  ", 
                                   "num_COR_Base_problems_in_set = ", 
                                   num_COR_Base_problems_in_set, 
                                   "\nand num_unique_COR_Base_problems_in_set = ", 
                                   num_unique_COR_Base_problems_in_set, 
                                   "\nfor reserve selector = ", cur_rs_name, "."),  
                       browser_on=TRUE)
            } else
            {
            cat ("\n\nVerified: No duplicate COR_Base problems within set for rs '", 
                 cur_rs_name, ".\n")
            }
        
        }
    
    return (TRUE)
    }

#-------------------------------------------------------------------------------

#  Check that test and train problem sets are independent of each other 
#  and that all problems within test set are independent of each other, 
#  i.e., no problems in the train and test sets have the same COR_Base problem 
#  and no problems within the test set have the same COR_Base problem.  
#  Return TRUE if both independence tests succeed. 
#  Stop if either test fails.  Stopping is controlled within the individual 
#  test, so failure will happen before returning to this function. 

train_and_test_sets_are_independent_between_and_test_set_is_independent_within <- 
    function (train_set_df, test_set_df)
    {
    return ((COR_base_sets_are_independent_of_each_other (train_set_df, test_set_df)) 
                  & 
            (no_duplicate_COR_base_problems_within_set (test_set_df)))
    }

#-------------------------------------------------------------------------------

#  Check that test and train problem sets are independent of each other 
#  and that all problems within train set and within test set are independent of each other, 
#  i.e., no problems in the train and test sets have the same COR_Base problem 
#  and no problems within the train set or within the test set have the same 
#  COR_Base problem.  
#  Return TRUE if all these independence tests succeed. 
#  Stop if any of the tests fail.  Stopping is controlled within the individual 
#  test, so failure will happen before returning to this function. 

train_and_test_sets_are_independent_within_and_between <- 
    function (train_set_df, test_set_df)
    {
    return ((COR_base_sets_are_independent_of_each_other (train_set_df, test_set_df)) 
                  & 
            (no_duplicate_COR_base_problems_within_set (test_set_df))
                  & 
            (no_duplicate_COR_base_problems_within_set (train_set_df))
            )
    }

#===============================================================================

