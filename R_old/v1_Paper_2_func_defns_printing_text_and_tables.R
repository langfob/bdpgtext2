#===============================================================================
#
#               v1_Paper_2_func_defns_printing_text_and_tables.R
#
#===============================================================================

#  History

#  2020 08 19 - BTL - v1
#     - Extracted from v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R 
#       as part of simplifying the paper 2 functions.

#===============================================================================

#  Functions to build tables

#===============================================================================

test_create_rs_fivenum_tib   <- function ()
    {
    rs_method_names_list = c("Gurobi", "Marxan_SA", "UR_Forward", "Marxan_SA_SS")
    full_df = data.frame (rs_method_name=rs_method_names_list, a=1:1000, b=1000:1)
    
    num_prob_per_rs = count (full_df, rs_method_name)
    
    set.seed(123)
    df = data.frame (rs_method_name=rs_method_names_list, 
                     a=rnorm(100), b=rnorm(100), 
                     stringsAsFactors = FALSE)
    
    rs_fivenum_tib = 
        create_rs_fivenum_tib (rs_method_names_list, 
                               src_tib = filter (df, b <= 0), 
                               col_to_fivenum_str = "b", 
                               num_prob_per_rs = num_prob_per_rs, 
                               table_caption = "This is a kable table.", 
                               include_mad_in_fivenum_table_output = TRUE,
                               meth_name_col_str = "rs_method_name", 
                               # c("ct","frac","min","Q1","median", 
                               #   "Q3","max","mean","sd", "mad")
                               c("ct","min","Q1","median", 
                                 "Q3","max")

)
    
    #browser()
    }

create_rs_fivenum_tib   <- function (rs_method_names_list, 
                                     src_tib, 
                                     col_to_fivenum_str, 
                                     num_prob_per_rs, 
                                     table_caption = "This is a kable table.", 
                                     include_mad_in_fivenum_table_output = TRUE,
                                     meth_name_col_str = "rs_method_name", 
                                     names_of_cols_to_include = 
                                        c("ct","frac","min","Q1","median",
                                          "Q3","max","mean","sd", "mad")
)
    {
#browser()
    src_tib %>% 
      group_by (rs_method_name) %>% 
      summarise (ct = n(),
                 frac = NA,
                 min = min (!!sym (col_to_fivenum_str)), 
                 Q1 = fivenum (!!sym (col_to_fivenum_str))[2],
                 median = median (!!sym (col_to_fivenum_str)), 
                 Q3 = fivenum (!!sym (col_to_fivenum_str))[4],
                 max = max (!!sym (col_to_fivenum_str)), 
                 mean = mean (!!sym (col_to_fivenum_str)),
                 sd = sd (!!sym (col_to_fivenum_str)), 
                 mad = mad (!!sym (col_to_fivenum_str))
                 
                 )    -> fivenums    #fivenum (a)[5]) 
    
        #  Add n = original tib cts by reserve selector.  
        #  For example, in original full app_wrap_tib, there may have been 
        #  12,000 Gurobi records and 11,582 Marxan_SA records, etc.
        #  Here, we want to compute what fraction of those original values 
        #  is still included in the filtered set being analyzed, e.g, if 
        #  you've removed all of the FNs, then about half of the original 
        #  count of values for each reserve selector should be in this set.
    fivenums = left_join (fivenums, num_prob_per_rs)
    fivenums$frac = fivenums$ct / fivenums$n
    
        #  Showing all of the computed columns is often overwhelming, so 
        #  allow the caller to choose which columns to show.
    fivenums = select (fivenums, all_of (names_of_cols_to_include))

    
    
    #  Delete the dummy initialization line from the table since 
    #  it was only needed to create an initial object for bind_rows() 
    #  to act on.
    #  Then, label the rows with their reserve selector names.
    #  Note that we convert from a tibble to a data frame since 
    #  we need rownames to get kable() to properly label the 
    #  rows with reserve selector names and the tibble class 
    #  has deprecated the use of rownames and will eventually 
    #  prohibit them.
fivenums = as.data.frame (fivenums)
rownames(fivenums) = unique (src_tib$rs_method_name)
    
    
        
    return (fivenums)
    }
  
old_create_rs_fivenum_tib   <- function (rs_method_names_list, 
                                     src_tib, 
                                     col_to_fivenum_str, 
                                     num_prob_per_rs, 
                                     table_caption = "This is a kable table.", 
                                     include_mad_in_fivenum_table_output = TRUE,
                                     meth_name_col_str = "rs_method_name")
    {
    if (is.null (include_mad_in_fivenum_table_output))
        include_mad_in_fivenum_table_output = TRUE

        #  Initialize the results table with a dummy line since 
        #  it seems that you can't create an empty tibble.  
        #  Will delete the dummy line after table is fully loaded.
    if (include_mad_in_fivenum_table_output) 
        {
        fivenum_tib = tibble (ct = c(0), frac = c(0), mad = c(0), 
                              min = c(1), Q1 = c(2), median = c(3), 
                              Q3 = c(4), max = c(5))
        } else 
        {
        fivenum_tib = tibble (ct = c(0), frac = c(0), 
                              min = c(1), Q1 = c(2), median = c(3), 
                              Q3 = c(4), max = c(5))
        }

    fivenum_col_names = c("min", "Q1", "median", "Q3", "max")

        #  Extract the values for the given column name and reserve selector.
    meth_names = src_tib [[meth_name_col_str]]
    all_values = src_tib [[col_to_fivenum_str]]
    
        #  Get min, max, median, Q1, and Q3 for each reserve selector 
        #  for the variable of interest, i.e., the column name given.
    for (cur_meth_name in rs_method_names_list)
        {
            #  Pull out just the values for the current reserve selector.
        input_vec = all_values [meth_names == cur_meth_name]

        ct = length (input_vec)
#        frac = ct / num_prob_per_rs

        cur_idx = which (num_prob_per_rs$rs_method_name == cur_meth_name)
        #cat ("\nnum_prob_per_rs$num_prob [", cur_idx, "] = ", num_prob_per_rs$num_prob [cur_idx])
        frac = ct / num_prob_per_rs$num_prob [cur_idx]
                
        if (include_mad_in_fivenum_table_output)
            {
            mad_value = mad (input_vec)
            ct_vec    = c (ct = ct, frac = frac, mad = mad_value)
            
            } else 
            {
            ct_vec = c (ct = ct, frac = frac)
            }
  
            #  Turn the fivenum vector into a named vector 
            #  since later calls to bind_rows() require the vectors to 
            #  have column names.
        five_num_vec         = fivenum (input_vec)
        names (five_num_vec) = fivenum_col_names
        
        five_num_vec = unlist (c(ct_vec, five_num_vec))

            #  Add the fivenum results for the current reserve selector 
            #  to the set of fivenum results for all methods.
        fivenum_tib = bind_rows (fivenum_tib, five_num_vec)
        }

        #  Delete the dummy initialization line from the table since 
        #  it was only needed to create an initial object for bind_rows() 
        #  to act on.
        #  Then, label the rows with their reserve selector names.
        #  Note that we convert from a tibble to a data frame since 
        #  we need rownames to get kable() to properly label the 
        #  rows with reserve selector names and the tibble class 
        #  has deprecated the use of rownames and will eventually 
        #  prohibit them.
    fivenum_tib = as.data.frame (fivenum_tib [-1,])
    rownames(fivenum_tib) = rs_method_names_list
    
    return (fivenum_tib)
    }
    
#----------

create_rs_fivenum_table <- function (rs_method_names_list, 
                                     src_tib, 
                                     col_to_fivenum_str, 
                                     num_prob_per_rs, 
                                     table_caption = "This is a kable table.", 
                                     include_mad_in_fivenum_table_output = TRUE,
                                     meth_name_col_str = "rs_method_name", 
                                     digits_to_show = 2)
    {
    fivenum_tib = create_rs_fivenum_tib (rs_method_names_list, 
                                         src_tib, 
                                         col_to_fivenum_str, 
                                         num_prob_per_rs, 
                                         table_caption, 
                                         include_mad_in_fivenum_table_output, 
                                         meth_name_col_str)

#  FOR SOME REASON, CALLING kable FROM INSIDE THIS FUNCTION DOESN'T 
#  PRODUCE THE FORMATTED TABLE IN THE PDF FILE.  
#  INSTEAD, IT PRODUCES A CRUDELY FORMATTED VERSION OF IT, I.E., 
#  IT'S JUST ASCII TEXT WITH NO LATEX-LIKE NICETIES.
        #  Print out the table of fivenum outputs in a way that looks 
        #  good in pdf output.  
        #  Note that it looks crappy in the embedded Rmarkdown notebook 
        #  in RStudio but looks good in the pdf output.  
        #  Not sure how to fix that.  
    return (
            print (
            kable (fivenum_tib,
                   digits = digits_to_show,    #c(3, 3, 3, 4, 6),
                   padding = 10,
#  Caption was not printing until I changed the format option from markdown to 
#  pandoc (as suggested in Erik's answer 
#  in this stackoverflow question:
#  https://stackoverflow.com/questions/46686579/kable-not-printing-captions-anymore
#     "I had the same problem (table caption not printing when using the 
#      example above). Got it working by adding format = 'pandoc' to the 
#      kable() argument."
                   format = 'pandoc',    #'markdown', 
                   caption = table_caption)    #"This is an example of kable output.")
            )
        )

#    return (fivenum_tib)
#    return (NULL)
    }

#===============================================================================

    #  Create a tibble with five rows that contain statistics for a given 
    #  column of a given tibble computed over five different subsettings of 
    #  the tibble: 
    #   - All problems
    #   - FN problems only
    #   - FP problems only
    #   - gurobi_status == "OPTIMAL" problems only
    #   - gurobi_status == "TIME_LIMIT" problems only
    #  The statistics that are computed for each subset are determined by the 
    #  settings of the create_rs_fivenum_tib() function. 
    #  Returns: the tibble that is created.

create_five_row_tibble_of_All_FN_FP_OPTIMAL_TIME_LIMIT <- 
        function (cur_tib, colname_str, rs_method_names_list, num_prob_per_rs)
    {
    row_all = 
        create_rs_fivenum_tib (rs_method_names_list, 
                               cur_tib,
                               colname_str, 
                               num_prob_per_rs)
    
    row_FN = 
        create_rs_fivenum_tib (rs_method_names_list, 
                               filter (cur_tib, dom_err_type == "FN"),
                               colname_str, 
                               num_prob_per_rs)
    
    row_FP = 
        create_rs_fivenum_tib (rs_method_names_list, 
                               filter (cur_tib, dom_err_type == "FP"),
                               colname_str, 
                               num_prob_per_rs)
    
    row_OPTIMAL = 
        create_rs_fivenum_tib (rs_method_names_list, 
                               filter (cur_tib, 
                                       gurobi_status == "OPTIMAL"),
                               colname_str, 
                               num_prob_per_rs)
    
    row_TIME_LIMIT = 
        create_rs_fivenum_tib (rs_method_names_list, 
                               filter (Marxan_SA_only_tib, 
                                       gurobi_status == "TIME_LIMIT"),
                               colname_str, 
                               num_prob_per_rs)
    
    #----------
    
    five_rows_tib = bind_rows (row_all, row_FN, row_FP, row_OPTIMAL, row_TIME_LIMIT)
    rownames (five_rows_tib) = c("All", "FN", "FP", "OPTIMAL", "TIME_LIMIT")
    
    return (five_rows_tib)
    }

#-------------------------------------------------------------------------------

    #  Display a table with five rows that contain statistics for a given 
    #  column of a given tibble computed over five different subsettings of 
    #  the tibble: 
    #   - All problems
    #   - FN problems only
    #   - FP problems only
    #   - gurobi_status == "OPTIMAL" problems only
    #   - gurobi_status == "TIME_LIMIT" problems only
    #  The statistics that are computed for each subset are determined by the 
    #  settings of the create_rs_fivenum_tib() function. 
    #  Returns: the tibble that is created.

create_five_rows_kable_table <- function (tib_to_analyze, 
                                          colname_str, 
                                          rs_method_names_list, 
                                          num_prob_per_rs,
                                          table_caption = "NO TABLE CAPTION GIVEN", 
                                          kable_format_str = "pandoc", 
                                          digits_to_show = 2)
    {
    five_rows_tib = 
        create_five_row_tibble_of_All_FN_FP_OPTIMAL_TIME_LIMIT (tib_to_analyze, 
                                                                colname_str, 
                                                                rs_method_names_list, 
                                                                num_prob_per_rs)
    
    if (is.null (table_caption))
        {
        return (print (kable (five_rows_tib, 
                              align = 'rccccccccc',    #'rrrrrrrrrr'
                              digits = digits_to_show,    #c(3, 3, 3, 4, 6),
                              linesep = c("\\addlinespace", "", "\\addlinespace", ""), 
                              
                                  #  2022 01 13 - BTL
                                  #  If format is set to "latex", then the table looks 
                                  #  the way I want it to, i.e., it has a blank line 
                                  #  after the first and third lines to give a visual 
                                  #  grouping of All, FN/FP, and OPTIMAL/TIME_LIMIT.  
                                  #  However, the table floats when you do this and 
                                  #  there's no telling where it will end up in the 
                                  #  pdf.  It's often in a very confusing location.  
                                  #  If it's set to "pandoc", then it doesn't float 
                                  #  but it doesn't put the blank lines in.  
                                  #  
                                  #  At the moment, I think it's more confusing to 
                                  #  have the tables floating away from the sections 
                                  #  where they're supposed to occur (especially 
                                  #  in cases where there is just the table and no 
                                  #  text in the section.  So, I'm choosing pandoc.
                              format = kable_format_str,    #"pandoc",    #"latex", 
                              
                              booktabs = TRUE 
                            #, caption = table_caption
                             )
                      )
                )
        } else 
        {
        return (print (kable (five_rows_tib, 
                              align = 'rccccccccc',    #'rrrrrrrrrr'
                              digits = digits_to_show,    #c(3, 3, 3, 4, 6),
                              linesep = c("\\addlinespace", "", "\\addlinespace", ""), 
                              
                                  #  2022 01 13 - BTL
                                  #  If format is set to "latex", then the table looks 
                                  #  the way I want it to, i.e., it has a blank line 
                                  #  after the first and third lines to give a visual 
                                  #  grouping of All, FN/FP, and OPTIMAL/TIME_LIMIT.  
                                  #  However, the table floats when you do this and 
                                  #  there's no telling where it will end up in the 
                                  #  pdf.  It's often in a very confusing location.  
                                  #  If it's set to "pandoc", then it doesn't float 
                                  #  but it doesn't put the blank lines in.  
                                  #  
                                  #  At the moment, I think it's more confusing to 
                                  #  have the tables floating away from the sections 
                                  #  where they're supposed to occur (especially 
                                  #  in cases where there is just the table and no 
                                  #  text in the section.  So, I'm choosing pandoc.
                              format = kable_format_str,    #"pandoc",    #"latex", 
                              
                              booktabs = TRUE, 
                              caption = table_caption
                             )
                      )
                )
        }
    }

#===============================================================================

#  These 3 variations on print_..._dist_tables() replace the old 
#  hard-coded cat() statements that only printed the values for all of the 
#  magnifications aggregated across all reserve selectors.  
#  These functions now break out by reserve selector and print a table rather 
#  than just a line of text.  
#
#  There are 3 functions here because each one has to access a different mag 
#  value in the input data, i.e., total error magnification, absolute cost 
#  error magnification, and rep shortfall magnification.  This should really 
#  be a parameter, but I've had problems with passing the variable name as a 
#  parameter in other things, so I've just done the quick and dirty thing here 
#  and cloned the whole function using different variable names.  

#-------------------------------------------------------------------------------

print_err_mag_dist_tables <- 
    function (cur_tib, 
              num_probs_per_rs, 
              label_qualifier_str,  #  empty or "FN-dominated" or "FP-dominated"
                                                
          #  This occasionally changes. Most change is related to  
          #  showing how often error correction happens via the 
          #  mag < 1 test.
          
              show_lt_1 = FALSE, 
              
          #  These are rarely changed.  
          
              show_gt_1 = FALSE, 
              show_unbounded = TRUE, 
              show_near_1 = FALSE, 
              near_1_tol = 0.05, 
              show_eq_1 = FALSE)
    {
    if (show_unbounded)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 cur_tib, 
                                 "err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Err Magnification (All ", 
                                         label_qualifier_str, 
                                         "App Wrap)"
                                         )
                                 )
        }
    
    if (show_gt_1)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 filter (cur_tib, err_mag > 1), 
                                 "err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Err Magnification > 1 (All ", 
                                         label_qualifier_str, 
                                         "App Wrap)"
                                         )
                                 )
        }
    
    if (show_lt_1)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 filter (cur_tib, err_mag < 1), 
                                 "err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Err Magnification < 1 (All ", 
                                         label_qualifier_str, "App Wrap)")
                                 )
        }
    
    if (show_eq_1)
        {
        create_rs_fivenum_table (rs_method_names_list,
                                 filter (cur_tib, err_mag == 1),
                                 "err_mag",
                            num_probs_per_rs,
                                paste0 ("Err Magnification == 1 (All ", 
                                        label_qualifier_str, "App Wrap)")
                                 )
        }
    
    if (show_near_1)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 filter (cur_tib, 
                                         ((err_mag >= (1 - near_1_tol)) & 
                                            (err_mag <= (1 + near_1_tol)))), 
                                 "err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Err Magnification NEAR 1 (All ", 
                                         label_qualifier_str, "App Wrap)")
                                 )
        }
    }

#-------------------------------------------------------------------------------

print_abs_cost_err_mag_dist_tables <- 
    function (cur_tib, 
              num_probs_per_rs, 
              label_qualifier_str,  #  empty or "FN-dominated" or "FP-dominated"
                                                
          #  This occasionally changes. Most change is related to  
          #  showing how often error correction happens via the 
          #  mag < 1 test.
          
              show_lt_1 = FALSE, 
              
          #  These are rarely changed.  
          
              show_gt_1 = FALSE, 
              show_unbounded = TRUE, 
              show_near_1 = FALSE, 
              near_1_tol = 0.05, 
              show_eq_1 = FALSE)
    {
    if (show_unbounded)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 cur_tib, 
                                 "abs_cost_err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Abs Cost Err Magnification (All ", 
                                         label_qualifier_str, 
                                         "App Wrap)"
                                         )
                                 )
        }
    
    if (show_gt_1)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 filter (cur_tib, abs_cost_err_mag > 1), 
                                 "abs_cost_err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Abs Cost Err Magnification > 1 (All ", 
                                         label_qualifier_str, 
                                         "App Wrap)"
                                         )
                                 )
        }
    
    if (show_lt_1)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 filter (cur_tib, abs_cost_err_mag < 1), 
                                 "abs_cost_err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Abs Cost Err Magnification < 1 (All ", 
                                         label_qualifier_str, "App Wrap)")
                                 )
        }
    
    if (show_eq_1)
        {
        create_rs_fivenum_table (rs_method_names_list,
                                 filter (cur_tib, abs_cost_err_mag == 1),
                                 "abs_cost_err_mag",
                            num_probs_per_rs,
                                paste0 ("Abs Cost Err Magnification == 1 (All ", 
                                        label_qualifier_str, "App Wrap)")
                                 )
        }
    
    if (show_near_1)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 filter (cur_tib, 
                                         ((abs_cost_err_mag >= (1 - near_1_tol)) & 
                                            (abs_cost_err_mag <= (1 + near_1_tol)))), 
                                 "abs_cost_err_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Abs Cost Err Magnification NEAR 1 (All ", 
                                         label_qualifier_str, "App Wrap)")
                                 )
        }
    }

#-------------------------------------------------------------------------------

print_rep_shortfall_err_mag_dist_tables <- function (cur_tib, num_probs_per_rs, near_1_tol, 
                                                     label_str, show_near_1 = FALSE)
    {
    create_rs_fivenum_table (rs_method_names_list, 
                             cur_tib, 
                             "rep_shortfall_mag", 
                        num_probs_per_rs, 
                             paste0 ("Rep Shortfall Err Magnification (All ", 
                                     label_str, 
                                     "App Wrap)"
                                     )
                             )
    #                          Rep Shortfall Err Magnification (All FP-dom App Wrap)
    
    create_rs_fivenum_table (rs_method_names_list, 
                             filter (cur_tib, rep_shortfall_mag > 1), 
                             "rep_shortfall_mag", 
                        num_probs_per_rs, 
                             paste0 ("Rep Shortfall Err Magnification > 1 (All ", 
                                     label_str, 
                                     "App Wrap)"
                                     )
                             )
    #                         "Rep Shortfall Err Magnification > 1 (All App Wrap)")
    
    create_rs_fivenum_table (rs_method_names_list, 
                             filter (cur_tib, rep_shortfall_mag < 1), 
                             "rep_shortfall_mag", 
                        num_probs_per_rs, 
                             paste0 ("Rep Shortfall Err Magnification < 1 (All ", label_str, "App Wrap)")
                             )
    #                         "Rep Shortfall Err Magnification < 1 (All App Wrap)")
    
    # create_rs_fivenum_table (rs_method_names_list, 
    #                          filter (cur_tib, rep_shortfall_mag == 1), 
    #                          "rep_shortfall_mag", 
    #                     num_probs_per_rs, 
    #                         paste0 ("Rep Shortfall Err Magnification == 1 (All ", label_str, "App Wrap)")
    #                          )
    # #                          "Rep Shortfall Err Magnification == 1 (All App Wrap)")
    
    if (show_near_1)
        {
        create_rs_fivenum_table (rs_method_names_list, 
                                 filter (cur_tib, ((rep_shortfall_mag >= (1 - near_1_tol)) & (rep_shortfall_mag <= (1 + near_1_tol)))), 
                                 "rep_shortfall_mag", 
                            num_probs_per_rs, 
                                 paste0 ("Rep Shortfall Err Magnification NEAR 1 (All ", label_str, "App Wrap)")
                                 )
        #                         "Rep Shortfall Err Magnification NEAR 1 (All App Wrap)")
        }
    }

#===============================================================================

#  Functions to print things to console normally handled by cat() or print()

#===============================================================================

#  These 3 variations on calc_and_print_near_1_..._mags() replace the old 
#  hard-coded cat() statements that only printed the values for all of the 
#  magnifications aggregated across all reserve selectors.  
#  These functions still aggregate across all reserve selectors, but now 
#  it's done with a single function call instead of cloning all the cat() 
#  statements everywhere.
#  Since the value calculated for the "== 1" test always returns 0 (probably 
#  due to floating point calculation not exactly equalling 1), I've added a test 
#  for near 1, where you specify a tolerance in the Rmd file header.  
#  Currently, it's set there to call "near 1" anything within 0.05 on either 
#  side of 1, i.e., [0.95, 1.05].  
#
#  The values aggregated across all reserve selectors aren't that useful and 
#  will probably be dropped from the paper in the end.  However, the empty 
#  interval around the garbage in/garbage out magnification value of 1 might 
#  be of interest if calculated for each reserve selector instead of across 
#  all reserve selectors, so I'm leaving all this code here as a starting point 
#  for doing that later.
#
#  There are 3 functions here because each one has to access a different mag 
#  value column in the input data, i.e., total error magnification, absolute cost 
#  error magnification, and rep shortfall magnification.  This should really 
#  be a parameter, but I've had problems with passing the variable name as a 
#  parameter in other things, so I've just done the quick and dirty thing here 
#  and cloned the whole function using different variable names.  

#-------------------------------------------------------------------------------

calc_and_print_near_1_err_mags <- function (cur_tib, 
                                            label_str,  #  "FP-dom " OR "FN-dom " OR "" 
                                            near_1_tol)
    {
    cat ("============================================================================================\n\n")
    cat ("           ERR MAG FRACTIONS for data aggregated across all reserve selectors\n\n")
    cat ("This text box will not be in final document.  It's only here now so that\n",
         "you can capture the values printed here if you need them in the text.\n", 
         "However, they're probably not that useful since they're aggregated across all RS.\n",
         "If interesting, it probably wouldn't be hard to create the NEAR 1 intervals\n",
         "for each reserve selector here.\n\n")
  
    near_1_low = 1 - near_1_tol
    near_1_hi  = 1 + near_1_tol
    
    near_indices = 
        which ((cur_tib$err_mag >= near_1_low) & (cur_tib$err_mag <= near_1_hi))
    
    near_values = cur_tib$err_mag [near_indices]
    num_near = length (near_values)
    
    cur_tib_err_mag_length = length (cur_tib$err_mag)
    frac_near = num_near / cur_tib_err_mag_length
    
    cat ("fraction of ", label_str, 
         "app wrap problems with err mag > 1 (i.e., out err > in err): ", 
         round (length (which (cur_tib$err_mag > 1)) / cur_tib_err_mag_length, 2), 
         "\n", sep='')
    
    cat ("fraction of ", label_str, 
         "app wrap problems with err mag < 1 (i.e., out err < in err): ", 
         round (length (which (cur_tib$err_mag < 1)) / cur_tib_err_mag_length, 2), 
         "\n", sep='')

    cat ("fraction of ", label_str, 
         "app wrap problems with err mag == 1 (i.e., out err == in err): ", 
         round (length (which (cur_tib$err_mag == 1)) / cur_tib_err_mag_length, 2), 
         "\n\n", sep='')

    #-----
    
    cat ("fraction of ", label_str, 
         "app wrap problems with err mag NEAR 1 (+/-", 
         near_1_tol, "%): ", 
         round (frac_near, 6), 
         "\n", sep='')
    
    cat ("empty interval around 1 = [", 
         max (cur_tib$err_mag [which (cur_tib$err_mag < 1)]), ", ", 
         min (cur_tib$err_mag [which (cur_tib$err_mag > 1)]), "]", 
         "\n", sep='')

    cat ("\n============================================================================================\n\n")
    }

#-------------------------------------------------------------------------------

calc_and_print_near_1_abs_cost_err_mags <- 
    function (cur_tib, 
              label_str,  #  "FP-dom " OR "FN-dom " OR "" 
              near_1_tol)
    {
    cat ("================================================================================================\n\n")
    cat ("           ABS COST ERR MAG FRACTIONS for data aggregated across all reserve selectors\n\n")
    cat ("This text box will not be in final document.  It's only here now so that\n",
         "you can capture the values printed here if you need them in the text.\n", 
         "However, they're probably not that useful since they're aggregated across all RS.\n",
         "If interesting, it probably wouldn't be hard to create the NEAR 1 intervals\n",
         "for each reserve selector here.\n\n")
  
    near_1_low = 1 - near_1_tol
    near_1_hi  = 1 + near_1_tol
    
    near_indices = 
        which ((cur_tib$abs_cost_err_mag >= near_1_low) & 
                 (cur_tib$abs_cost_err_mag <= near_1_hi))
    
    near_values = cur_tib$abs_cost_err_mag [near_indices]
    num_near = length (near_values)
    
    cur_tib_abs_cost_err_mag_length = length (cur_tib$abs_cost_err_mag)
    frac_near = num_near / cur_tib_abs_cost_err_mag_length
    
    cat ("fraction of ", label_str, 
         "app wrap problems with abs cost err mag > 1 (i.e., out err > in err): ", 
         round (length (which (cur_tib$abs_cost_err_mag > 1)) / 
                cur_tib_abs_cost_err_mag_length, 2), 
         "\n", sep='')
    
    cat ("fraction of ", label_str, 
         "app wrap problems with abs cost err mag < 1 (i.e., out err < in err): ", 
         round (length (which (cur_tib$abs_cost_err_mag < 1)) / 
                cur_tib_abs_cost_err_mag_length, 2), 
         "\n", sep='')

    cat ("fraction of ", label_str, 
         "app wrap problems with abs cost err mag == 1 (i.e., out err == in err): ", 
         round (length (which (cur_tib$abs_cost_err_mag == 1)) / 
                cur_tib_abs_cost_err_mag_length, 2), 
         "\n\n", sep='')

    #-----
    
    cat ("fraction of ", label_str, 
         "app wrap problems with abs cost err mag NEAR 1 (+/-", 
         near_1_tol, "%): ", 
         round (frac_near, 6), 
         "\n", sep='')
    
    cat ("empty interval around 1 = [", 
         max (cur_tib$abs_cost_err_mag [which (cur_tib$abs_cost_err_mag < 1)]), ", ", 
         min (cur_tib$abs_cost_err_mag [which (cur_tib$abs_cost_err_mag > 1)]), "]", 
         "\n", sep='')

    cat ("\n====================================================================================================\n\n")
    }

#-------------------------------------------------------------------------------

calc_and_print_near_1_rep_shortfall_err_mags <- 
    function (cur_tib, 
              label_str,  #  "FP-dom " OR "FN-dom " OR "" 
              near_1_tol)
    {
    cat ("======================================================================================================\n\n")
    cat ("           REP SHORTFALL MAG FRACTIONS for data aggregated across all reserve selectors\n\n")
    cat ("This text box will not be in final document.  It's only here now so that\n",
         "you can capture the values printed here if you need them in the text.\n", 
         "However, they're probably not that useful since they're aggregated across all RS.\n",
         "If interesting, it probably wouldn't be hard to create the NEAR 1 intervals\n",
         "for each reserve selector here.\n\n")
  
    near_1_low = 1 - near_1_tol
    near_1_hi  = 1 + near_1_tol
    
    near_indices = 
        which ((cur_tib$rep_shortfall_mag >= near_1_low) & 
                 (cur_tib$rep_shortfall_mag <= near_1_hi))
    
    near_values = cur_tib$rep_shortfall_mag [near_indices]
    num_near = length (near_values)
    
    cur_tib_rep_shortfall_mag_length = length (cur_tib$rep_shortfall_mag)
    frac_near = num_near / cur_tib_rep_shortfall_mag_length
    
    cat ("fraction of ", label_str, 
         "app wrap problems with rep Shortfall err mag > 1 (i.e., out err > in err): ", 
         round (length (which (cur_tib$rep_shortfall_mag > 1)) / 
                cur_tib_rep_shortfall_mag_length, 2), 
         "\n", sep='')
    
    cat ("fraction of ", label_str, 
         "app wrap problems with rep Shortfall err mag < 1 (i.e., out err < in err): ", 
         round (length (which (cur_tib$rep_shortfall_mag < 1)) / 
                cur_tib_rep_shortfall_mag_length, 2), 
         "\n", sep='')

    cat ("fraction of ", label_str, 
         "app wrap problems with rep Shortfall err mag == 1 (i.e., out err == in err): ", 
         round (length (which (cur_tib$rep_shortfall_mag == 1)) / 
                cur_tib_rep_shortfall_mag_length, 2), 
         "\n\n", sep='')

    #-----
    
    cat ("fraction of ", label_str, 
         "app wrap problems with rep Shortfall err mag NEAR 1 (+/-", 
         near_1_tol, "%): ", 
         round (frac_near, 6), 
         "\n", sep='')
    
    cat ("empty interval around 1 = [", 
         max (cur_tib$rep_shortfall_mag [which (cur_tib$rep_shortfall_mag < 1)]), ", ", 
         min (cur_tib$rep_shortfall_mag [which (cur_tib$rep_shortfall_mag > 1)]), "]", 
         "\n", sep='')

    cat ("\n======================================================================================================\n\n")
    }

#===============================================================================

