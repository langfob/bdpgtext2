#===============================================================================
#
#         v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R
#
#===============================================================================

#  History

#  2020 08 18 - BTL - v3
#     - Lots of cruft in the v2 version, so I'm creating v3 to strip it down.
#       NOTE that if you're having issues with paper 1, it may be because 
#       it currently uses v2 of this file and that's been moved down to the 
#       older_R_versions directory.

#  2020 02 05 - BTL - v2
#     - Fixing options that didn't work, e.g., analyzing just FN-dominant or 
#       FP-dominant.
#       Marking removed things with "#####  2020 02 05  #####  "

#  2020 02 05 - BTL - v1
#     - Cloned from /Users/bill/D/Projects/ProblemDifficulty/RnotInPkgs/
#                           bdpgtext/Analysis_scripts/
#                           Paper_1-2_method_and_comparisons/
#                           bdpg_analysis_scripts_function_defns.paper_1-2.R

#  2019 11 21 - BTL
#     - Created by extracting existing functions from ...

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

library (knitr)    #  For kable().
library("tidylog", warn.conflicts = FALSE)    #  Load AFTER tidyverse packages

library(ggplot2)
library(scales)
library(ggthemes)

library (patchwork)    #  To make combinations of ggplots.
library (cowplot)    #  For background_grid() function (at least).

library(viridis)    #  For color scale.  Not sure if necessary in the end.

library(GGally)  #  For ggpairs() function.  Probably won't need this in the end.  

library (here)  

library (glue)  #  For function glue().

#===============================================================================

# source ("/Users/bill/D/Projects/ProblemDifficulty/RnotInPkgs/bdpgtext/Paper_2_comparison_of_reserve_selectors/v1_Paper_2_func_defns_to_load_data.R")
# source ("/Users/bill/D/Projects/ProblemDifficulty/RnotInPkgs/bdpgtext/Paper_2_comparison_of_reserve_selectors/v1_Paper_2_func_defns_for_plotting.R")
# source ("/Users/bill/D/Projects/ProblemDifficulty/RnotInPkgs/bdpgtext/Paper_2_comparison_of_reserve_selectors/v1_Paper_2_func_defns_printing_text_and_tables.R")

proj_dir = here()
source (file.path (proj_dir,"R/v2_Paper_2_func_defns_to_load_data.R"))

#source (file.path (proj_dir,"R/v1_Paper_2_func_defns_for_plotting.R"))
source (file.path (proj_dir,"R/v2_Paper_2_func_defns_for_plotting.R"))

source (file.path (proj_dir,"R/v1_Paper_2_func_defns_printing_text_and_tables.R"))

#===============================================================================

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
                                                  params)
    {
    write_a_tib_to_csv_file (tib_to_write, 
                             tib_name, 
                             params$data_out_loc, 
                             params$gurobi_problem_filter, 
                             params$exclude_imperfect_wraps, 
                             params$add_gen_time_to_csv_name 
                             )
    }

write_a_tib_to_csv_file <- function (tib_to_write, 
                                     tib_name, 
                                     data_out_loc, 
                                     gurobi_problem_filter, 
                                     exclude_imperfect_wraps, 
                                     add_gen_time_to_csv_name)
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
    
    csv_file_name_including_options = paste0 (file_name_including_options, ".csv")
    csv_full_outfile_path = file.path (data_out_loc, csv_file_name_including_options)
    cat ("Writing tib '", tib_name, "' to '", csv_full_outfile_path, "'\n", sep='')
    write.csv (tib_to_write, csv_full_outfile_path, 
               row.names=FALSE, quote=TRUE)
    
    rds_file_name_including_options = paste0 (file_name_including_options, ".rds")
    rds_full_outfile_path = file.path (data_out_loc, rds_file_name_including_options)
    cat ("Writing RDS '", tib_name, "' to '", rds_full_outfile_path, "'\n", sep='')
    saveRDS (tib_to_write, rds_full_outfile_path)
    }

#===============================================================================
#  Not sure if these are used/useful anymore, but may be, so leaving them in 
#  for now (2020 02 06 - BTL).
#===============================================================================

###  Function to write output data

write_output_data <- function (df, rs_name, base_path, suffix)
    {
    outfile = paste0 (base_path, rs_name, suffix)
    cat ("\n\nIn write_output_data(), outfile = '", outfile, 
         "'\n", sep='')
    write.csv (df, outfile, row.names=FALSE)
    }

#===============================================================================

save_this_ggplot <- function (a_ggplot, cur_plot_name, extension = "pdf")
    {
    proj_dir = here()
    cat ("\n\nproj_dir = here() = ", proj_dir, "\n", sep='')
    cur_plot_name_and_path = 
        file.path (proj_dir, 
                   "Analysis_scripts/Paper_3_learning_to_predict_error/Saved_plots", 
                   cur_plot_name) 
    cur_plot_name_and_path = paste0 (cur_plot_name_and_path, ".", extension)
    cat ("\ncur_plot_name_and_path = '", cur_plot_name_and_path, "'\n")
    
    ggsave (a_ggplot, file = cur_plot_name_and_path)
    }

#===============================================================================

select_hard_probs_tib <- function (in_tib, 
                                   shortfall_lower_bound = 0.5, 
                                   abs_cost_err_lower_bound = 0)
  {
  in_tib %>% 
      select (rsp_UUID, 
              rs_method_name, 
              rsr_COR_spp_rep_shortfall,
              abs_rs_solution_cost_err_frac) %>% 
      filter (rs_method_name == "Gurobi" & 
                (rsr_COR_spp_rep_shortfall >= shortfall_lower_bound &    #| OR give ~25% of all problems while AND gives ~5%
                abs_rs_solution_cost_err_frac >= abs_cost_err_lower_bound)) %>% 
      select (rsp_UUID) -> hard_gurobi_uuids

  hard_gurobi_uuids_vec = hard_gurobi_uuids$rsp_UUID
  
  in_tib %>% 
      filter (rsp_UUID %in% hard_gurobi_uuids_vec) -> hard_probs_tib
  
  return (hard_probs_tib)
  }

#===============================================================================


