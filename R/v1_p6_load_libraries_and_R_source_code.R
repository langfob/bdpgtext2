#===============================================================================
#
#                   v1_p6_load_libraries_and_R_source_code.R
#
#===============================================================================

#  History

#  2022 01 24 - BTL - v1
#     - Cloned from v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R.
#     - Stripping down to just the loading of R libraries and source code.

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

proj_dir = here()

# source (file.path (proj_dir,"/Paper_2_comparison_of_reserve_selectors/v1_Paper_2_func_defns_to_load_data.R"))
# source (file.path (proj_dir,"/Paper_2_comparison_of_reserve_selectors/v1_Paper_2_func_defns_for_plotting.R"))
# source (file.path (proj_dir,"/Paper_2_comparison_of_reserve_selectors/v1_Paper_2_func_defns_printing_text_and_tables.R"))

#source (file.path (proj_dir,"R/v2_Paper_2_func_defns_to_load_data.R"))

#source (file.path (proj_dir,"R/v2_Paper_2_func_defns_for_plotting.R"))
# source (file.path (proj_dir,"R_new/v2_Paper_2_func_defns_for_plotting.R"))
source (file.path (proj_dir,"R/v2_Paper_2_func_defns_for_plotting.R"))

#source (file.path (proj_dir,"R/v1_Paper_2_func_defns_printing_text_and_tables.R"))
# source (file.path (proj_dir,"R_new/v1_Paper_2_func_defns_printing_text_and_tables.R"))
source (file.path (proj_dir,"R/v1_Paper_2_func_defns_printing_text_and_tables.R"))

#source (file.path (proj_dir, "R/v2_p6_unifiedDataLoading.R"))    # "R/v06_Paper_6.R"))
# source (file.path (proj_dir, "R_new/v2_p6_unifiedDataLoading.R"))    # "R/v06_Paper_6.R"))
source (file.path (proj_dir, "R/v2_p6_unifiedDataLoading.R"))    # "R/v06_Paper_6.R"))

#===============================================================================


