You can ignore any file or directory that does not appear in this list and  
is not explicitly called out somewhere else.  (I have 
tried to include all relevant files in here, but it's possible that I've missed
one or two that are referenced elsewhere in my directions to you.)

In cases below where there are Rmd files and pdf files with the same main part 
of the name (e.g., x.Rmd and x.pdf), the pdf is just the rendered version of the
Rmd file and is not annotated separately here.

.
|-- _claude_project_guide/                                                      [GUIDE - Read this first]
|   |-- CLEANUP_GOALS.MD
|   `-- DIRECTORY_INVENTORY.md
|-- Data -> /Users/bill/...                                                     [SYMLINK - Ignore, data elsewhere]
|-- Excel_files/                                                                [ACTIVE - Excel files documenting supporting variables and literature]
|   |-- bdpg_var_defns__formerly_called__initial_app_wrap_tib.xlsx              [ACTIVE - definitions and information about variables in experiment output files]
|   |-- Paper2LitReviewOfComparisonPaperTraits.xlsx                             [ACTIVE, BUT NOT IMPORTANT - notes about some of the papers referenced in the manuscript]
|   `-- xu variable values.xlsx                                                 [ACTIVE - spreadsheet used to experiment with possible values to use for Xu model input parameters, shows relationship between variable values]
|-- Figures/                                                                    [ACTIVE - input figures used to help explain concepts in manuscript; all appear in the rendered pdfs of the manuscript so may not need to be read separately here but are provided here just in case that is easier for claude to use]
|   |-- 2022 01 31 Shiny app opening page zoomed so entire menu shows.png
|   |-- confusionMatrixDiagram_PPscreenGrab.png
|   |-- figure wrap from powerpoint v2 2022 07 03.png
|   `-- Figure_RB.png
|-- MetadataForDataFilesAndTibs/
|   `-- full_out_tree.txt                                                       [ACTIVE - example of full output directory structure for one full experiment]
|-- Notes/
|   `-- Notes-BootstrapPredInterval-And-Bmotif-Etc/
|       `-- bootstrapPredInterval.Rmd                                           [ACTIVE - notes about a possible way to derive prediction intervals using a bootstrap, not implemented yet]
|-- Paper_8_all_combined_for_Ecological_Monographs/
|   |-- p8_v18_prep_data_for_p8_to_load_from_files.Rmd                          [ACTIVE - I believe this is the code that was used to assemble the input files that are read into Paper 9, but I'm not 100% sure since assembling the files was done quite a while ago - one of the things I need to sort out in this cleanup]
|   |-- p8_v18_all_combined__body.Rmd                                           [SEMI-ACTIVE - a much longer, incomplete version of the paper that I'm working on now in the Paper 9 directory - good for seeing a broader picture of the project, but too long to submit for publication]
|   |-- bdpg_modelFitting_NOTES_nestedCallSequence_Doc.Rmd                      [ACTIVE - notes where I have tried to figure out what the calling sequence is for a particularly nested and confusing part of the code where predictive model fitting is done]
|   |-- Dormann-like correlations of bipartite measures.xlsx                    [SEMI-ACTIVE - notes where I tried to display correlations between different input features as part of feature selection, probably not completely accurate]
|   |-- matilda_notes.v01.Rmd                                                   [SEMI-ACTIVE - notes related to my attempting to use the matilda application for instance space analysis, not currently using matilda for various reasons but would like to in the future]
|   `-- notes_on_log10_problem_found_by_way_of_matilda__2022_12_29.Rmd          [ACTIVE - notes related to my inconsistent calculation of logs of some input features]
|-- Paper_9_heavily_abridged_version_of_p8/
|   |-- p9_v01_all_combined__body.Rmd                                           [ACTIVE - current version of the main manuscript that I'm developing for publication, i.e., the most important file of all for this project]
|   |-- p9_v01_all_combined__body.pdf                                           
|   |-- p9_v01_prep_data_for_p8_to_load_from_files.Rmd                          [ACTIVE - Very similar to p8_v18_prep_data_for_p8_to_load_from_files.Rmd if not identical]
|   |-- p9_v01_all_combined____APP__supplemental_information.Rmd                [SEMI-ACTIVE - supplemental information about APP problems containing input uncertainties and experiments that doesn't fit in manuscript but could be part of a submission]
|   |-- p9_v01_all_combined____APP__supplemental_information.pdf
|   |-- p9_v01_all_combined____COR__supplemental_information.Rmd                [SEMI-ACTIVE - supplemental information about COR problems containing NO input uncertainties and experiments that doesn't fit in manuscript but could be part of a submission]
|   |-- p9_v01_all_combined____COR__supplemental_information.pdf
|   |-- p9_v01_all_combined____Shiny_app__supplemental_information.Rmd          [SEMI-ACTIVE - supplemental information acting as a user manual for the Shiny app developed along with this manuscript to allow readers to experiment with different assumptions and output displays]
|   `-- Saved_plots/                                                            [IGNORE - transient outputs] 
|-- R/                                                                          [DEAD - Archived - will later be renamed to R_old]
|-- R_new/                                                                      [ACTIVE - Current R code - will later be renamed to R]
|-- Summary/                                                                    [SEMI-ACTIVE - At various times, I have provided different versions of a fairly short, summary of the manuscript to explain the basic ideas to people of interest.]
|   |-- bdpg_short_summary.possible_smith-miles_revision.docx                   [SEMI-ACTIVE - Most comprehensive summary of the 3.]
|   |-- bdpg_short_summary-learning_to_predict_output_error-for_Tom-2022_04_28.docx
|   |-- bdpg_short_summary.Rmd
|   |-- bdpg_short_summary.docx
|   `-- Figs                                                                    [SEMI-ACTIVE - Explanatory diagrams and output screen captures etc used to illustrate the summaries]
