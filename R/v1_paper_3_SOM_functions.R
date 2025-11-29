#===============================================================================
#
#            v1_paper_3_SOM_functions.R
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
#===============================================================================

##  SOM-related code

#  Copied from and/or based on code in:
#      https://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/#comments

#===============================================================================

# library (kohonen)
# 
# create_SOM <- function (data_train)
#     {
#   
#         # Change the data frame with training data to a matrix
#   
#     data_train_matrix <- as.matrix (data_train)
#     
#         # Create the SOM Grid - you generally have to specify the size of the 
#         # training grid prior to training the SOM. Hexagonal and Circular 
#         # topologies are possible
#     
#     som_grid <- somgrid (xdim = 20, ydim=20, topo="hexagonal")
#     
#       # Finally, train the SOM, options for the number of iterations,
#       # the learning rates, and the neighbourhood are available
#     
#     som_model <- som (data_train_matrix, 
#                       grid = som_grid, 
#                       rlen = 500, 
#                       alpha = c(0.05,0.01), 
#                       keep.data = TRUE )
#     
#         #  Training progress for SOM
#     plot (som_model, type = "changes")
#     
#         #  Node count plot
#     plot (som_model, type = "count", main = "Node Counts")
#     
#         # U-matrix visualisation
#     plot (som_model, type = "dist.neighbours", main = "SOM neighbour distances")
#     
#         # Weight Vector View
#     plot (som_model, type = "codes")
#     
#         # Kohonen Heatmap creation
#     plot (som_model, 
#           type = "property", 
#           property = getCodes (som_model) [,4], 
#           main = colnames (getCodes (som_model)) [4], 
#           palette.name = coolBlueHotRed)
#     
#         #  Unscaled Heatmaps
#         #  define the variable to plot 
#     var_unscaled <- aggregate (as.numeric (data_train [,var]), 
#                                by = list (som_model$unit.classif), 
#                                FUN = mean, 
#                                simplify = TRUE) [,2] 
#     plot (som_model, 
#           type = "property", 
#           property = var_unscaled, 
#           main = colnames (getCodes (som_model)) [var], 
#           palette.name = coolBlueHotRed)
#     }
    
#===============================================================================

run_SOM_resids <- function (som_model, 
                            full_true_vs_pred_df, 
                            rs_names_vec, 
                            num_som_cells)
  {
  if (SOM_cost_err_frac)
    {
    run_SOM_cost_err_frac_resids (som_model, 
                                  full_true_vs_pred_df, 
                                  rs_names_vec, 
                                  num_som_cells)
    } else 
    {
    run_SOM_rep_shortfall_resids (som_model, 
                                  full_true_vs_pred_df, 
                                  rs_names_vec, 
                                  num_som_cells)
    }
  }

#===============================================================================

