#===============================================================================
#
#                   v2_Paper_2_func_defns_for_plotting.R
#
#===============================================================================

#  History

#  2022 01 26 - BTL - v2
#     - Changing max_FN_FP to max_TOT_FN_FP everywhere.

#  2020 08 19 - BTL - v1
#     - Extracted from v3_Paper_2_bdpg_analysis_scripts_function_defns.paper_2.R 
#       as part of simplifying the paper 2 functions.

#===============================================================================

#  Plotting functions

#===============================================================================

#  Force the coloring of points by dominant error type to always use the 
#  same color for FN and the same color for FP.  
#  If you don't do this forcing and you do a plot that is only FN_dominant or 
#  FP_dominant, then the points will always just get the first color in the 
#  ordering and so sometimes FN will be one color and other times the other 
#  color.  Here, we check to see if there is just one type of error appearing 
#  in the plot and if so, force it to be consistent in coloring FNs and FPs 
#  in different plots.

force_dom_err_type_colors <- function (dom_err_type,      #sorted_msa_tib, 
                                       gg_verbose = FALSE)
    {
        #  Choose the colors that will be used for FN and FP when manual 
        #  coloring is done in calls to ggplot.
  
    scale_color_breaks = c("FN", "FP")
    scale_color_values = c("blue", "red")
    
        #  Check to see whether there's only one type of point in the current 
        #  plot and if so, choose which color is to be used according to 
        #  whether the points are FN_dominant or FP_dominant.
    
    unique_values = unique (dom_err_type)    #sorted_msa_tib$dom_err_type)
    num_unique_values = length (unique_values)
    
    if (gg_verbose) cat ("\nIn force_dom_err_type_colors(), num_unique_values = ", 
                         num_unique_values, "\n")
    if (num_unique_values == 1)
        {
        if (unique_values == "FN")    #dom_err_type [1] == "FN")
            {
            scale_color_breaks = c("FN")
            scale_color_values = c("blue")
            
            } else if (unique_values == "FP")    #dom_err_type [1] == "FP")
            {
            scale_color_breaks = c("FP")
            scale_color_values = c("red")
            
            } else
            {
            err_string = paste0 ("\nBad break value in force_dom_err_type_colors() = '", 
                                 unique_values, "'.\n")
            stop (err_string)
            }
        }
        
    return (list (breaks = scale_color_breaks, 
                  values = scale_color_values))
    }

#===============================================================================

###  Function to plot facetted data with error magnification rays

#  Set up fairly generic call to ggplot that can replace most of the very 
#  specific calls in many of the functions scattered throughout this file.

ggplot_faceted_with_mag_rays <- function (rs_method_names_list, 
                                          plot_title_str, 
                                          sorted_msa_tib,
                                          x_var, 
                                          y_var, 
                                          color_var, 
                                          facet_var, 
                                          
                                    y_min = 0, 
                                    y_max = 1.25, 
                                    
                  x_min = 0, 
                  x_max = 0.1,
                                          
                  x_axis_label = NULL, 
                  y_axis_label = NULL,

                  show_x_as_percent = TRUE, 
                  show_y_as_percent = TRUE, 
                  
                                          plot_rays = TRUE, 
                                          plot_rep_shortfall_err_bound = FALSE, 
                                          plot_neg_rays_too = FALSE, 
                  
                                          
                                          ref_y = 0, 
                                          shape_type = ".", 
                                          alpha_level = 0.1, 
                                          
                                          gg_verbose = FALSE
                                          )
    {
    x_var     = ensym (x_var)        #enquo (x_var)
    y_var     = ensym (y_var)        #enquo (y_var)
    color_var = ensym (color_var)    #enquo (color_var)                #  rsp_combined_err_label
    
    facet_formula = as.formula (paste0 ("~ ", facet_var))
    num_facets = length (unique (sorted_msa_tib [[facet_var]]))
    
        #  If coloring points by dominant error type, make sure that the 
        #  same color is always used for each type across all graphs.  
        #  If you don't do this and only one error type is appearing in the 
        #  current graph, it will take on whatever color is first in the 
        #  default list of colors.  This makes it harder to see at a glance 
        #  what kinds of points are being plotted in a set of graphs.  
        #  In particular, FNs show up as 2 different colors while FPs 
        #  are always the same color if you don't do this fix.
    
    if (color_var == "dom_err_type")
        {
        if (gg_verbose) cat ("\ncolor_var IS dom_err_type\n")
        force_colors = TRUE
        
        color_breaks_and_values = force_dom_err_type_colors (sorted_msa_tib$dom_err_type)
        
        scale_color_breaks = color_breaks_and_values$breaks
        scale_color_values = color_breaks_and_values$values
        
        } else
        {
        if (gg_verbose) cat ("\ncolor_var is NOT dom_err_type\n")
        force_colors = FALSE
        }
    
        #  Now have the colors sorted out, so draw the plot.
    
    base_plot = 
        ggplot (data = sorted_msa_tib) +
        
        geom_point (mapping = aes (x = !!x_var,    #rsp_euc_realized_Ftot_and_cost_in_err_frac,
                                   y = !!y_var,    #rsr_COR_euc_out_err_frac,
                                   color = !!color_var),    #id
                                   shape = 15,    #".", 
                                   size=0.5, 
                                   alpha = alpha_level    #1
                    ) +
        
            #  If forcing colors, then manually scale the colors.  
            #  Otherwise, use the default behavior.
            #  To use an "if" statement inside these sets of statements 
            #  separated by "+" signs, you have to put curly brackets around 
            #  your test and result.
      
        { 
          if (color_var == "dom_err_type") 
          {
          if (force_colors) scale_color_manual (breaks = scale_color_breaks, 
                                                values = scale_color_values, 
                                                name = "dominant\nerror type") else if 
                      ((color_var != "gurobi_status") & (color_var != "id") & (color_var != "rsp_base_wrap_str"))
                                                  #scale_color_viridis(option = "D")
                                                  #scale_colour_gradient(low = "darkolivegreen1", high = "darkolivegreen"))
                                                  scale_colour_gradient(low = "red", high = "blue")
          } else 
          {
          if (force_colors) scale_color_manual (breaks = scale_color_breaks, 
                                                values = scale_color_values) else if 
                      ((color_var != "gurobi_status") & (color_var != "id") & (color_var != "rsp_base_wrap_str"))
                                                  #scale_color_viridis(option = "D")
                                                  #scale_colour_gradient(low = "darkolivegreen1", high = "darkolivegreen"))
                                                  scale_colour_gradient(low = "red", high = "blue")
          }
        } +
      
{ if (show_x_as_percent) scale_x_continuous (labels = percent) } + 
{ if (show_y_as_percent) scale_y_continuous (labels = percent, 
                                             minor_breaks = seq(y_min , y_max, 0.25), 
                                             breaks = seq(y_min , y_max, 0.5)) } + 

    #     #  Set background grid to only have horizontal lines with 
    #     #  wider grey lines at major ticks than minor ticks.
    # background_grid (major = c("y"), 
    #                  minor = c("y"), size.major = 0.5, 
    #                  size.minor = 0.4, 
    #                  color.major = "grey85", color.minor = "grey85") +
    

        ggtitle (plot_title_str) +
            
        theme (plot.title = element_text (hjust = 0.5)) +    #  To center the title

#  theme (strip.background = element_rect (fill="grey95")) +
  theme (strip.text = element_text (face="bold")) +    #  Specify header bar for each facet to show reserve selector name
      
#ylim (y_min, y_max) + 
# coord_cartesian (xlim = c(x_min, x_max)) +     #, xlim = c(0, 50))
# coord_cartesian (ylim = c(y_min, y_max)) +     #, xlim = c(0, 50))
coord_cartesian (xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +     #, xlim = c(0, 50))
      
      
        #theme (legend.position = "bottom") + 
        { if (num_facets == 5) theme (legend.position = c(0.85, 0.25),
                                      legend.direction = "vertical") } + 
      
        facet_wrap (facet_formula, nrow = 2) +    #2) +               #5) +
      
            #  Symbols on the plot itself are small and semi-transparent, 
            #  but in the legend that makes them nearly invisible.
            #  Override the symbol size and alpha values in the legend to 
            #  make the symbols much larger and not transparent at all.  
            #  Note that the fill=NA is trying to get rid of the grey background 
            #  around the symbols in the legend, but doesn't seem to be working.
            #  Leaving it in for now to remind me to try to find some other 
            #  way to get this to work.
      
        guides (color = 
                  guide_legend (override.aes = list (size = 4, 
                                                     alpha = 1, 
                                                     fill = NA)), 
                  fill=guide_legend
                ) + 
      
        geom_hline (yintercept = ref_y, linetype="dashed", color = "black", size=0.5)
      
    #  If both axis labels are specified, use them.
    #  If either or both are missing, then just use the variable name.
if (!is.null (x_axis_label) & !is.null (y_axis_label))
    {
    base_plot = 
        base_plot + 
            
        labs (y=y_axis_label, x=x_axis_label)
    }

        #  Plots of raw values can often use a set of magnification rays to 
        #  show where there is no magnification, 5x magnification, and 10x 
        #  magnification (but not all plots need this, so it's an option).
    
    if (plot_rays)
        {
        base_plot = 
          base_plot + 
            
        geom_abline (intercept=0, slope=1, size=0.2)  + #, color="green") +  #, linetype, color, size
        geom_abline (intercept=0, slope=5, size=0.2)  + #, color="orange") +  #, linetype, color, size
        geom_abline (intercept=0, slope=10, size=0.2)   #, color="red")  #, linetype, color, size

num_values = length (rs_method_names_list)
values_10x = data.frame (rs_method_name = rs_method_names_list, 
                      label_str = rep ("10x", num_values), 
                      x_loc = rep (0.095, num_values), 
                      y_loc = rep (1.1, num_values)
                      )
values_5x = data.frame (rs_method_name = rs_method_names_list, 
                      label_str = rep ("5x", num_values), 
                      x_loc = rep (0.097, num_values), 
                      y_loc = rep (0.6, num_values)
                      )

values_1x = data.frame (rs_method_name = rs_method_names_list, 
                      label_str = rep ("1x", num_values), 
                      x_loc = rep (0.097, num_values), 
                      y_loc = rep (0.2, num_values)
                      )

base_plot = 
    base_plot + 
        geom_text (data = values_10x,
               aes(x = x_loc, y = y_loc,
                   label = label_str), 
                          family="Times", fontface="italic", #lineheight=.03, 
               size=2, hjust = 0) + 
    
        geom_text (data = values_5x,
               aes(x = x_loc, y = y_loc,
                   label = label_str), 
                          family="Times", fontface="italic", #lineheight=.03, 
               size=2, hjust = 0) +
    
        geom_text (data = values_1x,
               aes(x = x_loc, y = y_loc,
                   label = label_str), 
                          family="Times", fontface="italic", #lineheight=.03, 
               size=2, hjust = 0)

        
          if (plot_neg_rays_too)
            {
            base_plot = 
              base_plot + 
                
            geom_abline (intercept=0, slope=-1, size=0.2)  + #, color="green") +  #, linetype, color, size
            geom_abline (intercept=0, slope=-5, size=0.2)  + #, color="orange") +  #, linetype, color, size
            geom_abline (intercept=0, slope=-10, size=0.2)  #, color="red")  #, linetype, color, size
          }

        } else
        {
            #  Representation shortfall error is bounded above by 100% error, 
            #  so its largest possible error magnifcation is also bounded 
            #  above for all input error > 0 by 1/inputError.  
            #  It's often useful to draw that 1/x upper bound on the plot 
            #  to see how close the error magnifications come to that bound 
            #  (but not all plots need this, so it's an option).
    
        if (plot_rep_shortfall_err_bound)
            {
                #    https://stackoverflow.com/questions/7705345/how-can-i-extract-plot-axes-ranges-for-a-ggplot2-object
                #    November 2018 UPDATE
                #    As of ggplot2 version 3.1.0, the following works:
            y_max = ggplot_build(base_plot)$layout$panel_params[[1]]$y.range[2]
            if (gg_verbose) cat ("\ny max is: ", y_max, "\n")
        
            base_plot = 
              base_plot + 
              
                ylim (0, y_max) + 
                #coord_cartesian (ylim = c(0, y_max)) +   Seems like this should work better than ylim() but it makes a mess of the y axis labels
                #
                stat_function (fun = function (x) 1/x)
               
            } 
        }
    
    return (base_plot)
    }

#===============================================================================

#  This function plots 10 boxplots for each reserve selector specified.  
#  Each boxplot shows the output error for each 1% level of input 
#  error.  
#  The function is actually more general than that, but that's what I'm using 
#  it for and is easier to state than its more general purpose.  
#  It does these plots in a fairly clean-looking way if do_USGS_version is FALSE 
#  and in a USGS style if that flag is TRUE.  
#  Each style has its advantages, but I don't want to mess with trying to 
#  build a better combined style right now.  For the moment, I'll default to 
#  the non-USGS style.
#-------------------------------------------------------------------------------

ggplot_boxplot_over_bins <- function (data, 
                                      x_grouping_var,    # = input_error_group_by_max_TOT_FN_FP, 
                                      y_output_var,    # = rsr_COR_euc_out_err_frac, 
                                      facet_var,    # = rs_method_name, 
                                      y_min = 0, 
                                      y_max = 1.25, 
                                      plot_title = "Total output error in groups of 1% input error\nFP-dominated", 
                                      x_axis_label = NULL,    #"Input Error Group", 
                                      y_axis_label = NULL,    #"Output Error", 
                                      ncol_in_facet_wrap = 2, 
                                      rs_method_names_to_show = c("Marxan_SA", "Marxan_SA_SS"), 

                                      show_y_as_percent = TRUE, 

                                      plotting_mags = TRUE, 
                                      draw_mag_rays = TRUE, 
                                      draw_x_mag_labels_on_ray_ends = TRUE, 
                                      draw_symbols_on_stems = TRUE, 
                                      draw_neg_mag_rays = FALSE, 
                                      gigo_symbol = 10,    #8, 
                                      gigo_symbol_color = "red", 
                                      gigo_symbol_size = 1.0    #1.2
                                      )


    {
        #  Set up line slopes, intercepts, and boxplot stem crossing points for 
        #  reference line rays and symbols to lay over the box plots.  
        #  Also provide ?x labels to put at the end of each ray.
        #  If plotting magnifications, the reference line is always horizontal 
        #  at y = 1.
        #  Otherwise, it's at whatever value the bin corresponds to, e.g., 
        #  bin 5 is for input error 0.05 so the line and the symbol hit the 
        #  boxplot stem at 0.05.
    
    if (plotting_mags)
        {
        ref_line_slope_1 = 0
        ref_line_intercept_1 = 1
        ref_line_slope_5 = 0
        ref_line_intercept_5 = 5
        ref_line_slope_10 = 0
        ref_line_intercept_10 = 10
        
        crossings = rep(1,10)
        neg_crossings = rep(-1,10)
        
        ref_line_labels_df = tibble (label_x    = c(11, 11, 11), #c(Inf,Inf, Inf),
                                     label_y    = c(1.75, 5.75, 10.75), 
                                     label_text = c("1x", "5x", "10x"))
        neg_ref_line_labels_df = tibble (label_x    = c(11, 11, 11), #c(Inf,Inf, Inf),
                                         label_y    = c(-1.75, -5.75, -10.75), 
                                         label_text = c("1x", "5x", "10x"))
        } else
        {
        ref_line_slope_1 = 0.01
        ref_line_intercept_1 = 0
        ref_line_slope_5 = 0.05
        ref_line_intercept_5 = 0
        ref_line_slope_10 = 0.1
        ref_line_intercept_10 = 0
        
        crossings = seq(0.01, 0.10, 0.01)
        neg_crossings = seq(-0.01, -0.10, -0.01)
        
        ref_line_labels_df = tibble (label_x    = c(11, 11, 11), #c(Inf,Inf, Inf),
                                     label_y    = c(0.16, 0.6, 1.15), 
                                     label_text = c("1x", "5x", "10x"))
        neg_ref_line_labels_df = tibble (label_x    = c(11, 11, 11), #c(Inf,Inf, Inf),
                                         label_y    = c(-0.16, -0.6, -1.15), 
                                         label_text = c("1x", "5x", "10x"))
        }
    
        #  Most of the time, I only want to show Marxan_SA and Marxan_SA_SS 
        #  even though there are more selectors in the dataset.  
        #  The other selectors generally look very similar to one of these 
        #  two, so I can get better display resolution and a simpler visual 
        #  comparison by only showing two.
        #  However, if showing more than two becomes desirable for some reason, 
        #  you only have to change the list of method names to show.
###    data = filter (data, {{facet_var}} %in% rs_method_names_to_show)
    data = filter (data, .data[[facet_var]] %in% rs_method_names_to_show)
    
    ggplot (data, 
            ### aes (x    = {{x_grouping_var}},    # input_error_group_by_max_TOT_FN_FP, 
            ###      y    = {{y_output_var}},    # rsr_COR_euc_out_err_frac, 
            ###      fill = {{x_grouping_var}}    # input_error_group_by_max_TOT_FN_FP
            aes (x    = .data[[x_grouping_var]],    #{{x_grouping_var}},    # input_error_group_by_max_TOT_FN_FP, 
                 y    = .data[[y_output_var]],    #{{y_output_var}},    # rsr_COR_euc_out_err_frac, 
                 fill = .data[[x_grouping_var]]    #{{x_grouping_var}}    # input_error_group_by_max_TOT_FN_FP
                 
  #             , show.legend = FALSE
                )
           )  +     
    
          #  geom_tufteboxplot() + 
          #  theme (axis.text.x = element_text (angle=65, vjust=0.6)) + 
    
    geom_boxplot(outlier.size = 0.1, notch=TRUE, #outlier.color="blue"
                 ) +        
###    facet_wrap (vars ({{facet_var}}), ncol = ncol_in_facet_wrap) +    #  facet_wrap (~rs_method_name, ncol = 2) + 
    facet_wrap (vars (.data[[facet_var]]), ncol = ncol_in_facet_wrap) +    #  facet_wrap (~rs_method_name, ncol = 2) + 
    
        #  Don't show a legend because the colors don't really mean anything 
        #  in this plot and displaying a legend means less space for the data  
        #  part of the plots.
    theme (legend.position = "none") +
    
    ggtitle (plot_title) +
    theme (plot.title = element_text (hjust = 0.5)) +    #  To center the title
    
#    ylim (y_min, y_max) + 
coord_cartesian (ylim = c(y_min, y_max)) +     #, xlim = c(0, 50))
      
# { if (show_y_as_percent) scale_y_continuous (labels = percent) } + 
{ if (show_y_as_percent) scale_y_continuous (labels = percent, 
                                             minor_breaks = seq(y_min , y_max, 0.25), 
                                             breaks = seq(y_min , y_max, 0.5)) } + 

    labs (x = x_axis_label,        # "Input Error Group",
          y = y_axis_label) +      # "Output Error") + 
             
        #  "theme (strip...)" options control the display of facet labels.  
        #  Put a box around the facet labels showing the reserve selector names 
        #  so that they stand out more.  
        #  Also, make the names be bold for now, though mostly just so that I 
        #  remember how to do that.
    theme (strip.background = element_rect (fill = "grey"))+
    theme (strip.text = element_text (face="bold")) +
            
        #  Set background grid to only have horizontal lines with 
        #  wider grey lines at major ticks than minor ticks.
    background_grid (major = c("y"), 
                     minor = c("y"), size.major = 0.5, 
                     size.minor = 0.3, 
                     color.major = "grey85", color.minor = "grey85") +
    
#----------
    
        #  Draw reference line rays across set of boxes.
    {if (draw_mag_rays){
    geom_abline (intercept = ref_line_intercept_1,
                 slope = ref_line_slope_1,
                 size = 0.2,
                 linetype = "dashed")}} + #, color, size
    {if (draw_mag_rays){
    geom_abline (intercept = ref_line_intercept_5,
                 slope = ref_line_slope_5,
                 size = 0.2,
                 linetype = "dashed")}} + #, color, size
    {if (draw_mag_rays){
    geom_abline (intercept = ref_line_intercept_10,
                 slope = ref_line_slope_10,
                 size = 0.2,
                 linetype = "dashed")}}  + #, color, size
    
#----------
    
        #  Draw NEGATIVE reference line rays across set of boxes.
    {if (draw_neg_mag_rays){
    geom_abline (intercept = -ref_line_intercept_1,
                 slope = -ref_line_slope_1,
                 size = 0.2,
                 linetype = "dashed")}} + #, color, size
    {if (draw_neg_mag_rays){
    geom_abline (intercept = -ref_line_intercept_5,
                 slope = -ref_line_slope_5,
                 size = 0.2,
                 linetype = "dashed")}} + #, color, size
    {if (draw_neg_mag_rays){
    geom_abline (intercept = -ref_line_intercept_10,
                 slope = -ref_line_slope_10,
                 size = 0.2,
                 linetype = "dashed")}}  + #, color, size
    
#----------
    
    {if (draw_x_mag_labels_on_ray_ends){
            #  Draw the ?x labels at the end of each ray.
        geom_text (data = ref_line_labels_df, 
                   aes(x = label_x, y = label_y, label = label_text), 
                   vjust = "top", 
                   hjust = "right", 
                   inherit.aes = FALSE
                   )}} + 
    
    {if (draw_x_mag_labels_on_ray_ends & draw_neg_mag_rays){
            #  Draw the ?x labels at the end of each ray.
        geom_text (data = neg_ref_line_labels_df, 
                   aes(x = label_x, y = label_y, label = label_text), 
                   vjust = "top", 
                   hjust = "right", 
                   inherit.aes = FALSE
                   )}} + 
#----------
    
        #  Draw a symbol on each boxplot stem at the value corresponding 
        #  to input error equals output error.  
        #  If plotting magnifications, this is always 1.
        #  Otherwise, it's at whatever value the bin corresponds to, e.g., 
        #  bin 5 is for input error 0.05 so plot the symbol at 0.05.
        #  Note that all of these geom_point() calls need the "inherit.aes=FALSE" 
        #  argument to avoid getting the error:
        #      object 'input_error_group_by_max_TOT_FN_FP' not found
      
        #  NOTE that you have to put this "if" statement around each of these 
        #  geom_point() calls separately even though they should all be included 
        #  in the same "if".  It looks like this is because each one is followed 
        #  by the "+" and apparently you can't have a "+" operator inside the 
        #  bracketed region.  
        #  If you do try to put them all inside a single wrapping bracket, 
        #  you get the following error message:
        #      Error: Cannot add ggproto objects together. Did you forget to add this object to a ggplot object?
      
    {if (draw_symbols_on_stems){
    geom_point (data = data.frame (x = factor(1:10), y = crossings),
                aes(x=x, y=y),
                shape = gigo_symbol, 
                color = gigo_symbol_color, 
                size  = gigo_symbol_size, 
                inherit.aes = FALSE)}} +
    
    {if (draw_symbols_on_stems){
    geom_point (data = data.frame (x = factor(1:10), y = 5*crossings),
                aes(x=x, y=y),
                shape = gigo_symbol,
                color = gigo_symbol_color,
                size  = gigo_symbol_size, 
                inherit.aes = FALSE)}} +
      
    {if (draw_symbols_on_stems){
    geom_point (data = data.frame (x = factor(1:10), y = 10*crossings),
                aes(x=x, y=y),
                shape = gigo_symbol,
                color = gigo_symbol_color,
                size  = gigo_symbol_size, 
                inherit.aes = FALSE)}} + 
    
#-----
    
      {if (draw_symbols_on_stems & draw_neg_mag_rays){
    geom_point (data = data.frame (x = factor(1:10), y = -crossings),
                aes(x=x, y=y),
                shape = gigo_symbol, 
                color = gigo_symbol_color, 
                size  = gigo_symbol_size, 
                inherit.aes = FALSE)}} +
    
    {if (draw_symbols_on_stems & draw_neg_mag_rays){
    geom_point (data = data.frame (x = factor(1:10), y = -5*crossings),
                aes(x=x, y=y),
                shape = gigo_symbol,
                color = gigo_symbol_color,
                size  = gigo_symbol_size, 
                inherit.aes = FALSE)}} +
      
    {if (draw_symbols_on_stems & draw_neg_mag_rays){
    geom_point (data = data.frame (x = factor(1:10), y = -10*crossings),
                aes(x=x, y=y),
                shape = gigo_symbol,
                color = gigo_symbol_color,
                size  = gigo_symbol_size, 
                inherit.aes = FALSE)}} 
    }

#===============================================================================

###  Function to scatterplot per-problem differences between RS scores

scatterplot_diff_vars <- function (compared_method_name, 
                                   reference_method_name, 
                                   diff_var_to_plot, 
                                   tib_to_plot, 
                                   plot_title, 
                                   y_axis_title) 
    {
    y_var     = ensym (diff_var_to_plot)        #enquo (y_var)
    
    ggplot (tib_to_plot, 
            aes (x = max_TOT_FN_FP_rate, 
                 y = !!y_var, 
                 color = dom_err_type)
            ) + 
    
        scale_color_manual (breaks = scale_color_breaks, 
                            values = scale_color_values,  
                            name = "dominant\nerror type"
                          ) +
    
        geom_point (size=0.1, shape=15) +    #23) + 
        
        guides (color = 
                  guide_legend (override.aes = list (size = 4, 
                                                     alpha = 1, 
                                                     fill = NA)), 
                  fill=guide_legend
                ) + 
            
        ggtitle (plot_title) +
        theme (plot.title = element_text (hjust = 0.5)) +    #  To center the title
        xlab ("Input Error") +
        ylab (y_axis_title) +

        geom_hline (yintercept = 0, #linetype="dashed", 
                    color = "black", size=0.5)
    }

#===============================================================================

double_scatter_diffs <- function (compared_method_name, 
                                  reference_method_name, 
                                  tib_to_plot, 
                                  tot_diff_var_to_plot, 
                                  rep_diff_var_to_plot)
    {
enquo_tot_diff_var_to_plot = enquo (tot_diff_var_to_plot)
enquo_rep_diff_var_to_plot = enquo (rep_diff_var_to_plot)

    tot_scatter = 
#        scatterplot_diff_vars ("Marxan_SA", "Gurobi", 
        scatterplot_diff_vars (compared_method_name, reference_method_name, 
                               !!enquo_tot_diff_var_to_plot,    #diff__rsr_COR_euc_out_err_frac__Marxan_SA__Gurobi, 
                               tib_to_plot, 
#                               plot_title = "Tot Out Err diff: Marxan_SA - Gurobi", 
                               plot_title = str_c ("Tot Out Err diff: ", compared_method_name, " - ", reference_method_name), 
                               y_axis_title = "tot out err diff") 
    
    rep_scatter = 
#        scatterplot_diff_vars ("Marxan_SA", "Gurobi", 
        scatterplot_diff_vars (compared_method_name, reference_method_name, 
                               !!enquo_rep_diff_var_to_plot,    #diff__rsr_COR_spp_rep_shortfall__Marxan_SA__Gurobi, 
                               tib_to_plot, 
#                               plot_title = "Rep Shortfall diff: Marxan_SA - Gurobi", 
                               plot_title = str_c ("Rep Shortfall diff: ", compared_method_name, " - ", reference_method_name), 
                               y_axis_title = "rep shortfall diff") 
    
    ###  Combined tot and rep plot
    
    patched <- tot_scatter / rep_scatter
    
    patched + # plot_annotation(tag_levels = 'a') + 
              plot_layout (guides="collect") + 
              plot_layout (heights=c(2,2))
    }

#===============================================================================

##  Function to plot histogram or density cloned from ggplot_faceted_with_mag_rays().

ggplot_hist_or_density <- function (tib_to_plot, 
                                    x_var, 
                                    plot_type = "hist",                   
                                    plot_title_str, 
                                    x_axis_label, 
                                    color = "black"
                                    )
    {
    x_var     = ensym (x_var)        #enquo (x_var)

    
    if (plot_type == "hist")
        {
        base_plot = 
            ggplot (tib_to_plot, 
                    aes(x = !!x_var)) +    #!!x_var)) + 
            geom_histogram (binwidth = 0.025, center = 0, fill = color)
        
        } else if (plot_type == "density")
        {
        base_plot = 
            ggplot (tib_to_plot, 
                    aes(x = !!x_var)) + 
            geom_density (alpha = .4, fill = color)    #"cyan")
        
        } else stop ("\nBad plot_type in ggplot_hist_and_dist().\n")
    
    base_plot = 
        base_plot + 
    
            ggtitle (plot_title_str) +
            theme (plot.title = element_text (hjust = 0.5)) #xxx# +    #  To center the title
    
        #  If both axis labels are specified, use them.
        #  If either or both are missing, then just use the variable name.
    if (!is.null (x_axis_label))    ##### & !is.null (y_axis_label))
        {
        base_plot = 
            base_plot + 
                
            labs (#####y=y_axis_label, 
              x=x_axis_label)
        }

    return (base_plot)
    }

#===============================================================================

double_hist <- function (tot_diff_var_to_plot, 
                         rep_diff_var_to_plot, 
                         tib_to_plot, 
                      rs_method_name_subtracted_FROM, 
                      rs_method_name_BEING_subtracted, 
                      plot_title_str_finish, 
                         color = "black")
{
enquo_tot_diff_var_to_plot = enquo (tot_diff_var_to_plot)
enquo_rep_diff_var_to_plot = enquo (rep_diff_var_to_plot)

tot_hist = 
    ggplot_hist_or_density (filter (tib_to_plot, rs_method_name == rs_method_name_subtracted_FROM),    #"Marxan_SA"),
                          x_var = !!enquo_tot_diff_var_to_plot,    #"diff__rsr_COR_euc_out_err_frac__Marxan_SA__Gurobi", 
                          plot_type = show_hist_or_density,                   
                          plot_title_str = str_c (rs_method_name_subtracted_FROM, 
                                                  " - ", 
                                                  rs_method_name_BEING_subtracted, 
                                                  "\ntot out error diffs\n", 
                                                  plot_title_str_finish), 
                          x_axis_label = "tot out error diff", 
                          color = color
                          )

rep_hist = 
    ggplot_hist_or_density (filter (tib_to_plot, rs_method_name == rs_method_name_subtracted_FROM),    #"Marxan_SA"),
                          x_var = !!enquo_rep_diff_var_to_plot,    #"diff__rsr_COR_spp_rep_shortfall__Marxan_SA__Gurobi", 
                          plot_type = show_hist_or_density,                   
#                          plot_title_str = str_c ("Marxan_SA - Gurobi\nrep shortfall diffs\n", plot_title_str_finish), 
                          plot_title_str = str_c (rs_method_name_subtracted_FROM, 
                                                  " - ", 
                                                  rs_method_name_BEING_subtracted, 
                                                  "\nrep shortfall diffs\n", 
                                                  plot_title_str_finish), 
                          x_axis_label = "rep shortfall diff", 
                          color = color
                          )

###  Combined tot and rep plot

patched <- tot_hist + rep_hist
patched + plot_layout (guides="collect")
}

#===============================================================================

