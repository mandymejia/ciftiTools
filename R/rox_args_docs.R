#' labels
#' 
#' @section Label Levels:
#'  \code{xifti$meta$subcort$labels} is a factor with the following levels:
#' 
#'  \enumerate{
#'    \item{Cortex-L}
#'    \item{Cortex-R}
#'    \item{Accumbens-L}
#'    \item{Accumbens-R}
#'    \item{Amygdala-L}
#'    \item{Amygdala-R}
#'    \item{Brain Stem}
#'    \item{Caudate-L}
#'    \item{Caudate-R}
#'    \item{Cerebellum-L}
#'    \item{Cerebellum-R}
#'    \item{Diencephalon-L}
#'    \item{Diencephalon-R}
#'    \item{Hippocampus-L}
#'    \item{Hippocampus-R}
#'    \item{Pallidum-L}
#'    \item{Pallidum-R}
#'    \item{Putamen-L}
#'    \item{Putamen-R}
#'    \item{Thalamus-L}
#'    \item{Thalamus-R}
#'  }
#' 
#'  These correspond to the same structures as given by 
#'  \code{ft_read_cifti} in the \code{cifti-matlab} MATLAB toolbox. 
#' @name labels_Description
#' @keywords internal
NULL

#' Connectome_Workbench
#' 
#' @section Connectome Workbench Requirement:
#'  This function uses a system wrapper for the 'wb_command' executable. The 
#'  user must first download and install the Connectome Workbench, available 
#'  from https://www.humanconnectome.org/software/get-connectome-workbench . 
#'  
#' @name Connectome_Workbench_Description
#' @keywords internal
NULL

#' brainstructures
#'
#' @param brainstructures Character vector indicating which brain structure(s) 
#'  to obtain: \code{"left"} (left cortical surface), \code{"right"} (right 
#'  cortical surface) and/or \code{"subcortical"} (subcortical and cerebellar
#'  gray matter). Can also be \code{"all"} (obtain all three brain structures). 
#'  Default: \code{"all"}. 
#' 
#'  If a brain structure is indicated but does not exist, a warning will be
#'  raised and that brain structure will be skipped.
#' @name brainstructures_Param_all
#' @keywords internal
NULL

#' brainstructures
#'
#' @param brainstructures Character vector indicating which brain structure(s) 
#'  to obtain: \code{"left"} (left cortical surface), \code{"right"} (right 
#'  cortical surface) and/or \code{"subcortical"} (subcortical and cerebellar
#'  gray matter). Can also be \code{"all"} (obtain all three brain structures). 
#'  Default: \code{c("left","right")} (cortical surface only).
#' 
#'  If a brain structure is indicated but does not exist, a warning will be
#'  raised and that brain structure will be skipped.
#' @name brainstructures_Param_LR
#' @keywords internal
NULL

#' cifti_fname
#'
#' @param cifti_fname File path of CIFTI-format data (ending in ".d*.nii").
#' @name cifti_fname_Param
#' @keywords internal
NULL

#' original_fnames: for resampling
#'
#' @param original_fnames The files to resample. This is a named list 
#'  where each element's name is a file type label, and each element's value
#'  is a file name. Labels must be one of the following: "cortexL", "cortexR", 
#'  "ROIcortexL", "ROIcortexR", "surfL", or "surfR". If 
#'  \code{read_dir} is not \code{NULL}, then all these file names should be
#'  relative to \code{read_dir}. 
#' @name original_fnames_Param_resampled
#' @keywords internal
NULL

#'  read_dir: separated files
#'  
#' @param read_dir Directory to append to the path of every file being read,
#'  e.g. \code{cortexL_original_fname}. If \code{NULL} (default), do not append
#'  any directory to the path.
#' 
#'  \code{read_dir} must already exist, or an error will be raised.
#' @name read_dir_Param_separated
#' @keywords internal
NULL

#' resamp_res: required
#'
#' @param resamp_res Target resolution for resampling (number of 
#'  cortical surface vertices per hemisphere).  
#' @name resamp_res_Param_required
#' @keywords internal
NULL

#' resamp_res: optional
#'
#' @param resamp_res (Optional) Target resolution for resampling (number of 
#'  cortical surface vertices per hemisphere). If \code{NULL} (default) or 
#'  \code{FALSE}, do not perform resampling.
#' @name resamp_res_Param_optional
#' @keywords internal
NULL

#' ROI_brainstructures
#'
#' @param ROI_brainstructures Character vector indicating which ROIs should be 
#'  obtained. \code{NULL} (default) to not get any ROIs. Otherwise, this should 
#'  be a subset of the \code{brainstructures} argument. 
#' 
#' ROIs are typically the medial wall mask for the cortex and subcortical mask
#'  for the subcortex.
#' @name ROI_brainstructures_Param_LR
#' @keywords internal
NULL

#' surfL
#' 
#' @param surfL (Optional) Left brain surface model. Can be a file
#'  path to a GIFTI surface geometry file (ends in "*.surf.gii"), a 
#'  \code{"gifti"} object representing surface geometry, or a \code{"surf"}
#'  object.
#' @name surfL_Param_optional
#' @keywords internal
NULL

#' surfR
#' 
#' @param surfR (Optional) Right brain surface model. Can be a file
#'  path to a GIFTI surface geometry file (ends in "*.surf.gii"), a 
#'  \code{"gifti"} object representing surface geometry, or a \code{"surf"}
#'  object.
#' @name surfR_Param_optional
#' @keywords internal
NULL

#' surfL_fname
#'
#' @param surfL_fname (Optional) File path of GIFTI surface geometry
#'  file representing the left cortex.
#' @name surfL_fname_Param
#' @keywords internal
NULL

#' surfL_original_fname
#'
#' @param surfL_original_fname (Optional) File path of GIFTI surface geometry
#'  file representing the left cortex.
#' @name surfL_original_fname_Param
#' @keywords internal
NULL

#' surfL_target_fname
#'
#' @param surfL_target_fname (Optional) File path to save the resampled GIFTI 
#'  surface geometry file representing the left cortex at.
#' @name surfL_target_fname_Param
#' @keywords internal
NULL

#' surfR_fname
#'
#' @param surfR_fname (Optional) File path of GIFTI surface geometry
#'  file representing the right cortex.
#' @name surfR_fname_Param
#' @keywords internal
NULL

#' surfR_original_fname
#'
#' @param surfR_original_fname (Optional) File path of GIFTI surface geometry
#'  file representing the right cortex.
#' @name surfR_original_fname_Param
#' @keywords internal
NULL

#' surfR_target_fname
#'
#' @param surfR_target_fname (Optional) File path to save the resampled GIFTI 
#'  surface geometry file representing the right cortex at.
#' @name surfR_target_fname_Param
#' @keywords internal
NULL

#' verbose: FALSE
#'
#' @param verbose Should occasional updates be printed? Default: \code{FALSE}.
#' @name verbose_Param_FALSE
#' @keywords internal
NULL

#' verbose: TRUE
#'
#' @param verbose Should occasional updates be printed? Default: \code{TRUE}.
#' @name verbose_Param_TRUE
#' @keywords internal
NULL

#' write_dir: generic
#'  
#' @param write_dir Where should the separated
#'  files be placed? \code{NULL} (default) will write them to
#'  the current working directory.
#' 
#'  \code{write_dir} must already exist, or an error will occur.
#' @name write_dir_Param_generic
#' @keywords internal
NULL

#' xifti
#' 
#' @param xifti Object of class "xifti". 
#'  See \code{\link{is.xifti}} and \code{\link{make_xifti}}.
#' @name xifti_Param
#' @keywords internal
NULL

#' x: xifti
#' 
#' @param x Object of class "xifti". 
#'  See \code{\link{is.xifti}} and \code{\link{make_xifti}}.
#' @name x_Param_xifti
#' @keywords internal
NULL

#' Navigating and Embedding the Interactive Plots
#' 
#' @section Navigating and Embedding the Interactive Plots:
#'  This function opens an interactive Open GL window rendered by \code{rgl}. 
#'  If \code{save==TRUE} and \code{close_after_save==TRUE}, the window will be
#'  closed after the function call. Otherwise, it is kept open and the following
#'  information applies:
#' 
#'  To navigate the plot, left click and drag the cursor to rotate. Use the 
#'  scroll wheel or right click and drag to zoom. Press the scroll wheel and drag
#'  to change the field-of-view. Execute \code{\link[rgl]{snapshot}} to save the
#'  current window as a .png file. Execute \code{\link[rgl:rgl.open]{rgl.close}} to close
#'  the window. \code{\link[rgl:viewpoint]{rgl.viewpoint}} can be used for programmatic
#'  navigation. 
#' 
#'  The Open GL window can be embedded as an htmlwidget in an R Markdown document
#'  using one of two methods. The first is executing \code{\link[rgl]{rglwidget}}
#'  in the chunk where the plot is made. This first method should work within
#'  both the RStudio IDE and a knitted .html file. The second method is 
#'  executing \code{rgl::hook_rgl} at the start of the document and 
#'  then using the chunk option \code{webgl=TRUE} in the chunk where the plot is
#'  made. The second method is specifically for knitted .html files. Although
#'  the first method is the newest approach and is recommended by others, we
#'  used the second method in the \code{ciftiTools} vignette because the first
#'  is not compatible with htmlpreview. For both methods, the window still
#'  needs to be open to render the widget. Also for both methods, you will 
#'  probably need to tweak the image dimensions e.g. 
#'  \code{fig.width=8, fig.height=5} in the chunk options, because it uses the 
#'  defaults from RMarkdown/Knitr instead of what makes sense based on the 
#'  dimensions of the Open GL window.
#'  
#'  For \code{view_xifti_surface}, if \code{length(idx) > 1}, this function will
#'  automatically return an htmlwidget using the first method, but with a
#'  \code{\link[rgl]{playwidget}} wrapper to add a slider to control which
#'  column index is being displayed. All the meshes will be rendered on top of
#'  one another in the Open GL window, so only the widget will be useful for
#'  viewing the data interactively. Since it uses the first method, it will not
#'  be visible with htmlpreview. No additional call to 
#'  \code{\link[rgl]{rglwidget}} is necessary, but \code{\link[rgl:rgl.open]{rgl.close}}
#'  must be called in a following chunk to close the Open GL window.
#' 
#' @name rgl_interactive_plots_Description
#' @keywords internal
NULL

#' Embedding the Static Plots
#' 
#' @section Embedding the Static Plots:
#'  If \code{save==TRUE}, the plot(s) is written to a .png file. (For 
#'  \code{view_xifti_surface}, if \code{length(idx) > 1}, each \code{idx} will
#'  be written to a separate image file.) You can use 
#'  \code{\link[knitr]{include_graphics}} to embed an image file in an R
#'  Markdown document. If \code{close_after_save==TRUE}, the return value of this
#'  function call is the name(s) of the image file(s) that were written, so it
#'  can be used directly to display the image.
#' 
#'  There's an additional way to embed an image of this plot without writing a
#'  .png file: use \code{save==FALSE} and set the chunk options 
#'  \code{rgl=TRUE, format="png"}. You will probably need to tweak the image
#'  dimensions e.g. \code{fig.width=8, fig.height=5} in the chunk options, 
#'  because it uses the defaults from RMarkdown/Knitr instead of what makes
#'  sense based on the dimensions of the Open GL window.
#' 
#' @name rgl_static_plots_Description
#' @keywords internal
NULL

#' surface plot
#' 
#' @param view Which view to display: \code{"lateral"}, \code{"medial"}, or 
#'  \code{"both"}. If \code{NULL} (default), both views will be shown. Each view 
#'  will be plotted in a separate panel row.
#' @param widget Display the plot in an htmlwidget? Default: \code{TRUE}. If
#'  \code{FALSE}, use an Open GL Window instead. If multiple files are being
#'  written (\code{length(idx) > 1} and \code{!isFALSE(fname)}), curently neither the
#'  widget nor the Open GL window are able to be displayed.
#' @param title Optional title(s) for the plot(s). It will be printed at the top 
#'  in a separate subplot with 1/4 the height of the brain cortex subplots.
#'  
#'  Default: \code{NULL} will not use any title if \code{length(idx)==1}.
#'  Otherwise, it will use the time index (".dtseries") or name
#'  (.dscalar or .dlabel) of each data column.
#' 
#'  To use a custom title(s), use a length 1 character vector (same title for
#'  each plot) or length \code{length(idx)} character vector (different title
#'  for each plot). Set to an empty string \code{""} to omit the title. 
#' 
#'  If the title is non-empty but does not appear, \code{cex.title} may need to 
#'  be lowered.
#' @param fname,fname_suffix Save the plot(s), and color legend(s) if applicable, to png files?
#'  Set \code{fname} to \code{FALSE} (default) to not save any files. Otherwise, each
#'  index (and corresponding legend, if applicable) will be saved to a file.
#'  Set \code{fname} to \code{TRUE} to name the files by the data column names. (Spaces
#'  will be replaced with underscores). Set \code{fname} to a length 1 character vector
#'  to name files by this suffix followed by the \code{suffix}: either the
#'  data column names (\code{"names"}) or the index value (\code{"idx"}). 
#'  Set this to a character vector with the same length as \code{idx} to name 
#'  the files exactly. 
#' 
#'  The color legend(s) will be saved to "[surf_fname_for_this_idx]_legend.png".
#' @param cex.title Font size multiplier for the title. \code{NULL} (default)
#'  will use \code{2} for titles less than 20 characters long, and smaller
#'  sizes for increasingly longer titles.
#' @param text_color Color for text in title and colorbar legend. Default:
#'  "black".
#' @param bg Background color. \code{NULL} will not color the background (white).
#' @param alpha Transparency value for mesh coloring, between 0 and 1. Default:
#'  \code{1.0} (no transparency).
#' @param edge_color Outline each edge in this color. Default: \code{NULL} (do
#'  not outline the edges).
#' @param vertex_color Draw each vertex in this color. Default: 
#'  \code{"black"}. Vertices are only drawn if \code{vertex_size > 0}
#' @param vertex_size Draw each vertex with this size. Default: \code{0} 
#'  (do not draw the vertices).
#' @param width,height The dimensions of the RGL window, in pixels. If both are
#'  \code{NULL} (default), the dimensions will be set to
#'  1000 (width) x 700 (height) for 1x1 and 2x2 subplots,
#'  1500 x 525 for 2x1 subplots, and
#'  500 x 700 for 1x2 subplots. These defaults are chosen to fit comfortably
#'  within a 1600 x 900 screen. Specifying only one will set the other to maintain
#'  the same aspect ratio. Both can be specified to set the dimensions exactly.
#' @param zoom Adjustment to size of brain meshes. Default: \code{3/5}
#'  (100\% + 3/5*100\% = 160\% the original size).
#' @name surface_plot_Params
#' @keywords internal
NULL