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
#'  \code{ft_read_cifti} in the \code{cifti-matlab} MATLAB toolbox. Note that
#'  the first two levels (left and right cortex) are not used.
#' @name labels_Description
#' @keywords internal
NULL

#' brainstructures
#'
#' @param brainstructures Character vector indicating which brain structure(s)
#'  to obtain: \code{"left"} (left cortex), \code{"right"} (right cortex)
#'  and/or \code{"subcortical"} (subcortex and cerebellum). Can also be
#'  \code{"all"} (obtain all three brain structures). Default: \code{"all"}.
#'
#'  If a brain structure is indicated but is not included, a warning will be
#'  raised and that brain structure will be skipped.
#' @name brainstructures_Param_all
#' @keywords internal
NULL

#' brainstructures
#'
#' @param brainstructures Character vector indicating which brain structure(s)
#'  to obtain: \code{"left"} (left cortex), \code{"right"} (right cortex)
#'  and/or \code{"subcortical"} (subcortex and cerebellum). Can also be
#'  \code{"all"} (obtain all three brain structures). Default:
#'  \code{c("left","right")} (cortex only).
#'
#'  If a brain structure is indicated but does not exist in the CIFTI file, a
#'  warning will occur and that brain structure will be skipped.
#' @name brainstructures_Param_LR
#' @keywords internal
NULL

#' cifti_fname
#'
#' @param cifti_fname File path to a CIFTI file (ending in ".d*.nii").
#' @name cifti_fname_Param
#' @keywords internal
NULL

#' idx
#'
#' @param idx Numeric vector indicating the data indices (columns) to read. If
#'  \code{NULL} (default), read in all the data. Must be a subset of the indices
#'  present in the file, or an error will occur.
#'
#'  For high-resolution CIFTI files, reading in only a subset of the data saves
#'  memory, but will be slower than reading in the entire file due to the
#'  required intermediate steps.
#' @name idx_Param
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
#' ROIs are typically the medial wall masks for the left and right cortex, and
#'  the subcortical mask for the subcortex.
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
#' @param surfL_fname (Optional) File path to a GIFTI surface geometry
#'  file representing the left cortex.
#' @name surfL_fname_Param
#' @keywords internal
NULL

#' surfL_original_fname
#'
#' @param surfL_original_fname (Optional) File path to a GIFTI surface geometry
#'  file representing the left cortex.
#' @name surfL_original_fname_Param
#' @keywords internal
NULL

#' surfL_target_fname
#'
#' @param surfL_target_fname (Optional) Where to save the resampled GIFTI
#'  surface geometry file representing the left cortex.
#' @name surfL_target_fname_Param
#' @keywords internal
NULL

#' surfR_fname
#'
#' @param surfR_fname (Optional) File path to a GIFTI surface geometry
#'  file representing the right cortex.
#' @name surfR_fname_Param
#' @keywords internal
NULL

#' surfR_original_fname
#'
#' @param surfR_original_fname (Optional) File path to a GIFTI surface geometry
#'  file representing the right cortex.
#' @name surfR_original_fname_Param
#' @keywords internal
NULL

#' surfR_target_fname
#'
#' @param surfR_target_fname (Optional) Where to save the resampled GIFTI
#'  surface geometry file representing the right cortex.
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
#' @param write_dir Where should the separated files be placed? \code{NULL}
#'  (default) will write them to the current working directory.
#'
#'  \code{write_dir} must already exist, or an error will occur.
#' @name write_dir_Param_generic
#' @keywords internal
NULL

#' xifti
#'
#' @param xifti A \code{"xifti"} object.
#' @name xifti_Param
#' @keywords internal
NULL

#' x: xifti
#'
#' @param x A \code{"xifti"} object.
#' @name x_Param_xifti
#' @keywords internal
NULL

#' Navigating and Embedding the Interactive Plots
#'
#' @section Navigating and Embedding the Interactive Plots:
#'
#'  To navigate the interactive Open GL window and html widget, left click and
#'  drag the cursor to rotate the meshes. Use the scroll wheel or right click
#'  and drag to zoom. Press the scroll wheel and drag to change the field-of-view.
#'  For Open GL windows, execute
#'  \code{\link[rgl]{snapshot}} to save the current window as a .png file,
#'  \code{\link[rgl:open3d]{close3d}} to close the window, and
#'  \code{\link[rgl:viewpoint]{view3d}} to programmatically control the
#'  perspective.
#'
#'  To embed an interactive plot in an R Markdown document, first execute
#'  \code{rgl::setupKnitr()} to prepare the document for embedding the widget.
#'  Then execute the plot command as you normally would to create a widget (i.e.
#'  without specifying \code{fname}, and by requesting more than one \code{idx}
#'  or by setting \code{widget} to \code{TRUE}). When the R Markdown document is
#'  knitted, the interactive widget should be displayed below the chunk in which
#'  the plot command was executed. See the vignette for an example.
#' @name rgl_interactive_plots_Description
#' @keywords internal
NULL

#' Embedding the Static Plots
#'
#' @section Embedding the Static Plots:
#'
#'  To embed a static plot in an R Markdown document, first execute
#'  \code{rgl::setupKnitr()} to prepare the document for embedding the snapshot
#'  of the Open GL window. Then execute the plot command as you normally would
#'  to create an Open GL window (i.e. without specifying \code{fname}, and by
#'  requesting only one \code{idx}). In the options for the chunk in which the
#'  plot command is executed, set \code{rgl=TRUE, format="png"}. You can also
#'  control the image dimensions here e.g. \code{fig.height=3.8, fig.width=5}.
#'  When the R Markdown document is knitted, the static plots should be
#'  displayed below the chunk in which the plot command was executed. See the
#'  vignette for an example.
#'
#' @name rgl_static_plots_Description
#' @keywords internal
NULL

#' Surface plot
#'
#' @param view Which view to display: \code{"lateral"}, \code{"medial"}, or
#'  \code{"both"}. If \code{NULL} (default), both views will be shown. Each view
#'  will be plotted in a separate panel row.
#' @param widget Display the plot in an htmlwidget? Should be logical or
#'  \code{NULL} (default), in which case a widget will be used only if needed
#'  (\code{length(idx)>1 & isFALSE(fname)}, \code{fname} is a file path to an
#'  .html file, or if \code{rgl.useNULL()}).
#' @param title Optional title(s) for the plot(s). It will be printed at the top
#'  in a separate subplot with 1/4 the height of the brain cortex subplots.
#'
#'  Default: \code{NULL} will not use any title if \code{length(idx)==1}.
#'  Otherwise, it will use the time index (".dtseries") or name
#'  (.dscalar or .dlabel) of each data column.
#'
#'  To use a custom title(s), use a length 1 character vector (same title for
#'  each plot) or length \code{length(idx)} character vector (different title
#'  for each plot). Set to \code{NULL} or an empty character to omit the title.
#'
#'  If the title is non-empty but does not appear, try lowering \code{cex.title}.
#' @param fname Save the plot(s) (and color legend if applicable)?
#'
#'  If \code{isFALSE(fname)} (default), no files will be written.
#'
#'  If \code{fname} is a length-1 character vector ending in ".html", an html
#'  with an interactive widget will be written.
#'
#'  If neither of the cases above apply, a png image will be written for each
#'  \code{idx}. If \code{isTRUE(fname)} the files will be named by the
#'  data column names (underscores will replace spaces). Or, set \code{fname} to a
#'  length 1 character vector to name files by this suffix followed by the
#'  \code{fname_suffix}. Or, set \code{fname} to a character vector with the same
#'  length as \code{idx} to name the files exactly.
#' @param fname_suffix Either the data column names (\code{"names"}) or the
#'  index value (\code{"idx"}).
#' @param cex.title Font size multiplier for the title. \code{NULL} (default)
#'  will use \code{2} for titles less than 20 characters long, and smaller
#'  sizes for increasingly longer titles.
#' @param text_color Color for text in title and colorbar legend. Default:
#'  \code{"black"}.
#' @param bg Background color. \code{NULL} will use \code{"white"}. Does not affect
#'  the color legend or color bar if printed separately: those will always have
#'  white backgrounds.
#' @param alpha Transparency value for mesh coloring, between 0 and 1. Default:
#'  \code{1.0} (no transparency).
#' @param edge_color Outline each edge in this color. Default: \code{NULL} (do
#'  not outline the edges).
#' @param vertex_color Draw each vertex in this color. Default:
#'  \code{"black"}. Vertices are only drawn if \code{vertex_size > 0}
#' @param vertex_size Draw each vertex with this size. Default: \code{0}
#'  (do not draw the vertices).
#' @param material A list of materials from \code{\link[rgl]{material3d}}
#'  to use. For example, \code{list(lit=FALSE, smooth=FALSE)} will use exact colors
#'  from the color scale, rather than adding geometric shading and interpolating
#'  vertex colors. If \code{NULL}, use defaults.
#' @param width,height The dimensions of the RGL window, in pixels. If both are
#'  \code{NULL} (default), these dimensions depend on type of output (Open GL
#'  window or widget) and subplots (\code{hemisphere}, \code{view}, \code{title},
#'  and \code{slider_title}) and are chosen to be the largest plot within a
#'  1500 x 700 area (Open GL window) or 600 x 700 area (widget) that maintains
#'  a brain hemisphere subplot dimensions ratio of 10 x 7. Specifying only one
#'  will set the other to maintain this aspect ratio. Both can be specified to
#'  set the dimensions exactly, but note that the dimensions cannot be larger
#'  than the screen resolution. (These arguments do not affect the size of the
#'  legend, which cannot be controlled.)
#'
#'  The plot will be taller than \code{height} to accommodate a title or color
#'  bar.
#'
#'  If multiple \code{idx} are being composited with \code{together}, these
#'  arguments refer to a single \code{idx} within the composited plot, and not
#'  the composited plot itself.
#' @param zoom Adjust the sizes of the brain meshes. Default: \code{NULL} (will
#'  be set to 0.6 or 160\% for the Open GL window, and 0.67 or 167\% for the
#'  widget.)
#' @name surface_plot_Params
#' @keywords internal
NULL

#'  faces
#'
#' @param faces An \eqn{F \times 3} matrix, where each row contains the vertex
#'  indices for a given triangular face in the mesh. \eqn{F} is the number of
#'  faces in the mesh.
#'
#' @name faces_Param
NULL

#'  vertices
#'
#' @param vertices A \eqn{V \times 3} matrix, where each row contains the Euclidean
#'  coordinates at which a given vertex in the mesh is located. \eqn{V} is the
#'  number of vertices in the mesh
#'
#' @name vertices_Param
NULL

#' mask: vertices
#'
#' @param mask  A length \eqn{V} logical vector indicating if each vertex is
#'  within the input mask.
#'
#' @name mask_Param_vertices
NULL
