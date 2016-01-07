#' Export animations from rmodflow 4d arrays
#' 
#' @param array rmodflow 4d array
#' @param i row number
#' @param j column number
#' @param k layer number
#' @param l vector of time step numbers; defaults to 1 to the size of the fourth dimension of array
#' @param dis dis object
#' @param ... other parameters passed to plot.rmodflow_4d_array
#' @param file animation filename
#' @param type animation filetype; options are 'gif' (default), 'html' and 'pdf'; defaults to file extension
#' @param plot_type type of plot for plot.rmodflow_4d_array
#' @param title animation title; if equal to 'totim', the total time is shown for each frame, as taken from the rmodflow_4d_array attributes, if equal to 'totim_unit', the time unit obtained from dis$itmuni is pasted to the totim times
#' @param width animation width; defaults to 600 pixels or 8 inch in case of pdf
#' @param height animation height; default equal to width
#' @param clean logical; should files other than the animation (e.g. frames) be removed?
#' @param interval time interval between frames, in seconds; can be a vector
#' @param overlay additional ggplot2 layers to add to all frames
#' @param background background ggplot2 plot for all frames
#' @rdname export_animation
#' @importFrom animation saveLatex saveGIF saveHTML
#' @importFrom tools file_ext file_path_sans_ext
#' @export
export_animation.rmodflow_4d_array <- function(array,
                                      i = NULL,
                                      j = NULL,
                                      k = NULL,
                                      l = 1:dim(array)[4],
                                      title = 'totim_unit',
                                      dis = dis,
                                      ...,
                                      file='rmodflow_animation.gif',
                                      type = tools::file_ext(file),
                                      plot_type = 'fill',
                                      width=c(600,600,8)[type==c('gif','html','pdf')],
                                      height=width,
                                      clean = TRUE,
                                      interval = 0.5,
                                      overlay = NULL,
                                      background = NULL) {
  if(!is.null(title)){
    if(title == 'totim') {
      title <- attributes(array)$totim[l]
    } else if(title == 'totim_unit') {
      title <- paste(attributes(array)$totim[l],c('??','s','min','h','d','y')[dis$itmuni+1])
    } else {
      stop('Please provide valid title argument.')
    }
  }
  delayedAssign('expr', {
    for(m in 1:length(l)) {
      if(is.null(background)) {
        p <- plot(array, i = i, j = j, k = k, l = l[m], title = title[m], dis = dis, ..., type = plot_type)
      } else {
        p <- background + plot(array, i = i, j = j, k = k, l = l[m], title = title[m], dis = dis, ..., type = plot_type, add = TRUE)
      }
      if(!is.null(overlay)) p <- p + overlay
      print(p)
    }
  })
  current_wd <- getwd()
  setwd(dirname(file))
  if(type == 'gif') {
    saveGIF(expr,img.name=paste0(basename(file_path_sans_ext(file)),'_frame'),movie.name=basename(file),
            ani.dev='png',ani.type='png',ani.width=width,ani.height=height, clean = clean, interval = interval)
  } else if(type == 'pdf') {
    ani.options(ani.dev='pdf',ani.type='pdf',ani.height=8, ani.width=8)
    saveLatex(expr,img.name=paste0(basename(file_path_sans_ext(file)),'_frames'),latex.filename=paste0(basename(file_path_sans_ext(file)),'.tex'),
              ani.dev='pdf',ani.type='pdf',ani.height=height,ani.width=width)
    if(clean) {
      file.remove('animate.sty','animfp.sty',paste0(basename(file_path_sans_ext(file)),'.tex'),paste0(basename(file_path_sans_ext(file)),'.log'),paste0(basename(file_path_sans_ext(file)),'.aux'),paste0(basename(file_path_sans_ext(file)),'_frames.pdf'))
    }
  } else if(type == 'html') {
    saveHTML(expr,img.name=paste0(basename(file_path_sans_ext(file)),'_frame'),htmlfile=basename(file),
            ani.dev='png',ani.type='png',ani.width=width,ani.height=height, clean = clean, interval = interval, verbose = FALSE)
  } else {
    stop('Please provide valid animation type.')
  }
  setwd(current_wd)
}
