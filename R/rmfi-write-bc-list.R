#'
#' Write a MODFLOW boundary condition package which uses list-directed input to a file
#'
#' @param file filename to write to
#' @param obj an \code{RMODFLOW} boundary condition rmf_package object with list directed input
#' @param dis an \code{RMODFLOW} dis object
#' @param varnames character vector with the names of the variables starting from the 4th column (so after ijk)
#' @param header character; package name. Part of the header comment written to the output file
#' @param package character; acronym (often 3 letters) used by MODFLOW to name to package
#' @param partyp character; specifies the parameter type
#' @param ... arguments passed to \code{rmfi_write_variables} when writing a fixed format file

#' @return \code{NULL}
#' @keywords internal
#' @seealso \code{\link{rmfi_create_bc_list}}, \code{\link{rmfi_read_bc_list}}
#' 

rmfi_write_bc_list <- function(file, obj, dis, varnames, header, package, partyp, ...) {
  
  
  # data set 0
  v <- packageDescription("RMODFLOW")$Version
  cat(paste(paste('# MODFLOW', header, 'created by RMODFLOW, version'),v,'\n'), file = file)
  cat(paste('#', comment(obj)), sep='\n', file=file, append=TRUE)
  
  # data set 1
  if(obj$dimensions$np > 0) rmfi_write_variables('PARAMETER', obj$dimensions$np, obj$dimensions$mxl, file=file)
  
  # data set 2
  rmfi_write_variables(obj$dimensions$mxact, obj[[paste0('i',tolower(package), 'cb')]], ifelse(obj$option['noprint'], 'NOPRINT', ''), rmfi_ifelse0((!is.null(obj$aux)), paste('AUX', obj$aux), ''), file=file, ...)
  
  # parameters
  if(obj$dimensions$np > 0){
    parm_names <- names(obj$parameter_values)
    tv_parm <- structure(rep(F,obj$dimensions$np), names = parm_names)
    
    for (i in 1:obj$dimensions$np){
      
      p_name <- parm_names[i]
      df <- subset(obj$data, name == p_name)
      
      tv_parm[i] <- (!is.null(obj$dimensions$instances) && obj$dimensions$instances[p_name] != 0)
      nlst <- unname(ifelse(tv_parm[i], nrow(df)/obj$dimensions$instances[p_name], nrow(df)))
      
      # data set 3
      rmfi_write_variables(p_name, toupper(partyp), obj$parameter_values[i], nlst, ifelse(tv_parm[i], 'INSTANCES', ''), ifelse(tv_parm[i],  obj$dimensions$instances[p_name], ''), file=file)
      
      # time-varying
      if(tv_parm[i]){
        instances <- unique(df$instance)
        for (jj in 1:obj$dimensions$instances[p_name]){
          
          df2 <- subset(df, instance == instances[jj])
          # data set 4a
          rmfi_write_variables(instances[jj], file=file)
          
          # data set 4b
          for (k in 1:nrow(df2)){
            rmfi_write_variables(df$k[k], df$i[k], df$j[k], df[k, varnames], rmfi_ifelse0(!is.null(obj$aux), df[k,obj[['aux']]], ''), file=file)
          }
          rm(df2)
        }
      } else { # non-time-varying
        for (k in 1:nrow(df)){
          rmfi_write_variables(df$k[k], df$i[k], df$j[k], df[k, varnames], rmfi_ifelse0(!is.null(obj$aux), df[k,obj[['aux']]], ''), file=file)
        }
      }  
      rm(df)
    }
  }
  
  
  # stress periods
  for (i in 1:dis$nper){
    
    # data set 5
    names_act <- colnames(obj$kper)[which(obj$kper[i,which(!is.na(obj$kper[i,]))] != FALSE)[-1]]
    if(i > 1 && identical(names_act, colnames(obj$kper)[which(obj$kper[i-1,which(!is.na(obj$kper[i-1,]))] != FALSE)[-1]])) {
      itmp <- -1 
    } else {
      list_names <- rmfi_ifelse0(obj$dimensions$np > 0, names_act[!(names_act %in% parm_names)], names_act)
      if(length(list_names) > 0) {
        itmp <- sum(obj$dimensions$itmp[list_names])
      } else {
        itmp <- 0
      }
    }
    
    if(obj$dimensions$np > 0) {
      parm_names_active <- parm_names[parm_names %in% names_act]
      np <- length(parm_names_active)
    } else {
      np <- 0
    }
    
    rmfi_write_variables(itmp, np, file=file, ...)
    
    # data set 6
    if(itmp > 0){
      df <- subset(obj$data, name %in% list_names)
      for(j in 1:nrow(df)){
        rmfi_write_variables(c(df$k[j], df$i[j], df$j[j], df[j, varnames],  rmfi_ifelse0(!is.null(obj$aux), df[j,obj[['aux']]], '')), file=file, ...)
      }
      rm(df)
    }
    
    # data set 7
    if(np > 0){
      for(j in 1:np){
        rmfi_write_variables(parm_names_active[j], ifelse(tv_parm[j], obj$kper[i,parm_names_active[j]], ''), file=file)
      }
    }
  }
  
  
}