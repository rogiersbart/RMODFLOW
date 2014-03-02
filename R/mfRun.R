mfRun <- function(nam,dir=getwd(),mfVersion='mf2005',doPlot=T)
{
  shell(paste('cd',dir,'&',mfVersion,nam))
#  underScore <- which(strsplit(nam, '')[[1]]=='_')[1]
#   if(!is.na(underScore)) mfPerformance(paste(dir,'/',substr(nam,1,underScore-1),'_obs.hed',sep=''),savePNG=T,
#                                        file=paste(dir,'/',substr(nam,1,nchar(nam)-4),'.png',sep=''))
#   if(is.na(underScore)) mfPerformance(paste(dir,'/',substr(nam,1,nchar(nam)-4),'_obs.hed',sep=''),savePNG=T,
#                                       file=paste(dir,'/',substr(nam,1,nchar(nam)-4),'.png',sep=''))
  rmse <- mfPerformance(paste(dir,'/hpr/',substr(nam,1,nchar(nam)-4),'.hpr',sep=''),savePNG=T,
                                        file=paste(dir,'/hpr/',substr(nam,1,nchar(nam)-4),'.png',sep=''), doPlot=doPlot)
  return(rmse)
}


# run.model <- function()
# {
#   if(Sys.info()['sysname']=='Linux') system('espeak -v en-us -p 10 -s 130 "Starting MODFLOW simulation"',ignore.stdout=TRUE, ignore.stderr=TRUE)
#   if(Sys.info()['sysname']=='Windows') shell(paste('cd','mf2005','&','mf2005','input.nam'))
#   if(Sys.info()['sysname']=='Linux') system('cd mf2005 && /usr/local/src/mf2005/src/mf2005 input.nam')
#   #if(Sys.info()['sysname']=='Linux') system('ogg123 -q /usr/share/sounds/ubuntu/stereo/desktop-logout.ogg')
#   if(Sys.info()['sysname']=='Linux') system('espeak -v en-us -p 10 -s 130 "Finished MODFLOW simulation"',ignore.stdout=TRUE, ignore.stderr=TRUE)
# }