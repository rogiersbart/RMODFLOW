################################################################################
### node.info ##################################################################
################################################################################

nodeInfo <- function(...) UseMethod('nodeInfo')

# Return column information from dis file
nodeInfo.dis <- function(dis, rownumber, colnumber)
{
  cat('Column width = ',dis$DELR[colnumber], '\n')
  cat('Row width = ', dis$DELC[rownumber], '\n')
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  cat('\t\t Top \t\t Bottom \t Thickness\n', sep='')
  cat('Layer 1:\t', dis$TOP[rownumber, colnumber], '\t', dis$BOTM[rownumber,colnumber,1],'\t', dis$TOP[rownumber, colnumber]-dis$BOTM[rownumber,colnumber,1],'\n', sep='')
  
  for(i in 2:dis$NLAY)
  {
    cat('Layer ',i,':\t', dis$BOTM[rownumber, colnumber, i-1], '\t', dis$BOTM[rownumber,colnumber,i],'\t', dis$BOTM[rownumber, colnumber, i-1]-dis$BOTM[rownumber,colnumber,i],'\n', sep='')
  }
}
nodeInfo.huf <- function(huf, rownumber, colnumber)
{
  cat('Vertical boundaries:\n')
  
  # layers: top bottom thickness
  cat('\t\t Name \t\t Top \t\t Bottom \t Thickness\n', sep='')
  
  for(i in 1:huf$NHUF)
  {
    cat('Layer ',i,':\t',huf$HGUNAM[i],'\t', huf$TOP[rownumber, colnumber, i], '\t', huf$TOP[rownumber, colnumber, i]-huf$THCK[rownumber, colnumber, i],'\t', huf$THCK[rownumber, colnumber, i],'\n', sep='')
  }
}