test_prospera <- function(mylist){
  result <- grepl('ALL', mylist)
  return(any(result))
}

get_unique_clues <- function(df){
  result <- unique(df[grepl('clues',colnames(df))])
  return(result)
}
get_unique_states <- function(df){
  clues <- unique(df[grepl('clues',colnames(df))])
  states <- unique(apply(clues,1,substr,1,2))
  return(states)
}
get_conbebe <- function(df,prospera=FALSE){
  conbebe <- nrow(df[df$belongs_PUERPERIUM==1,])
  if (prospera){
    df <- df[apply(df[,grepl('groups', colnames(df))] ,1 ,test_prospera),]
    conbebe <- nrow(df[df$belongs_PUERPERIUM==1,])
  } 
  return(conbebe)
}
get_aux <- function(df){
  aux <- nrow(df[df[,'fields_rp_isaux_decl']==1 & !is.na(df[,'fields_rp_isaux_decl']),])
  return(aux)
}
get_vocal <- function(df){
  vocal <- nrow(df[df[,'fields_rp_isvocal_decl']==1 & !is.na(df[,'fields_rp_isaux_decl']),])
  return(vocal)
}