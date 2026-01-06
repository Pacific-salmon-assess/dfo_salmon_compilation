`%notin%`<- Negate(`%in%`)

#convert gilbert-rich naming to european
gr_to_euro<-function(x){
  # names in the format 'rxy' or 'recruits_xy'
  f_age <- as.numeric(substr(x, nchar(x), nchar(x)))-1 #extract last element, -1 for f_age
  tot_age <- as.numeric(substr(x, nchar(x)-1, nchar(x)-1))-1
  oc_age <- tot_age - f_age
  
  eur.names <- paste0("r", f_age, ".", oc_age)
  return(eur.names)
}
