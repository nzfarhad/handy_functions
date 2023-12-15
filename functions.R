ptable <- function(data, perc = T){
  
  expr <- substitute(data)
  var_name <- sub(".*\\$", "", expr)[3]
  
  if(perc == T) {
    my_perc_table <- as.data.frame(prop.table(table(data)))
    names(my_perc_table) <- c(var_name, "Percentage")
  }
  else{
    my_perc_table <- as.data.frame(table(data))
    names(my_perc_table) <- c(var_name, "Freq")
  }
  
  return(my_perc_table)
}