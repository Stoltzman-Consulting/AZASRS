#` Newest function
#` @return the sample data
my_new_func = function(){
  df = data.frame(x=c(1,2,3),y=c(4,5,6))
  df2 = df %>%
    select(x)
  return(df2)
}
