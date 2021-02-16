"%ni%" <- function(x, y){
  # Kinda useless function which negates the %in% operator
  !x%in%y
  # a <- c(1, 2, 3)  
  # 2 %in% a -> TRUE
  # 2 %ni% a -> FALSE
  # 5 %ni% a -> TRUE
}
