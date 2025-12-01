numbers <- c(1,2,3,4) # numeric vector
species <- c('blue tit', 'chaffinch', 'nuthatch') #character vector
flags <- c(T,F,T) #logical vector (e.g, for pres/abs)

m <- matrix(1:9, nrow= 3, ncol = 3)
m

df <- data.frame(
  location = c('site1','site2','site3'),
  species.no = c(25,30,2),
  forest = c(T,F,T)
)
df

mylist <- list(
  numbers <- c(1,2,3),
  species = c('seal','fox'),
  data = df
)
mylist

species[2] #selects the second species in the vector, i.e. chaffinch

m[3,2] #selects the third row and second column, i.e. 6





