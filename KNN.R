# Statistical Methods for Machine Learning, 2015
# K-nearest neighbor algorithm - a set of functions that will let you
# execute a basic KNN
#Functions
#Create function to determine distance
distance <- function(x, y) sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2 )
#Create function to list nearest points
find_nearest <- function(data, point, k) 
  sort(apply(data[,1:2], 1, function(x) distance(x, point)))[1:k]
#Make matrix with distance and species 
make_matrix <- function(data, point) 
  apply(data, 1, function(data1=data, point1=point) 
    c(distance(data1[1:2], point1), data1[3]))
#Find classification based on nearest point and distance matrix
list_species <- function(nearest, dist_matrix) 
  lapply(names(nearest), function(x) dist_matrix[2,x])
#Sum up all the counts
count_species <- function(list_of_species) {
  x <- as.numeric(list_of_species[1])
  matrix(c(0, 1, 2, 3,
           sum(list_of_species == 0), 
           sum(list_of_species == 1), 
           sum(list_of_species == 2),
           x),
         nrow=2, ncol=4, byrow = T)}
# Pick the nearest neighbor
nearest_neighbor <- function(count_mat) {
  if ((max(count_mat[2,1:3]) == count_mat[2,1]) & 
        (max(count_mat[2,1:3]) != count_mat[2,2]) & 
        (max(count_mat[2,1:3]) != count_mat[2,3])) 
  {species <- 0}
  else if ((max(count_mat[2,1:3]) != count_mat[2,1]) & 
             (max(count_mat[2,1:3]) == count_mat[2,2]) & 
             (max(count_mat[2,1:3]) != count_mat[2,3]))
  {species <- 1}
  else if ((max(count_mat[2,1:3]) != count_mat[2,1]) & 
             (max(count_mat[2,1:3]) != count_mat[2,2]) & 
             (max(count_mat[2,1:3]) == count_mat[2,3]))
  {species <- 2}
  else {species <- count_mat[2,4]}}
# Function combining above that finds KNN for given point
KNN <- function(train, testp, k) {
  topk <- find_nearest(train, testp, k)
  dist_matrix <- make_matrix(train, testp)
  list_sp <- list_species(topk, dist_matrix)
  count_sp <- count_species(list_sp)
  nn <- nearest_neighbor(count_sp)}
#Applying KNN to testing and training data
New_KNN <- function(train, test, k) {
  KNN_list <- apply(test, 1, function(x) KNN(train, x, k))
}
#Find errors
error_matrix <- function(test) {
  a <- sum(test$'k=1' != test$V3)
  b <- sum(test$'k=3' != test$V3)
  c <- sum(test$'k=5' != test$V3)
  d <- as.numeric(1-(a/length(test$'k=1')))
  e <- as.numeric(1-(b/length(test$'k=1')))
  f <- as.numeric(1-(c/length(test$'k=1')))
  matrix(c(1,3,5,a,b,c,d,e,f), nrow = 3, ncol = 3, byrow = T, dimnames = list(c('k','errors','accuracy')))
}  


