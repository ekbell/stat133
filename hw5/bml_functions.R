#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  probs = c(1-p, p/2, p/2)
  x <- c(0, 1, 2)
  numbers <- sample(x, size = r*c, replace = TRUE, prob = probs)
  m <- matrix(data = numbers, nrow = r, ncol = c)
   return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

blocked_red <- function(m){
  new_m <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  for(i in 1:nrow(m)){
    for(j in 1:(ncol(m)-1)){
      if(m[i,j] == 1){
        if(m[i,j+1]== 0){
          new_m[i, j] = FALSE}
        else{new_m[i,j] = TRUE}
      }
      if(m[i,j] != 1){new_m[i,j] = FALSE}
    }
    if(m[i,ncol(m)] == 1){
      if(m[i,1]== 0){
        new_m[i, ncol(m)] = FALSE}
      else{new_m[i,ncol(m)] = TRUE}
    }
    if(m[i,j] != 1){new_m[i,j] = FALSE}
  }
  return(new_m)
}


blocked_blue <- function(m){
  new_m <- matrix(0, nrow = nrow(m), ncol = ncol(m))
  for(j in 1:ncol(m)){
    if(m[1,j] == 2){
      if(m[nrow(m),j]== 0){new_m[1, j] = FALSE}
      if(m[nrow(m),j] != 0){new_m[1, j] = TRUE}
    }
    if(m[1,j] != 2){new_m[1, j] = FALSE}
  }
  for(i in 2:nrow(m)){
    for(j in 1:ncol(m)){
      if(m[i,j] != 2){new_m[i,j] = FALSE}
      if(m[i,j] == 2){
        if(m[i-1,j]== 0){new_m[i, j] = FALSE}
        if(m[i-1,j] != 0){new_m[i,j] = TRUE}
      }
      if(m[i,j] != 2){new_m[i,j] = FALSE}
    }}
  return(new_m)
}

red_cars <- function(m){
  new_m <- matrix(, nrow = nrow(m), ncol = ncol(m))
  for(i in 1:nrow(m)){
    for(j in 1:ncol(m)){
      if(m[i,j] == 1){new_m[i,j] =1}
      if(m[i,j] != 1){new_m[i,j] =0}
    }}
  return(new_m)
}

blue_cars <- function(m){
  return(2*(m==2))
}

shift_right <- function(mat){
  mat = mat[,c(ncol(mat), 1:ncol(mat)-1)]
  return(mat)}

shift_up <- function(mat){
  mat = mat[c(2:(nrow(mat)),1),]
  return(mat)}


bml.step <- function(m){
original_m <- m
red_blocked <- red_cars(m)*blocked_red(m)
red_not_blocked <- red_cars(m)*!blocked_red(m)
m <- blue_cars(m) + shift_right(red_not_blocked) + red_blocked

blue_blocked <- blue_cars(m)*blocked_blue(m)
blue_not_blocked <- blue_cars(m)*!blocked_blue(m)
m <- shift_up(blue_not_blocked) + (m == 1) + blue_blocked
grid.new <- "TRUE"
if(all(m == original_m)){grid.new = "FALSE"}
   return(list(m, grid.new))
}


  

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m <- bml.init(r, c, p)
  max_it <- 1000
  new_list <- bml.step(m)
  for(i in 2:max_it){
    new_list <- bml.step(new_list[[1]])
    if(new_list[[2]] == FALSE){
       return(i)
    }
  }
  return(max_it)
}





