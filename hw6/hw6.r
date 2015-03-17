# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has_adopted <- matrix(0, nrow= n.doctors, ncol = n.days)
  has_adopted[,1] <- initial.doctors
  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)
  for(i in 2:n.days){
    has_adopted[,i] <- has_adopted[, i-1]
    docs <- sample(1:n.doctors, 2, replace = FALSE)
    doc.one.adopt <- has_adopted[docs[1],i-1]
    doc.two.adopt <- has_adopted[docs[2],i-1]
    if(doc.one.adopt == 1 & doc.two.adopt == 0){
      has_adopted[docs[2], i] <- sample(0:1, 1, replace = FALSE, prob = c(1-p,p))
    }
    if(doc.two.adopt == 1 & doc.one.adopt == 0){
      has_adopted[docs[1], i] <- sample(0:1, 1, replace = FALSE, prob = c(1-p,p))
    }
  }
  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output
  return(has_adopted)
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
init.doctors <- sample(0:1, 10, replace = TRUE, prob = c(0.9,0.1))

# Run your function for at least 5 different values of <p> and plot

sim3 <- sim.doctors(init.doctors, 10, 10, 0.3)
sim4 <- sim.doctors(init.doctors, 10, 10, 0.4)
sim5 <- sim.doctors(init.doctors, 10, 10, 0.5)
sim6 <- sim.doctors(init.doctors, 10, 10, 0.6)
sim8 <- sim.doctors(init.doctors, 10, 10, 0.8)
sim9 <- sim.doctors(init.doctors, 10, 10, 0.9)

plot(c(1:10), colSums(sim3), ylim = c(0, 7), cex = 0.2, col = "red", main = "Total Doctors Per Day for Values of P", 
     xlab = "Days", ylab = "Number of Doctors")
lines(c(1:10), colSums(sim4), col = "orange")
lines(c(1:10), colSums(sim5), col = "yellow")
lines(c(1:10), colSums(sim6), col = "green")
lines(c(1:10), colSums(sim8), col = "blue")
lines(c(1:10), colSums(sim9), col = "purple")
legend("topleft", c("0.3", "0.4", "0.5", "0.6", "0.8", "0.9"), col = par("col"), 
       fill = c("red", "orange", "gold", "green", "blue", "purple"), cex = 0.7, y.intersp = 0.5)

# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

