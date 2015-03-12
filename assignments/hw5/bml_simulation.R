#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

#Below are lists which record gridlock for three matrices: 10x10, 6x6, and 4x4.
#Each matrix is run 1000 times for values of p at 0.25, 0.3, 0.5, 0.75, and 0.9. 


#There are three sample matrices illustrated, one for each size. 
matrix10 <- matrix(sample(0:2, size=100, replace=T), ncol=10)
image(matrix10, col=c("white", "red", "blue"))
image(t(matrix10), col=c("white", "red", "blue"))
help(pdf)
pdf(file='/Users/evanbell/src/stat133/hw5/matrix10_plot.pdf')
image(matrix10, col=c("white", "red", "blue"))
image(t(matrix10), col=c("white", "red", "blue"))
dev.off()

matrix6 <- matrix(sample(0:2, size=36, replace=T), ncol=6)
image(matrix6, col=c("white", "red", "blue"))
image(t(matrix6), col=c("white", "red", "blue"))
help(pdf)
pdf(file='/Users/evanbell/Documents/matrix6_plot.pdf')
image(matrix6, col=c("white", "red", "blue"))
image(t(matrix6), col=c("white", "red", "blue"))
dev.off()

matrix4 <- matrix(sample(0:2, size=16, replace=T), ncol=4)
image(matrix4, col=c("white", "red", "blue"))
image(t(matrix4), col=c("white", "red", "blue"))
help(pdf)
pdf(file='/Users/evanbell/Documents/matrix4_plot.pdf')
image(matrix4, col=c("white", "red", "blue"))
image(t(matrix4), col=c("white", "red", "blue"))
dev.off()


sim10a <- replicate(1000, bml.sim(10,10,0.25))
sim10b <- replicate(1000, bml.sim(10,10,0.5))
sim10c <- replicate(1000, bml.sim(10,10,0.75))
sim10d <- replicate(1000, bml.sim(10,10,0.9))
sim10e <- replicate(1000, bml.sim(10,10,0.3))


sim6a <- replicate(1000, bml.sim(6,6,0.25))
sim6b <- replicate(1000, bml.sim(6,6,0.5))
sim6c <- replicate(1000, bml.sim(6,6,0.75))
sim6d <- replicate(1000, bml.sim(6,6,0.9))
sim6e <- replicate(1000, bml.sim(6,6,0.3))


sim4a <- replicate(1000, bml.sim(4,4,0.25))
sim4b <- replicate(1000, bml.sim(4,4,0.5))
sim4c <- replicate(1000, bml.sim(4,4,0.75))
sim4d <- replicate(1000, bml.sim(4,4,0.9))
sim4e <- replicate(1000, bml.sim(4, 4, 0.3))



boxplot(sim10a, sim10e, sim10b, sim10c, sim10d, names = c("0.25","0.3", "0.5", "0.75", "0.9"), xlab = "Value of P",
        ylab= "Number of Moves Until Gridlock", main = "10x10 Matrix Simulation")

boxplot(sim10c, sim10d, names = c("0.75", "0.9"), xlab = "Value of P",
        ylab= "Number of Moves Until Gridlock", main = "10x10 Matrix Simulation", ylim = c(0,175))

boxplot(sim6a, sim6e, sim6b, sim6c, sim6d, names = c("0.25","0.3", "0.5", "0.75", "0.9"), xlab = "Value of P",
        ylab= "Number of Moves Until Gridlock", main = "6x6 Matrix Simulation")

boxplot(sim6c, sim6d, names = c("0.75", "0.9"), xlab = "Value of P",
        ylab= "Number of Moves Until Gridlock", main = "6x6 Matrix Simulation", ylim = c(0,150))

boxplot(sim4a, sim4e, sim4b, sim4c, sim4d, names = c("0.25", "0.3", "0.5", "0.75", "0.9"), xlab = "Value of P",
        ylab= "Number of Moves Until Gridlock", main = "4x4 Matrix Simulation")

boxplot(sim4d, names = c("0.9"), xlab = "Value of P = 0.9",
        ylab= "Number of Moves Until Gridlock", main = "4x4 Matrix Simulation", ylime = c(0, 30))

