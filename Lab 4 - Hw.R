## Write the R code using ggplot function to recreate the graph from the Lecture 8 notes
library(ggplot2)

x1 = seq(-3, 3, by = 0.01)
y1 = dnorm(x1)
coord.x1 = c(qnorm(0.95), seq(qnorm(0.95), 3, by = 0.01), 3)
coord.y1 = c(0, dnorm(seq(qnorm(0.95), 3, by = 0.01)), 0)

x2 = seq(0, 6, by = 0.01)
y2 = dnorm(x2, mean = 3.2)
coord.x2 = c(0, seq(0, qnorm(0.95), by = 0.01), qnorm(0.95))
coord.y2 = c(0, dnorm(seq(0, qnorm(0.95), by = 0.01), mean = 3.2), 0)

ggplot() + theme_classic() +
  geom_line(aes(x1, y1), col = "blue", lwd = 1.3) +
  geom_line(aes(x2, y2), col = "red", lwd = 1.3) +
  geom_vline(xintercept = qnorm(0.95), linetype = 5, lwd = 1.1) +
  geom_polygon(aes(coord.x1, coord.y1, fill = "   Type I error"), alpha = 0.5, col = 1) +
  geom_polygon(aes(coord.x2, coord.y2, fill = "   Type II error"), alpha = 0.5, col = 1) +
  scale_fill_manual(values = c("blue", "red")) +
  geom_text(aes(1.05, 0.02), label = expression(beta), size = 7) + 
  geom_text(aes(2, 0.02), label = expression(alpha), size = 7, col = "white") +
  theme(panel.border = element_rect(colour = "black", fill=NA), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.ticks.length = unit(0.3, "cm"),
        legend.position = c(0.123, 0.92),
        legend.title = element_blank(),
        legend.background = element_rect(color = "black"),
        legend.text = element_text(size = 15)) +
  scale_x_continuous(breaks = c(0, 3), labels = c(expression(theta[0]), 
                                                  expression(theta[1])))
  


## Implement a function that will check if a given positive integer is a prime number.

is.wholenumber <- function(x) {
  if(abs(x - round(x)) < 0.0000001) {
    return(TRUE)
  }
  else {
    return(FALSE)
  }
} 

is.prime = function(x) {
  if (x <= 0 | is.wholenumber(x) == FALSE | abs(x - 1) < 0.000000001) {
    return("Please enter a positive integer (not 1) as parameter.")
  }
  else {
    if(x > 1) {
      y = 1
      for(i in 2:(x-1)) {
        if ((x %% i) == 0) {
          y = 0
          break
        }
      }
    } 
    if(x == 2)    y = 1
    if(y == 1) {
      print(paste(x,"is a prime number."))
    } else {
      print(paste(x,"is not a prime number."))
    }
  }
}
