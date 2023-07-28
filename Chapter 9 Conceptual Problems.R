#Chapter 9 Conceptual Problem 1:

#a)
X1=seq(-1,1,0.1)
plot(X1,1+3*X1,xlab='X1',ylab='X2',type='l',xlim=c(-1,1),ylim=c(-1,4))

for(i in seq(-1,1,length.out = 25)){
  pts=data.frame(rep(i,25),seq(-1,4,length.out = 25))
  points(pts,col=ifelse(1+3*pts[,1]-pts[,2]>0,'red','purple'))
}

#b)
X1=seq(-1,1,0.1)
plot(X1,1+3*X1,xlab='X1',ylab='X2',type='l',xlim=c(-1,1),ylim=c(-1,4))
lines(X1,1-1/2*X1)

for(i in seq(-1,1,length.out = 25)){
  pts=data.frame(rep(i,25),seq(-1,4,length.out = 25))
  points(pts,col=ifelse(1+3*pts[,1]-pts[,2]>0,'red','purple'),pch=ifelse(-2+pts[,1]+2*pts[,2]>0,1,2))
}



#Chapter 9 Conceptual Problem 2:
#a)
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)

#b)
plot(NA, NA, type = "n", xlim = c(-4, 2), ylim = c(-1, 5), asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "< 4")
text(c(-4), c(2), "> 4")

#c)
#It is sufficient to substitute the equation's points' coordinates for X1 and X2, then check to see if the result is less than or equal to 4. 
#We have 5>4 (blue class) for (0,0), 14 (red class), 9>4 (blue class), 52>4 (blue class), and 14 (red class) for (1,1), (2,2), and (3,8).

plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
