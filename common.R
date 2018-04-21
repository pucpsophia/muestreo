

 seq(10, 30, 2)
 
 trifecta <- c(6,8,2)
 repeated_trifecta <- rep(trifecta,4)
 repeated_trifecta
 
 
 num_matrix <- seq(5,100,5)
 
 dim(num_matrix) <-c(5,4)
 
 num_matrix
 
 # sample labels
 
 
 eye_color <- c(2,2,4,1,5,5,5,6,1,3,6,3,1,4)
 
 feye_color <- factor(eye_color)
 
 
 levels(feye_color) <- c("amber","blue", "brown","gray","green","hazel")
 
 feye_color
 
 
 library("MASS")
 data(Cars93)
 
 
 hist(Cars93$Price, xlab="Price (x $1,000)", xlim = c(0,70),
      main = "Prices of 93 Models of 1993 Cars",probability
      = TRUE)
 

 lines(density(Cars93$Price))

 
 barplot(table(Cars93$Type)) 
 
 
 type.frame <- data.frame(table(Cars93$Type))
 type.frame
 
 
 dotchart(type.frame[,2],type.frame[,1])
 
 
 plot(Cars93$Horsepower,Cars93$MPG.city, xlab="Horsepower",
      ylab="MPG City", main = "MPG City vs Horsepower",pch=16)
 
 
 ggplot(Cars93, aes(x=Cylinders,y=Horsepower)) +
   geom_boxplot()+
   geom_point()
 