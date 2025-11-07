# create factors with value labels 
mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1),labels=c("Automatic","Manual")) 
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),labels=c("4cyl","6cyl","8cyl")) 

# Kernel density plots for mpg
# grouped by number of gears (indicated by color)
qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5), 
      main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
      ylab="Density")

# Scatterplot of mpg vs. hp for each combination of gears and cylinders
# in each facet, transmittion type is represented by shape and color
qplot(hp, mpg, data=mtcars, shape=am, color=am, 
      facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon") 

# Separate regressions of mpg on weight for each number of cylinders
qplot(wt, mpg, data=mtcars, geom=c("point", "smooth"), 
      method="lm", formula=y~x, color=cyl, 
      main="Regression of MPG on Weight", 
      xlab="Weight", ylab="Miles per Gallon")

# Boxplots of mpg by number of gears 
# observations (points) are overlayed and jittered
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"), 
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")
##################################################################
set.seed(456)
n <- 20
dfr <- data.frame(
        id=rep(1:n, 2),
        group=rep(c("1","2"), each=n), value=c(rnorm(n), rnorm(n, sd=1.1))
)
require(ggplot2)

ggplot(data=dfr, mapping=aes(x=id, y=value)) +
        geom_line(mapping=aes(colour=group)) +
        geom_hline(yintercept=c(-1,1)*qnorm(0.95), color="orange") +
        geom_hline(yintercept=c(-1,1)*qnorm(0.99), color="darkred")
####################################################################
df_95ci <- data.frame(y_values=c(-1,1)*qnorm(0.95)) 
df_99ci <- data.frame(y_values=c(-1,1)*qnorm(0.99))
ggplot(data=dfr, mapping=aes(x=id, y=value)) +
        geom_line(mapping=aes(colour=group)) +
        geom_hline(data= df_95ci, mapping=aes(yintercept=y_values, colour="A")) +
        geom_hline(data= df_99ci, mapping=aes(yintercept=y_values, colour="B"))
