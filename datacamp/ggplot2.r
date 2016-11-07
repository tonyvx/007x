# Plot the correct variables of mtcars
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Change cyl inside mtcars to a factor
mtcars$cyl <- as.factor(mtcars$cyl)

# Make the same plot as in the first instruction
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Basic plot
mtcars$cyl <- as.factor(mtcars$cyl)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)

# Use lm() to calculate a linear model and save it as carModel
carModel <- lm(mpg ~ wt, data = mtcars)

# Call abline() with carModel as first argument and set lty to 2
abline(carModel, lty = 2)

# Plot each subset efficiently with lapply
# You don't have to edit this code
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})

# This code will draw the legend of the plot
# You don't have to edit this code
legend(
  x = 5,
  y = 33,
  legend = levels(mtcars$cyl),
  col = 1:3,
  pch = 1,
  bty = "n"
)

library("ggplot2")
# Convert cyl to factor (don't need to change)
mtcars$cyl <- as.factor(mtcars$cyl)

# Example from base R (don't need to change)
plot(mtcars$wt, mtcars$mpg, col = mtcars$cyl)
abline(lm(mpg ~ wt, data = mtcars), lty = 2)
lapply(mtcars$cyl, function(x) {
  abline(lm(mpg ~ wt, mtcars, subset = (cyl == x)), col = x)
})
legend(
  x = 5,
  y = 33,
  legend = levels(mtcars$cyl),
  col = 1:3,
  pch = 1,
  bty = "n"
)

# Plot 1: add geom_point() to this command to create a scatter plot
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point()  # Fill in using instructions Plot 1

# Plot 2: include the lines of the linear models, per cyl
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point() +
  geom_smooth(method =lm, se = FALSE)
# Copy from Plot 1
___   # Fill in using instructions Plot 2

# Plot 3: include a lm for the entire dataset in its whole
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + 
  geom_point() + geom_smooth(method =lm, se = FALSE) + 
  geom_smooth(aes(1),method = lm,se = FALSE,linetype = 2)
___ +# Copy from Plot 2
  ___ +# Copy from Plot 2
  ___   # Fill in using instructions Plot 3

str(iris)

head(iris)

# Consider the structure of iris, iris.wide and iris.tidy (in that order)
str(iris)
str(iris.wide)
str(iris.tidy)

# Think about which dataset you would use to get the plot shown right
# Fill in the ___ to produce the plot given to the right
ggplot(iris.tidy, aes(x = Species, y = Value, col = Part)) + 
  geom_jitter() + 
  facet_grid(. ~Measure)

# Load the tidyr package
library(tidyr)

# Fill in the ___ to produce to the correct iris.tidy dataset
iris.tidy <- iris %>% 
  gather(key, Value, -Species) %>% separate(key, c("Part", "Measure"), "\\.")

# Consider the head of iris, iris.wide and iris.tidy (in that order)
head(iris)
head(iris.wide)
head(iris.tidy)

# Think about which dataset you would use to get the plot shown right
# Fill in the ___ to produce the plot given to the right
ggplot(iris.wide, aes(x = Length, y = Width, col = Part)) + 
  geom_jitter() +
  facet_grid(. ~ Species)

# Load the tidyr package
library(tidyr)

# Add column with unique ids (don't need to change)
iris$Flower <- 1:nrow(iris)

# Fill in the ___ to produce to the correct iris.wide dataset
iris.wide <- iris %>% 
  gather(key, value, -Flower, -Species) %>% separate(key, c("Part", "Measure"), "\\.") %>% spread(Measure, value)

# Map cyl to y
ggplot(mtcars, aes(x = mpg, y = cyl)) + geom_point()


# Map cyl to x
ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()


# Map cyl to col
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point()


# Change shape and size of the points in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + 
  geom_point(shape = 1, size = 4)

# Given from the previous exercise
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) +
  geom_point(shape = 1, size = 4)

# Map cyl to fill
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + geom_point()



# Change shape, size and alpha of the points in the above plot
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + 
  geom_point(shape = 16,size = 6, alpha = 0.6)

# Map cyl to size

ggplot(mtcars, aes(x = wt, y = mpg, size = cyl)) + geom_point()

# Map cyl to alpha

ggplot(mtcars, aes(x = wt, y = mpg, alpha = cyl)) + geom_point()

# Map cyl to shape
ggplot(mtcars, aes(x = wt, y = mpg, shape = cyl)) + geom_point()


# Map cyl to labels
ggplot(mtcars, aes(x = wt, y = mpg, label = cyl)) + geom_text()

# Define a hexadecimal color
my_color <- "#123456"

# Set the color aesthetic
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + geom_point()


# Set the color aesthetic and attribute
ggplot(mtcars, aes(x = wt, y = mpg, col = cyl)) + 
  geom_point(col = my_color)

# Set the fill aesthetic and color, size and shape attributes
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + 
  geom_point(size = 10, shape = 23, col = my_color)

# Expand to draw points with alpha 0.5
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + 
  geom_point(alpha = 0.5)


# Expand to draw points with shape 24 and color yellow
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl))  + 
  geom_point(shape = 24, col = "yellow")


# Expand to draw text with label x, color red and size 10
ggplot(mtcars, aes(x = wt, y = mpg, fill = cyl)) + 
  geom_text(label = "x", col ="red", size = 10)

# Map mpg onto x, qsec onto y and factor(cyl) onto col

ggplot(mtcars, aes(x = mpg, y = qsec, col = factor(cyl))) 
  + geom_point()

# Add mapping: factor(am) onto shape

ggplot(mtcars, aes( x = mpg, y = qsec, col = factor(cyl),shape = factor(am))) + 
  geom_point()

# Add mapping: (hp/wt) onto size
ggplot(mtcars, aes( x = mpg, y = qsec,  col = factor(cyl), shape = factor(am) ,  size = (hp / wt))) + 
  geom_point()

cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))
# The base layer, cyl.am, is available for you
# Add geom (position = "stack" by default)
cyl.am + geom_bar(position = "stack")

# Fill - show proportion
cyl.am +
  geom_bar(position = "fill")

# Dodging - principles of similarity and proximity
cyl.am +
  geom_bar(position = "dodge")

# Clean up the axes with scale_ functions
val = c("#E41A1C", "#377EB8")
lab = c("Manual", "Automatic")
cyl.am +
  geom_bar(position = "dodge") +
  scale_x_discrete("Cylinders") +
  scale_y_continuous("Number") +
  scale_fill_manual("Transmission",
                    values = val,
                    labels = lab)

# Add a new column called group
mtcars$group <- 0

# Create jittered plot of mtcars: mpg onto x, group onto y
ggplot(mtcars, aes(x = mpg, y = group)) + geom_jitter()

# Change the y aesthetic limits
ggplot(mtcars, aes(x = mpg, y = group)) + 
  scale_y_continuous(limits = c(-2, 2)) + geom_jitter()

# Basic scatter plot: wt on x-axis and mpg on y-axis; map cyl to col
ggplot(mtcars, aes(x=wt, y=mpg, col=cyl)) +
  geom_point(size=4)

# Hollow circles - an improvement
ggplot(mtcars, aes(x=wt, y=mpg, col=cyl)) +
  geom_point(size=4, shape=1)

# Add transparency - very nice
ggplot(mtcars, aes(x=wt, y=mpg, col=cyl)) +
  geom_point(size=4, shape=1, alpha=0.6)

# Scatter plot: carat (x), price (y), clarity (col)
ggplot(diamonds,aes(x=carat, y=price, col=clarity)) + 
  geom_point()

# Adjust for overplotting
ggplot(diamonds,aes(x=carat, y=price, col=clarity)) + 
  geom_point(alpha=0.5)

# Scatter plot: clarity (x), carat (y), price (col)
ggplot(diamonds,aes(x=clarity, y=carat, col=price)) + 
  geom_point(alpha=0.5)

# Dot plot with jittering
ggplot(diamonds,aes(x=clarity, y=carat, col=price)) + 
  geom_point(alpha=0.5,position = "jitter")

# The dataset mtcars is available for you

# Plot the cyl on the x-axis and wt on the y-axis
ggplot(mtcars, aes(x=cyl, y=wt))+ 
  geom_point()

# Use geom_jitter() instead of geom_point()
ggplot(mtcars, aes(x=cyl, y=wt))+ 
  geom_jitter()

# Define the position object using position_jitter(): posn.j
posn.j=position_jitter(width=0.1)
  
# Use posn.j in geom_point()
ggplot(mtcars, aes(x=cyl, y=wt))+ 
  geom_point(position = posn.j)

# Examine the structure of Vocab
str(Vocab)
# Basic scatter plot of vocabulary (y) against education (x). Use geom_point()
ggplot(Vocab,aes(x=education, y=vocabulary)) + 
  geom_point()

# Use geom_jitter() instead of geom_point()
ggplot(Vocab,aes(x=education, y=vocabulary)) + 
  geom_jitter()

  
# Using the above plotting command, set alpha to a very low 0.2
ggplot(Vocab,aes(x=education, y=vocabulary)) + 
  geom_jitter(alpha=0.2)
  
# Using the above plotting command, set the shape to 1
ggplot(Vocab,aes(x=education, y=vocabulary)) + 
  geom_jitter(alpha=0.2, shape=1)

  # Make a univariate histogram
ggplot(mtcars, aes(x=mpg) ) + 
  geom_histogram()

# Change the bin width to 1
ggplot(mtcars, aes(x=mpg) ) + 
  geom_histogram(binwidth=1)

# Change the y aesthetic to density
ggplot(mtcars, aes(x=mpg) ) + 
  geom_histogram(binwidth=1, aes(y=..density..))

# Custom color code
myBlue <- "#377EB8"

# Change the fill color to myBlue
ggplot(mtcars, aes(x=mpg) ) + 
  geom_histogram(binwidth=1, aes(y=..density..), fill=myBlue)

  # Draw a bar plot of cyl, filled according to am
ggplot(mtcars,aes(x=cyl,fill=am)) + 
  geom_bar()

# Change the position argument to stack
ggplot(mtcars,aes(x=cyl,fill=am)) + 
  geom_bar(position="stack")

# Change the position argument to fill
ggplot(mtcars,aes(x=cyl,fill=am)) + 
  geom_bar(position="fill")

# Change the position argument to dodge
ggplot(mtcars,aes(x=cyl,fill=am)) + 
  geom_bar(position="dodge")

  # Draw a bar plot of cyl, filled according to am
ggplot(mtcars, aes(x=cyl, fill=am)) + 
  geom_bar() 

# Change the position argument to "dodge"
ggplot(mtcars, aes(x=cyl, fill=am)) + 
  geom_bar(position="dodge")

# Define posn_d with position_dodge()
posn_d=position_dodge(width=0.2)

# Change the position argument to posn_d
ggplot(mtcars, aes(x=cyl, fill=am)) + 
  geom_bar(position=posn_d)

# Use posn_d as position and adjust alpha to 0.6
ggplot(mtcars, aes(x=cyl, fill=am)) + 
  geom_bar(position=posn_d, alpha=0.6)

  # A basic histogram, add coloring defined by cyl 
ggplot(mtcars, aes(mpg, fill=cyl)) +
  geom_histogram(binwidth = 1)

# Change position to identity 
ggplot(mtcars, aes(mpg, fill=cyl)) +
  geom_histogram(binwidth = 1, position="identity")

# Change geom to freqpoly (position is identity by default) 
ggplot(mtcars, aes(mpg, col=cyl)) +
  geom_freqpoly(binwidth = 1)

  # Example of how to use a brewed color palette
ggplot(mtcars, aes(x = cyl, fill = am)) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1")

# Use str() on Vocab to check out the structure
str(Vocab)

# Plot education on x and vocabulary on fill
# Use the default brewed color palette
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set1")

# Final plot of last exercise
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_brewer()
  
# Definition of a set of blue colors
blues <- brewer.pal(9, "Blues")

# Make a color range using colorRampPalette() and the set of blues
blue_range <- colorRampPalette(blues)

# Use blue_range to adjust the color of the bars, use scale_fill_manual()
ggplot(Vocab, aes(x = education, fill = vocabulary)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=blue_range(11))


# Basic histogram plot command
ggplot(mtcars, aes(mpg)) + 
  geom_histogram(binwidth = 1)

# Expand the histogram to fill using am
ggplot(mtcars, aes(mpg, fill=am)) + 
  geom_histogram(binwidth = 1, position="stack")

# Change the position argument to "dodge"
ggplot(mtcars, aes(mpg, fill=am)) + 
  geom_histogram(binwidth = 1, position="dodge")

# Change the position argument to "fill"
ggplot(mtcars, aes(mpg, fill=am)) + 
  geom_histogram(binwidth = 1, position="fill")

# Change the position argument to "identity" and set alpha to 0.4
ggplot(mtcars, aes(mpg, fill=am)) + 
  geom_histogram(binwidth = 1, position="identity", alpha=0.4)

# Change fill to cyl
ggplot(mtcars, aes(mpg, fill=cyl)) + 
  geom_histogram(binwidth = 1, position="identity", alpha=0.4)

# Print out head of economics
head(economics)

# Plot unemploy as a function of date using a line plot
ggplot(economics, aes(x = date, y = unemploy)) + 
  geom_line()
    
# Adjust plot to represent the fraction of total population that is unemployed
ggplot(economics, aes(x = date, y = unemploy/pop))+ 
  geom_line()

  # Expand the following command with geom_rect() to draw the recess periods
ggplot(economics, aes(x = date, y = unemploy/pop)) +
  geom_line() + 
  geom_rect(data=recess, inherit.aes=FALSE ,  
  aes(xmin=begin, xmax=end, ymin=-Inf, ymax=+Inf), fill="red",alpha=0.2)

# Check the structure as a starting point
str(fish.species)

# Use gather to go from fish.species to fish.tidy
fish.tidy <- gather(fish.species, Species,Capture, -Year)

# Recreate the plot shown on the right
ggplot(fish.tidy, aes(x = Year, y = Capture, col=Species)) + 
  geom_line()

# titanic is avaliable in your workspace

# Check out the structure of titanic
str(titanic)

# Use ggplot() for the first instruction
ggplot(titanic, aes(x=factor(Pclass), fill=factor(Sex)) ) + 
  geom_bar(position="dodge")


# Use ggplot() for the second instruction
ggplot(titanic, aes(x=factor(Pclass), fill=factor(Sex)) ) + 
  geom_bar(position="dodge") +
  facet_grid(".~Survived")

# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

# Use ggplot() for the last instruction
ggplot(titanic, aes(x=factor(Pclass),y=Age, col=factor(Sex))) + 
  geom_jitter(position=posn.j, size=3,alpha=0.5) +
  facet_grid(".~Survived")

# ggplot2 is already loaded

# Explore the mtcars data frame with str()
str(mtcars)

# A scatter plot with LOESS smooth:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + geom_smooth()

# A scatter plot with an ordinary Least Squares linear model:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  geom_smooth(method="lm")

# The previous plot, without CI ribbon:
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE)

# The previous plot, without points:
ggplot(mtcars, aes(x = wt, y = mpg)) + 
  geom_smooth(method="lm", se=FALSE)

  # ggplot2 is already loaded

# Define cyl as a factor variable
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F)

# Complete the following ggplot command as instructed
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F)+
  stat_smooth(method = "lm", se = F,group=1)

# Plot 1: change the LOESS span
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  # Add span below 
  geom_smooth(se = F,span=0.7)

# Plot 2: Set the overall model to LOESS and use a span of 0.7
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  # Change method and add span below
  stat_smooth(method = "loess", aes(group = 1), 
              se = F, col = "black", span=0.7)

# Plot 3: Set col to "All", inside the aes layer of stat_smooth()
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F) +
  stat_smooth(method = "loess",
              # Add col inside aes()
              aes(group = 1,col="All"), 
              # Remove the col argument below
              se = F, span = 0.7)

# Plot 4: Add scale_color_manual to change the colors
myColors <- c(brewer.pal(3, "Dark2"), "black")
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = F, span = 0.75) +
  stat_smooth(method = "loess", 
              aes(group = 1, col="All"), 
              se = F, span = 0.7) + 
# Add correct arguments to scale_color_manual
  scale_color_manual("Cylinders", values=myColors)

# Plot 1: Jittered scatter plot, add a linear model (lm) smooth
ggplot(Vocab, aes(x = education, y = vocabulary)) +
  geom_jitter(alpha = 0.2) +
  stat_smooth(method = "lm", se = F)

# Plot 2: Only lm, colored by year
ggplot(Vocab, aes(x = education, y = vocabulary, col = factor(year))) +
  stat_smooth(method = "lm", se = F)

# Plot 3: Set a color brewer palette
ggplot(Vocab, aes(x = education, y = vocabulary, col = factor(year))) +
  stat_smooth(method = "lm", se = F) +
  scale_color_brewer()

# Plot 4: Change col and group, specify alpha, size and geom, and add scale_color_gradient
ggplot(Vocab, aes(x = education, y = vocabulary, col = year, group = factor(year))) +
  stat_smooth(method = "lm", se = F, alpha = 0.6, size = 2) +
  scale_color_gradientn(colors = brewer.pal(9,"YlOrRd"))

# Use stat_quantile instead of stat_smooth:
ggplot(Vocab, aes(x = education, y = vocabulary, col = year, group = factor(year))) +
  stat_quantile( alpha = 0.6, size = 2) +
  scale_color_gradientn(colors = brewer.pal(9,"YlOrRd"))

# Set quantile to 0.5:
ggplot(Vocab, aes(x = education, y = vocabulary, col = year, group = factor(year))) +
  stat_quantile( alpha = 0.6, size = 2, quantiles=0.5) +
  scale_color_gradientn(colors = brewer.pal(9,"YlOrRd"))

  # Plot with linear and loess model
p <- ggplot(Vocab, aes(x = education, y = vocabulary)) +
       stat_smooth(method = "loess", aes(col = "red"), se = F) +
       stat_smooth(method = "lm", aes(col = "blue"), se = F) +
       scale_color_discrete("Model", labels = c("red" = "LOESS", "blue" = "lm"))

# Add stat_sum
p + stat_sum()

# Add stat_sum and set size range
p + stat_sum() +scale_size(range=c(1,10))

# Display structure of mtcars
str(mtcars)

# Convert cyl and am to factors:
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- as.factor(mtcars$am)

# Define positions:
posn.d <- position_dodge(width=0.1)
posn.jd <- position_jitterdodge(jitter.width=0.1,dodge.width=0.2)
posn.j <- position_jitter(width=0.2)

# base layers:
wt.cyl.am <-ggplot(mtcars, aes(x=cyl,y=wt, col=am, fill=am, group=am))

# wt.cyl.am, posn.d, posn.jd and posn.j are available

# Plot 1: Jittered, dodged scatter plot with transparent points
wt.cyl.am +
  geom_point(position = posn.jd, alpha = 0.6)
  
# Plot 2: Mean and SD - the easy way
wt.cyl.am + stat_summary(fun.data=mean_sdl, fun.args=list(mult=1),position=posn.d)

  
# Plot 3: Mean and 95% CI - the easy way
wt.cyl.am + stat_summary(fun.data=mean_cl_normal,position=posn.d)

  
# Plot 4: Mean and SD - with T-tipped error bars - fill in ___
wt.cyl.am +
  stat_summary(geom = "point", fun.y = mean, 
               position = posn.d) +
  stat_summary(geom = "errorbar", fun.data = mean_sdl , 
               position = posn.d, fun.args = list(mult = 1), width = 0.1)

  # Play vector xx is available

# Function to save range for use in ggplot 
gg_range <- function(x) {
  # Change x below to return the instructed values
  data.frame(ymin = min(x), # Min
             ymax = max(x)) # Max
}

gg_range(xx)
# Required output:
#   ymin ymax
# 1    1  100

# Function to Custom function:
med_IQR <- function(x) {
  # Change x below to return the instructed values
  data.frame(y = median(x), # Median
             ymin = quantile(x)[2], # 1st quartile
             ymax = quantile(x)[4])  # 3rd quartile
}

med_IQR(xx)
# Required output:
#        y  ymin  ymax
# 25% 50.5 25.75 75.25

# The base ggplot command, you don't have to change this
wt.cyl.am <- ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am, group = am))

# Add three stat_summary calls to wt.cyl.am
wt.cyl.am + 
  stat_summary(geom = "linerange", fun.data =med_IQR, 
               position = posn.d, size = 3) +
  stat_summary(geom = "linerange", fun.data = gg_range, 
               position = posn.d, size = 3, 
               alpha = 0.4) +
  stat_summary(geom = "point", fun.y = median, 
               position = posn.d, size = 3, 
               col = "black"
              , shape = "X")


# Basic ggplot() command, coded for you
p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) + 
  geom_point() + 
  geom_smooth()

# Add scale_x_continuous
p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) + 
  geom_point() + 
  geom_smooth() + 
  scale_x_continuous(limits = c(3, 6),expand = c(0, 0))

# The proper way to zoom in:
p <- ggplot(mtcars, aes(x = wt, y = hp, col = am)) + 
  geom_point() + 
  geom_smooth() + 
  coord_cartesian(xlim = c(3, 6))

  # Complete basic scatter plot function
base.plot <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
               geom_jitter() +
               geom_smooth(method = "lm", se = F)

# Plot base.plot: default aspect ratio
base.plot

# Fix aspect ratio (1:1) of base.plot
base.plot + coord_equal(ratio=1)

# Create stacked bar plot: thin.bar
thin.bar <- ggplot(mtcars, aes(x=1, fill=cyl)) +
              geom_bar()

# Convert thin.bar to pie chart
thin.bar + coord_polar(theta="y")

# Create stacked bar plot: wide.bar
wide.bar <- ggplot(mtcars, aes(x=1, fill=cyl)) +
              geom_bar(width=1) 


# Convert wide.bar to pie chart
wide.bar + coord_polar(theta="y")
# Basic scatter plot:
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()

# Separate rows according to transmission type, am
p + facet_grid(am~.)

# Separate columns according to cylinders, cyl
p + facet_grid(.~cyl)

# Separate by both columns and rows 
p + facet_grid(am~cyl)

# Code to create the cyl_am col and myCol vector
mtcars$cyl_am <- paste(mtcars$cyl, mtcars$am, sep = "_")
myCol <- rbind(brewer.pal(9, "Blues")[c(3,6,8)],
               brewer.pal(9, "Reds")[c(3,6,8)])

# Basic scatter plot, add color scale:
ggplot(mtcars, aes(x = wt, y = mpg, col=cyl_am)) +
  geom_point() + 
  scale_color_manual(values=myCol)
  
# Facet according on rows and columns.
ggplot(mtcars, aes(x = wt, y = mpg, col=cyl_am)) +
  geom_point() + 
  scale_color_manual(values=myCol) + 
  facet_grid(gear~vs)

# Add more variables
ggplot(mtcars, aes(x = wt, y = mpg, col=cyl_am, size=disp)) +
  geom_point() + 
  scale_color_manual(values=myCol) + 
  facet_grid(gear~vs)

  # Basic scatter plot
ggplot(mamsleep, aes(x=time,y=name, col=sleep)) + 
  geom_point()

# Facet rows accoding to vore
ggplot(mamsleep, aes(x=time,y=name, col=sleep)) + 
  geom_point() + 
  facet_grid(vore~.)

# Specify scale and space arguments to free up rows
ggplot(mamsleep, aes(x=time,y=name, col=sleep)) + 
  geom_point() + 
  facet_grid(vore~.,scale = "free_y" , space = "free_y")

# Plot 1: change the plot background color to myPink:
z+theme(plot.background=element_rect(fill = myPink))

# Plot 2: adjust the border to be a black line of size 3
z+theme(plot.background=element_rect(fill = myPink, color="black", size=3))

# Plot 3: set panel.background, legend.key, legend.background and strip.background to element_blank()
uniform_panels <- theme(panel.background = element_blank(), 
                        legend.key = element_blank(), 
                        legend.background=element_blank(), 
                        strip.background = element_blank())
z+theme(plot.background=element_rect(fill = myPink, color="black", size=3)) + uniform_panels

# Extend z with theme() function and three arguments
 z+theme(panel.grid= element_blank(), 
         axis.line=element_line("black"), 
         axis.ticks=element_line("black"))

 z+theme(strip.text=element_text(size=16, color=myRed), 
         axis.title.y=element_text(hjust=0, color=myRed,face="italic"),
         axis.title.x=element_text(hjust=0, color=myRed, face="italic"),
         axis.text=element_text(color="black"))

# Move legend by position
z +theme(legend.position=c(0.85, 0.85))

# Change direction
z +theme(legend.direction="horizontal")

# Change location by name
z +theme(legend.position="bottom")

# Remove legend entirely
z +theme(legend.position="none")

# Increase spacing between facets
library("grid")
z+ theme(panel.margin.x=unit(2,"cm"))

# Add code to remove any excess plot margin space
library("grid")
z+ theme(panel.margin.x=unit(2,"cm"), 
         plot.margin=unit(c(0,0,0,0), "cm"))

# Theme layer saved as an object, theme_pink
theme_pink <- theme(panel.background = element_blank(),
                    legend.key = element_blank(),
                    legend.background = element_blank(),
                    strip.background = element_blank(),
                    plot.background = element_rect(fill = myPink, color = "black", size = 3),
                    panel.grid = element_blank(),
                    axis.line = element_line(color = "black"),
                    axis.ticks = element_line(color = "black"),
                    strip.text = element_text(size = 16, color = myRed),
                    axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
                    axis.text = element_text(color = "black"),
                    legend.position = "none")
  
# Apply theme_pink to z2
z2 + theme_pink

# Change code so that old theme is saved as old

old <-theme_update(panel.background = element_blank(),
             legend.key = element_blank(),
             legend.background = element_blank(),
             strip.background = element_blank(),
             plot.background = element_rect(fill = myPink, color = "black", size = 3),
             panel.grid = element_blank(),
             axis.line = element_line(color = "black"),
             axis.ticks = element_line(color = "black"),
             strip.text = element_text(size = 16, color = myRed),
             axis.title.y = element_text(color = myRed, hjust = 0, face = "italic"),
             axis.title.x = element_text(color = myRed, hjust = 0, face = "italic"),
             axis.text = element_text(color = "black"),
             legend.position = "none")

# Display the plot z2
z2 + theme_pink

# Restore the old plot
theme_set(old)
z2 + theme_pink

# Load ggthemes package
library(ggthemes)


# Apply theme_tufte
z2 + theme_tufte()


# Apply theme_tufte, modified:
z2 + theme_tufte() + 
     theme(legend.position = c(0.9, 0.9),
           legend.title = element_text(face = "italic", size =12),
           axis.title=element_text(face = "bold", size =14))

# Base layers
m <- ggplot(mtcars, aes(x = cyl, y = wt))

# Draw dynamite plot
m +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "skyblue") +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1), 
               geom = "errorbar", 
               width = 0.1)

  # Base layers
m <- ggplot(mtcars, aes(x = cyl,y = wt, col = am, fill = am))

# Plot 1: Draw dynamite plot
m +
  stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1)

# Plot 2: Set position dodge in each stat function
m +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.1, position = "dodge")

# Set your dodge posn manually
posn.d <- position_dodge(0.9)

# Plot 3:  Redraw dynamite plot
m +
  stat_summary(fun.y = mean, geom = "bar", position = posn.d) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, position = posn.d)


  # Base layers
m <- ggplot(mtcars.cyl, aes(x = cyl, y = wt.avg))

# Plot 1: Draw bar plot
m + geom_bar(stat="identity", fill="skyblue")

# Plot 2: Add width aesthetic
m + geom_bar(aes(width=prop),stat="identity", fill="skyblue")

# Plot 3: Add error bars
m + geom_bar(aes(width=prop),stat="identity", fill="skyblue") +
geom_errorbar(aes(ymin = wt.avg - sd , ymax = wt.avg + sd), width=0.1)

# Convert bar chart to pie chart
ggplot(mtcars, aes(x = factor(1), fill=am))+
      coord_polar(theta="y") + 
      geom_bar( position="fill",width=1)+ 
      facet_grid(.~cyl)


# Parallel coordinates plot using GGally
library(GGally)

# All columns except am
group_by_am <- 9
my_names_am <- (1:11)[-group_by_am]

# Basic parallel plot - each variable plotted as a z-score transformation
ggparcoord(mtcars, my_names_am, groupColumn = group_by_am, alpha = 0.8)

# Create color palette
myColors <- brewer.pal(9, "Reds")

# Build the heat map from scratch
 ggplot(barley, aes(x=year, y=variety, fill=yield)) + 
  geom_tile() + 
  facet_wrap(~site, ncol=1) + 
  scale_fill_gradientn(colors=myColors)

 


 







  
  








