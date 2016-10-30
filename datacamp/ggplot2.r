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
  
  








