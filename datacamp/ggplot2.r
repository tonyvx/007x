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
ggplot(mtcars, aes(x = mpg, y = group)) + scale_y_continuous(limits = c(-2, 2)) + geom_jitter()
