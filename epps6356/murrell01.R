### Paul Murrell's R examples (selected)
library(readxl)
## Start plotting from basics 
# Note the order
plot(pressure, pch=16)  # Can you change pch? Yes
text(150, 600, 
     "Pressure (mm Hg)\nversus\nTemperature (Celsius)")

#  Examples of standard high-level plots 
#  In each case, extra output is also added using low-level 
#  plotting functions.
# 

# Setting the parameter (3 rows by 2 cols)
par(mfrow=c(3, 2))

# Scatterplot
# Note the incremental additions

x <- c(0.5, 2, 4, 8, 12, 16)
y1 <- c(1, 1.3, 1.9, 3.4, 3.9, 4.8)
y2 <- c(4, .8, .5, .45, .4, .3)

# Setting label orientation, margins c(bottom, left, top, right) & text size
par(las=1, mar=c(4, 4, 2, 4), cex=.7) 
plot.new()
plot.window(range(x), c(0, 6))
lines(x, y1)
lines(x, y2)
points(x, y1, pch=16, cex=2.5) # Try different cex value?  
points(x, y2, pch=21, bg="white", cex=2.5)  # Different background color
par(col="gray50", fg="gray50", col.axis="gray50")
axis(1, at=seq(0, 16, 4)) # What is the first number standing for? Refers to which axis/side
axis(2, at=seq(0, 6, 2))
axis(4, at=seq(0, 6, 2))
box(bty="u")
mtext("Travel Time (s)", side=1, line=2, cex=0.8)
mtext("Responses per Travel", side=2, line=2, las=0, cex=0.8)
mtext("Responses per Second", side=4, line=2, las=0, cex=0.8)
text(4, 5, "Bird 131")
par(mar=c(5.1, 4.1, 4.1, 2.1), col="black", fg="black", col.axis="black")

# Histogram
# Random data
Y <- rnorm(50)
# Make sure no Y exceed [-3.5, 3.5]
Y[Y < -3.5 | Y > 3.5] <- NA # Selection/set range
x <- seq(-3.5, 3.5, .1)
dn <- dnorm(x)
par(mar=c(4.5, 4.1, 3.1, 0))
hist(Y, breaks=seq(-3.5, 3.5), ylim=c(0, 0.5), 
     col="gray80", freq=FALSE)
lines(x, dnorm(x), lwd=2)
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Barplot
par(mar=c(2, 3.1, 2, 2.1)) 
midpts <- barplot(VADeaths, 
                  col=gray(0.1 + seq(1, 9, 2)/11), 
                  names=rep("", 4))
mtext(sub(" ", "\n", colnames(VADeaths)),
      at=midpts, side=1, line=0.5, cex=0.5)
text(rep(midpts, each=5), apply(VADeaths, 2, cumsum) - VADeaths/2,
     VADeaths, 
     col=rep(c("white", "black"), times=3:2), 
     cex=0.8)
par(mar=c(5.1, 4.1, 4.1, 2.1))  

# Boxplot
par(mar=c(3, 4.1, 2, 0))
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset= supp == "VC", col="white",
        xlab="",
        ylab="tooth length", ylim=c(0,35))
mtext("Vitamin C dose (mg)", side=1, line=2.5, cex=0.8)
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        
        subset= supp == "OJ")
legend(1.5, 9, c("Ascorbic acid", "Orange juice"), 
       fill = c("white", "gray"), 
       bty="n")
par(mar=c(5.1, 4.1, 4.1, 2.1))

# Persp
x <- seq(-10, 10, length= 30)
y <- x
f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z <- outer(x, y, f)
z[is.na(z)] <- 1
# 0.5 to include z axis label
par(mar=c(0, 0.5, 0, 0), lwd=0.5)
persp(x, y, z, theta = 30, phi = 30, 
      expand = 0.5)
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)

# Piechart
par(mar=c(0, 2, 1, 2), xpd=FALSE, cex=0.5)
pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry",
                      "Apple", "Boston Cream", "Other", "Vanilla")
pie(pie.sales, col = gray(seq(0.3,1.0,length=6))) 

#
#
#
# HPI Data
df <- read_excel('myHPI.xlsx')
df <- na.omit(df)
#
#
#
#

# Reset window
par(mfrow = c(1,1))                 
par(mar = c(5.1, 4.1, 4.1, 2.1))     
par(las = 0, cex = 1)                

# Scatterplot of HPI data: Life Expectancy vs Wellbeing
plot(df$WB, df$LE, 
     main = "Scatterplot of Well Being vs. Life Expectancy", 
     xlab = "Well Being (0-10)",                       
     ylab = "Life Expectancy",                       
     pch = 18,                          
     col = "deepskyblue4",
     cex = 1.5,
     bty = 'l'
     )          
abline(lm(df$LE ~ df$WB),
       col = 'deepskyblue',
       lwd = 2)

# Histogram of GDP per Capita

hist_density <- density(df$GDP)
hist(df$GDP,
     breaks = 7,
     freq = FALSE,
     col = 'azure3',
     main = 'Histogram of GDP per Capita',
     xlab = 'GDP'
     )
lines(hist_density,
      col = 'cyan4',
      lwd = 2)

# Barplot of GDP by Continent

continent_labels <- c(
  "1" = "Latin\nAmerica",
  "2" = "N. America\n& Oceania",
  "3" = "Western\nEurope",
  "4" = "Middle\nEast",
  "5" = "Africa",
  "6" = "South\nAsia",
  "7" = "Eastern Europe &\nCentral Asia",
  "8" = "East Asia"
)

df_summary <- aggregate(
  GDP ~ Continent,
  data = df,
  FUN = mean
)

df_summary <- df_summary[order(df_summary$GDP, decreasing = TRUE), ]
df_summary$Continent <- continent_labels[as.character(df_summary$Continent)]

par(mar = c(9, 7, 4, 2)) 
par(mgp = c(4.5, 1, 0))   
barplot(
  height = df_summary$GDP,
  names.arg = df_summary$Continent,

  # Plot Aesthetics
  main = "Average GDP by Continent",
  ylab = "Average GDP",
  ylim = c(0, 60000),
  las = 2,
  col = "azure3"
)
# par(mar = c(5.1, 4.1, 4.1, 2.1))     
# par(mgp = c(3, 1, 0))

# Boxplot of GDP by Continent

df$Continent <- factor(df$Continent,
                       levels = 1:8,
                       labels = continent_labels)

boxplot(CF ~ Continent,
        data = df,
        main = "Distribution of Carbon Footprint by Continent",
        ylab = "Carbon Footprint",
        xlab = "",
        las = 2,                
        col = "cyan3"
        )       

# Surface of Wellbeing, GDP, and Life Expectancy
# Load interpolation helper
library(akima)   # for interp()

# Take your raw data
x <- df$WB
y <- df$GDP
z <- df$LE

x_scaled <- (x - min(x)) / (max(x) - min(x))
y_scaled <- (y - min(y)) / (max(y) - min(y))

interp_grid <- interp(x_scaled, y_scaled, z, duplicate = "mean", nx = 30, ny = 30)

par(mar = c(2, 2, 2, 2), lwd = 0.5)

# 3D surface plot
persp(interp_grid$x, interp_grid$y, interp_grid$z,
      theta = 30, phi = 30, expand = 0.5,
      col = "lightblue",
      ticktype = "simple",
      xlab = "Well Being",
      ylab = "GDP",
      zlab = "Life Expectancy")


# Pie chart of gdp by continent

total_gdp <- sum(df$GDP, na.rm = TRUE)
gdp_by_cont <- tapply(df$GDP, df$Continent, sum, na.rm = TRUE)
gdp_by_cont <- gdp_by_cont[c(
  "Western\nEurope",
  "N. America\n& Oceania",
  "Latin\nAmerica",
  "East Asia",
  "Eastern Europe &\nCentral Asia",
  "Africa",
  "Middle\nEast",
  "South\nAsia"
)]

pie(gdp_by_cont,
    main = "GDP Distribution by Continent",
    col = colorRampPalette(c("lightblue", "deepskyblue"))(length(gdp_by_cont)),
    labels = names(gdp_by_cont)
    )




