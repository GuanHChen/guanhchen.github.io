library(readxl)
df <- read_excel('epps6356/myHPI.xlsx')
df <- na.omit(df)

boxplot(df$GDP,
        main = "Boxplot of GDP",  # Add a title
        ylab = "GDP Values",      # Label the y-axis
        col = "lightblue",        # Set the box color
        border = "black"          # Set the border color
)