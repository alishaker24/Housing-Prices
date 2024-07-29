library(viridis)
# Reading Data
housing <- read.csv("C:/Users/Shiko/Desktop/Big Data/Labs/Project/Housing.csv")
dim(housing)
names(housing)
str(housing)
summary(housing)
pairs(housing[, sapply(housing, is.numeric)], col = rainbow(length(housing)))

# Histogram Price Distribution
hist(housing$price/10^6, main = 'Price Distribution', xaxt = "n", xlab = 'Price in Millions', ylab = 'Frequency', col = 'dodgerblue')
axis(side = 1, at = seq(1, 14, by = 1), labels = paste(seq(1, 14, by = 1), "m", sep = ""))

# Histogram Area Distribution
hist(housing$area, main = 'Area Distribution' , xaxt = "n", xlab = 'Area', ylab = 'Frequency', col = 'turquoise')
axis(side = 1, at = seq(1000, 16000, by = 1000), labels = paste(seq(1000, 16000, by = 1000), "sq ft", sep = ""))

# Histogram Distribution
par(mfrow = c(2, 2))
hist(housing$bedrooms, main = 'Bedrooms Distribution', xlab = 'Bedrooms', ylab = 'Frequency', col = 'violetred', breaks = seq(0.5, 6.5, by = 1))
hist(housing$bathrooms, main = 'Bathrooms Distribution', xlab = 'Bathrooms', ylab = 'Frequency', col = 'mediumpurple', breaks = seq(0.5, 4.5, by = 1))
hist(housing$stories, main = 'Stories Distribution', xlab = 'Number of Stories', ylab = 'Frequency', col = 'lightsalmon', breaks = seq(0.5, 4.5, by = 1))
hist(housing$parking, main = 'Parking Distribution', xlab = 'Parking Spaces', ylab = 'Frequency', col = 'bisque1', breaks = seq(-0.5, 3.5, by = 1))

# Categorical Bar Plots
par(mfrow = c(2, 3))
barplot(table(housing$mainroad), main = 'Main Road', col = 'lightsteelblue2')
barplot(table(housing$guestroom), main = 'Guestroom', col = 'darkolivegreen1')
barplot(table(housing$basement), main = 'Basement', col = 'sienna')
barplot(table(housing$hotwaterheating), main = 'Hot Water Heating', col = 'red2')
barplot(table(housing$airconditioning), main = 'Air Conditioning', col = 'skyblue1')
barplot(table(housing$prefarea), main = 'Preferred Area', col = 'lightgreen')
par(mfrow = c(1, 1))

# Pie Chart
variables <- c("stories", "mainroad", "guestroom", "basement", "hotwaterheating", "airconditioning", "parking", "prefarea", "furnishingstatus")
for (var in variables) {
  freq <- table(housing[[var]])
  pct <- freq/sum(freq)*100
  lbls <- paste(names(freq), "\n", round(pct, 2), "%")  
  pie(freq, labels = lbls, col = rainbow(length(lbls)), main = paste("Pie Chart of", var))
}

# Pie Chart Bedrooms
freq <- table(housing$bedrooms)
pct <- round(freq/sum(freq)*100, 2)
lbls <- c(1, 2, 6, 3, 5, 4)
freq <- freq[match(lbls, names(freq))]
pct <- pct[match(lbls, names(pct))]
lbls <- paste(lbls, "\n", pct, " %")
pie(freq, labels = lbls, col = rainbow(length(lbls)), main = paste("Pie Chart of Bedrooms"))

# Pie Chart Bathroom
freq <- table(housing$bathrooms)
pct <- round(freq/sum(freq)*100, 2)
lbls <- c(1, 4, 2, 3)
freq <- freq[match(lbls, names(freq))]
pct <- pct[match(lbls, names(pct))]
lbls <- paste(lbls, "\n", pct, "%")
pie(freq, labels = lbls, col = rainbow(length(lbls)), main = "Pie Chart of Bathrooms")

# Furnishing Status Plot
barplot(table(housing$furnishingstatus), main = 'Furnishing Status', col = 'navy')

# Boxplot Of Numerical Variables
small_num <- c("bedrooms", "bathrooms", "stories", "parking")
boxplot(housing[, small_num], main = "Boxplot of Numerical Variables", ylab = "Number", col = rainbow(length(housing)))

# Price Boxplot
boxplot(housing$price/10^6, main = "Price Boxplot", yaxt="n", ylab = "Price", col = 'seagreen')
axis(side = 2, at = seq(1, 14, by = 1), labels = paste(seq(1, 14, by = 1), "m", sep = ""))

# Area Boxplot
boxplot(housing$area, main = "Area Boxplot", yaxt="n", ylab = "Area", col = 'cornsilk')
axis(side = 2, at = seq(1000, 16000, by = 1000), labels = paste(seq(1000, 16000, by = 1000), "sq ft", sep = ""))

# Price by others Bars
par(mfrow = c(2, 2))
colors <- c("darkslateblue", "cyan3", "magenta4", "darkgreen")
for (i in 1:length(colors)) {
  col_name <- names(housing)[sapply(housing, is.numeric) & names(housing) != "price" & names(housing) != "area"][i]
  AvgPrice <- aggregate(housing$price, by = list(housing[[col_name]]), FUN = mean)
  barplot(AvgPrice$x/10^6, names.arg = AvgPrice$Group.1, xlab = col_name, ylab = "Price (Millions)", main = paste("Price by", col_name), col = colors[i])
}
par(mfrow = c(1, 1))

# Price by others Lines
par(mfrow = c(2, 2))
colors <- c("darkslateblue", "cyan3", "magenta4", "darkgreen")
for (i in 1:length(colors)) {
  col_name <- names(housing)[sapply(housing, is.numeric) & names(housing) != "price" & names(housing) != "area"][i]
  AvgPrice <- aggregate(housing$price, by = list(housing[[col_name]]), FUN = mean)
  plot(AvgPrice$x/10^6, xaxt="n", names.arg = AvgPrice$Group.1, xlab = col_name, ylab = "Price (Millions)", main = paste("Price by", col_name), col = colors[i], type="l", lwd=3)
  axis(side = 1, at = 1:length(AvgPrice$x), labels = AvgPrice$Group.1)
}
par(mfrow = c(1, 1))

# Price by Area
par(mfrow = c(2, 1))
intervals <- seq(0, ceiling(max(housing$area)), by = 1000)
area_group <- cut(housing$area, breaks = intervals, labels = intervals[-length(intervals)])
AvgPrice <- aggregate(housing$price, by = list(area_group), FUN = mean)
barplot(AvgPrice$x/10^6, names.arg = AvgPrice$Group.1, xlab = "Area", ylab = "Price (Millions)", main = "Average Price by Area", col = "peachpuff2")
plot(AvgPrice$x/10^6, col = "maroon", type = "l", lwd = 2, xlab = "Area (1000sq ft)", ylab = "Price (Millions)", main = "Average Price by Area")
par(mfrow = c(1, 1))

# Price by Categorical
par(mfrow = c(2, 3))
colors <- c("coral1", "springgreen3", "royalblue3", "mediumorchid3", "yellow2" , "deeppink2")
Y_N_col <- c("mainroad", "guestroom", "basement", "hotwaterheating", "airconditioning", "prefarea")
housing[Y_N_col] <- lapply(housing[Y_N_col], factor, levels = c("yes", "no"))
for (i in seq_along(colors)) {
  col_name <- names(housing)[sapply(housing, is.factor)][i]
  AvgPrice <- aggregate(housing$price, by = list(housing[[col_name]]), FUN = mean)
  barplot(AvgPrice$x/10^6, names.arg = AvgPrice$Group.1, xlab = col_name, ylab = "Price (Millions)", main = paste("Price by", col_name), col = colors[i])
}
par(mfrow = c(1, 1))

# Price by Furnishing Status
housing$furnishingstatus <- factor(housing$furnishingstatus, levels = c("furnished", "semi-furnished", "unfurnished"))
AvgPrice <- aggregate(housing$price, by = list(housing$furnishingstatus), FUN = mean)
barplot(AvgPrice$x/10^6, names.arg = AvgPrice$Group.1, xlab = "Furnishing Status" , ylab = "Price (Millions)", main = paste("Price by Furnishing Status"), col = "honeydew2")

# Categorical Box Plot
par(mfrow = c(2, 3))
boxplot(price/10^6 ~ mainroad, data = housing, main = "Boxplot of Price by Main Road", xlab = "Main Road", ylab = "Price (Millions)", col = c("aquamarine2", "salmon"))
boxplot(price/10^6 ~ guestroom, data = housing, main = "Boxplot of Price by Guestroom", xlab = "Guestroom", ylab = "Price (Millions)", col = c("aquamarine2", "salmon"))
boxplot(price/10^6 ~ basement, data = housing, main = "Boxplot of Price by Basement", xlab = "Basement", ylab = "Price (Millions)", col = c("aquamarine2", "salmon"))
boxplot(price/10^6 ~ hotwaterheating, data = housing, main = "Boxplot of Price by Hot Water Heating", xlab = "Hot Water Heating", ylab = "Price (Millions)", col = c("aquamarine2", "salmon"))
boxplot(price/10^6 ~ airconditioning, data = housing, main = "Boxplot of Price by Air Conditioning", xlab = "Air Conditioning", ylab = "Price (Millions)", col = c("aquamarine2", "salmon"))
boxplot(price/10^6 ~ prefarea, data = housing, main = "Boxplot of Price by Preferred Area", xlab = "Preferred Area", ylab = "Price (Millions)", col = c("aquamarine2", "salmon"))
par(mfrow = c(1, 1))

# Numerical Box Plot
par(mfrow = c(2, 2))
boxplot(price/10^6 ~ bedrooms, data = housing, main = "Boxplot of Price by Bedrooms", xlab = "Bedrooms", ylab = "Price (Millions)", col = viridis_pal()(8))
boxplot(price/10^6 ~ bathrooms, data = housing, main = "Boxplot of Price by Bathrooms", xlab = "Bathrooms", ylab = "Price (Millions)", col = viridis_pal()(8))
boxplot(price/10^6 ~ stories, data = housing, main = "Boxplot of Price by Stories", xlab = "Stories", ylab = "Price (Millions)", col = viridis_pal()(8))
boxplot(price/10^6 ~ parking, data = housing, main = "Boxplot of Price by Parking", xlab = "Parking", ylab = "Price (Millions)", col = viridis_pal()(8))
par(mfrow = c(1, 1))

# Furnishing Status Box Plot
boxplot(price/10^6 ~ furnishingstatus, data = housing, main = "Boxplot of Price by Furnishing Status", xlab = "Furnishing Status", ylab = "Price (Millions)", col = viridis_pal()(8))

# Scatter Plot
plot(housing$area, housing$price/10^6, pch=16, col="skyblue4", main="Price vs. Area", xlab="Area (sq ft)", ylab="Price (Million)")

# ANOVA
library(lattice)

# For Furnishing Status
aggregate(x=housing$price, by=list(housing$furnishingstatus), FUN="mean") 
model <- aov((price ~ furnishingstatus), data=housing)
summary(model) 
TukeyHSD(model)
densityplot(~ log10(price/10^6), group=furnishingstatus, data=housing, auto.key=T, xlab = "Price (Log 10)", main="ANOVA Price & Furnishing Status")

# For Numbers
variables <- c("bedrooms", "bathrooms", "stories", "parking")
for (var in variables) {
  avg <- aggregate(x = housing$price, by = list(housing[[var]]), FUN = "mean")
  housing[[var]] <- as.factor(housing[[var]])
  print(levels(housing[[var]]))
  model <- aov(housing$price ~ housing[[var]])
  print(summary(model))
  print(TukeyHSD(model))
  DP <- densityplot(~ log10(price / 10^6), group = housing[[var]], data = housing, auto.key = TRUE, main = paste("Density plot of price by", var), xlab = "Price (Millions)")
  print(DP)
}

# T-Test For Y/N
binary_variables <- c("mainroad", "guestroom", "basement", "hotwaterheating", "airconditioning", "prefarea")
t_test_results <- list()
for (variable in binary_variables) {
  y <- housing$price[housing[[variable]] == "yes"]
  n <- housing$price[housing[[variable]] == "no"]
  t_test_result <- t.test(y, n, var.equal = TRUE)
  print(variable)
  print(t_test_result)
}

# K-Means
mean_price <- mean(housing$price)
housing$Price_class <- ifelse(housing$price < mean_price, "cheap", "expensive")
housing <- as.data.frame(model.matrix(~ . - 1, data = housing))
newhousing <- housing
newhousing$price_class <- NULL
centers <- 2
kc <- kmeans(newhousing, centers, iter.max = 10, nstart = 1)
kc$cluster
kc$center
table(housing$Price_class, kc$cluster)
plot(housing$area, housing$price/10^6, col = c("palegreen3", "lightpink3")[kc$cluster], xlab="Area", ylab ="Price", yaxt="n", main = "K-Means")
axis(side = 2, at = seq(1, 14, by = 1), labels = paste(seq(1, 14, by = 1), "m", sep = ""))

# Convert categorical variables to factors
housing$mainroad <- factor(housing$mainroad, levels = c("yes", "no"))
housing$guestroom <- factor(housing$guestroom, levels = c("yes", "no"))
housing$basement <- factor(housing$basement, levels = c("yes", "no"))
housing$hotwaterheating <- factor(housing$hotwaterheating, levels = c("yes", "no"))
housing$airconditioning <- factor(housing$airconditioning, levels = c("yes", "no"))
housing$prefarea <- factor(housing$prefarea, levels = c("yes", "no"))
housing$furnishingstatus <- factor(housing$furnishingstatus, levels = c("furnished", "semi-furnished", "unfurnished"))

# Linear Regression
fit <- lm(price ~ ., data = housing)

# Summary of the model
attributes(fit)
residuals(fit)
summary(fit)

# Predictions for the dataset
predictions <- predict(fit, newdata = housing)