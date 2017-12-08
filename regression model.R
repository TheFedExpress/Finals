library(Hmisc)
library(corrplot)
getHdata(boston)
head(boston)

mat <- cor(select(boston_subset, -river, -black))
corrplot(mat, type = "upper")

describe(boston$value)
boston %>% ggplot() + geom_histogram(aes(x = value), color = "black", fill = "darkblue")


describe(boston$rooms)
boston %>% ggplot() + geom_histogram(aes(x = rooms), color = "black", fill = "darkgreen")
boston %>% ggplot() + geom_point(aes(x = rooms, y = value))

describe(boston$distance)
boston %>% ggplot() + geom_histogram(aes(x = distance), color = "black", fill = "purple")

boston$distance_ln <- log(boston$distance)
boston %>% ggplot() + geom_histogram(aes(x = distance_ln), color = "black", fill = "purple")
boston %>% ggplot() + geom_point(aes(x = distance_ln, y = value))


describe(boston$lstat)
boston %>% ggplot() + geom_histogram(aes(x = lstat), color = "black", fill = "yellow")
boston$lstat_ln <- log(boston$lstat)
boston %>% ggplot() + geom_histogram(aes(x = lstat_ln), color = "black", fill = "yellow")
boston %>% ggplot() + geom_point(aes(x = lstat_ln, y = value))


describe(boston$ptratio)
boston %>% ggplot() + geom_histogram(aes(x = ptratio), color = "black", fill = "brown")
boston %>% ggplot() + geom_point(aes(x = ptratio, y = value))


describe(boston$tax)
boston %>% ggplot() + geom_histogram(aes(x = tax), color = "black", fill = "brown")
boston %>% ggplot() + geom_point(aes(x = tax, y = value))

table(boston$highway)

describe(boston$crime)

boston %>% ggplot() + geom_histogram(aes(x = crime), color = "black", fill = "orange")
boston %>% ggplot() + geom_point(aes(x = crime, y = value))


boston$crime_sqrt <- boston$crime^(1/3)

boston %>%  ggplot() + geom_histogram(aes(x = crime_sqrt), color = "black", fill = "orange")
boston %>% ggplot() + geom_point(aes(x = crime_sqrt, y = value))

boston_final <- boston_subset %>%
  select(-older, - industrial, - latitude, - longitude, -residential)


boston_subset <- boston %>%
  select(-(1:2), -cmedv)
full_model <- lm(value ~ ., data = boston_subset)
summary(full_model)

older <- lm(value ~ older, boston)
summary(older)


m2 <- lm(value ~ ., data = select(boston_subset, -older, - industrial, - latitude, - longitude, -residential, - crime_sqrt
                  ))
summary(m2)


mat2 <- cor(select(boston_final, -river, -black))
corrplot(mat2)

diagnostics <- data.frame(m2$model, residuals = m2$residuals, fitted_values = m2$fitted.values, residuals_abs = abs(m2$residuals))


ggplot(diagnostics) + geom_point(aes(x = fitted_values, y = residuals)) + geom_hline(yintercept = 0)
qqnorm(diagnostics$residuals)
qqline(diagnostics$residuals)
names(boston_final)

boston_names <- boston_final %>%
  select(-value, -crime_sqrt, -crime, -river) %>%
  names

ggplot(diagnostics) + geom_point(aes(x = fitted_values, y = residuals_abs))

for (i in 1:length(boston_names)){
  plot <- ggplot(diagnostics) + geom_point(aes_string(x = boston_names[i], y = "residuals"))
  Sys.sleep(2)
  print(plot)
}
ggplot(diagnostics) + geom_boxplot(aes(y = residuals, x = ethnicity)) 


