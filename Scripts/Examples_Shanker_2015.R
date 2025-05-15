# In this script you can find the R code to replicate the 
# examples from Shaker et al. (2015) "On modeling of lifetimes 
# data using exponential and lindley distributions".

# Loading useful tools and packages
url <- "https://raw.githubusercontent.com/fhernanb/T_exp_lin/main/Scripts/exp_lin_test.R"
source(url)

library(RelDists) # R package with reliability dists. To use dLIN distribution
library(gamlss)   # To use dEXP distribution

# Example 1 ---------------------------------------------------------------

y <- c(0.55, 0.93, 1.25, 1.36, 1.49, 1.52, 1.58, 1.61, 1.64, 1.68, 1.73, 
       1.81, 2.00, 0.74, 1.04, 1.27, 1.39, 1.49, 1.53, 1.59, 1.61, 1.66, 
       1.68, 1.76, 1.82, 2.01, 0.77, 1.11, 1.28, 1.42, 1.50, 1.54, 1.60, 
       1.62, 1.66, 1.69, 1.76, 1.84, 2.24, 0.81, 1.13, 1.29, 1.48, 1.50, 
       1.55, 1.61, 1.62, 1.66, 1.70, 1.77, 1.84, 0.84, 1.24, 1.30, 1.48, 
       1.51, 1.55, 1.61, 1.63, 1.67, 1.70, 1.78, 1.89)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 2 ---------------------------------------------------------------

y <- c(5, 25, 31, 32, 34, 35, 38, 39, 39, 40, 42, 43, 43,
       43, 44, 44, 47, 47, 48, 49, 49, 49, 51, 54, 55, 55,
       55, 56, 56, 56, 58, 59, 59, 59, 59, 59, 63, 63, 64,
       64, 65, 65, 65, 66, 66, 66, 66, 66, 67, 67, 67, 68,
       69, 69, 69, 69, 71, 71, 72, 73, 73, 73, 74, 74, 76,
       76, 77, 77, 77, 77, 77, 77, 79, 79, 80, 81, 83, 83,
       84, 86, 86, 87, 90, 91, 92, 92, 92, 92, 93, 94, 97,
       98, 98, 99, 101, 103, 105, 109, 136, 147)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 3 ---------------------------------------------------------------

y <- c(17.88, 28.92, 33.00, 41.52, 42.12, 45.60, 
       48.80, 51.84, 51.96, 54.12, 55.56, 67.80,
       68.44, 68.64, 68.88, 84.12, 93.12, 98.64, 
       105.12, 105.84, 127.92, 128.04, 173.40)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 4 ---------------------------------------------------------------

y <- c(86, 146, 251, 653, 98, 249, 400, 292, 131, 169, 175, 176, 76,
       264, 15, 364, 195, 262, 88, 264, 157, 220, 42, 321, 180, 198,
       38, 20, 61, 121, 282, 224, 149, 180, 325, 250, 196, 90, 229,
       166, 38, 337, 65, 151, 341, 40, 40, 135, 597, 246, 211, 180,
       93, 315, 353, 571, 124, 279, 81, 186, 497, 182, 423, 185, 229,
       400, 338, 290, 398, 71, 246, 185, 188, 568, 55, 55, 61, 244,
       20, 284, 393, 396, 203, 829, 239, 236, 286, 194, 277, 143, 198,
       264, 105, 203, 124, 137, 135, 350, 193, 188)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 5 ---------------------------------------------------------------

y <- c(12, 15, 22, 24, 24, 32, 32, 33, 34, 38, 38, 43, 44,
       48, 52, 53, 54, 54, 55, 56, 57, 58, 58, 59, 60, 60,
       60, 60, 61, 62, 63, 65, 65, 67, 68, 70, 70, 72, 73,
       75, 76, 76, 81, 83, 84, 85, 87, 91, 95, 96, 98, 99,
       109, 110, 121, 127, 129, 131, 143, 146, 146, 175, 
       175, 211, 233, 258, 258, 263, 297, 341, 341, 376)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 6 ---------------------------------------------------------------

y <- rep(c(19:37, 39, 42), 
         times=c(16, 15, 14, 9, 12, 10, 6, 9, 8, 5, 6, 4, 3, 4, 1, 
                 1, 4, 2, 2, 1, 1))

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 7 ---------------------------------------------------------------

y <- c(6.53, 7, 10.42, 14.48, 16.10, 22.70, 34, 41.55, 42, 45.28, 49.40, 
       53.62, 63, 64, 83, 84, 91, 108, 112, 129, 133, 133, 139, 140, 
       140, 146, 149, 154, 157, 160, 160, 165, 146, 149, 154, 157, 160, 
       160, 165, 173, 176, 218, 225, 241, 248, 273, 277, 297, 405, 417, 
       420, 440, 523, 583, 594, 1101, 1146, 1417)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 8 ---------------------------------------------------------------

y <- c(12.20, 23.56, 23.74, 25.87, 31.98, 37, 41.35,
       47.38, 55.46, 58.36, 63.47, 68.46, 78.26,
       74.47, 81.43, 84, 92, 94, 110, 112, 119, 127,
       130, 133, 140, 146, 155, 159, 173, 179, 194, 
       195, 209, 249, 281, 319, 339, 432, 469,
       519, 633, 725, 817, 1776)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 9 ---------------------------------------------------------------

y <- c(0.08, 2.09, 3.48, 4.87, 6.94, 8.66, 13.11, 23.63, 0.20, 2.23, 3.52, 4.98, 6.97,
       9.02, 13.29, 0.40, 2.26, 3.57, 5.06, 7.09, 9.22, 13.80, 25.74, 0.50, 2.46, 3.64,
       5.09, 7.26, 9.47, 14.24, 25.82, 0.51, 2.54, 3.70, 5.17, 7.28, 9.74, 14.76, 6.31,
       0.81, 2.62, 3.82, 5.32, 7.32, 10.06, 14.77, 32.15, 2.64, 3.88, 5.32, 7.39, 10.34,
       14.83, 34.26, 0.90, 2.69, 4.18, 5.34, 7.59, 10.66, 15.96, 36.66, 1.05, 2.69, 4.23,
       5.41, 7.62, 10.75, 16.62, 43.01, 1.19, 2.75, 4.26, 5.41, 7.63, 17.12, 46.12, 1.26,
       2.83, 4.33, 5.49, 7.66, 11.25, 17.14, 79.05, 1.35, 2.87, 5.62, 7.87, 11.64, 17.36,
       1.40, 3.02, 4.34, 5.71, 7.93, 11.79, 18.10, 1.46, 4.40, 5.85, 8.26, 11.98, 19.13,
       1.76, 3.25, 4.50, 6.25, 8.37, 12.02, 2.02, 3.31, 4.51, 6.54, 8.53, 12.03,
       20.28, 2.02, 3.36, 6.76, 12.07, 21.73, 2.07, 3.36, 6.93, 8.65, 12.63, 22.69)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 10 ---------------------------------------------------------------

y <- c(23, 261, 87, 7, 120, 14, 62, 47, 225, 71, 246, 21, 42,
       20, 5, 12, 120, 11, 3, 14, 71, 11, 14, 11, 16, 90,
       1, 16, 52, 95)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 11 ---------------------------------------------------------------

y <- c(5.1, 1.2, 1.3, 0.6, 0.5, 2.4, 0.5, 1.1, 8, 0.8, 0.4, 0.6, 0.9,
       0.4, 2, 0.5, 5.3, 3.2, 2.7, 2.9, 2.5, 2.3, 1, 0.2, 0.1, 0.1,
       1.8, 0.9, 2, 4, 6.8, 1.2, 0.4, 0.2)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 12 ---------------------------------------------------------------

y <- c(0.8, 0.8, 1.3, 1.5, 1.8, 1.9, 1.9, 2.1, 2.6, 2.7, 2.9, 3.1, 3.2,
       3.3, 3.5, 3.6, 4.0, 4.1, 4.2, 4.2, 4.3, 4.3, 4.4, 4.4, 4.6, 4.7,
       4.7, 4.8, 4.9, 4.9, 5, 5.3, 5.5, 5.7, 5.7, 6.1, 6.2, 6.2, 6.2,
       6.3, 6.7, 6.9, 7.1, 7.1, 7.1, 7.1, 7.4, 7.6, 7.7, 8, 8.2, 8.6,
       8.6, 8.6, 8.8, 8.8, 8.9, 8.9, 9.5, 9.6, 9.7, 9.8, 10.7, 10.9, 11,
       11, 11.1, 11.2, 11.2, 11.5, 11.9, 12.4, 12.5, 12.9, 13, 13.1, 13.3, 13.6,
       13.7, 13.9, 14.1, 15.4, 15.4, 17.3, 17.3, 18.1, 18.2, 18.4, 18.9, 19, 19.9,
       20.6, 21.3, 21.4, 21.9, 23.0, 27, 31.6, 33.1, 38.5)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 13 ---------------------------------------------------------------

y <- c(74, 57, 48, 29, 502, 12, 70, 21, 29, 386, 59, 27, 153,
       26, 326)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 14 ---------------------------------------------------------------

y <- c(1.1, 1.4, 1.3, 1.7, 1.9, 1.8, 1.6, 2.2, 1.7, 2.7, 4.1, 1.8, 1.5,
       1.2, 1.4, 3, 1.7, 2.3, 1.6, 2)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")

# Example 15 ---------------------------------------------------------------

y <- c(18.83, 20.8, 21.657, 23.03, 23.23, 24.05, 24.321, 25.5, 25.52, 25.8, 26.69, 26.77,
       26.78, 27.05, 27.67, 29.9, 31.11, 33.2, 33.73, 33.76, 33.89, 34.76, 35.75, 35.91,
       36.98, 37.08, 37.09, 39.58, 44.045, 45.29, 45.381)

# Applying the test
test1 <- exp_lin_test(y, alternative="not.exp", type="complete")
test2 <- exp_lin_test(y, alternative="not.lin", type="complete")

test1
test2

# Comparing the estimated pdf's
hist(y, freq=FALSE)
curve(dEXP(x, mu=test1$lambda), add=TRUE, col="tomato")
curve(dLIN(x, mu=test2$theta), add=TRUE, col="blue2")



