########################################## AN INTRODUCTION TO ANOVA ##################################################

# Summary statistics by group
describeBy(wm, wm$cond)

# Boxplot of iq versus cond
boxplot( wm$iq ~ wm$cond, main = "Boxplot", xlab = "Group (cond)", ylab = "IQ" )


# Create the vector x
x <- seq(from = 0, to = 2, length = 200)

# Evaluate the densities
y_1 <- df(x, 1, 1)
y_2 <- df(x, 3, 1)
y_3 <- df(x, 6, 1)
y_4 <- df(x, 3, 3)
y_5 <- df(x, 6, 3)
y_6 <- df(x, 3, 6)
y_7 <- df(x, 6, 6)

# Plot the densities
plot(x, y_1, col = 1, type = "l")
lines(x, y_2, col = 2)
lines(x, y_3, col = 3)
lines(x, y_4, col = 4)
lines(x, y_5, col = 5)
lines(x, y_6, col = 6)
lines(x, y_7, col = 7)

# Add the legend
legend("topright", title = "F distributions",
       c("df = (1,1)", "df = (3,1)", "df = (6,1)", "df = (3,3)", 
         "df = (6,3)", "df = (3,6)", "df = (6,6)"), 
       col = c(1, 2, 3, 4, 5, 6, 7), lty = 1)



# Define number of subjects in each group
n <- 20
# Calculate group means
y_j <- tapply(wm$iq, wm$condition, mean)

# Calculate the grand mean
y_t <- mean(wm$iq)

# Calculate the sum of squares
ss_a <- n * sum((y_j - y_t) * (y_j - y_t))


# Create a separate vector of IQ gains for each training group
y_i1 <- subset(wm$iq, wm$cond == "8 days")
y_i2 <- subset(wm$iq, wm$cond == "12 days")
y_i3 <- subset(wm$iq, wm$cond == "17 days")
y_i4 <- subset(wm$iq, wm$cond == "19 days")

# Subtract group means from the individual values
s_1 <- y_i1 - y_j[1]
s_2 <- y_i2 - y_j[2]
s_3 <- y_i3 - y_j[3]
s_4 <- y_i4 - y_j[4]

# Put everything back together into one vector
s_t <- c(s_1, s_2, s_3, s_4)

# Calculate the sum of squares using s_t
ss_sa <- sum(s_t ^ 2)



# Number of groups
a <- 4

# Number of subjects in each group
n <- 20

# Define degrees of freedom
df_a <- 3
df_sa <- 4 * 19

# Calculate mean squares using ss_a and ss_sa
ms_a <- ss_a / df_a
ms_sa <- ss_sa / df_sa

# Calculate the F-ratio
f_rat <- ms_a / ms_sa



## wm is already loaded

# Apply the aov function
anova_wm <- aov( wm$iq ~ wm$condition)

# Look at the summary table of the result
summary(anova_wm)



# wm is already loaded

# Levene's test
leveneTest(wm$iq, wm$condition)

# Levene's test with center = mean
leveneTest(wm$iq, wm$condition, center = mean)


########################################## POST HOC ANALYSIS #########################################################


??TukeyHSD


# Conduct ANOVA
anova_wm <- aov(wm$gain ~ wm$cond)

# View summary
summary(anova_wm)

# Conduct Tukey procedure
tukey <- TukeyHSD(anova_wm)

# Plot confidence intervals
plot(tukey)


# Use p.adjust
bonferroni_ex <- p.adjust(0.005, method = "bonferroni", n = 8)

# Print bonferroni_ex
bonferroni_ex

# Pairwise t-test
pairwise.t.test(wm$gain , wm$cond, p.adjust = "bonferroni")

####################################### BETWEEN GROUPS FACTORIAL ANOVA ###############################################

# Create 6 subgroups
ab_groups <- tapply(ab$errors, list(ab$driving, ab$conversation), sum)

# Make the required barplot
barplot(ab_groups, beside = TRUE, 
        col = c("orange", "blue"), 
        main = "Driving Errors", 
        xlab = "Conversation Demands", 
        ylab = "Errors")

# Add the legend
legend("topright", c("Difficult","Easy"), 
       title = "Driving",
       fill = c("orange", "blue"))


## The data frame ab is preloaded in your workspace

# Test the homogeneity of variance assumption
leveneTest(ab$errors ~ ab$conversation * ab$driving)



# Create the two subsets
ab_1 <- subset(ab, ab$driving == "Easy")
ab_2 <- subset(ab, ab$driving == "Difficult")

# Perform the one-way ANOVA for both subsets
aov_ab_1 <- aov(ab_1$errors ~ ab_1$conversation) 
aov_ab_2 <- aov(ab_2$errors ~ ab_2$conversation)

# Get the summary tables for both aov_ab_1 and aov_ab_2
summary(aov_ab_1)
summary(aov_ab_2)


## aov_ab_1 and aov_ab_2 are preloaded in your workspace

# Calculate the etaSquared for the easy driving case
etaSquared(aov_ab_1, anova = T)

# Calculate the etaSquared for the difficult driving case
etaSquared(aov_ab_2, anova = T)



## aov_ab_1 and aov_ab_2 are preloaded in your workspace

# Tukey for easy driving
TukeyHSD(aov_ab_1)

# Tukey for difficult driving
TukeyHSD(aov_ab_2)




