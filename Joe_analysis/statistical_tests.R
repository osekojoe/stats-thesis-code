library(fmsb)
library(dplyr)
library(ggplot2)

dataReq4 <- read.csv("../data/vbov_comb.csv")

######################################################################
# --- test for association between planned, actual places and 
# --- responsible person AND sociodemographic and obstetric factors ---
######################################################################

## - planned
table_data <- table(dataReq4$planned, dataReq4$mothertongue)
chisq.test(table_data)

table_data <- table(dataReq4$planned, dataReq4$relationship)
chisq.test(table_data)
fisher.test(table_data, simulate.p.value=TRUE)

table_data <- table(dataReq4$planned, dataReq4$educLevel)
chisq.test(table_data)
fisher.test(table_data, simulate.p.value=TRUE)


## - actual
table_data <- table(dataReq4$actual, dataReq4$mothertongue)
chisq.test(table_data)

table_data <- table(dataReq4$actual, dataReq4$relationship)
chisq.test(table_data)
fisher.test(table_data, simulate.p.value=TRUE)

table_data <- table(dataReq4$actual, dataReq4$educLevel)
chisq.test(table_data)
fisher.test(table_data, simulate.p.value=TRUE)

kruskal.test(maternalAge ~ actual, data = dataReq4)

## - responsible person
table_data <- table(dataReq4$birthingResponsibility, dataReq4$mothertongue)
chisq.test(table_data)
fisher.test(table_data, simulate.p.value=FALSE)

table_data <- table(dataReq4$birthingResponsibility, dataReq4$relationship)
chisq.test(table_data)
fisher.test(table_data, simulate.p.value=FALSE)

table_data <- table(dataReq4$birthingResponsibility, dataReq4$educLevel)
chisq.test(table_data)
fisher.test(table_data, simulate.p.value=FALSE)

kruskal.test(maternalAge ~ birthingResponsibility, data = dataReq4)
wilcox.test(maternalAge ~ birthingResponsibility, data = dataReq4)

## age and planned, delivery, birthresponsibility
## assuming age not normally distributed
# Kruskal-Wallis


ggplot(dataReq4, aes(x = maternalAge)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Age with Density Curve",
       x = "Age",
       y = "Density") +
  theme_minimal()


######################################################################
### Chi-square test for homogeneity of proportions
######################################################################

# 1A col. planned vs language ------------------------------------------------

# birthcenter
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(235, 28, 595, 44),
  tries = c(3444, 103, 1413, 302)
)

df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(2110, 40, 265, 137),
  tries = c(3444, 103, 1413, 302)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(1086, 27, 207, 102),
  tries = c(3444, 103, 1413, 302)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(13, 8, 346, 19),
  tries = c(3444, 103, 1413, 302)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")



# 1A row planned vs language ------------------------------------------------

# birthcenter
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(235, 28, 595, 44),
  tries = c(902, 902, 902, 902)
)

df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(2110, 40, 265, 137),
  tries = c(2552, 2552, 2552, 2552)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(1086, 27, 207, 102),
  tries = c(1422, 1422, 1422, 1422)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(13, 8, 346, 19),
  tries = c(386, 386, 386, 386)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 1B col. planned vs relationship ------------------------------------------------

# birthcenter
df <- data.frame(
  language = c("Married", "Single", "Widowed", "Other"),
  successes = c(262, 357, 0, 0),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(1013, 77, 1, 4),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(560, 40, 1, 1),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(0, 296, 0, 0),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 1B row planned vs relationship ------------------------------------------------

# birthcenter
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(262, 357, 0, 0),
  tries = c(619, 619, 619, 619)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(1013, 77, 1, 4),
  tries = c(1095, 1095, 1095, 1095)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(560, 40, 1, 1),
  tries = c(602, 602, 602, 602)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(0, 296, 0, 0),
  tries = c(296, 296, 296, 296)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 1C col. planned vs educLevel ------------------------------------------------

# birthcenter
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(3, 21, 117),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(10, 86, 430),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(3, 15, 239),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(0, 38, 171),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")



# 1C row planned vs educLevel ------------------------------------------------

# birthcenter
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(3, 21, 117),
  tries = c(141, 141, 141)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(10, 86, 430),
  tries = c(526, 526, 526)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(3, 15, 239),
  tries = c(257, 257, 257)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(0, 38, 171),
  tries = c(209, 209, 209)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 2A col. actual vs language ------------------------------------------------

# birthcenter
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(181, 14, 257, 18),
  tries = c(3444, 103, 1413, 302)
)

df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(1938, 34, 231, 136),
  tries = c(3444, 103, 1413, 302)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(1295, 43, 512, 121),
  tries = c(3444, 103, 1413, 302)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(30, 12, 413, 27),
  tries = c(3444, 103, 1413, 302)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 2A row. actual vs language ------------------------------------------------

# birthcenter
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(181, 14, 257, 18),
  tries = c(470, 470, 470, 470)
)

df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(1938, 34, 231, 136),
  tries = c(2339, 2339, 2339, 2339)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(1295, 43, 512, 121),
  tries = c(1971, 1971, 1971, 1971)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(30, 12, 413, 27),
  tries = c(482, 482, 482, 482)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 2B col. actual vs relationship ------------------------------------------------

# birthcenter
df <- data.frame(
  language = c("Married", "Single", "Widowed", "Other"),
  successes = c(218, 27, 0, 0),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(949, 72, 1, 2),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(666, 259, 1, 3),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(2, 412, 0, 0),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 2B row actual vs relationship ------------------------------------------------

# birthcenter
df <- data.frame(
  relationship = c("Married", "Single", "Widowed", "Other"),
  successes = c(218, 27, 0, 0),
  tries = c(245, 245, 245, 245)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  relationship = c("Dutch", "English", "French", "Other"),
  successes = c(949, 72, 1, 2),
  tries = c(1024, 1024, 1024, 1024)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  relationship = c("Dutch", "English", "French", "Other"),
  successes = c(666, 259, 1, 3),
  tries = c(929, 929, 929, 929)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  relationship = c("Dutch", "English", "French", "Other"),
  successes = c(2, 412, 0, 0),
  tries = c(414, 414, 414, 414)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# 2C col. actual vs educLevel ------------------------------------------------

# birthcenter
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(3, 21, 97),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(9, 77, 408),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(4, 37, 331),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(0, 25, 121),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 2C row actual vs educLevel ------------------------------------------------

# birthcenter
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(3, 21, 97),
  tries = c(121, 121, 121)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# home
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(9, 77, 408),
  tries = c(494, 494, 494)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# hospital
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(4, 37, 331),
  tries = c(372, 372, 372)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# MLU
df <- data.frame(
  education = c("none", "secondary", "tertiary"),
  successes = c(0, 25, 121),
  tries = c(146, 146, 146)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 3A col. responsible person vs language ------------------------------------------------

# midwife
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(2796, 73, 1073, 241),
  tries = c(3444, 103, 1413, 302)
)

df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# obstetrician
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(648, 30, 340, 61),
  tries = c(3444, 103, 1413, 302)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 3A row responsible person vs language ------------------------------------------------

# midwife
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(2796, 73, 1073, 241),
  tries = c(4183, 4183, 4183, 4183)
)

df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# obstetrician
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(648, 30, 340, 61),
  tries = c(1079, 1079, 1079, 1079)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 3B col. responsible person vs relationship ------------------------------------------------

# midwife
df <- data.frame(
  language = c("Married", "Single", "Widowed", "Other"),
  successes = c(1505, 541, 1, 3),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# obstetrician
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(330, 229, 1, 2),
  tries = c(1835, 770, 2, 5)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")



# 3B row responsible person vs relationship ------------------------------------------------

# midwife
df <- data.frame(
  language = c("Married", "Single", "Widowed", "Other"),
  successes = c(1505, 541, 1, 3),
  tries = c(2050, 2050, 2050, 2050)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# obstetrician
df <- data.frame(
  language = c("Dutch", "English", "French", "Other"),
  successes = c(330, 229, 1, 2),
  tries = c(562, 562, 562, 562)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 4, byrow = FALSE,
                     dimnames = list(c("Married", "Single", "Widowed", "Other"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
#fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 3C col. responsible person vs educLevel ------------------------------------------------

# midwife
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(14, 131, 780),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# obstetrician
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(2, 29, 177),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")


# 3C row responsible person vs educLevel ------------------------------------------------

# midwife
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(14, 131, 780),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")

# obstetrician
df <- data.frame(
  language = c("none", "secondary", "tertiary"),
  successes = c(2, 29, 177),
  tries = c(16, 160, 957)
)
df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")

cont_table <- matrix(c(df$successes, df$failures), 
                     nrow = 3, byrow = FALSE,
                     dimnames = list(c("none", "secondary", "tertiary"), 
                                     c("success", "failure")))
print(cont_table)

cat("\nFisher's Exact Test:\n")
fisher.test(cont_table)
pairwise.fisher.test(df$successes, df$tries, p.adjust.method = "bonferroni")



######################################################################
### Test for equality of proportions of diagonal proportions ---------
### (planned-actual agreement proportions)------------
######################################################################\

df <- data.frame(
  setting = c("BC", "home", "hospital", "MLU"),
  successes = c(455, 2148, 1239, 394),
  tries = c(982, 2556, 1426, 478)
)

df$failures <- df$tries - df$successes
df$proportion <- df$successes / df$tries
print(df)

prop.test(df$successes, df$tries)
pairwise.prop.test(df$successes, df$tries, p.adjust.method = "bonferroni")


### ------ differences in proportions and CIs

# Differences matrix
prop_mat <- df$proportion
names(prop_mat) <- df$setting

# pairwise differences with CIs
library(dplyr)

combs <- combn(df$setting, 2, simplify = FALSE)

results <- lapply(combs, function(pair) {
  i <- which(df$setting == pair[1])
  j <- which(df$setting == pair[2])
  
  # proportions
  p1 <- df$proportion[i]
  p2 <- df$proportion[j]
  
  # difference
  diff <- p1 - p2
  
  # calculate standard error for difference of proportions
  se <- sqrt(p1*(1-p1)/df$tries[i] + p2*(1-p2)/df$tries[j])
  
  # 95% CI
  lower <- diff - qnorm(0.975)*se
  upper <- diff + qnorm(0.975)*se
  
  data.frame(
    group1 = pair[1],
    group2 = pair[2],
    difference = diff,
    lower95 = lower,
    upper95 = upper
  )
})

# Combine all results
diff_df <- bind_rows(results)
print(diff_df)


######################################################################
# Stuart-Maxwell test for marginal homogeneity-----------------
######################################################################

library(DescTools)

tbl <- matrix(c(
  455, 15, 312, 200,
  8, 2148, 399, 1,
  8, 178, 1239, 1,
  0, 0, 84, 394
), nrow = 4, byrow = TRUE)

rownames(tbl) <- c("Birth Center", "Home", "Hospital", "MLU")
colnames(tbl) <- c("Birth Center", "Home", "Hospital", "MLU")

print(tbl)

result <- StuartMaxwellTest(tbl)
print(result)

###  ---
library(coin)


# Stuart-Maxwell TEST for marginal homogeneity ---------------------
marginalHomogeneityTest(actual ~ planned, data = data, weights = ~weights)

##pairwise McNemar tests for all pairs------------------

pairs <- combn(rownames(tbl), 2, simplify = FALSE)

results <- lapply(pairs, function(pair) {
  # Indices of pair in the matrix
  i <- which(rownames(tbl) == pair[1])
  j <- which(rownames(tbl) == pair[2])
  
  # Construct 2x2 table for this pair
  sub_tbl <- matrix(c(
    tbl[i, i], tbl[i, j],
    tbl[j, i], tbl[j, j]
  ), nrow = 2, byrow = TRUE)
  
  rownames(sub_tbl) <- colnames(sub_tbl) <- pair
  
  # Perform McNemar test 
  test <- mcnemar.test(sub_tbl, correct = FALSE)
  
  data.frame(
    Group1 = pair[1],
    Group2 = pair[2],
    Statistic = test$statistic,
    p_value = test$p.value
  )
})

# Combine results in a data frame
pairwise_results <- do.call(rbind, results)
print(pairwise_results)

# bonferroni correction
pairwise_results$p_adjusted <- p.adjust(pairwise_results$p_value, method = "bonferroni")
print(pairwise_results)


