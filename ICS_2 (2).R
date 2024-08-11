# ICS Project - 2

# Group Members:
# 1. Ashish Saini
# 2. Dhanunjaya Elluri Thimmaraju
# 3. Harshini Eggoni
# 4. Kunal Kochar
# 5. Naveen Kumar Bhageradhi
# 6. Supritha Palguna

#Read data
rent_index_data <- read.csv("D:/A lectures/summer 2022/ICS/P2/rent_index_99.csv")

# Creating Rent per square meter column for our analysis
rent_index_data$rentpsqm <- rent_index_data$net.rent/rent_index_data$living.area

#Check for null values
is.null(rent_index_data)

# Summary
summary(rent_index_data)
#Summary statistics - Count, mean, std
library(dplyr)
group_by(rent_index_data, quality.of.location) %>%
  summarise(
    count = n(),
    min = min(rentpsqm, na.rm = TRUE),
    mean = mean(rentpsqm, na.rm = TRUE),
    var = var(rentpsqm, na.rm = TRUE),
    max = max(rentpsqm, na.rm = TRUE),
    iqr = IQR(rentpsqm, na.rm = TRUE)
  )

# groups count bases on quality of location
table(rent_index_data$quality.of.location)
rent_index_data$quality.of.location <- ordered(rent_index_data$quality.of.location,
                         levels = c(1, 2, 3))


#Variance comparison
# install.packages("ggpubr")
library("ggpubr")
ggboxplot(rent_index_data, x = "quality.of.location", y = "rentpsqm", 
          color = "quality.of.location", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c(1,2,3),
          ylab = "rentpsqm", xlab = "quality.of.location")

# how to make a QQ plot in R
# x = rnorm(100, 50, 25)
# y = rnorm(100, 50, 25)

# qqplot function in r package
# qqplot(x, y, xlab = "test x", ylab = "test y", main = "Q-Q Plot")

#QQPlot for normality assessment
library(ggplot2)
for (i in rent_index_data$quality.of.location){
  
  new_df <- rent_index_data[rent_index_data$quality.of.location == i,]
  
  p <- ggplot(data = new_df, mapping = aes(sample = rentpsqm)) + 
    geom_qq(colour = "darkcyan") + 
    geom_qq_line(size = 1) +
    labs(x = "", y = i)
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1.5))
  
  assign(paste0("qq_",i,"_plot"),p)
}

# install.packages("gridExtra")
library(gridExtra)
qq_1_plot # Quality of location 1
qq_2_plot # Quality of location 2
qq_3_plot # Quality of location 3
grid.arrange(qq_1_plot, qq_2_plot, qq_3_plot, ncol = 3, nrow = 1)


# Task 1 - Kruskal-Wallis test
res <- kruskal.test(rentpsqm ~ quality.of.location, data = rent_index_data)
# Summary of the analysis
res


# Pairwise Wilcox test without adjustment
pairwise.wilcox.test(x = rent_index_data$rentpsqm, g = rent_index_data$quality.of.location,
                     p.adjust.method = "none", paired = FALSE)


# Pairwise Test with Bonferroni adjustment
pairwise.wilcox.test(x = rent_index_data$rentpsqm, g = rent_index_data$quality.of.location,
                p.adjust.method = "bonferroni", paired = FALSE) #, pool.sd = TRUE

