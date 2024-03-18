library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(tidyverse)


# Total References Bar Plot
# Data cleaning
KSdata <- read_excel("KS_data.xlsx", sheet = 2)

KSD <- KSdata[3:16, ]
KSD1 <- KSD |>
  select(c("...1", "Total number of references"))
colnames(KSD1) <- c("KDC", "total")

KSI <- KSdata[19:23, ]
KSI1 <- KSI |>
  select(c("...1", "Total number of references"))
colnames(KSI1) <- c("KII", "total")

CI <- KSdata[26:31, ]
CI1 <- CI |>
  select(c("...1", "Total number of references"))
colnames(CI1) <- c("CI", "total")

Age <- KSdata[34:37, ]
Age1 <- Age |>
  select(c("...1", "Total number of references"))
colnames(Age1) <- c("age", "total")

# Plot of the data
CI1$color_value <- CI1$total / sum(CI1$total)
p1 <- ggplot(CI1, aes(x = total, y = reorder(CI, total), fill = color_value)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_gradient(low="lightgrey", high="darkblue") +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = margin(t = 10, r = 5, b = 10, l = 0, unit = "pt")) +
  labs(x = NULL, y = " KS Caregiver 
Impacts")
p1 <- p1 + theme(
  axis.title.y = element_text(margin = margin(t = 0, r = 59, b = 0, l = 10, unit = "pt"))
)

KSI1$color_value <- KSI1$total / sum(KSI1$total)
p2 <- ggplot(KSI1, aes(x = total, y = reorder(KII, total), fill = color_value)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_gradient(low="grey", high="darkgreen") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 0, unit = "pt")) +
  labs(x = NULL, y = "KS Individual 
Impacts")
p2 <- p2 + theme(
  axis.title.y = element_text(margin = margin(t = 0, r = 144, b = 0, l = 10, unit = "pt"))
)

KSD1$color_value <- KSD1$total / sum(KSD1$total)
p3 <- ggplot(KSD1, aes(x = total, y = reorder(KDC, total), fill = color_value)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_gradient(low="lightgrey", high="darkred") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 0, unit = "pt")) +
  labs(x = NULL, y = "KS Defining 
Features")
p3 <- p3 + theme(
  axis.title.y = element_text(margin = margin(t = 0, r = 74, b = 0, l = 10, unit = "pt"))
)

Age1$color_value <- Age1$total / sum(Age1$total)
p4 <- ggplot(Age1, aes(x = total, y = reorder(age, total), fill = color_value)) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_gradient(low="lightgrey", high="purple") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.y = element_text(),
        plot.margin = margin(t = 10, r = 5, b = 10, l = 0, unit = "pt")) +
  labs(x = NULL, y = "Developmental Age 
Groups")
p4 <- p4 + theme(
  axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 10, unit = "pt"))
)

p4 <- p4 + 
  labs(x = "References")

p5 <- grid.arrange(p3, p2, p1, p4, ncol = 1, heights = c(2, 1, 1, 1))
#generate plot
grid.arrange(p5)

########################################
#Heatmap

#load in data
KS_data <- read.csv("KS_lifespan.csv")
KS_data$X <- str_replace(KS_data$X, " impact", "")
KS_data$X <- str_replace(KS_data$X, " and ", " & ")

#defining concepts subset data
concepts <- KS_data[2:15, -6]

#rename for ease
concepts <- rename(concepts, Symptoms = `X`)
concepts <- rename(concepts, `< 2` = `under.2.years.of.age..infancy...N.16.`,
                   `2-4` = `X2.4.years.of.age...early.childhood...N.15.`,
                   `5-11` = `X5.11.years.of.age..late.childhood...N.13.`,
                   `12-17` = `X12.17.years.of.age..adolescence.and.teenage.years...N.6.`)

#find sum of individual columns
conc_csums <- colSums(concepts[ , -1])

#divide cell values by column total for frequency
#put frequency values in new columns
concepts <- concepts |>
  mutate(
    `Under 2` = `< 2` / conc_csums[1],
    `2-4 Years` = `2-4` / conc_csums[2],
    `5-11 Years` = `5-11` / conc_csums[3],
    `12-17 Years` = `12-17` / conc_csums[4]
  )

#format for graphing
concepts_2 <- select(concepts, -`< 2`, -`2-4`, -`5-11`, -`12-17`)
concepts_2 <- pivot_longer(concepts_2, cols = c(`Under 2`, `2-4 Years`, `5-11 Years`, `12-17 Years`), names_to = "age", values_to = "Frequencies")

concepts_2$age <- factor(concepts_2$age, levels = c("Under 2", "2-4 Years", "5-11 Years", "12-17 Years"))

concepts_2$Symptoms <- factor(concepts_2$Symptoms, levels = c("Immune", "Hearing", 
                                                              "Opthalmalogic", "Musculoskeletal",
                                                              'Post-pubertal regression',
                                                              "Cognition", "Cardiorespiratory", 
                                                              "Sleep", "Emotional", "Gastrointestinal", 
                                                              "Neurological",
                                                              "Behavioral & Neurodevelopmental", 
                                                              "Motor", "Communication"))

#heatmap
heatmap_1 <- ggplot(concepts_2, aes(x=age, y=Symptoms, fill= Frequencies)) +
  geom_tile(color = "black") +  
  geom_tile(color = "white", fill = NA) +  
  geom_tile(color = "white", fill = NA) +  
  geom_text(aes(label = sprintf("%.2f", Frequencies)), color = "black", size = 5) +  # Add text labels
  scale_fill_gradient(low="white", high="darkred") + 
  labs(x = NULL, y = "A. KS Defining Features")+
  scale_x_discrete(expand = c(0, 0), position = 'top', labels = c("Under 2", limits = c(0, 1))) +
  theme_minimal() +  
  theme(panel.grid = element_blank())+
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(face = "bold", size = 13)
  )


#individual impacts subset data
ind_impacts <- KS_data[18:21, -6]

ind_impacts <- rename(ind_impacts, Impacts = `X`)
ind_impacts <- rename(ind_impacts, `< 2` = `under.2.years.of.age..infancy...N.16.`,
                      `2-4` = `X2.4.years.of.age...early.childhood...N.15.`,
                      `5-11` = `X5.11.years.of.age..late.childhood...N.13.`,
                      `12-17` = `X12.17.years.of.age..adolescence.and.teenage.years...N.6.`)


#find sum of individual columns
ind_csums <- colSums(ind_impacts[ ,-1])

#divide cell values by column total for frequency
#put frequency values in new columns
ind_impacts <- ind_impacts |>
  mutate(
    `Under 2` = `< 2` / ind_csums[1],
    `2-4 Years` = `2-4` / ind_csums[2],
    `5-11 Years` = `5-11` / ind_csums[3],
    `12-17 Years` = `12-17` / ind_csums[4]
  )

#format for graphing
ind_impacts_2 <- select(ind_impacts, -`< 2`, -`2-4`, -`5-11`, -`12-17`)
ind_impacts_2 <- pivot_longer(ind_impacts_2, cols = c(`Under 2`, `2-4 Years`, `5-11 Years`, `12-17 Years`), names_to = "age", values_to = "Frequencies")

ind_impacts_2$age <- factor(ind_impacts_2$age, levels = c("Under 2", "2-4 Years", "5-11 Years", "12-17 Years"))

ind_impacts_2$Impacts <- factor(ind_impacts_2$Impacts, levels = c("Education",
                                                                  "Daily living",
                                                                  "Social",
                                                                  "Health"))

#heatmap
heatmap_2 <- ggplot(ind_impacts_2, aes(x=age, y=Impacts, fill= Frequencies)) +
  geom_tile(color = "black") +  
  geom_tile(color = "white", fill = NA) + 
  geom_tile(color = "white", fill = NA) +  
  geom_text(aes(label = sprintf("%.2f", Frequencies)), color = "black", size = 5) +  # Add text labels
  scale_fill_gradient(low = "white", high = "darkgreen", limit = c(0,1))+
  labs(x = NULL, y = "B. KS Individual Impacts")+
  scale_x_discrete(expand = c(0, 0),position = 'top') +
  theme_minimal() +  
  theme(panel.grid = element_blank())+
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 0, r = .1, b = 0, l = .1, unit = "cm")
  )


#caregiver impacts subset data
care_impacts <- KS_data[24:27, -6]

care_impacts <- rename(care_impacts, Impacts = `X`)
care_impacts <- rename(care_impacts, `< 2` = `under.2.years.of.age..infancy...N.16.`,
                       `2-4` = `X2.4.years.of.age...early.childhood...N.15.`,
                       `5-11` = `X5.11.years.of.age..late.childhood...N.13.`,
                       `12-17` = `X12.17.years.of.age..adolescence.and.teenage.years...N.6.`)

#find sum of individual columns
care_csums <- colSums(care_impacts[ ,-1])

#divide cell values by column total for frequency
#put frequency values in new columns
care_impacts <- care_impacts |>
  mutate(
    `Under 2` = `< 2` / care_csums[1],
    `2-4 Years` = `2-4` / care_csums[2],
    `5-11 Years` = `5-11` / care_csums[3],
    `12-17 Years` = `12-17` / care_csums[4]
  )

#format for graphing
care_impacts_2 <- select(care_impacts, -`< 2`, -`2-4`, -`5-11`, -`12-17`)
care_impacts_2 <- pivot_longer(care_impacts_2, cols = c(`Under 2`, `2-4 Years`, `5-11 Years`, `12-17 Years`), names_to = "age", values_to = "Frequencies")

care_impacts_2$age <- factor(care_impacts_2$age, levels = c("Under 2", "2-4 Years", "5-11 Years", "12-17 Years"))

care_impacts_2$Impacts <- factor(care_impacts_2$Impacts, levels = c("Health",
                                                                    'Financial & professional',
                                                                    "Social",
                                                                    "Emotional"))


#heatmap
heatmap_3 <- ggplot(care_impacts_2, aes(x=age, y=Impacts, fill= Frequencies)) +
  geom_tile(color = "black") +  
  geom_tile(color = "white", fill = NA) +  
  geom_text(aes(label = sprintf("%.2f", Frequencies)), color = "black", size = 5) +  
  scale_fill_gradient(low="white", high="darkblue", limits = c(0,1)) + 
  labs(x = NULL, y = "C. KS Caregiver Impacts")+
  scale_x_discrete(expand = c(0, 0),position = 'top') +
  theme_minimal() +  
  theme(panel.grid = element_blank())+
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 0, r = .1, b = 0, l = .1, unit = "cm")
  )
#generate plot
grid.arrange(heatmap_1, arrangeGrob(heatmap_2, heatmap_3), ncol=2)

########################################
#Stacked barplot

#load data
lifespan <- read.csv("KS_lifespan.csv")
lifespan$X <- str_replace(lifespan$X, " impact", "")
lifespan$X <- str_replace(lifespan$X, "and", "&")
#data set for references to symptoms
symptoms <- lifespan[2:15,-6]
colnames(symptoms) <- c("symptom", "1", "2", "3", "4")
symptoms <- symptoms %>% pivot_longer(names_to = "age", values_to = "value", cols = 2:5)
#data set for references to individual impacts
ind_impacts <- lifespan[18:21,-6]
colnames(ind_impacts) <- c("impact", "1", "2", "3", "4")
ind_impacts <- ind_impacts %>% pivot_longer(names_to = "age", values_to = "value", cols = 2:5)
#data set for references to caregiver impacts
care_impacts <- lifespan[24:27,-6]
colnames(care_impacts) <- c("impact", "1", "2", "3", "4")
care_impacts <- care_impacts %>% pivot_longer(names_to = "age", values_to = "value", cols = 2:5)

#frequencies
symptoms_freq <- symptoms %>% group_by(age) %>% 
  mutate(sum = sum(value), freq = value/sum)

symptoms$symptom <- factor(symptoms$symptom, levels=c('Communication',
                                                      'Motor',
                                                      'Behavioral & Neurodevelopmental',
                                                      'Neurological',
                                                      'Gastrointestinal',
                                                      'Emotional',
                                                      'Sleep',
                                                      'Cardiorespiratory',
                                                      'Cognition',
                                                      'Post-pubertal regression',
                                                      'Musculoskeletal',
                                                      'Opthalmalogic',
                                                      'Hearing',
                                                      'Immune'
))

#stacked barplots for KS Defining Concepts
plot1 <- ggplot(symptoms, aes(fill=symptom, y=value, x=age)) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Age Group", y = "Frequencies") + 
  theme_grey() +
  scale_fill_manual(name = "A. KS Defining Features",
                    values = c("#cc6666", "#cc8a90", "#ccb966", "#e59366", "#2d9f48", "#71b04d", "#94e1a6", "#66bbe5", "#6b98e6", "#4052c9", "#7e40c9", "#a766cc", "#c94072", "#d15c5c"))+
  scale_x_discrete(labels=c("<2","2-4","5-11", "12-17")) +
  theme(legend.title = element_text(face = "bold")) +
  annotate("text", x = 1, y = 0.85, label = 0.1786) +
  annotate("text", x = 2, y = 0.66, label = 0.2393) +
  annotate("text", x = 3, y = 0.90, label = 0.2034) +
  annotate("text", x = 4, y = 0.72, label = 0.1749)

#frequencies
ind_freq <- ind_impacts %>% group_by(age) %>% 
  mutate(sum = sum(value), freq = value/sum)

ind_impacts$impact <- factor(ind_impacts$impact, levels=c('Health',
                                                          'Social',
                                                          'Daily living',
                                                          'Education'))

#stacked barplots for KS individual impacts
plot2 <- ggplot(ind_impacts, aes(fill=impact, y=value, x=age)) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Age Group", y = "Frequencies") + 
  theme_grey() +
  scale_fill_manual(name = "B. KS Individual Impacts",
                    values = c("#bd8dc9", "#6f99bf","#ba6575", "#637594"))+
  scale_x_discrete(labels=c("<2","2-4","5-11", "12-17")) +
  theme(legend.title = element_text(face = "bold")) +
  annotate("text", x = 1, y = 0.77, label = 0.9355) +
  annotate("text", x = 2, y = 0.77, label = 0.65) +
  annotate("text", x = 3, y = 0.77, label = 0.4957) +
  annotate("text", x = 4, y = 0.77, label = 0.3626)

#frequencies
care_freq <- care_impacts %>% group_by(age) %>% 
  mutate(sum = sum(value), freq = value/sum)

care_impacts$impact <- factor(care_impacts$impact, levels=c('Emotional',
                                                            'Social',
                                                            'Financial & professional',
                                                            'Health'))

#stacked barplots for KS caregiver impacts
plot3 <- ggplot(care_impacts, aes(fill=impact, y=value, x=age)) + 
  geom_bar(position="fill", stat="identity") +
  labs(x = "Age Group", y = "Frequencies") + 
  theme_grey() +
  scale_fill_manual(name = "C. KS Caregiver Impacts",
                    values = c("#71b04d", "#6f99bf","#e09a6e", "#bd8dc9"))+
  scale_x_discrete(labels=c("<2","2-4","5-11", "12-17")) +
  theme(legend.title = element_text(face = "bold")) +
  annotate("text", x = 1, y = 0.75, label = 0.6667) +
  annotate("text", x = 2, y = 0.75, label = 0.5366) +
  annotate("text", x = 3, y = 0.75, label = 0.4875) +
  annotate("text", x = 4, y = 0.49, label = 0.4028)

#generate plot
grid.arrange(plot1, arrangeGrob(plot2, plot3), ncol = 2)