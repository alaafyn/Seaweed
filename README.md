# Alpha diversity
# Load necessary libraries
library(dplyr)
library(tidyverse)
library(qiime2R)
library(ggpubr)


# Read timemeta
timemeta <- read_q2metadata("project_metadata.txt")

# Filter timemeta for each time point
timemeta_time1 <- timemeta %>% filter(`time-point` == "Day15")
timemeta_time2 <- timemeta %>% filter(`time-point` == "Day22")

# Read alpha diversity data
evenness <- read_qza("evenness_vector.qza")$data %>% rownames_to_column("SampleID")
observed_features <- read_qza("observed_features_vector.qza")$data %>% rownames_to_column("SampleID")
shannon <- read_qza("shannon_vector.qza")$data %>% rownames_to_column("SampleID")
faith_pd <- read_qza("faith_pd_vector.qza")$data %>% rownames_to_column("SampleID")

# Filter alpha diversity data for each time point
evenness_time1 <- evenness %>% filter(SampleID %in% timemeta_time1$SampleID)
evenness_time2 <- evenness %>% filter(SampleID %in% timemeta_time2$SampleID)

observed_features_time1 <- observed_features %>% filter(SampleID %in% timemeta_time1$SampleID)
observed_features_time2 <- observed_features %>% filter(SampleID %in% timemeta_time2$SampleID)

shannon_time1 <- shannon %>% filter(SampleID %in% timemeta_time1$SampleID)
shannon_time2 <- shannon %>% filter(SampleID %in% timemeta_time2$SampleID)




faith_pd <- read_qza("faith_pd_vector.qza")
faith_pd<-faith_pd$data %>% rownames_to_column("SampleID")
faith_pd <- faith_pd[,-1]
colnames(faith_pd) <- c('SampleID', 'faith_pd')

faith_pd_time1 <- faith_pd %>% filter(SampleID %in% timemeta_time1$SampleID)
faith_pd_time2 <- faith_pd %>% filter(SampleID %in% timemeta_time2$SampleID)

# Now you can perform alpha diversity analysis separately for each time point
# For example, you can calculate means, perform statistical tests, or create plots for each time point

# Example: Mean alpha diversity for each time point
mean_evenness_time1 <- mean(evenness_time1$pielou_evenness)
mean_evenness_time2 <- mean(evenness_time2$pielou_evenness)

mean_observed_features_time1 <- mean(observed_features_time1$observed_features)
mean_observed_features_time2 <- mean(observed_features_time2$observed_features)

mean_shannon_time1 <- mean(shannon_time1$shannon_entropy)
mean_shannon_time2 <- mean(shannon_time2$shannon_entropy)

mean_faith_pd_time1 <- mean(faith_pd_time1$faith_pd)
mean_faith_pd_time2 <- mean(faith_pd_time2$faith_pd)

# Then you can perform statistical tests or create plots based on these means

# Assuming evenness, observed_features, shannon, and faith_pd have been filtered for each time point as shown in your code.

# Combine alpha diversity metrics into a single dataframe for each time point.
alpha_diversity_time1 <- reduce(list(faith_pd_time1, evenness_time1, observed_features_time1, shannon_time1), full_join, by = "SampleID")
alpha_diversity_time2 <- reduce(list(faith_pd_time2, evenness_time2, observed_features_time2, shannon_time2), full_join, by = "SampleID")

# Ensure all relevant metric names are correct and consistent across dataframes before merging.

# Merge combined alpha diversity data with the corresponding time-specific metadata.
metadata_time1 <- merge(timemeta_time1, alpha_diversity_time1, by = "SampleID")
metadata_time2 <- merge(timemeta_time2, alpha_diversity_time2, by = "SampleID")

# Now, metadata_time1 and metadata_time2 contain all the relevant information for each time point.

# Example analysis: Calculating mean alpha diversity metrics for each time point
# You've already done this. You can further perform statistical tests or generate plots for each time point separately.


# Plotting Faith PD for time 1)
ggplot(metadata_time1, aes(x = treatment, y = faith_pd)) +
  geom_boxplot() +
  labs(title = "Faith PD by Treatment for Day 15", x = "Treatment", y = "Faith PD") +
  theme_minimal()

# Create directory to store the plots
dir.create("alpha_diversity_plots", showWarnings = FALSE)

# Combine both time-point plots into a facet with color
combined_plot <- ggplot(mapping = aes(x = treatment, y = faith_pd, fill = treatment)) +
  geom_boxplot(data = metadata_time1, alpha = 0.7) +
  geom_boxplot(data = metadata_time2, alpha = 0.7) +
  labs(title = "Faith PD by Treatment", x = "Treatment", y = "Faith PD") +
  theme_minimal() +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#F0E442"), name = "Treatment") +
  facet_wrap(~`time-point`)+
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
plot.background = element_rect(fill = "white"))

# Print the combined facet plot
print(combined_plot)
# Save the plot to a specific path
pathFPD <- "alpha_diversity_plots/FaithPD_plot.png"
ggsave(filename = pathFPD, plot = combined_plot, width = 10, height = 5)



# Shapiro-Wilk test for normality
shapiro.test(metadata_time1$faith_pd)  #normal
shapiro.test(metadata_time2$faith_pd) #not normal


#ANOVA and Kruskal Walis
aovfpd <- aov(faith_pd ~ treatment, data = metadata_time1)
summary(aovfpd)
kruskal.fp <- kruskal.test(faith_pd~ treatment, data = metadata_time2)
kruskal_p_value <- kruskal.fp$p.value

#the above are recorded in mean, so we have to get rid of boxplot
#and use mean bar

# Plot Faith PD with standard errors for time1
meanfpd <- metadata_time1 %>%
  group_by(treatment) %>%
  summarise(mean_faith_pd = mean(faith_pd),
            sd_faith_pd = sd(faith_pd),
            n = n(),
            se_faith_pd = sd_faith_pd / sqrt(n))


# Create plot for Faith PD
fpd_plot <- ggplot(meanfpd, aes(x = treatment, y = mean_faith_pd, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_faith_pd - se_faith_pd, ymax = mean_faith_pd + se_faith_pd), width = 0.2) +
  labs(y = "Mean Faith PD", x = "Treatment") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.title = element_blank())

fpd_plot
ggsave("Mean_Faith_PD_Day15.png", fpd_plot, path = "alpha_diversity_plots")


# Plot Faith PD with standard errors for time2
meanfpd2 <- metadata_time2 %>%
  group_by(treatment) %>%
  summarise(mean_faith_pd2 = mean(faith_pd),
            sd_faith_pd2 = sd(faith_pd),
            n = n(),
            se_faith_pd2 = sd_faith_pd2 / sqrt(n))

# Create plot for Faith PD
fpd_plot2 <- ggplot(meanfpd2, aes(x = treatment, y = mean_faith_pd2, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_faith_pd2 - se_faith_pd2, ymax = mean_faith_pd2 + se_faith_pd2), width = 0.2) +
  labs(y = "Mean Faith PD", x = "Treatment") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.title = element_blank())

print(fpd_plot2)
ggsave("Mean_Faith_PD_Day22.png", fpd_plot2, path = "alpha_diversity_plots")

#combining the two
# Add a 'time_point' column to each data frame
meanfpd$time_point <- "Day 15"
meanfpd2$time_point <- "Day 22"


colnames(meanfpd)
colnames(meanfpd2)
colnames(meanfpd2) <- c("treatment", "mean_faith_pd", "sd_faith_pd", "n",
                       "se_faith_pd", "time_point")

# Combine the data frames
fpdcomb_data <- rbind(meanfpd, meanfpd2)

# Define custom colors for treatments
custom_colors <- c("#0072B2", "#D55E00", "#009E73", "#F0E442")

# Create the plot
fpdcomb_plots <- ggplot(fpdcomb_data, aes(x = treatment, y = mean_faith_pd, fill = treatment)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_faith_pd - se_faith_pd, ymax = mean_faith_pd + se_faith_pd), width = 0.2) +
  labs(title = "Faith Phylodiversity ", x = "Treatment", y = "Faith PD") +
  scale_fill_manual(values = custom_colors, name = "Treatment") +
  facet_wrap(~time_point, scales = "free_y") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "white"))
print(fpdcomb_plots)
ggsave("Mean_FithPD_Combined.png", fpdcomb_plots, path = "alpha_diversity_plots", width = 10, height = 5)




#####shannon#####
shannon_plot <- ggplot(mapping = aes(x = treatment, y = shannon_entropy, fill = treatment)) +
  geom_boxplot(data = metadata_time1, alpha = 0.7) +
  geom_boxplot(data = metadata_time2, alpha = 0.7) +
  labs(title = "Shannon Diversity Index", x = "Treatment", y = "Shannon Index") +
  theme_minimal() +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#F0E442"), name = "Treatment") +
  facet_wrap(~`time-point`)+
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "white"))

# Print the combined facet plot
print(shannon_plot)

# Save the plot to a specific path
pathsh <- "alpha_diversity_plots/Shannon_plot.png"
ggsave(filename = pathsh, plot = shannon_plot, width = 10, height = 5)


# Shapiro-Wilk test for normality
shapiro.test(metadata_time1$shannon_entropy)  #not  normal
shapiro.test(metadata_time2$shannon_entropy) #not normal

# Kruskal-Wallis test for shannon_entropy
kruskal.test(shannon_entropy ~ treatment, data = metadata_time1)
kruskal.test(shannon_entropy ~ treatment, data = metadata_time2)

# Plot Shannon Entropy with standard errors for time1
shannonn<- metadata_time1 %>%
  group_by(treatment) %>%
  summarise(mean_shannon_entropy = mean(shannon_entropy),
            sd_shannon_entropy = sd(shannon_entropy),
            n = n(),
            se_shannon_entropy = sd_shannon_entropy / sqrt(n))

# Create plot for Shannon Entropy
shannon_plot <- ggplot(shannonn, aes(x = treatment, y = mean_shannon_entropy, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_shannon_entropy - se_shannon_entropy, ymax = mean_shannon_entropy + se_shannon_entropy), width = 0.2) +
  labs(y = "Mean Shannon Entropy", x = "Treatment") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.title = element_blank())

print(shannon_plot)
ggsave("Mean_Shannon_Entropy_Day15.png", shannon_plot, path = "alpha_diversity_plots")

# Plot Shannon Entropy with standard errors for time2
shannonn2 <- metadata_time2 %>%
  group_by(treatment) %>%
  summarise(mean_shannon_entropy2 = mean(shannon_entropy),
            sd_shannon_entropy2 = sd(shannon_entropy),
            n = n(),
            se_shannon_entropy2 = sd_shannon_entropy2 / sqrt(n))

# Create plot for Shannon Entropy
shannon_plot2 <- ggplot(shannonn2, aes(x = treatment, y = mean_shannon_entropy2, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_shannon_entropy2 - se_shannon_entropy2, ymax = mean_shannon_entropy2 + se_shannon_entropy2), width = 0.2) +
  labs(y = "Mean Shannon Entropy D22", x = "Treatment") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.title = element_blank())

print(shannon_plot2)
ggsave("Mean_Shannon_Entropy_Day22.png", shannon_plot2, path = "alpha_diversity_plots")

#combining the two
# Add a 'time_point' column to each data frame
shannonn$time_point <- "Day 15"
shannonn2$time_point <- "Day 22"


colnames(shannonn)
colnames(shannonn2)
colnames(shannonn2) <- c("treatment", "mean_shannon_entropy", "sd_shannon_entropy",
                         "n", "se_shannon_entropy" , "time_point")

# Combine the data frames
Shacomb_data <- rbind(shannonn, shannonn2)

# Define custom colors for treatments
custom_colors <- c("#0072B2", "#D55E00", "#009E73", "#F0E442")

# Create the plot
Shacomb_plots <- ggplot(Shacomb_data, aes(x = treatment, y = mean_shannon_entropy, fill = treatment)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_shannon_entropy - se_shannon_entropy, ymax = mean_shannon_entropy + se_shannon_entropy), width = 0.2) +
  labs(title = "Shannon Diversity Index", x = "Treatment", y = "Shannon Index") +
  scale_fill_manual(values = custom_colors, name = "Treatment") +
  facet_wrap(~time_point, scales = "free_y") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "white"))
print(Shacomb_plots)
ggsave("Mean_Shannon_Combined.png", Shacomb_plots, path = "alpha_diversity_plots", width = 10, height = 5)


#EVENNESS

Eveness_plot <- ggplot(mapping = aes(x = treatment, y = pielou_evenness, fill = treatment)) +
  geom_boxplot(data = metadata_time1, alpha = 0.7) +
  geom_boxplot(data = metadata_time2, alpha = 0.7) +
  labs(title = "Pielou Evenness Index", x = "Treatment", y = "Pielou Evenness") +
  theme_minimal() +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#F0E442"), name = "Treatment") +
  facet_wrap(~`time-point`)+
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "white"))

print(Eveness_plot)
path <- "alpha_diversity_plots/Eveness.png"
ggsave(filename = path, plot = Eveness_plot, width = 10, height = 5)

# Shapiro-Wilk test for normality
shapiro.test(metadata_time1$pielou_evenness) #normal
shapiro.test(metadata_time2$pielou_evenness) #not normal

#ANOVA and Kruskal Walis
aoveveness <- aov(pielou_evenness ~ treatment, data = metadata_time1)
summary(aoveveness)
kruskal.test(pielou_evenness ~ treatment, data = metadata_time2)


# Plot Shannon Entropy with standard errors for time1
pielou<- metadata_time1 %>%
  group_by(treatment) %>%
  summarise(mean_pielou= mean(pielou_evenness),
            sd_pielou = sd(pielou_evenness),
            n = n(),
            se_pielou = sd_pielou / sqrt(n))

# Create plot for pielou_evenness
pielou_plot <- ggplot(pielou, aes(x = treatment, y = mean_pielou, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_pielou - se_pielou, ymax = mean_pielou + se_pielou), width = 0.2) +
  labs(y = "Mean Pielou Evenness", x = "Treatment") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.title = element_blank())

print(pielou_plot)
ggsave("Mean_pielou_plot_Day15.png", pielou_plot, path = "alpha_diversity_plots")

# Plot Shannon Entropy with standard errors for time2
pielou2<- metadata_time2 %>%
  group_by(treatment) %>%
  summarise(mean_pielou2= mean(pielou_evenness),
            sd_pielou2 = sd(pielou_evenness),
            n = n(),
            se_pielou2 = sd_pielou2 / sqrt(n))

# Create plot for pielou_evenness
pielou_plot2 <- ggplot(pielou2, aes(x = treatment, y = mean_pielou2, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_pielou2 - se_pielou2, ymax = mean_pielou2 + se_pielou2), width = 0.2) +
  labs(y = "Mean Pielou Evenness", x = "Treatment") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.title = element_blank())

print(pielou_plot2)
ggsave("Mean_pielou_plot_Day22.png", pielou_plot2, path = "alpha_diversity_plots")

#combining the two
pielou$time_point <- "Day 15"
pielou2$time_point <- "Day 22"

colnames(pielou)
colnames(pielou2)
colnames(pielou2) <- c("treatment", "mean_pielou", "sd_pielou", "n", "se_pielou" ,
                          "time_point")

# Combine the data frames
piecomb_data <- rbind(pielou, pielou2)

# Define custom colors for treatments
custom_colors <- c("#0072B2", "#D55E00", "#009E73", "#F0E442")

# Create the plot
piecomb_plots <- ggplot(piecomb_data, aes(x = treatment, y = mean_pielou, fill = treatment)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_pielou - se_pielou, ymax = mean_pielou + se_pielou), width = 0.2) +
  labs(title = "Pielou Evenness", x = "Treatment", y = "Pielou Evenness") +
  scale_fill_manual(values = custom_colors, name = "Treatment") +
  facet_wrap(~time_point, scales = "free_y") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "white"))
print(piecomb_plots)
ggsave("mean Pielou Evenness.png", piecomb_plots, path = "alpha_diversity_plots", width = 10, height = 5)

##############################################################################################
#observed features#####
obs_plot <- ggplot(mapping = aes(x = treatment, y = observed_features, fill = treatment)) +
  geom_boxplot(data = metadata_time1, alpha = 0.7) +
  geom_boxplot(data = metadata_time2, alpha = 0.7) +
  labs(title = "Observed features", x = "Treatment", y = "Observed Features") +
  theme_minimal() +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#F0E442"), name = "Treatment") +
  facet_wrap(~`time-point`)+
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "white"))

print(obs_plot)
path <- "alpha_diversity_plots/observed_features.png"
ggsave(filename = path, plot = obs_plot, width = 10, height = 5)

# Shapiro-Wilk test for normality
shapiro.test(metadata_time1$observed_features) #normal
shapiro.test(metadata_time2$observed_features) #normal


#if its normal and categorical, use ANOVA.
aov.observed_features.time1 <- aov(observed_features ~ treatment, data = metadata_time1)
summary(aov.observed_features.time1)

aov.observed_features.time2 <- aov(observed_features ~ `treatment`, data = metadata_time2)
summary(aov.observed_features.time2)

#ANOVA USE MEAN, BOXPLOT USES MEDIAN, SO WE NEED TO FIND MEAN DATA TO TALLY WITH OUR ANOVA
# Plot Observed Features with standard errors
obs_features <- metadata_time1 %>%
  group_by(treatment) %>%
  summarise(mean_obs_features = mean(observed_features),
            sd_obs_features = sd(observed_features),
            n = n(),
            se_obs_features = sd_obs_features / sqrt(n))
obs_features 
# Define custom colors for the treatments (assuming 4 treatments as per previous context)
custom_colors <- c("#0072B2", "#D55E00", "#009E73", "#F0E442")

obs_features_se <- ggplot(obs_features, aes(x = treatment, y = mean_obs_features, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_obs_features - se_obs_features, ymax = mean_obs_features + se_obs_features), width = 0.2) +
  scale_fill_manual(values = custom_colors) +
  labs(y = "Mean Observed Features", x = "Treatment") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA), # White background
        panel.border = element_rect(colour = "black", fill=NA, size=1), # Black border around each plot
        legend.title = element_blank())

print(obs_features_se)
ggsave("Mean Observe features_day15.png", obs_features_se, path = "alpha_diversity_plots") 


# Plot Observed Features with standard errors
obs_features2 <- metadata_time2 %>%
  group_by(treatment) %>%
  summarise(mean_obs_features2 = mean(observed_features),
            sd_obs_features2 = sd(metadata_time2$observed_features),
            n = n(),
            se_obs_features2 = sd_obs_features2 / sqrt(n))
obs_features2 
# Define custom colors for the treatments (assuming 4 treatments as per previous context)
custom_colors <- c("#0072B2", "#D55E00", "#009E73", "#F0E442")

obs_features_se2 <- ggplot(obs_features2, aes(x = treatment, y = mean_obs_features2, fill = treatment)) +
  geom_col(show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_obs_features2 - se_obs_features2, ymax = mean_obs_features2 + se_obs_features2), width = 0.2) +
  scale_fill_manual(values = custom_colors) +
  labs(y = "Mean Observed FeaturesD22", x = "Treatment") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.background = element_rect(fill = "white", colour = NA), # White background
        panel.border = element_rect(colour = "black", fill=NA, size=1), # Black border around each plot
        legend.title = element_blank())

print(obs_features_se2)
ggsave("Mean Observe features_day22.png", obs_features_se2, path = "alpha_diversity_plots") 

#combining the two
# Add a 'time_point' column to each data frame
obs_features$time_point <- "Day 15"
obs_features2$time_point <- "Day 22"


colnames(obs_features)
colnames(obs_features2)

# Rename columns in obs_features2 to match obs_features
colnames(obs_features2) <- c("treatment", "mean_obs_features", "sd_obs_features", "n", "se_obs_features", "time_point")

# Combine the data frames
combined_data <- rbind(obs_features, obs_features2)

# Define custom colors for treatments
custom_colors <- c("#0072B2", "#D55E00", "#009E73", "#F0E442")

# Create the plot
observe_plots <- ggplot(combined_data, aes(x = treatment, y = mean_obs_features, fill = treatment)) +
  geom_col(alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_obs_features - se_obs_features, ymax = mean_obs_features + se_obs_features), width = 0.2) +
  labs(title = "Observed Features", x = "Treatment", y = "Mean Observed Features") +
  scale_fill_manual(values = custom_colors, name = "Treatment") +
  facet_wrap(~time_point, scales = "free_y") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        plot.background = element_rect(fill = "white"))
print(observe_plots)
ggsave("Mean_Observed_Features_Combined.png", observe_plots, path = "alpha_diversity_plots", width = 10, height = 5)

#Combining all boxplots
library(ggpubr)
figure <- ggarrange(combined_plot, shannon_plot, obs_plot, 
                    Eveness_plot,
                    labels = c("A", "B", "C", "D"),common.legend = TRUE,
                    legend = "bottom", ncol = 2, nrow = 2)
figure
path <- "alpha_diversity_plots/alpha diversity.png"
ggsave(filename = path, plot = figure, width = 10, height = 5)

#Combining all bar plots
figure2 <- ggarrange(observe_plots, piecomb_plots, Shacomb_plots, 
                     fpdcomb_plots,
                    labels = c("A", "B", "C", "D"),common.legend = TRUE,
                    legend = "bottom", ncol = 2, nrow = 2)
figure2
path2 <- "alpha_diversity_plots/alpha diversity2.png"
ggsave(filename = path2, plot = figure2, width = 10, height = 5)


#######################################################################
#Alpha and BETA_ Diversity using Phyloseq

library(DECIPHER) # This package will help in importing, maintaining, analyzing, manipulating, and exporting a massive amount of sequences.
library(ape) # Analyses of Phylogenetics and Evolution package. Required for tree calculations to be used with phyloseq
library(DESeq2) # This package will help analyze "differential expression" in the microbiota alongside phyloseq
library(ggplot2) # Graphing package used in phyloseq. To edit the default setting of a plot, you need to use functions in this package.
library(phyloseq) # The phyloseq package seeks to address issues with multiple microbiome analysis packages by providing a set of functions that internally manage the organizing, linking, storing, and analyzing of phylogenetic sequencing data. In general, this package is used for UniFrac analyses.
library(plotly) # A package to create interactive web graphics of use in 3D plots
library(vegan) # The vegan package provides tools for descriptive community ecology. It has most basic functions of diversity analysis, community ordination and dissimilarity analysis. In general, this package is used for Bray-Curtis and Jaccard analyses.
library(philr) # This package provides functions for the analysis of compositional data 
library(tidyverse) # This package is designed to make it easy to install and load multiple 'tidyverse' packages in a single step
library(adespatial) # Tools for the multiscale spatial analysis of multivariate data
library(devtools) # Make package development easier by providing R functions that simplify and expedite common tasks
library(qiime2R) # A package for importing qiime artifacts into an R session
library(microbiome) # Data analysis and visualization
library("pander") # provide a minimal and easy tool for rendering R objects into Pandoc's markdown
library(grid) # support data visualization
library(gridExtra)  # support data visualization
library(knitr) # Provides a general-purpose tool for dynamic report generation in R using Literate Programming techniques.
library(png) # Figure download
library(ggdendro) #set of tools for dendrograms and tree plots using 'ggplot2'
library(ggpubr) # publication quality figures, based on ggplot2
library(RColorBrewer) # nice color options
library(microbiomeutilities)
library(reshape2)
library(scales)
library(data.table)


ASVs <- read_qza("rarefied_table.qza")

## Importing metadata
metadata <- read.table("project_metadata.txt", sep='\t', header = TRUE, row.names = 1, comment = "")
metadata <- metadata[-1,] # remove the second line that specifies the data type

## Importing tree
tree <- read_qza("rooted-tree.qza")

## Importing taxonomy
taxonomy <- read_qza("taxonomy.qza")
tax_table <- do.call(rbind, strsplit(as.character(taxonomy$data$Taxon), ";"))


taxonomy <- read_qza("taxonomy.qza")
tax_table <- do.call(rbind, strsplit(as.character(taxonomy$data$Taxon), "; "))
colnames(tax_table) <- c("Kingdom","Phylum","Class","Order","Family","Genus","Species")
rownames(tax_table) <- taxonomy$data$Feature.ID


# Creating phyloseq object
physeq <- phyloseq(
  otu_table(ASVs$data, taxa_are_rows = TRUE),
  phy_tree(tree$data),
  tax_table(tax_table),
  sample_data(metadata)
)

#cleaning_taxtable
ps <- physeq
# Taxonomy table
tax_tab <- phyloseq::tax_table(ps)
tax_tab[1:5,1:5]# check 

# accessing the OTUids 
taxa_names(ps)[1:50] # print first 5 ids
tax_table(ps)[, colnames(tax_table(ps))] <- gsub(tax_table(ps)[,
        colnames(tax_table(ps))],     pattern = "[a-z]__", replacement = "")

#checking
colnames(tax_table(ps))
phyloseq::tax_table(ps)[1:3,1:3]

tax_table(ps)[,colnames(tax_table(ps))] <- gsub(tax_table(ps)[,
                colnames(tax_table(ps))],pattern="=*",replacement="") #replace special character

 #https://microbiome.github.io/tutorials/cleaning_taxonomy_table.html
# For each column separately we attempt to replace "k__<empty>" with "" for consistency.
tax_table(ps)[tax_table(ps) == "k__<empty>"] <- ""
tax_table(ps)[tax_table(ps) == "p__<empty>"] <- ""
tax_table(ps)[tax_table(ps) == "c__<empty>"] <- ""
tax_table(ps)[tax_table(ps) == "o__<empty>"] <- ""
tax_table(ps)[tax_table(ps) == "f__<empty>"] <- ""
tax_table(ps)[tax_table(ps) == "g__<empty>"] <- ""
# some more ambiguities 
tax_table(ps)[tax_table(ps) == "o__Unknown_Order"] <- ""
tax_table(ps)[tax_table(ps) == "c__Unknown_Class"] <- ""
tax_table(ps)[tax_table(ps) == "f__Unknown_Family"] <- ""

ps.gen <- phyloseq::tax_glom(ps, "Genus", NArm = TRUE)
taxa_names(ps.gen)[1:5]
unique(tax_table(ps.gen)[,"Genus"] )
taxa_names(ps) <- gsub("\\[|\\]", "", taxa_names(ps))

physeq <- pssort(sample_sums(physeq))

# If there are NA's in you taxonomy table then do this 
physeq <- ps
tax_table(physeq)[is.na(tax_table(physeq)[,1])] <- "k__"
tax_table(physeq)[is.na(tax_table(physeq)[,2])] <- "p__"
tax_table(physeq)[is.na(tax_table(physeq)[,3])] <- "c__"
tax_table(physeq)[is.na(tax_table(physeq)[,4])] <- "o__"
tax_table(physeq)[is.na(tax_table(physeq)[,5])] <- "f__"
tax_table(physeq)[is.na(tax_table(physeq)[,6])] <- "g__"


# Then you can convert those unclassified such as p__ without phylum name to this or skip this part
tax_table(physeq)[tax_table(physeq)[,"Phylum"]== "p__", "Phylum" ] <- "Unknown_Phylum"

# Alpha Diversity
# First we will create a dataframe containing all diversity measures from above using the `estimate_richness()` function.
adiv <- estimate_richness(physeq, measures = c("Observed", "Shannon", "Simpson", "Chao1", "InvSimpson"))

#https://rpubs.com/charliehobman/fefefe
# Create a data frame for 
physeq.m = sample_data(physeq)
plot_data <- as.data.frame(physeq.m)

# Assign custom colors to treatments
custom_colors <- c("#0072B2", "#D55E00", "#009E73", "#F0E442")

# Create boxplot for Simpson's Diversity Index by time point
simpsonbox <- ggplot(plot_data, aes(x = treatment, y = adiv$Simpson, fill = treatment)) + 
  geom_boxplot() +
  labs(fill = "Treatment", x = "Treatment", y = "Simpson's Diversity Index") +
  facet_wrap(~time.point, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = custom_colors) +
  theme_bw()

# Create boxplot for Shannon Diversity Index by time point
shannonbox <- ggplot(plot_data, aes(x = treatment, y = adiv$Shannon, fill = treatment)) + 
  geom_boxplot() +
  labs(fill = "Treatment", x = "Treatment", y = "Shannon Diversity Index") +
  facet_wrap(~time.point, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = custom_colors) +
  theme_bw()
#  Add p-value
shannonbox + stat_compare_means()

# Create boxplot for Observed Diversity Index by time point
observedbox <- ggplot(plot_data, aes(x = treatment, y = adiv$Observed, fill = treatment)) + 
  geom_boxplot() +
  labs(fill = "Treatment", x = "Treatment", y = "Observed Diversity Index") +
  facet_wrap(~time.point, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = custom_colors) +
  theme_bw()

# Create boxplot for Chao1 Diversity Index by time point
chao1box <- ggplot(plot_data, aes(x = treatment, y = adiv$Chao1, fill = treatment)) + 
  geom_boxplot() +
  labs(fill = "Treatment", x = "Treatment", y = "Chao1 Diversity Index") +
  facet_wrap(~time.point, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = custom_colors) +
  theme_bw()

# Create boxplot for InvSimpson Diversity Index by time point
invsimpsonbox <- ggplot(plot_data, aes(x = treatment, y = adiv$InvSimpson, fill = treatment)) + 
  geom_boxplot() +
  labs(fill = "Treatment", x = "Treatment", y = "InvSimpson Diversity Index") +
  facet_wrap(~time.point, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = custom_colors) +
  theme_bw()

# Print the plots
print(simpsonbox + stat_compare_means())
print(shannonbox+ stat_compare_means())
print(observedbox+ stat_compare_means())
print(chao1box+ stat_compare_means())
print(invsimpsonbox+ stat_compare_means())

#Barplot composition
#https://mibwurrepo.github.io/Microbial-bioinformatics-introductory-course-Material-2018/composition-plots.html
#Filter phyloseq
physeqa <- subset_taxa(physeq,Class!="c__Chloroplast")
physeqb <- subset_taxa(physeqa,Order!="o__Mitochondria")
physeq <- subset_taxa(physeqb,Family!="k__Archaea")
sort(sample_sums(physeq)) #check the number of reads/sample


#clean up the data
ps <- physeq
tax_table(ps)
ps_clean <- subset_taxa(ps, !is.na(Phylum) & Phylum != "Unclassified")
taxa <- as.data.frame(tax_table(ps_clean)) #Consolidate Taxonomic Levels
taxa$Genus_Species <- paste(taxa$Genus, taxa$Species, sep="_")
tax_table(ps_clean) <- tax_table(as.matrix(taxa))
ps_genus <- tax_glom(ps_clean, "Genus") #Aggregate at a Specific Taxonomic Level
ps_filt <- prune_taxa(taxa_sums(ps_genus) > 10, ps_genus) # Removing Rare Taxa
ps_filt 
physeq.com <- ps_filt 

# We need to set Palette
taxic <- as.data.frame(physeq.com@tax_table)  # this will help in setting large color options
library(viridis)
# Define number of unique "Family" levels for color count
colourCount <- length(unique(taxic$Family))
getPalette <- viridis::viridis(50) # Create a color palette with 50 distinct colors
otu.df <- as.data.frame(otu_table(physeq.com))  # make a dataframe for OTU information.

# head(otu.df) # check the rows and columns
taxic$OTU <- row.names.data.frame(otu.df)  # Add the OTU ids from OTU table into the taxa table at the end.
colnames(taxic)  # You can see that we now have extra taxonomy levels.

library(knitr)
head(kable(taxic))  # check the table.
taxmat <- as.matrix(taxic)  # convert it into a matrix.
new.tax <- tax_table(taxmat)  # convert into phyloseq compatible file.
tax_table(physeq.com) <- new.tax  # incroporate into phyloseq Object

# now edit the unclassified taxa
tax_table(physeq.com)[tax_table(physeq.com)[, "Family"] == "f__", "Family"] <- "Unclassified family"

# We will also remove the 'f__' patterns for cleaner labels
tax_table(physeq.com)[, colnames(tax_table(physeq.com))] <- gsub(tax_table(physeq.com)[, 
                                                                              colnames(tax_table(physeq.com))], pattern = "[a-z]__", replacement = "")

# it would be nice to have the Taxonomic names in italics.
# for that we set this
guide_italics <- guides(fill = guide_legend(label.theme = element_text(size = 15, 
                                      face = "italic", colour = "Black", angle = 0)))
## Now we need to plot at family level, We can do it as follows:

# first remove the phy_tree
physeq.com@phy_tree <- NULL

# Second merge at family level
physeq.com.fam <- aggregate_taxa(physeq.com, "Family")
#physeq.com.fam  <- tax_glom(physeq.com, "Family")

library(ggplot2)
plot.composition.COuntAbun <- plot_composition(physeq.com.fam) + theme(legend.position = "bottom") + 
  scale_fill_manual("Family", values = getPalette)  + theme_bw() + 
  theme(axis.text.x = element_text(angle = 10)) + 
  ggtitle("Relative abundance") + guide_italics + theme(legend.title = element_text(size=10))

plot.composition.COuntAbun

#use this website to see if the image an be modified: https://github.com/microbiome/microbiome/issues/113

#Make it relative abundance
# the previous pseq object physeq.com.fam is only counts.

# Use transform function of microbiome to convert it to rel abun.

physeq.com.fam.rel <- microbiome::transform(physeq.com.fam, "compositional")
physeq.com.fam.rel

plot.composition.relAbun <- plot_composition(physeq.com.fam.rel) + theme(legend.position = "bottom") + 
  scale_fill_manual("Family", values = getPalette) + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Relative abundance") + guide_italics + theme(legend.title = element_text(size=18))

plot.composition.relAbun
ggsave("./alpha_diversity_plots/Family_barplot_RelAbundance.pdf", height = 6, width = 20)

#Beta diversity metrics
physeq.rel <- transform(physeq, "relative.abundance")
physeq.rel

set.seed(49275)  #set seed for reproducible rooting of the tree
ordu.wt.uni = ordinate(physeq.rel, "PCoA", "wunifrac")
scree.plot <- plot_scree(ordu.wt.uni, "Check for importance of axis in Scree plot for MCs, UniFrac/PCoA")
print(scree.plot)

#overall unifrac
wt.unifrac <- plot_ordination(physeq.rel, ordu.wt.uni, 
                              color = "treatment", 
                              shape = "time.point")

wt.unifrac <- wt.unifrac + scale_fill_manual(values = c("#CBD588", 
                                                        "#5F7FC7", "orange", "#DA5724", "#508578")) + 
  ggtitle("Weighted UniFrac relative abundance") + 
  geom_point(size = 3)

print(wt.unifrac)

# Subset data for Day 15
physeq_day15 <- subset_samples(physeq.rel, time.point == "Day15")

# Perform PCoA using weighted UniFrac for Day 15
ordu.wt.uni_day15 <- ordinate(physeq_day15, "PCoA", "wunifrac")
scree.plot_day15 <- plot_scree(ordu.wt.uni_day15, "Check for importance of axis in Scree plot for MCs, UniFrac/PCoA - Day 15")
print(scree.plot_day15)

#PERMOVA
metawu15 <- as(sample_data(physeq_day15), "data.frame")
Permetawu15 <-adonis2(distance(physeq_day15, method="wunifrac") ~ treatment,
        data = metawu15)
Permetawu15 


# Create weighted UniFrac plot for Day 15
wt.unifrac_day15 <- plot_ordination(physeq_day15, ordu.wt.uni_day15, 
                                    color = "treatment", 
                                    shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("Weighted UniFrac relative abundance - Day 15") +
  geom_point(size = 3)

wt.unifrac_day15

# Subset data for Day 22
physeq_day22 <- subset_samples(physeq.rel, time.point == "Day22")

# Perform PCoA using weighted UniFrac for Day 22
ordu.wt.uni_day22 <- ordinate(physeq_day22, "PCoA", "wunifrac")
scree.plot_day22 <- plot_scree(ordu.wt.uni_day22, "Check for importance of axis in Scree plot for MCs, UniFrac/PCoA - Day 22")
print(scree.plot_day22)

#PERMOVA
metawu22 <- as(sample_data(physeq_day22), "data.frame")
Permetawu22 <-adonis2(distance(physeq_day22, method="wunifrac") ~ treatment,
                      data = metawu22)
Permetawu22


# Create weighted UniFrac plot for Day 22
wt.unifrac_day22 <- plot_ordination(physeq_day22, ordu.wt.uni_day22, 
                                    color = "treatment", 
                                    shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("Weighted UniFrac relative abundance - Day 22") +
  geom_point(size = 3)

print(wt.unifrac_day22)
a= ggarrange(wt.unifrac_day15, wt.unifrac_day22, 
                labels = c("A", "B"),
                legend = "bottom", ncol = 2)
a
path3 <- "alpha_diversity_plots/wt.unifrac.png"
ggsave(filename = path3, plot = a, width = 15, height = 6)


###################################################################
#unweighted unifrac
set.seed(475)  #set seed for reproducible rooting of the tree
ordu.unwt.uni = ordinate(physeq.rel, "PCoA", "unifrac", weighted = F)
unwt.unifrac <- plot_ordination(physeq.rel, ordu.unwt.uni, 
                               color = "treatment", 
                               shape = "time.point") + 
  ggtitle("Unweighted UniFrac relative abundance") + 
  geom_point(size = 3)

print(unwt.unifrac)
path4 <- "alpha_diversity_plots/unwt.unifrac.png"
ggsave(filename = path4, plot = unwt.unifrac, width = 10, height = 5)


# Perform PCoA using weighted UniFrac for Day 15
ordu.unwt.uni_day15 <- ordinate(physeq_day15, "PCoA", "unifrac", weighted = F)
unwt.unifrac_day15 <- plot_ordination(physeq_day15, ordu.unwt.uni_day15, 
                                    color = "treatment", 
                                    shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("UnWeighted UniFrac relative abundance - Day 15") +
  geom_point(size = 3)

print(unwt.unifrac_day15)

#PERMOVA
metaunwu15 <- as(sample_data(physeq_day15), "data.frame")
Permetaunwu15 <-adonis2(distance(physeq_day15, method="unifrac") ~ treatment,
                      data = metaunwu15)
Permetaunwu15 



# Perform PCoA using unweighted UniFrac for Day 22
ordu.unwt.uni_day22 <- ordinate(physeq_day22, "PCoA", "unifrac", weighted = F)
unwt.unifrac_day22 <- plot_ordination(physeq_day22, ordu.unwt.uni_day22, 
                                    color = "treatment", 
                                    shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("unWeighted UniFrac relative abundance - Day 22") +
  geom_point(size = 3)

print(unwt.unifrac_day22)

#PERMOVA
metaunwu22 <- as(sample_data(physeq_day22), "data.frame")
Permetaunwu22 <-adonis2(distance(physeq_day22, method="unifrac") ~ treatment,
                        data = metaunwu22)
Permetaunwu22

#combining the plots
unweighted= ggarrange(unwt.unifrac_day15, unwt.unifrac_day22, 
             labels = c("A", "B"),
             legend = "bottom", ncol = 2)
unweighted
pathunweighted<- "alpha_diversity_plots/unweighted.unifrac.png"
ggsave(filename = pathunweighted, plot = unweighted, width = 14, height = 6)



## Bray Curtis dissimilarity (purely OTU based, doesn't take
# the sequence of the OTUs into account)

ordu.bray = ordinate(physeq.rel, "PCoA", "bray", weighted = F)
bray <- plot_ordination(physeq.rel, ordu.bray, 
                        color = "treatment", 
                        shape = "time.point")

bray <- bray + scale_fill_manual(values = c("#CBD588", "#5F7FC7", 
                                            "orange", "#DA5724", "#508578")) + 
  ggtitle("Bray-Curtis relative abundance") + 
  geom_point(size = 3) + theme_bw()

print(bray)
# Perform PCoA using brac  for Day 15
ordu.brayday15 <- ordinate(physeq_day15, "PCoA", "bray", weighted = F)
bray15 <- plot_ordination(physeq_day15, ordu.brayday15, 
                                      color = "treatment", 
                                      shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("Bray-Curtis relative abundance- Day 15") +
  geom_point(size = 3)

print(bray15)

#PERMOVA
metaB15 <- as(sample_data(physeq_day15), "data.frame")
PermetaB15 <-adonis2(distance(physeq_day15, method="bray") ~ treatment,
                      data = metaB15)
PermetaB15 

# Perform PCoA using BC for Day 22
ordu.brayday22 <- ordinate(physeq_day22, "PCoA", "bray", weighted = F)
bray22 <- plot_ordination(physeq_day22, ordu.brayday22, 
                                      color = "treatment", 
                                      shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("Bray-Curtis relative abundance - Day 22") +
  geom_point(size = 3)

print(bray22)

#permova
metaB22 <- as(sample_data(physeq_day22), "data.frame")
PermetaB22 <-adonis2(distance(physeq_day22, method="bray") ~ treatment,
                      data = metaB22)
PermetaB22 

#combing the plots
bcurtis= ggarrange(bray15, bray22, 
                      labels = c("A", "B"),
                      legend = "bottom", ncol = 2)
bcurtis
b.curtis<- "alpha_diversity_plots/Bray_Curtis.png"
ggsave(filename = b.curtis, plot = bcurtis, width = 14, height = 6)

######################################################
#jaccarrd

jac15 <- ordinate(physeq_day15, "PCoA", "jaccard", weighted = F)
jac.15 <- plot_ordination(physeq_day15, jac15, 
                          color = "treatment", 
                          shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("Jaccard- Day 15") +
  geom_point(size = 3)

print(jac.15)

#permova
metaJ15 <- as(sample_data(physeq_day15), "data.frame")
PermetaJ15 <-adonis2(distance(physeq_day15, method="jaccard") ~ treatment,
                     data = metaJ15)
PermetaJ15 


# Perform PCoA using BC for Day 22
jac22 <- ordinate(physeq_day22, "PCoA", "jaccard", weighted = F)
jac.22 <- plot_ordination(physeq_day22, jac22, 
                          color = "treatment", 
                          shape = "time.point") +
  scale_fill_manual(values = c("#CBD588", "#5F7FC7", "orange", "#DA5724", "#508578")) +
  ggtitle("jaccard - Day 22") +
  geom_point(size = 3)

print(jac.22)


#permova
metaJ22 <- as(sample_data(physeq_day22), "data.frame")
PermetaJ22 <-adonis2(distance(physeq_day22, method="jaccard") ~ treatment,
                     data = metaJ22)
PermetaJ22


#COMBINING THE PLOTS
jacc= ggarrange(jac.15, jac.22, 
                   labels = c("A", "B"),
                   legend = "bottom", ncol = 2)
jacc
jacc.ard<- "alpha_diversity_plots/Jaccard.png"
ggsave(filename = jacc.ard, plot = jacc, width = 14, height = 6)

#Taxa barplot
#################################################################
asv_table <- data.frame(otu_table(physeq), check.names = F)
metadata <- data.frame(sample_data(physeq), check.names = F)
taxonomy <- data.frame(tax_table(physeq), check.names = F)



#Clean up taxonomy
head(taxonomy)
tax.clean <- taxonomy
head(tax.clean)

#Assign our edited and formatted tables as variables to be feed into phyloseq
OTU.physeq = otu_table(as.matrix(asv_table), taxa_are_rows=TRUE)
tax.physeq = tax_table(as.matrix(tax.clean))    
meta.physeq = sample_data(metadata)

#We then merge these into an object of class phyloseq.
physeq_bar_plot = phyloseq(OTU.physeq, tax.physeq, meta.physeq)
physeq_bar_plot 

# Set colors for plotting
my_colors <- c(
  '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
  '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928', 
  "#CBD588", "#5F7FC7", "orange","#DA5724", "#508578", "#CD9BCD",
  "#AD6F3B", "#673770","#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861", "gray", "black"
)

#If you want different taxonomic level, find and replace the taxonomic level listed here
my_level <- c("Phylum", "Family", "Genus")
my_column <- "treatment"  #this is the metadata column that we will use in the taxa barplot

rm(taxa.summary)

abund_filter <- 0.05  # Our abundance threshold
#ml ="Genus"

library(gridExtra)

for(ml in my_level){
  print(ml)
  
  taxa.summary <- physeq_bar_plot %>%
    tax_glom(taxrank = ml, NArm = FALSE) %>%  # agglomerate at `ml` level
    transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
    psmelt()  %>%                               # Melt to long format
    group_by(get(my_column), get(ml), time.point) %>%
    summarise(Abundance.average=mean(Abundance)) 
  taxa.summary <- as.data.frame(taxa.summary)
  colnames(taxa.summary)[1] <- my_column
  colnames(taxa.summary)[2] <- ml
  
  physeq.taxa.max <- taxa.summary %>% 
    group_by(get(ml)) %>%
    summarise(overall.max=max(Abundance.average))
  
  physeq.taxa.max <- as.data.frame(physeq.taxa.max)
  colnames(physeq.taxa.max)[1] <- ml
  
  # merging the phyla means with the metadata #
  physeq_meta <- merge(taxa.summary, physeq.taxa.max)
  physeq_meta 
  
  physeq_meta_filtered <- filter(physeq_meta, overall.max>abund_filter)
  str(physeq_meta_filtered)
  
  physeq_meta_filtered$treatment = factor(physeq_meta_filtered$treatment, c("Basal", "BSea05", "BSea10", "BSea15"))
  
  # Plot 
  ggplot(physeq_meta_filtered, aes(x = get(my_column), y = Abundance.average, fill = get(ml))) + 
    facet_grid(.~time.point) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = my_colors) +
    # Remove x axis title
    #theme(axis.title.x = element_blank()) + 
    ylim(c(0,1)) +
    guides(fill = guide_legend(reverse = F, keywidth = .5, keyheight = .5, ncol = 1)) +
    theme(legend.text=element_text(size=8)) +
    theme(legend.position="right") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    theme(legend.title = element_blank()) +
    ylab("Relative Abundance") +
    xlab(my_column) +
    ggtitle(paste0(ml, " (>", abund_filter * 100,"%) in at least 1 sample")) 
  ggsave(paste0("alpha_diversity_plots/taxa/", ml, "BarPlot_", my_column, ".png"), height = 5, width = 4, create.dir = TRUE)
}

#################################################################
###Differential Abundance with DESeq2
#################################################################


#Adapted from https://joey711.github.io/phyloseq-extensions/DESeq2.html

library("DESeq2")
packageVersion("DESeq2")

t15 = subset_samples(physeq, time.point == "Day15")
head(sample_data(t15)$treatment, 10)
str(t15)
diagdds = phyloseq_to_deseq2(t15, ~ treatment)
diagdds = DESeq(diagdds, test="Wald", fitType="parametric")

#Investigate test results table
res = results(diagdds, cooksCutoff = FALSE)
alpha = 0.05
res <- na.omit(res)
res
sigtab15 = res[which(res$padj < alpha), ]
sigtab15 = cbind(as(sigtab15, "data.frame"),
               as(tax_table(t15)[rownames(sigtab15), ], "matrix"))
head(sigtab15)
dim(sigtab15)
# Phylum order
x = tapply(sigtab15$log2FoldChange, sigtab15$Phylum, function(x) max(x))
x = sort(x, TRUE)
sigtab15$Phylum = factor(as.character(sigtab15$Phylum), levels=names(x))
# Genus order
x = tapply(sigtab15$log2FoldChange, sigtab15$Genus, function(x) max(x))
x = sort(x, TRUE)
sigtab15$Genus = factor(as.character(sigtab15$Genus), levels=names(x))

DESeq_fig =ggplot(sigtab15, aes(x=Genus, y=log2FoldChange, color=Phylum)) + geom_point(size=6) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0.5, vjust=0.5))
print(DESeq_fig)

#22
t22 = subset_samples(physeq, time.point == "Day22")
head(sample_data(t22)$treatment, 10)
str(t22)
diagdds2 = phyloseq_to_deseq2(t22, ~ treatment)
diagdds2 = DESeq(diagdds2, test="Wald", fitType="parametric")

#Investigate test results table
res2 = results(diagdds2, cooksCutoff = FALSE)
alpha = 0.05
res2 <- an.omit(res2)
sigtab22 = res2[which(res2$padj < alpha), ]
sigtab22 = cbind(as(sigtab22, "data.frame"),
               as(tax_table(t22)[rownames(sigtab22), ], "matrix"))
head(sigtab22)
dim(sigtab22)
# Phylum order
x = tapply(sigtab22$log2FoldChange, sigtab22$Phylum, function(x) max(x))
x = sort(x, TRUE)
sigtab22$Phylum = factor(as.character(sigtab22$Phylum), levels=names(x))
# Genus order
x = tapply(sigtab22$log2FoldChange, sigtab22$Genus, function(x) max(x))
x = sort(x, TRUE)
sigtab22$Genus = factor(as.character(sigtab22$Genus), levels=names(x))

DESeq_fig2 =ggplot(sigtab22, aes(x=Genus, y=log2FoldChange, color=Phylum)) + geom_point(size=6) + 
  theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0.5))
print(DESeq_fig2)



# Combine the two plots using ggarrange
combined_plot <- ggarrange(DESeq_fig, DESeq_fig2, 
                           labels = c("Time Point 15", "Time Point 22"),
                           nrow = 1, ncol = 2)

# Print the combined plot
print(combined_plot)

# Save the combined plot
ggsave("alpha_diversity_plots/combined_DESeq_plots.png", combined_plot, width = 12, height = 6)



# Compare significant ASVs between Day 15 and Day 22
common_genus <- intersect(sigtab15$Genus, sigtab22$Genus)
unique_genus_t15 <- setdiff(sigtab15$Genus, sigtab22$Genus)
unique_genus_t22 <- setdiff(sigtab22$Genus, sigtab15$Genus)

# Print the common and unique ASVs
print("Common ASVs:")
print(common_genus)

print("Unique ASVs in Day 15:")
print(unique_genus_t15)

print("Unique ASVs in Day 22:")
print(unique_genus_t22)

########cooccurrence.R

library(Hmisc)
library(plyr)
library(reshape2)
library(qiime2R)
#library(igraph)
#library(fdrtool)

ASVs <- read_qza("rarefied_table.qza")
ASV_table <- as.data.frame(ASVs$data)

#####################################################################
ASV_table$ASVnos <- paste0("ASV", 1:nrow(ASV_table))
ASV_table$ASVstring <- rownames(ASV_table)
rownames(ASV_table) <- ASV_table$ASVnos
ASVkey <- ASV_table[, (ncol(ASV_table)-1):ncol(ASV_table)]
ASV_table <- ASV_table[,-(ncol(ASV_table)-1):-ncol(ASV_table)]
######################################################################

dataset <- as.data.frame(t(ASV_table))
dataset

# we are going to create a network per treatment
head(dataset[,1:10])
metadata<-read_q2metadata("project_metadata.txt")
str(metadata)
colnames(metadata)[6] = "treatment"

dataset <- merge(metadata, dataset, by.x = "SampleID", by.y = 0)
treatments<-as.vector(unique(dataset$treatment))
datasetn<-dataset
datasetn[datasetn==0]<-NA

#i = 1

summary(metadata$treatment)

my_column <- "treatment"
n1 <- 10
n2 <- 10
n3 <- 10
n4 <- 10

num_metadata_columns <- ncol(metadata)


q_cutoff <- 0.05

final_results<-data.frame()

for(i in 1:length(treatments)){
  #subset the data for a particular treatment YOU MUST ENTER THE HEADER OF THE COLUMN THAT HAS THE DIFFERENT TREATMENTS IN THIS CASE “Foaming_Status”
  print(paste("reading ",treatments[i],sep=""))
  temp<-subset(dataset, get(my_column)==treatments[i])
  tempn<-subset(datasetn, get(my_column)==treatments[i])
  print(paste("finished reading ",treatments[i],sep=""))
  # making an object that has all the results in it (both rho and P values)
  results<-rcorr(as.matrix(temp[,-c(1:num_metadata_columns)]),type="spearman") ## use the "-c" parameter to remove metadata columns
  resultsn<-rcorr(as.matrix(tempn[,-c(1:num_metadata_columns)]),type="spearman")
  
  #make two seperate objects for p-value and correlation coefficients
  rhos<-results$r
  ps<-results$P
  ns<-resultsn$n
  # going to melt these objects to 'long form' where the first two columns make up the pairs of OTUs, I am also removing NA's as they are self-comparisons, not enough data, other bad stuff
  ps_melt<-na.omit(melt(ps))
  #creating a qvalue based on FDR
  ps_melt$qval<-p.adjust(ps_melt$value, method = "BH")
  #making column names more relevant
  
  names(ps_melt)[3]<-"pval"
  # if you are of the opinion that it is a good idea to subset your network based on adjusted P-values (qval in this case), you can then subset here
  ps_sub<-subset(ps_melt, qval < q_cutoff)
  
  # now melting the rhos, note the similarity between ps_melt and rhos_melt
  rhos_melt<-na.omit(melt(rhos))
  names(rhos_melt)[3]<-"rho"
  
  # now melting the ns
  ns_melt<-(melt(ns))
  names(ns_melt)[3]<-"n"
  
  #merging together and remove negative rhos
  merged<-merge(ps_sub,rhos_melt,by=c("Var1","Var2"))
  if (treatments[i]==treatments[1]) {
    merged<-merge(merged,subset(ns_melt, n > n1),by=c("Var1","Var2"))
  }   else if (treatments[i]==treatments[2]) {
    merged<-merge(merged,subset(ns_melt, n > n2),by=c("Var1","Var2"))
  }   else if (treatments[i]==treatments[3]) {
    merged<-merge(merged,subset(ns_melt, n > n3),by=c("Var1","Var2"))
  }   else if (treatments[i]==treatments[4]) {
    merged<-merge(merged,subset(ns_melt, n > n4),by=c("Var1","Var2"))
  }   else
    print("Somethings wrong with your treatment designations. Please Check!!")
  
  if (nrow(merged) > 0) {
    merged$trt<-treatments[i]
    final_results<-rbind(final_results, merged)
  }   else {
    print("no correlations for this variable")
  }
  
  print(paste("finished ",treatments[i],sep=""))
}
final_results
strong_results <- final_results
strong_results

# In the following line, you don't have to use use rho >= 0.9. 
# If your final_results table has less than 500 rows, 
# then I consider that not too many and there's no need to filter your results.
# If you don't want to filter, just use 
strong_results <- final_results
strong_results
# If you have too many correlations and need to simplify, 
# consider running the following line, and choose an appropriate cutoff for rho.
# This line will keep both positive and negative correlations. abs() means absolute value
#strong_results<-subset(final_results, abs(rho) >= 0.9)


###############################################################
# If you want to see the correlation scatterplot of 
# two significant ASVs
###############################################################


trt_ASVs<-subset(dataset, get(my_column)==treatments[1])
trt_ASVs
colnames(trt_ASVs[1:10])
head(final_results)
ggplot(trt_ASVs, aes(x = ASV106, y = ASV64)) +
  geom_point()



###############################################################
# If you want to see the the taxonomic assignment of  
# these significant ASVs
###############################################################

taxonomy<-read_qza("taxonomy.qza")
head(taxonomy$data)

tax.clean<-parse_taxonomy(taxonomy$data)
tax.clean <- na.omit(tax.clean)
head(tax.clean)

#All this is OK except that in future use of the taxonomy table, 
#these ASVs will be ignored because they are not classified. Why 
#are ASVs not classified? Its because there is not a close enough 
#match in the database. Just because there is not a good match in 
#the database does not mean they don’t exist, so I wanted to make 
#sure this data was not lost. So in my new code, from lines 200 – 224 
#I make it so that ASVs that are unclassified at any level are 
#classified as the lowest taxonomic level for which there is a 
#classification.
#Next, all these `NA` classifications with the last level that was 
#classified

tax.clean[is.na(tax.clean)] <- ""
for (i in 1:nrow(tax.clean)){
  if (tax.clean[i,2] == ""){
    kingdom <- paste("unclassified_", tax.clean[i,1], sep = "")
    tax.clean[i, 2:7] <- kingdom
  } else if (tax.clean[i,3] == ""){
    phylum <- paste("unclassified_", tax.clean[i,2], sep = "")
    tax.clean[i, 3:7] <- phylum
  } else if (tax.clean[i,4] == ""){
    class <- paste("unclassified_", tax.clean[i,3], sep = "")
    tax.clean[i, 4:7] <- class
  } else if (tax.clean[i,5] == ""){
    order <- paste("unclassified_", tax.clean[i,4], sep = "")
    tax.clean[i, 5:7] <- order
  } else if (tax.clean[i,6] == ""){
    family <- paste("unclassified_", tax.clean[i,5], sep = "")
    tax.clean[i, 6:7] <- family
  } else if (tax.clean[i,7] == ""){
    tax.clean$Species[i] <- paste("unclassified_",tax.clean$Genus[i], sep = "_")
  }
}

strong_results_taxa
strong_results_taxa <- merge(strong_results, ASVkey, by.x = "Var1", by.y = "ASVnos")
strong_results_taxa <- merge(strong_results_taxa, ASVkey, by.x = "Var2", by.y = "ASVnos")
strong_results_taxa <- merge(strong_results_taxa, tax.clean, by.x = "ASVstring.x", by.y = 0)
#strong_results_taxa <- merge(strong_results_taxa, tax.clean, by.x = "ASVstring.y", by.y = 0)

write.csv(strong_results_taxa, "alpha_diversity_plots/strong-results-taxa.csv", row.names = F)

write.csv(subset(strong_results_taxa, trt=="Basal"), "alpha_diversity_plots/strong-results-taxa-basal.csv", row.names = F)
write.csv(subset(strong_results_taxa, trt=="BSea05"), "alpha_diversity_plots/moving-pictures-strong-results-taxa-bsea05.csv", row.names = F)
write.csv(subset(strong_results_taxa, trt=="BSea10"), "alpha_diversity_plots/moving-pictures-strong-results-taxa-bsea10.csv", row.names = F)
write.csv(subset(strong_results_taxa, trt=="BSea15"), "alpha_diversity_plots/moving-pictures-strong-results-taxa-bsea15.csv", row.names = F)




