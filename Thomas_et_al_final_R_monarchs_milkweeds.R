# List the packages to be used

library(tidyverse)
library(lme4)
library(car)
library(emmeans)
library(jtools)
library(wesanderson)
library(binom)

# Set the working directory (i.e. folder where .csv data file is located)

setwd(choose.dir())

# Read in the data

Grace_with_NIO <- read.csv("milkweed_monarch.by.grazing_type_2023.csv")
head(Grace_with_NIO)

# Make data frame without NIO site (without Niobrara Valley Preserve, which is block=6 in the data frame)

Grace <- Grace_with_NIO %>% filter(block<=5) 

# Add minimum value to milkweed and monarch data that includes zeros to prevent error messages from dividing by zero

Grace$milkweeds_ha_plus_one <- (Grace$milkweeds_ha + 1)

Grace$monarchs_ha_plus_one <- (Grace$eggs_plus_larvae_ha + 1)

Grace$com_show_ha_plus_one <- (Grace$com_show_ha + 1)
Grace$log10_com_show_ha_plus_one <- (log10(Grace$com_show_ha_plus_one))

Grace$grass_percent_plus_one <- (Grace$grass_percent + 1)
Grace$log10grass_percent_plus_one <- (log10(Grace$grass_percent_plus_one))

# Convert block, field_number, and visit from a continuous to a categorical [i.e. factor] variable (R defaults to continuous for numerical variables)

Grace <- within(Grace, {
  field_number <- factor(field_number)
  block <- factor(block)
  visit <- factor(visit)
})

# Set the order of groups in tables and plots.

Grace_with_NIO$graze_type <- factor(Grace_with_NIO$graze_type, levels = c("Ungrazed","Bison","Cattle"))

Grace$graze_type <- factor(Grace$graze_type, levels = c("Ungrazed","Bison","Cattle"))


# Generalized Linear Mixed-Effects Model of the interactive effects of graze_type and visit (i.e. time) on milkweeds per hectare
# Note (1 | block / field_number) is a random factor of field nested within site
# Note the glmerControl() function is allowing additional iterations of the model to run to better estimate parameters

gm1 <- glmer(formula = milkweeds_ha_plus_one ~ graze_type * visit + (1 | block / field_number), data = Grace, 
             family = Gamma(link = "log"), control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000000)))

summary(gm1)

Anova(gm1, test = "Chisq")

# Examine all pairwise contrasts (post-hoc)
pairs(emmeans(gm1, "graze_type"))

emmeans(gm1, "graze_type")

# Add output from gm1 to data frames that can be used to create graphs

milkweed_by_graze_type <- as.data.frame(emmeans(gm1, "graze_type"))

milkweed_by_graze_type$milkweeds_ha <- (exp(milkweed_by_graze_type$emmean)-1)

# Diagnostics for milkweed gamma models

plot(resid(gm1, type = "deviance")~predict(gm1, type = "response")) #deviance residuals vs. predicted values

hist(resid(gm1)) #histogram of residuals from statistical model

# Generalized Linear Mixed-Effects Model of the interactive effects of graze_type and visit (i.e. time) on juvenile monarchs per hectare
# Note (1 | block / field_number) is a random factor of field (i.e. grassland unit) nested within site
# Note the glmerControl() function is allowing additional iterations of the model to run to better estimate parameters

gm2 <- glmer(formula = monarchs_ha_plus_one ~ graze_type * visit + (1 | block / field_number), data = Grace, 
             family = Gamma(link = "log"), control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)))

summary(gm2)

Anova(gm2, test = "Chisq")

# Examine all pairwise contrasts (post-hoc)
pairs(emmeans(gm2, "graze_type"))

emmeans(gm2, "graze_type")
emmeans(gm2, "visit")

# Add output from gm1 to data frames that can be used to create graphs

monarch_by_graze_type <- as.data.frame(emmeans(gm2, "graze_type"))

monarch_by_graze_type$monarchs_ha <- (exp(monarch_by_graze_type$emmean)-1)

# Diagnostics for milkweed gamma models

plot(resid(gm2, type = "deviance")~predict(gm2, type = "response"))#deviance residuals vs. predicted values

hist(resid(gm2))#histogram of residuals from statistical model

# Generalized Linear Mixed-Effects Model of the interactive effects of graze_type and visit (i.e. time) on milkweeds per hectare +
# the covariate of common_showy abundance (log10_com_show_ha_plus_one) is included to try to remove variation
# because common_showy milkweeds are the main plant for monarch oviposition in all sites except NIO.

# Note (1 | block / field_number) is a random factor of field (i.e. grassland unit) nested within site
# Note the glmerControl() function is allowing additional iterations of the model to run to better estimate parameters

gm3 <- glmer(formula = monarchs_ha_plus_one ~ graze_type * visit + log10_com_show_ha_plus_one + (1 | block / field_number), data = Grace, 
             family = Gamma(link = "log"), control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)))

# Estimate the trendline for log10_com_show_ha_plus_one explanatory variable and add values to the data frame under new "pi.hat" variable
summary(gm3)

# The pi.hat predicted trendline uses the summary(gm3) coefficients to solve the equation y = exp(intercept + (slope*log10_com_show_ha_plus_one)) and then subtracts 1 because 1 had been added to the raw data
Grace$pi.hat <- ((exp((1.6336 + (0.6623 * Grace$log10_com_show_ha_plus_one))))-1)

Anova(gm3, test = "Chisq")

# Examine all pairwise contrasts (post-hoc)
pairs(emmeans(gm3, "graze_type"))

emmeans(gm3, "graze_type")

# Diagnostics for milkweed gamma models (includes log10_com_show_ha_plus_one covariate)

plot(resid(gm3, type = "deviance")~predict(gm3, type = "response"))#deviance residuals vs. predicted values

hist(resid(gm3))#histogram of residuals from statistical model

# Visualization of relationship between log10_com_show_ha_plus_one and juvenile monarch abundance (cannot color data points by graze_type)
basic_plot <- effect_plot(gm3, pred = log10_com_show_ha_plus_one, interval = TRUE, plot.points = TRUE, jitter = 0.1, colors = "blue")

basic_plot

# Linear Mixed-Effects Model of the interactive effects of graze_type and visit (i.e. time) on the percentage of grass clumps grazed
# Unlike the previous statistical models, the log10 transformed response variable data are generally normally distributed, so a generalized model is unnecessary
# Note (1 | block / field_number) is a random factor of field (i.e. grassland unit) nested within site

gm4 <- lmer(formula = log10grass_percent_plus_one ~ graze_type * visit + (1 | block / field_number), data = Grace)

summary(gm4)

Anova(gm4, test = "F")

# Examine all pairwise contrasts (post-hoc)
pairs(emmeans(gm4, "graze_type"))

emmeans(gm4, "graze_type")

# Diagnostics for grass_percent linear mixed effect model

plot(resid(gm4, type = "deviance")~predict(gm4, type = "response"))#deviance residuals vs. predicted values

hist(resid(gm4))#histogram of residuals from statistical model

####################### Bar + datapoint plots for average milkweeds and monarchs #########

# Create a data frame of mean numbers of milkweeds and monarchs per field across all 3 visits

field_mean <- Grace_with_NIO %>% group_by(field_number) %>% dplyr::summarise(site=unique(site), block=mean(block), graze_type=unique(graze_type), graze_num=mean(graze_num), NIO1=mean(NIO1_other0), milkweeds_ha_field=mean(milkweeds_ha), monarchs_ha_field=mean(eggs_plus_larvae_ha)) 

# Set the order of groups in tables and plots.

field_mean$graze_type <- factor(field_mean$graze_type, levels = c("Ungrazed","Bison","Cattle"))

##### The first graph is for milkweeds stems per hectare
# Create a dot plot with averages per field, single bars per graze_type, sites connected by lines, and highlight NIO in red

ggplot() +
  geom_bar(data = milkweed_by_graze_type, aes(x = graze_type, y = milkweeds_ha),
           stat = "identity", fill = "lightgray", alpha = 0.5, show.legend = FALSE) +
  geom_point(data = subset(field_mean, NIO1==0), aes(x = graze_num, y = milkweeds_ha_field),
             size = 3, color = "black", position = position_dodge2(width = 0.01), show.legend = FALSE) +
  geom_point(data = subset(field_mean, NIO1==1), aes(x = graze_num, y = milkweeds_ha_field),
             size = 3, color = "red", position = position_dodge2(width = 0), show.legend = FALSE) +
  geom_text(data = subset(field_mean, NIO1==0 & graze_type=='Ungrazed'), aes(x = graze_num, y = milkweeds_ha_field, label = site),
            size = 5, color = "black", hjust =1.5, show.legend = FALSE) +
  geom_text(data = subset(field_mean, NIO1==1 & graze_type=='Ungrazed'), aes(x = graze_num, y = milkweeds_ha_field, label = site),
            size = 5, color = "red", hjust =1.5, vjust = -0.2, show.legend = FALSE) +
  geom_line(data = subset(field_mean,site=='BRO'), aes(x = graze_num, y = milkweeds_ha_field), color = "magenta4") +
  geom_line(data = subset(field_mean,site=='DUN'), aes(x = graze_num, y = milkweeds_ha_field), color = "blue") +
  geom_line(data = subset(field_mean,site=='KON'), aes(x = graze_num, y = milkweeds_ha_field), color = "aquamarine3") +
  geom_line(data = subset(field_mean,site=='PLA'), aes(x = graze_num, y = milkweeds_ha_field), color = "sienna3") +
  geom_line(data = subset(field_mean,site=='TAL'), aes(x = graze_num, y = milkweeds_ha_field), color = "gold2") +
  labs(
    x =NULL,
    y ="Number of milkweed stems per hectare"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family = "serif", size = 16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family = "serif", size = 14),
    legend.text = element_text(family = "serif", size = 14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family = "serif", face = "bold", size = 22),
    axis.text = element_text(family = "serif", face = "bold", size = 18, color = "black")
  )

##### The second graph is for monarchs per hectare
# Create a dot plot with averages per field, single bars per graze_type, sites connected by lines, and highlight NIO in red
# Note that the hash marks ("#") in the plot code can be removed to add the NIO data in red

ggplot() +
  geom_bar(data = monarch_by_graze_type, aes(x = graze_type, y = monarchs_ha),
           stat = "identity", fill = "lightgray", alpha = 0.5, show.legend = FALSE) +
  geom_point(data = subset(field_mean, NIO1==0), aes(x = graze_num, y = monarchs_ha_field),
             size = 3, color = "black", position = position_dodge2(width = 0.01), show.legend = FALSE) +
  geom_text(data = subset(field_mean, NIO1==0 & graze_type=='Ungrazed'), aes(x = graze_num, y = monarchs_ha_field, label = site),
            size = 5, color = "black", hjust = 1.5, vjust = -0.5, position = position_dodge2(width = 0.3), show.legend = FALSE) +
#  geom_point(data = subset(field_mean, NIO1==1), aes(x = graze_num, y = monarchs_ha_field),
#             size = 3, color = "red", position = position_dodge2(width = 0), show.legend = FALSE) +
#  geom_text(data = subset(field_mean, NIO1==1 & graze_type=='Ungrazed'), aes(x = graze_num, y = monarchs_ha_field, label = site),
#            size = 5, color = "red", hjust =1.5, position = position_dodge2(width = 0.7), show.legend = FALSE) +
  geom_line(data = subset(field_mean,site=='BRO'), aes(x = graze_num, y = monarchs_ha_field), color = "magenta4") +
  geom_line(data = subset(field_mean,site=='DUN'), aes(x = graze_num, y = monarchs_ha_field), color = "blue") +
  geom_line(data = subset(field_mean,site=='KON'), aes(x = graze_num, y = monarchs_ha_field), color = "aquamarine3") +
  geom_line(data = subset(field_mean,site=='PLA'), aes(x = graze_num, y = monarchs_ha_field), color = "sienna3") +
  geom_line(data = subset(field_mean,site=='TAL'), aes(x = graze_num, y = monarchs_ha_field), color = "gold2") +
  labs(
    x =NULL,
    y ="Number of juvenile monarchs per hectare"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family = "serif", size = 16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family = "serif", size = 14),
    legend.text = element_text(family = "serif", size = 14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family = "serif", face = "bold", size = 22),
    axis.text = element_text(family = "serif", face = "bold", size = 18, color = "black")
  )


# Trying to visualize effects of common/showy milkweed abundance on juvenile monarchs in ggplot
# Using ggplot to graph data and predicted line (pi.hat was calculated above)
com_show_juveniles_graph <- ggplot(data = Grace,
                            aes(x = log10_com_show_ha_plus_one, y = jitter(eggs_plus_larvae_ha, 8), 
                                color = factor(graze_type))) +
    geom_smooth(mapping = aes(y = pi.hat),
            color = "black",
            linewidth = 1) +
  geom_point(show.legend = FALSE, size = 1.75) +
  scale_color_manual(values = wes_palette("FantasticFox1")) +
  labs( x = expression(bold(paste("log10(", bolditalic("A. speciosa")," and ", bolditalic("A. syriaca "), "stems per hectare + 1)"))), 
        y = "Number of juvenile monarchs per hectare") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family = "serif", size = 16, face = "bold"),
    legend.position = "none",
    legend.title = element_text(family = "serif", size = 14),
    legend.text = element_text(family = "serif", size = 14),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(family = "serif", face = "bold", size = 22),
    axis.text = element_text(family = "serif", face = "bold", size = 14, color = "black")
  )

com_show_juveniles_graph

############ calculating confidence intervals for percentage of milkweed stems containing juvenile monarch per milkweed species in 2023 #########
# For the confidence intervals below, input is the number of stems containing at least one juvenile monarchs, n = the number of total stems sampled
# The Agresti-Coull method is used to calculate confidence intervals

binom.confint(28, n = 508, method = "ac") #A. arenaria
binom.confint(19, n = 363, method = "ac") #A. speciosa / A. syriaca
#binom.confint(0, n = 6, method = "ac") # A. stenophylla (this species is not included in the graph because n is too low)
#binom.confint(0, n = 52, method = "ac") #A. sullivantii (this species is not included in the graph because n is too low)
binom.confint(1, n = 225, method = "ac") #A. tuberosa
binom.confint(0, n = 910, method = "ac") #A. verticillata
binom.confint(1, n = 158, method = "ac") #A. viridiflora
binom.confint(3, n = 459, method = "ac") #A. viridis

# # The following code takes the mean percentage, lower and upper 95% confidence interval for the binom.confint() output for each species and produces Fig. S3
# # The user needs to enter the required data into a dataframe labeled "species_counts"

# ggplot(species_counts, aes(x = Species, y = Percentage)) +
#   geom_point(size = 3) +
#   geom_errorbar(aes(ymin = pmax(0, Lower_95CI), ymax = Upper_95CI), width = 0.2) +
#   labs(
#     x = NULL,
#     y = "Stems with juvenile monarchs (%)"
#   ) +
#   scale_y_continuous(limits = c(0, 10), labels = scales::label_number(accuracy = 0.1)) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(family = "serif", size = 16, colour = "black", face = "bold.italic", angle = 45, hjust = 1),  # X-axis label style
#     axis.text.y = element_text(family = "serif", size = 14, colour = "black"),  # Y-axis text style
#     axis.title = element_text(family = "serif", size = 18, face = "bold"),
#     axis.line = element_line(colour = "black"),
#     axis.ticks.x = element_line(colour = "black"),  # Adds x-axis tick marks
#     axis.ticks.y = element_line(colour = "black"),  # Adds y-axis tick marks
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     strip.text = element_text(family = "serif", size = 16, face = "bold"),
#     legend.position = "none"
#   )