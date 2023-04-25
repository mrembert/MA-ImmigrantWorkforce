library(tidycensus)
library(tidyverse)
library(srvyr)
library(ggplot2)


# Looking up PUMS variables

lf_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("employment", var_label, ignore.case = TRUE))

## employment status - ESR
## 1 - Civilian employed, at work
## 2 - Civilian employed, not at work
## 3 - Unemployed


nativity_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("nativity", var_label, ignore.case = TRUE))

## Country of origin - NATIVITY
## Foreign Born - 2

edu_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("education", var_label, ignore.case = TRUE))

## Educational Attainment - SCHL
## 21 - Bachelor's degree
## 22 - Master's degree
## 23 - Professional degree
## 24 - Doctorate

english_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("english", var_label, ignore.case = TRUE))

## Ability to speak English - ENG
## b - Speaks English Only
## 1 - Very Well
## 2 - Well
## 3 - Not Well
## 4 - Not at all


occ_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("occupation", var_label, ignore.case = TRUE))


# Pulling in PUMS data ----------------

ma_pums <- get_pums(
  variables = c("ESR", "NATIVITY", "SCHL", "ENG", "AGEP", "OCCP"),
  state = "MA",
  survey = "acs1",
  year = 2021,
  recode = TRUE,
  rep_weights = "person"
) |> 
  to_survey()


# Calculating total and share of immigrant workers by occupation ---------------


df_occ <- ma_pums |>
  filter(ESR %in% c("1", "2", "3"), # is in labor force
  ) |> 
  group_by(OCCP_label) |> 
  summarize(
    occ_n = survey_total(vartype = "ci"),
    occ_immigrant_n = survey_total(NATIVITY==2, vartype="ci"),
    immigrant_pct = occ_immigrant_n/occ_n
  )


# Visualizing the data ---------------


df_occ_10 <- df_occ |> 
  arrange(desc(occ_immigrant_n)) |> 
  slice_head(n= 10)  


# Read the logo image file (replace "logo.png" with the path to your logo file)
logo <- png::readPNG("reduced_onecolor.png")

# Create a raster graphics object from the logo image
logo_grob <- grid::rasterGrob(logo, interpolate = TRUE)


library(cowplot)

# Create a ggplot object containing the logo
logo_plot <- ggplot() +
  annotation_custom(logo_grob, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# Assuming your data frame is called 'df_occ'
df_occ_10 <- df_occ |>
  arrange(desc(occ_immigrant_n)) |>
  slice_head(n = 10)

# Create the main ggplot with an adjusted left-hand margin
main_plot <- ggplot(df_occ_10, aes(y = factor(OCCP_label, levels = df_occ_10$OCCP_label[order(df_occ_10$occ_immigrant_n, decreasing = FALSE)]), x = immigrant_pct)) +
  geom_point(size=5) +
  geom_point(data = subset(df_occ_10, immigrant_pct > 0.21), color = "red", size = 5) +
  geom_errorbarh(aes(xmin = immigrant_pct_low, xmax = immigrant_pct_upp), height = 0.2) +
  geom_text(aes(label = scales::percent(round(immigrant_pct, 2), accuracy = 1)), hjust = 0.5, vjust = -1.5, size=5, family = "NotoSans") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, vjust = 0.5, family = "NotoSans", size = 18),
        axis.text.x = element_text(family = "NotoSans", size=18),
        # axis.title.y = element_text(family = "NotoSans", margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        # axis.title.x = element_text(family = "NotoSans", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(family = "NotoSans", size=20, margin = margin(t = 0, r = 0, b = 10, l = 0, unit = "pt")),
        plot.margin = margin(t = 0, r = 0, b = 10, l = 30, unit = "pt")) +
  labs(title = "Immigrant Percentage by Occupation",
       y = "Occupation",
       x = "Immigrant Percentage") +
  scale_x_continuous(labels = scales::percent)


# Combine the plots
combined_plot <- cowplot::plot_grid(empty_plot, logo_plot,
                                    main_plot, NULL,
                                    ncol = 2, nrow = 2,
                                    rel_widths = c(1, 0.15),
                                    rel_heights = c(0.15, 1),
                                    labels = c("", "", "A", ""),
                                    label_size = 0)

# Display the combined plot
print(combined_plot)

