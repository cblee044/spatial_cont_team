library(tidyverse)

mloe_data <- read_csv("Bight_MLOE_98-18.csv", show_col_types = FALSE) %>%
    select(-"Strata") %>%
    pivot_wider(names_from = "LOE",
        values_from = c("Score", "Category")) %>%
    select("StationID",
        "Survey",
        "TrendStrata",
        "AreaWeight",
        "Category_Benthic",
        "Category_Chemistry",
        "Category_SQO Integrated",
        "Category_Toxicity") %>%
    rename(
        Benthic = Category_Benthic,
        Chemistry = Category_Chemistry,
        SQO_Integrated = "Category_SQO Integrated",
        Toxicity = Category_Toxicity) %>%
    filter(SQO_Integrated != "NA") %>%
    filter(TrendStrata == 'Ports/Marinas/Bays')



### Calculate the percent area for each Strata
total_area_weight_sqo <- mloe_data %>%
  filter(!is.na(SQO_Integrated)) %>%
  group_by(TrendStrata, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

total_area_weight_benthic <- mloe_data %>%
  filter(!is.na(Benthic)) %>%
  group_by(TrendStrata, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

total_area_weight_chemistry <- mloe_data %>%
  filter(!is.na(Chemistry)) %>%
  group_by(TrendStrata, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

total_area_weight_toxicity <- mloe_data %>%
  filter(!is.na(Toxicity)) %>%
  group_by(TrendStrata, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_sqo" =
    rep(0, nrow(mloe_data)))

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_benthic" =
    rep(0, nrow(mloe_data)))

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_chemistry" =
    rep(0, nrow(mloe_data)))

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_toxicity" =
    rep(0, nrow(mloe_data)))

for (i in seq_len(nrow(mloe_data))) {
    total_area_weight_sqo_group <- total_area_weight_sqo %>%
        filter(TrendStrata == mloe_data[[i, "TrendStrata"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()


    total_area_weight_benthic_group <- total_area_weight_benthic %>%
        filter(TrendStrata == mloe_data[[i, "TrendStrata"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()

    total_area_weight_chemistry_group <- total_area_weight_chemistry %>%
        filter(TrendStrata == mloe_data[[i, "TrendStrata"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()

    total_area_weight_toxicity_group <- total_area_weight_toxicity %>%
        filter(TrendStrata == mloe_data[[i, "TrendStrata"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()


    mloe_data[i, "Percent_Area_Weight_sqo"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_sqo_group

    mloe_data[i, "Percent_Area_Weight_benthic"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_benthic_group

    mloe_data[i, "Percent_Area_Weight_chemistry"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_chemistry_group

    mloe_data[i, "Percent_Area_Weight_toxicity"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_toxicity_group

}



### triad score plot
triad_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(SQO_Integrated = factor(SQO_Integrated,
             levels = c("Clearly impacted",
            "Likely impacted",
            "Possibly impacted",
            "Likely unimpacted",
            "Unimpacted",
            "Inconclusive"))) %>%
    group_by(TrendStrata, SQO_Integrated, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_sqo) * 100,
              counts = n())

totals <- triad_data_plot %>%
    group_by(TrendStrata, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(triad_data_plot, aes(fill = SQO_Integrated,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 105) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da", "#e3edc2", "#5d6150")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Sediment Quality Triad \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))


### benthic score plot
benthic_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    filter(!is.na(Benthic)) %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Benthic = factor(Benthic,
             levels = c("High",
            "Moderate",
            "Low",
            "None"))) %>%
    group_by(TrendStrata, Benthic, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_benthic) * 100,
              counts = n())

totals <- benthic_data_plot %>%
    group_by(TrendStrata, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(benthic_data_plot, aes(fill = Benthic,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 105) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Benthic \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))

### chemistry score plot
chemistry_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    filter(!is.na(Chemistry)) %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Chemistry = factor(Chemistry,
             levels = c("High",
            "Moderate",
            "Low",
            "None"))) %>%
    group_by(TrendStrata, Chemistry, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_chemistry) * 100,
              counts = n())

totals <- chemistry_data_plot %>%
    group_by(TrendStrata, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(chemistry_data_plot, aes(fill = Chemistry,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 105) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Chemistry \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))


### toxicity score plot
toxicity_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    filter(!is.na(Toxicity)) %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Toxicity = factor(Toxicity,
             levels = c("High",
            "Moderate",
            "Low",
            "None"))) %>%
    group_by(TrendStrata, Toxicity, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_toxicity) * 100,
              counts = n())

totals <- toxicity_data_plot %>%
    group_by(TrendStrata, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(toxicity_data_plot, aes(fill = Toxicity,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 105) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Toxicity \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))















### Calculate the percent area for each Region
total_area_weight_sqo <- mloe_data %>%
  filter(!is.na(SQO_Integrated)) %>%
  group_by(Region, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

total_area_weight_benthic <- mloe_data %>%
  filter(!is.na(Benthic)) %>%
  group_by(Region, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

total_area_weight_chemistry <- mloe_data %>%
  filter(!is.na(Chemistry)) %>%
  group_by(Region, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

total_area_weight_toxicity <- mloe_data %>%
  filter(!is.na(Toxicity)) %>%
  group_by(Region, Survey) %>%
  summarise(Total_Area_Weight = sum(AreaWeight)) %>%
  as_tibble()

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_sqo" =
    rep(0, nrow(mloe_data)))

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_benthic" =
    rep(0, nrow(mloe_data)))

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_chemistry" =
    rep(0, nrow(mloe_data)))

mloe_data <- add_column(mloe_data, "Percent_Area_Weight_toxicity" =
    rep(0, nrow(mloe_data)))

for (i in seq_len(nrow(mloe_data))) {
    total_area_weight_sqo_group <- total_area_weight_sqo %>%
        filter(Region == mloe_data[[i, "Region"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()

    total_area_weight_benthic_group <- total_area_weight_benthic %>%
        filter(Region == mloe_data[[i, "Region"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()

    total_area_weight_chemistry_group <- total_area_weight_chemistry %>%
        filter(Region == mloe_data[[i, "Region"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()

    total_area_weight_toxicity_group <- total_area_weight_toxicity %>%
        filter(Region == mloe_data[[i, "Region"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]]) %>%
        select(Total_Area_Weight) %>%
        as.numeric()


    mloe_data[i, "Percent_Area_Weight_sqo"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_sqo_group

    mloe_data[i, "Percent_Area_Weight_benthic"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_benthic_group

    mloe_data[i, "Percent_Area_Weight_chemistry"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_chemistry_group

    mloe_data[i, "Percent_Area_Weight_toxicity"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_toxicity_group

}


### triad score plot
triad_data_plot <- mloe_data %>%
    filter(Region %in% c('Marina Del Rey',
                         'Mission Bay',
                         'Newport Beach',
                         'Port of LA',
                         'Port of LB',
                         'Port of LB to Seal Beach',
                         'Port of San Diego')) %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(SQO_Integrated = factor(SQO_Integrated,
             levels = c("Clearly impacted",
            "Likely impacted",
            "Possibly impacted",
            "Likely unimpacted",
            "Unimpacted",
            "Inconclusive"))) %>%
    group_by(Region, SQO_Integrated, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_sqo) * 100,
              counts = n())

totals <- triad_data_plot %>%
    group_by(Region, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(triad_data_plot, aes(fill = SQO_Integrated,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(Region), ncol = 2) +
    ylim(0, 115) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da", "#e3edc2", "#5d6150")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Sediment Quality Triad \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))



### benthic score plot
benthic_data_plot <- mloe_data %>%
    filter(Region %in% c('Marina Del Rey',
                         'Mission Bay',
                         'Newport Beach',
                         'Port of LA',
                         'Port of LB',
                         'Port of LB to Seal Beach',
                         'Port of San Diego')) %>%
    filter(!is.na(Benthic)) %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Benthic = factor(Benthic,
             levels = c("High",
            "Moderate",
            "Low",
            "None"))) %>%
    group_by(Region, Benthic, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_benthic) * 100,
              counts = n())

totals <- benthic_data_plot %>%
    group_by(Region, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(benthic_data_plot, aes(fill = Benthic,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(Region), ncol = 2) +
    ylim(0, 115) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Benthic \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))

### chemistry score plot
chemistry_data_plot <- mloe_data %>%
    filter(Region %in% c('Marina Del Rey',
                         'Mission Bay',
                         'Newport Beach',
                         'Port of LA',
                         'Port of LB',
                         'Port of LB to Seal Beach',
                         'Port of San Diego')) %>%
    filter(!is.na(Chemistry)) %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Chemistry = factor(Chemistry,
             levels = c("High",
            "Moderate",
            "Low",
            "None"))) %>%
    group_by(Region, Chemistry, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_chemistry) * 100,
              counts = n())

totals <- chemistry_data_plot %>%
    group_by(Region, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(chemistry_data_plot, aes(fill = Chemistry,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(Region), ncol = 2) +
    ylim(0, 115) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Chemistry \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))


### toxicity score plot
toxicity_data_plot <- mloe_data %>%
    filter(Region %in% c('Marina Del Rey',
                         'Mission Bay',
                         'Newport Beach',
                         'Port of LA',
                         'Port of LB',
                         'Port of LB to Seal Beach',
                         'Port of San Diego')) %>%
    filter(!is.na(Toxicity)) %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Toxicity = factor(Toxicity,
             levels = c("High",
            "Moderate",
            "Low",
            "None"))) %>%
    group_by(Region, Toxicity, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight_toxicity) * 100,
              counts = n())

totals <- toxicity_data_plot %>%
    group_by(Region, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area),
                counts= sum(counts))

ggplot(toxicity_data_plot, aes(fill = Toxicity,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    geom_text(aes(label = counts, fill = NULL), data = totals, vjust = -1) +
    facet_wrap(vars(Region), ncol = 2) +
    ylim(0, 115) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37", "#f0f2da")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Toxicity \nAssessment Score") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))