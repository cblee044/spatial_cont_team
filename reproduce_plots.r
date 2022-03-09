library(tidyverse)

mloe_data <- read_csv("Bight_MLOE_98-18.csv", show_col_types = FALSE) %>%
    pivot_wider(names_from = "LOE",
        values_from = c("Score", "Category")) %>%
    select("StationID",
        "Survey",
        "Strata",
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
    filter(SQO_Integrated != "NA")


### Calculate the percent area for each strata
total_area_weight <- mloe_data %>%
    group_by(TrendStrata, Survey) %>%
    summarise(Total_Area_Weight = sum(AreaWeight)) %>%
    as_tibble()

mloe_data <- add_column(mloe_data, "Percent_Area_Weight" =
    rep(0, nrow(mloe_data)))

for (i in seq_len(nrow(mloe_data))) {
    total_area_weight_group <- total_area_weight %>%
        filter(TrendStrata == mloe_data[[i, "TrendStrata"]]) %>%
        filter(Survey == mloe_data[[i, "Survey"]])

    total_area_weight_group <- total_area_weight_group[[1, "Total_Area_Weight"]]

    mloe_data[i, "Percent_Area_Weight"] <- mloe_data[i, "AreaWeight"] /
        total_area_weight_group
}

### triad score plot
mloe_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    filter(SQO_Integrated == "Possibly impacted" |
        SQO_Integrated == "Likely impacted" |
        SQO_Integrated == "Clearly impacted") %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(SQO_Integrated = factor(SQO_Integrated,
        levels = c("Clearly impacted",
            "Likely impacted",
            "Possibly impacted"))) %>%
    group_by(TrendStrata, SQO_Integrated, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight) * 100)

ggplot(mloe_data_plot, aes(fill = SQO_Integrated,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 60) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Sediment Quality Triad \nAssessment Score") +
    ggtitle("SQO Integrated") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))


### benthic score plot
mloe_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    filter(Benthic == "High" |
        Benthic == "Moderate" |
        Benthic == "Low") %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Benthic = factor(Benthic,
        levels = c("High",
            "Moderate",
            "Low"))) %>%
    group_by(TrendStrata, Benthic, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight) * 100)

ggplot(mloe_data_plot, aes(fill = Benthic,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 100) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Sediment Quality Triad \nAssessment Score") +
    ggtitle("Benthic") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))

### chemistry score plot
mloe_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    filter(Chemistry == "High" |
        Chemistry == "Moderate" |
        Chemistry == "Low") %>%
    mutate(Survey = as_factor(Survey)) %>%
    mutate(Chemistry = factor(Chemistry,
        levels = c("High",
            "Moderate",
            "Low"))) %>%
    group_by(TrendStrata, Chemistry, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight) * 100)

ggplot(mloe_data_plot, aes(fill = Chemistry,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 60) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Sediment Quality Triad \nAssessment Score") +
    ggtitle("Chemistry") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))


### toxicity score plot
mloe_data_plot <- mloe_data %>%
    filter(TrendStrata == "Ports/Marinas/Bays" |
        TrendStrata == "Estuaries" |
        TrendStrata == "Shelf") %>%
    filter(Toxicity == "High" |
        Toxicity == "Moderate" |
        Toxicity == "Low") %>%
    mutate(Survey = as_factor(Survey)) %>%
    group_by(TrendStrata, Toxicity, Survey) %>%
    summarise(Percent_Area = sum(Percent_Area_Weight) * 100)

ggplot(mloe_data_plot, aes(fill = Toxicity,
        y = Percent_Area,
        x = Survey)) +
    geom_bar(position = "stack", stat = "identity", color = "black") +
    facet_wrap(vars(TrendStrata), ncol = 1) +
    ylim(0, 60) +
    scale_fill_manual(values = c("#f00c0c", "#ffc118", "#fffc37")) +
    xlab("Bight Survey") +
    ylab("Percent Area") +
    labs(fill = "Sediment Quality Triad \nAssessment Score") +
    ggtitle("Toxicity") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 12, face = "bold"))
