# ============================================================
# 1. PROJECT SETUP
# ============================================================

## 1.1 Load required libraries

library(tidyverse)


## 1.2 Configure tibble display options

options(tibble.width = Inf)


# ============================================================
# 2. DATA IMPORT AND PREPARATION
# ============================================================

## 2.1 Import the WHO TB dataset and rename variables

tb_dataset <- read_csv("data/raw/TB_burden_countries_2026-11-03.csv") |>
  rename(
    region_code = g_whoregion,
    population_number = e_pop_num,
    tb_incidence_rate = e_inc_100k,
    tb_incidence_rate_low = e_inc_100k_lo,
    tb_incidence_rate_high = e_inc_100k_hi,
    tb_incidence_cases = e_inc_num,
    tb_incidence_cases_low = e_inc_num_lo,
    tb_incidence_cases_high = e_inc_num_hi,
    tb_hiv_percentage = e_tbhiv_prct,
    tb_hiv_percentage_low = e_tbhiv_prct_lo,
    tb_hiv_percentage_high = e_tbhiv_prct_hi,
    tb_hiv_incidence_rate = e_inc_tbhiv_100k,
    tb_hiv_incidence_rate_low = e_inc_tbhiv_100k_lo,
    tb_hiv_incidence_rate_high = e_inc_tbhiv_100k_hi,
    tb_hiv_incidence_cases = e_inc_tbhiv_num,
    tb_hiv_incidence_cases_low = e_inc_tbhiv_num_lo,
    tb_hiv_incidence_cases_high = e_inc_tbhiv_num_hi,
    tb_mortality_rate_excl_hiv = e_mort_exc_tbhiv_100k,
    tb_mortality_rate_excl_hiv_low = e_mort_exc_tbhiv_100k_lo,
    tb_mortality_rate_excl_hiv_high = e_mort_exc_tbhiv_100k_hi,
    tb_deaths_excl_hiv = e_mort_exc_tbhiv_num,
    tb_deaths_excl_hiv_low = e_mort_exc_tbhiv_num_lo,
    tb_deaths_excl_hiv_high = e_mort_exc_tbhiv_num_hi,
    tb_hiv_mortality_rate = e_mort_tbhiv_100k,
    tb_hiv_mortality_rate_low = e_mort_tbhiv_100k_lo,
    tb_hiv_mortality_rate_high = e_mort_tbhiv_100k_hi,
    tb_hiv_deaths = e_mort_tbhiv_num,
    tb_hiv_deaths_low = e_mort_tbhiv_num_lo,
    tb_hiv_deaths_high = e_mort_tbhiv_num_hi,
    tb_mortality_rate = e_mort_100k,
    tb_mortality_rate_low = e_mort_100k_lo,
    tb_mortality_rate_high = e_mort_100k_hi,
    tb_deaths = e_mort_num,
    tb_deaths_low = e_mort_num_lo,
    tb_deaths_high = e_mort_num_hi,
    tb_case_fatality_ratio = cfr,
    tb_case_fatality_ratio_low = cfr_lo,
    tb_case_fatality_ratio_high = cfr_hi,
    tb_case_fatality_percentage = cfr_pct,
    tb_case_fatality_percentage_low = cfr_pct_lo,
    tb_case_fatality_percentage_high = cfr_pct_hi,
    tb_notification_rate = c_newinc_100k,
    tb_treatment_coverage = c_cdr,
    tb_treatment_coverage_low = c_cdr_lo,
    tb_treatment_coverage_high = c_cdr_hi
  )


## 2.2 Select relevant variables for analysis

tb_data <- tb_dataset |>
  select(
    country,
    iso2,
    iso3,
    iso_numeric,
    region_code,
    year,
    population_number,
    tb_incidence_rate,
    tb_incidence_rate_low,
    tb_incidence_rate_high,
    tb_mortality_rate,
    tb_mortality_rate_low,
    tb_mortality_rate_high
  )


## 2.3 Import the WHO data dictionary

tb_dictionary <- read_csv("data/raw/TB_data_dictionary_2026-11-03.csv")


## 2.4 Extract WHO regional codes and names

tb_dict_regions <- tb_dictionary |>
  filter(variable_name == "g_whoregion") |>
  separate_longer_delim(
    code_list,
    delim = ";"
  ) |>
  separate_wider_delim(
    code_list,
    delim = "=",
    names = c("code", "region")
  ) |>
  select(code, region) |>
  mutate(
    code = str_trim(code)
  )


## 2.5 Join regional information to the TB dataset

tb_data <- tb_data |>
  left_join(
    tb_dict_regions,
    join_by(region_code == code)
  ) |>
  relocate(region, .after = region_code)


# ============================================================
# 3. DATA EXPLORATION
# ============================================================

## 3.1 Examine dataset structure and dimensions

tb_data |> glimpse()

tb_data |> nrow()

tb_data |> ncol()


## 3.2 Assess missing values

tb_data |>
  summarize(
    across(everything(), ~ sum(is.na(.)))
  )


## 3.3 Inspect the prepared dataset

if (interactive()) {
  View(tb_data)
}


# ============================================================
# 4. GLOBAL ANALYSIS
# ============================================================

## 4.1 Examine global TB incidence and mortality trends

global_trends <- tb_data |>
  filter(
    !is.na(region),
    !is.na(population_number),
    population_number > 0
  ) |>
  group_by(year) |>
  summarize(
    yearly_incidence_rate = weighted.mean(
      tb_incidence_rate,
      population_number,
      na.rm = TRUE
    ),
    yearly_mortality_rate = weighted.mean(
      tb_mortality_rate,
      population_number,
      na.rm = TRUE
    )
  ) |>
  pivot_longer(
    cols = c(yearly_incidence_rate, yearly_mortality_rate),
    names_to = "rate",
    values_to = "value"
  )

global_trends_plot <- global_trends |>
  ggplot(aes(x = year, y = value)) +
  geom_line(
    linewidth = 1,
    color = "#2878A5"
  ) +
  geom_point(
    size = 1,
    color = "#C65368"
  ) +
  facet_wrap(
    ~rate,
    scales = "free_y",
    labeller = as_labeller(c(
      "yearly_incidence_rate" = "TB Incidence",
      "yearly_mortality_rate" = "TB Mortality"
    ))
  ) +
  labs(
    title = "Global Trends in TB Incidence and Mortality",
    subtitle = "Population-weighted estimates, 2000–2024",
    x = "Year",
    y = "Estimated Rate per 100,000 Population",
    caption = "Source: WHO Global Tuberculosis Programme; global estimates derived from country-level data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "grey25"),
    axis.title = element_text(color = "grey20"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave(
  filename = "figures/global_tb_trends.png",
  plot = global_trends_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 5. REGIONAL ANALYSIS
# ============================================================

## 5.1 Calculate annual regional incidence and mortality rates

regional_yearly_data <- tb_data |>
  filter(
    !is.na(region),
    !is.na(population_number),
    population_number > 0
  ) |>
  group_by(region, year) |>
  summarize(
    regional_incidence_rate = weighted.mean(
      tb_incidence_rate,
      population_number,
      na.rm = TRUE
    ),
    regional_mortality_rate = weighted.mean(
      tb_mortality_rate,
      population_number,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


## 5.2 Extract the latest available estimates for each region

latest_year_regional_data <- regional_yearly_data |>
  group_by(region) |>
  arrange(desc(year), .by_group = TRUE) |>
  summarize(
    latest_year = first(year),
    latest_incidence_rate = first(regional_incidence_rate),
    latest_mortality_rate = first(regional_mortality_rate)
  )


## 5.3 Compare incidence and mortality rates across regions

regional_comparison <- latest_year_regional_data |>
  pivot_longer(
    cols = c(latest_incidence_rate, latest_mortality_rate),
    names_to = "measure",
    values_to = "rate"
  )

regional_comparison_plot <- regional_comparison |>
  ggplot(aes(x = region, y = rate, fill = measure)) +
  geom_col(
    position = "dodge",
    width = 0.7
  ) +
  labs(
    title = "TB Incidence and Mortality Rates Across WHO Regions",
    subtitle = "Latest Available Year (2024)",
    x = "WHO Region",
    y = "Estimated Rate per 100,000 Population",
    fill = "Measure",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  scale_fill_manual(
    values = c(
      "latest_incidence_rate" = "steelblue",
      "latest_mortality_rate" = "coral"
    ),
    labels = c(
      "latest_incidence_rate" = "Incidence",
      "latest_mortality_rate" = "Mortality"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top"
  )

ggsave(
  filename = "figures/regional_tb_comparison.png",
  plot = regional_comparison_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 5.4 Examine regional mortality trends over time

regional_mortality_trends_plot <- regional_yearly_data |>
  ggplot(aes(x = year, y = regional_mortality_rate, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Trends in TB Mortality Rates Across WHO Regions",
    subtitle = "2000–2024",
    x = "Year",
    y = "Estimated TB Mortality Rate per 100,000 Population",
    caption = "Source: WHO Global Tuberculosis Programme",
    color = "WHO Region"
  ) +
  theme(legend.position = "bottom")

ggsave(
  filename = "figures/regional_tb_mortality_trends.png",
  plot = regional_mortality_trends_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 5.5 Examine regional incidence trends over time

regional_incidence_trends_plot <- regional_yearly_data |>
  ggplot(aes(x = year, y = regional_incidence_rate, color = region)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Trends in TB Incidence Rates Across WHO Regions",
    subtitle = "2000–2024",
    x = "Year",
    y = "Estimated TB Incidence Rate per 100,000 Population",
    caption = "Source: WHO Global Tuberculosis Programme",
    color = "WHO Region"
  ) +
  theme(legend.position = "bottom")

ggsave(
  filename = "figures/regional_tb_incidence_trends.png",
  plot = regional_incidence_trends_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 5.6 Calculate percentage changes in regional rates

regional_percent_changes <- regional_yearly_data |>
  group_by(region) |>
  arrange(year, .by_group = TRUE) |>
  summarize(
    first_year = first(year),
    last_year = last(year),
    first_incidence = first(regional_incidence_rate),
    last_incidence = last(regional_incidence_rate),
    percent_change_incidence = round(
      ((last_incidence - first_incidence) / first_incidence) * 100,
      0
    ),
    first_mortality = first(regional_mortality_rate),
    last_mortality = last(regional_mortality_rate),
    percent_change_mortality = round(
      ((last_mortality - first_mortality) / first_mortality) * 100,
      0
    )
  )


## 5.7 Compare percentage changes in regional incidence rates

regional_incidence_change_plot <- regional_percent_changes |>
  ggplot(
    aes(
      y = reorder(region, percent_change_incidence),
      x = percent_change_incidence,
      fill = region
    )
  ) +
  geom_col() +
  labs(
    title = "Percentage Change in TB Incidence Rates Across WHO Regions",
    subtitle = "2000–2024",
    x = "Percentage Change in Incidence Rate (%)",
    y = "WHO Region",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "figures/regional_tb_incidence_percentage_change.png",
  plot = regional_incidence_change_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 5.8 Compare percentage changes in regional mortality rates

regional_mortality_change_plot <- regional_percent_changes |>
  ggplot(
    aes(
      y = reorder(region, percent_change_mortality),
      x = percent_change_mortality,
      fill = region
    )
  ) +
  geom_col() +
  labs(
    title = "Percentage Change in TB Mortality Rates Across WHO Regions",
    subtitle = "2000–2024",
    x = "Percentage Change in Mortality Rate (%)",
    y = "WHO Region",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = "figures/regional_tb_mortality_percentage_change.png",
  plot = regional_mortality_change_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 6. COUNTRY-LEVEL ANALYSIS
# ============================================================

## 6.1 Extract the latest available estimates for each country

country_data_latest_year <- tb_data |>
  group_by(country) |>
  arrange(desc(year), .by_group = TRUE) |>
  summarize(
    latest_year = first(year),
    latest_incidence_rate = first(tb_incidence_rate),
    latest_mortality_rate = first(tb_mortality_rate),
    .groups = "drop"
  )


## 6.2 Identify countries with the highest TB incidence rates

country_incidence_top10_plot <- country_data_latest_year |>
  arrange(desc(latest_incidence_rate)) |>
  slice_head(n = 10) |>
  ggplot(
    aes(
      x = latest_incidence_rate,
      y = reorder(country, latest_incidence_rate)
    )
  ) +
  geom_col(
    fill = "#426B7A",
    width = 0.7
  ) +
  labs(
    title = "Countries with the Highest Estimated TB Incidence Rates",
    subtitle = "Top 10 countries by latest available estimates (2024)",
    x = "Estimated TB Incidence Rate per 100,000 Population",
    y = "Country",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "grey25"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "figures/top10_countries_tb_incidence.png",
  plot = country_incidence_top10_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 6.3 Identify countries with the highest TB mortality rates

country_mortality_top10_plot <- country_data_latest_year |>
  arrange(desc(latest_mortality_rate)) |>
  slice_head(n = 10) |>
  ggplot(
    aes(
      x = latest_mortality_rate,
      y = reorder(country, latest_mortality_rate)
    )
  ) +
  geom_col(
    fill = "#426B7A",
    width = 0.7
  ) +
  labs(
    title = "Countries with the Highest Estimated TB Mortality Rates",
    subtitle = "Top 10 countries by latest available estimates (2024)",
    x = "Estimated TB Mortality Rate per 100,000 Population",
    y = "Country",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "grey25"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "figures/top10_countries_tb_mortality.png",
  plot = country_mortality_top10_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 6.4 Determine Egypt's global TB mortality ranking

country_mortality_ranking <- country_data_latest_year |>
  arrange(desc(latest_mortality_rate)) |>
  mutate(
    mortality_rank = min_rank(desc(latest_mortality_rate))
  )

global_mortality_ranking_plot <- country_mortality_ranking |>
  ggplot(aes(x = mortality_rank, y = latest_mortality_rate)) +
  geom_point(
    color = "#AAB4BE",
    alpha = 0.7,
    size = 2
  ) +
  geom_point(
    data = filter(country_mortality_ranking, country == "Egypt"),
    color = "#B45A62",
    size = 4
  ) +
  geom_label(
    data = filter(country_mortality_ranking, country == "Egypt"),
    aes(label = paste0("Egypt (Rank", mortality_rank, ")")),
    color = "#B45A62",
    fill = "white",
    fontface = "bold",
    nudge_x = 5,
    nudge_y = 12,
    label.size = 0.2
  ) +
  labs(
    title = "Global Distribution of TB Mortality Rates by Country",
    subtitle = "Egypt's position in the global ranking, 2024",
    x = "Country Mortality Rank (Highest to Lowest)",
    y = "Estimated TB Mortality Rate per 100,000 Population",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10)
  )

ggsave(
  filename = "figures/egypt_global_tb_mortality_ranking.png",
  plot = global_mortality_ranking_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 6.5 Determine Egypt's global TB incidence ranking

country_incidence_ranking <- country_data_latest_year |>
  arrange(desc(latest_incidence_rate)) |>
  mutate(
    incidence_rank = min_rank(desc(latest_incidence_rate))
  )

global_incidence_ranking_plot <- country_incidence_ranking |>
  ggplot(aes(x = incidence_rank, y = latest_incidence_rate)) +
  geom_point(
    color = "#AAB4BE",
    alpha = 0.7,
    size = 2
  ) +
  geom_point(
    data = filter(country_incidence_ranking, country == "Egypt"),
    color = "#B45A62",
    size = 4
  ) +
  geom_label(
    data = filter(country_incidence_ranking, country == "Egypt"),
    aes(label = paste0("Egypt (Rank", incidence_rank, ")")),
    color = "#B45A62",
    fill = "white",
    fontface = "bold",
    nudge_x = 5,
    nudge_y = 50,
    label.size = 0.2
  ) +
  labs(
    title = "Global Distribution of TB Incidence Rates by Country",
    subtitle = "Egypt's position in the global ranking, 2024",
    x = "Country Incidence Rank (Highest to Lowest)",
    y = "Estimated TB Incidence Rate per 100,000 Population",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10)
  )

ggsave(
  filename = "figures/egypt_global_tb_incidence_ranking.png",
  plot = global_incidence_ranking_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 7. EASTERN MEDITERRANEAN REGION (EMR) ANALYSIS
# ============================================================

## 7.1 Extract EMR country-level data

tb_EMR_data <- tb_data |>
  filter(region_code == "EMR")


## 7.2 Identify the latest available estimates for EMR countries

tb_EMR_data_2024 <- tb_EMR_data |>
  group_by(country) |>
  arrange(desc(year), .by_group = TRUE) |>
  summarize(
    latest_year = first(year),
    latest_population = first(population_number),
    latest_incidence_rate = first(tb_incidence_rate),
    latest_mortality_rate = first(tb_mortality_rate),
    .groups = "drop"
  )


## 7.3 Check the latest available reporting years for EMR countries

tb_EMR_data_2024 |>
  count(latest_year)

tb_EMR_data_2024


## 7.4 Rank EMR countries by TB mortality rates

EMR_mortality_ranking_2024 <- tb_EMR_data_2024 |>
  arrange(desc(latest_mortality_rate)) |>
  mutate(
    mortality_ranking = min_rank(desc(latest_mortality_rate))
  )


## 7.5 Visualize mortality rates and Egypt's regional ranking

emr_mortality_ranking_plot <- EMR_mortality_ranking_2024 |>
  ggplot(
    aes(
      x = latest_mortality_rate,
      y = reorder(country, latest_mortality_rate)
    )
  ) +
  geom_col(
    aes(fill = country == "Egypt"),
    width = 0.7
  ) +
  geom_label(
    data = filter(EMR_mortality_ranking_2024, country == "Egypt"),
    aes(label = paste0("Egypt: Rank ", mortality_ranking)),
    color = "#A65D68",
    fill = "white",
    fontface = "bold",
    nudge_x = 5,
    hjust = 0,
    label.size = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#668B91",
      "TRUE" = "#A65D68"
    ),
    guide = "none"
  ) +
  labs(
    title = "TB Mortality Rates Across Eastern Mediterranean Countries",
    subtitle = "Country-level estimates and Egypt's regional ranking, 2024",
    x = "Estimated TB Mortality Rate per 100,000 Population",
    y = "Country",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "grey25"),
    axis.title = element_text(color = "grey20"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  expand_limits(
    x = max(EMR_mortality_ranking_2024$latest_mortality_rate) * 1.2
  )

ggsave(
  filename = "figures/emr_tb_mortality_ranking.png",
  plot = emr_mortality_ranking_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


## 7.6 Rank EMR countries by TB incidence rates

EMR_incidence_ranking_2024 <- tb_EMR_data_2024 |>
  arrange(desc(latest_incidence_rate)) |>
  mutate(
    incidence_ranking = min_rank(desc(latest_incidence_rate))
  )

EMR_incidence_ranking_2024


## 7.7 Visualize incidence rates and Egypt's regional ranking

emr_incidence_ranking_plot <- EMR_incidence_ranking_2024 |>
  ggplot(
    aes(
      x = latest_incidence_rate,
      y = reorder(country, latest_incidence_rate)
    )
  ) +
  geom_col(
    aes(fill = country == "Egypt"),
    width = 0.7
  ) +
  geom_label(
    data = filter(EMR_incidence_ranking_2024, country == "Egypt"),
    aes(label = paste0("Egypt: Rank ", incidence_ranking)),
    color = "#A65D68",
    fill = "white",
    fontface = "bold",
    nudge_x = 2,
    hjust = 0,
    label.size = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#668B91",
      "TRUE" = "#A65D68"
    ),
    guide = "none"
  ) +
  labs(
    title = "TB Incidence Rates Across Eastern Mediterranean Countries",
    subtitle = "Country-level estimates and Egypt's regional ranking, 2024",
    x = "Estimated TB Incidence Rate per 100,000 Population",
    y = "Country",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "grey25"),
    axis.title = element_text(color = "grey20"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  expand_limits(
    x = max(EMR_incidence_ranking_2024$latest_incidence_rate) * 1.2
  )

ggsave(
  filename = "figures/emr_tb_incidence_ranking.png",
  plot = emr_incidence_ranking_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 8. EGYPT-FOCUSED ANALYSIS
# ============================================================

## 8.1 Examine TB incidence and mortality trends in Egypt

egypt_yearly_data <- tb_EMR_data |>
  filter(country == "Egypt") |>
  select(
    country,
    year,
    tb_incidence_rate,
    tb_mortality_rate
  ) |>
  pivot_longer(
    cols = c(tb_incidence_rate, tb_mortality_rate),
    names_to = "measure",
    values_to = "rate"
  )

egypt_trends_plot <- egypt_yearly_data |>
  ggplot(aes(x = year, y = rate, color = measure)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "tb_incidence_rate" = "#2878A5",
      "tb_mortality_rate" = "#C65368"
    ),
    labels = c(
      "tb_incidence_rate" = "Incidence",
      "tb_mortality_rate" = "Mortality"
    )
  ) +
  labs(
    title = "Trends in TB Incidence and Mortality in Egypt",
    subtitle = "Estimated rates per 100,000 population, 2000–2024",
    x = "Year",
    y = "Estimated Rate per 100,000 Population",
    color = "Measure",
    caption = "Source: WHO Global Tuberculosis Programme"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "grey25"),
    axis.title = element_text(color = "grey20"),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

ggsave(
  filename = "figures/egypt_tb_trends.png",
  plot = egypt_trends_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 9. EGYPT VS. THE REST OF THE EMR
# ============================================================

## 9.1 Calculate annual EMR estimates excluding Egypt

emr_except_egypt <- tb_EMR_data |>
  filter(country != "Egypt") |>
  group_by(year) |>
  summarize(
    tb_incidence_rate = weighted.mean(
      tb_incidence_rate,
      population_number,
      na.rm = TRUE
    ),
    tb_mortality_rate = weighted.mean(
      tb_mortality_rate,
      population_number,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    country = "Eastern Mediterranean Region (excluding Egypt)"
  ) |>
  pivot_longer(
    cols = c(tb_incidence_rate, tb_mortality_rate),
    names_to = "measure",
    values_to = "rate"
  )


## 9.2 Combine Egypt and EMR data

combined <- bind_rows(
  egypt_yearly_data,
  emr_except_egypt
) |>
  arrange(year, country)


## 9.3 Determine legend order based on latest incidence rates

legend_order <- combined |>
  filter(measure == "tb_incidence_rate") |>
  group_by(country) |>
  filter(year == max(year)) |>
  arrange(desc(rate)) |>
  pull(country)


## 9.4 Visualize Egypt's trends relative to the rest of the EMR

egypt_emr_comparison_plot <- combined |>
  mutate(country = factor(country, levels = legend_order)) |>
  ggplot(aes(x = year, y = rate)) +
  geom_line(
    aes(color = country),
    linewidth = 1
  ) +
  geom_point(size = 1) +
  facet_wrap(
    ~measure,
    scales = "free_y",
    labeller = as_labeller(c(
      "tb_incidence_rate" = "TB Incidence",
      "tb_mortality_rate" = "TB Mortality"
    ))
  ) +
  scale_color_manual(
    values = c(
      "Egypt" = "#C65368",
      "Eastern Mediterranean Region (excluding Egypt)" = "#2878A5"
    ),
    breaks = legend_order
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2025, by = 5),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  labs(
    title = "TB Trends in Egypt Compared with the Eastern Mediterranean Region",
    subtitle = "Population-weighted estimates, 2000–2024",
    x = "Year",
    y = "Estimated Rate per 100,000 Population",
    color = "Geographical Area",
    caption = "Source: WHO Global Tuberculosis Programme; EMR estimates derived from country-level data, excluding Egypt"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 10),
    axis.text = element_text(color = "grey25"),
    axis.title = element_text(color = "grey20"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  )

ggsave(
  filename = "figures/egypt_vs_emr_tb_comparison.png",
  plot = egypt_emr_comparison_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)
