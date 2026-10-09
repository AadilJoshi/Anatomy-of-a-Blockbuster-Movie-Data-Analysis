library(tidyverse)
library(tidytext)
library(scales)

# COMP3020 Social Web Analytics
# The Anatomy of a Blockbuster
# Section 3: Text/Content Analysis
# Author: Noor

# Load packages
library(tidyverse)
library(tidytext)
library(scales)

# Import movie dataset
movies <- read_csv("data/movies.csv", show_col_types = FALSE)

# Check dataset structure
glimpse(movies)

# Count movies
nrow(movies)

# Check missing descriptions
sum(is.na(movies$overview) | trimws(movies$overview) == "")

# Check duplicate movie IDs
sum(duplicated(movies$id))


# ============================================================
# SECTION 3: TEXT ANALYSIS
# ============================================================

# 1. Prepare movie descriptions
movies_clean <- movies %>%
  filter(!is.na(overview), trimws(overview) != "") %>%
  distinct(id, .keep_all = TRUE)

# 2. Convert descriptions into individual words
movie_words <- movies_clean %>%
  select(id, overview) %>%
  unnest_tokens(word, overview)

# 3. Remove common English stopwords
movie_words_clean <- movie_words %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  filter(nchar(word) >= 3)

# 4. Calculate word frequencies
word_frequency <- movie_words_clean %>%
  count(word, sort = TRUE)

# 5. Display the 20 most frequent words
top_20_words <- word_frequency %>%
  slice_head(n = 20)

print(top_20_words)

# 6. Save frequency results
write_csv(word_frequency, "data/word_frequency.csv")



# ============================================================
# FIGURE 1: TOP 20 MOST FREQUENT WORDS
# ============================================================

figure1 <- ggplot(
  top_20_words,
  aes(x = reorder(word, n), y = n)
) +
  geom_col(fill = "#7A1538", width = 0.75) +
  coord_flip() +
  labs(
    title = "Most Frequent Words in Movie Descriptions",
    subtitle = "Analysis of 500 TMDb movies (2015–2024)",
    x = NULL,
    y = "Word frequency",
    caption = "Source: TMDb | Analysis conducted in R"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    panel.grid.major.y = element_blank()
  )

# Display graph
print(figure1)

# Save graph
ggsave(
  filename = "figures/word_frequency.png",
  plot = figure1,
  width = 10,
  height = 7,
  dpi = 300
)



# ============================================================
# FIGURE 2: WORD USAGE BY MOVIE POPULARITY
# ============================================================

# 1. Calculate median popularity
median_popularity <- median(
  movies_clean$popularity,
  na.rm = TRUE
)

print(median_popularity)

# 2. Divide movies into popularity groups
movies_groups <- movies_clean %>%
  mutate(
    popularity_group = if_else(
      popularity >= median_popularity,
      "Higher popularity",
      "Lower popularity"
    )
  )

# Check how many movies are in each group
print(
  movies_groups %>%
    count(popularity_group)
)

# 3. Join popularity groups to cleaned words
words_by_popularity <- movie_words_clean %>%
  left_join(
    movies_groups %>%
      select(id, popularity_group),
    by = "id"
  )

# 4. Count how many distinct movies mention each word
word_document_counts <- words_by_popularity %>%
  distinct(id, popularity_group, word) %>%
  count(popularity_group, word, name = "movie_count")

# 5. Calculate percentage of movies mentioning each word
group_sizes <- movies_groups %>%
  count(popularity_group, name = "total_movies")

word_percentages <- word_document_counts %>%
  left_join(group_sizes, by = "popularity_group") %>%
  mutate(
    percentage = 100 * movie_count / total_movies
  )

# 6. Compare word percentages between groups
word_comparison <- word_percentages %>%
  select(popularity_group, word, movie_count, percentage) %>%
  pivot_wider(
    names_from = popularity_group,
    values_from = c(movie_count, percentage),
    values_fill = 0
  ) %>%
  mutate(
    difference = `percentage_Higher popularity` -
      `percentage_Lower popularity`,
    total_mentions = `movie_count_Higher popularity` +
      `movie_count_Lower popularity`
  ) %>%
  filter(total_mentions >= 8)

# 7. Select the 10 most distinctive words for each group
higher_words <- word_comparison %>%
  slice_max(difference, n = 10, with_ties = FALSE)

lower_words <- word_comparison %>%
  slice_min(difference, n = 10, with_ties = FALSE)

distinctive_words <- bind_rows(
  higher_words,
  lower_words
) %>%
  distinct(word, .keep_all = TRUE) %>%
  mutate(
    more_common_in = if_else(
      difference >= 0,
      "Higher popularity",
      "Lower popularity"
    )
  )

# Display results
print(
  distinctive_words %>%
    select(word, difference, more_common_in)
)

# Save results
write_csv(
  word_comparison,
  "data/popularity_word_comparison.csv"
)



# ============================================================
# FIGURE 2: POPULARITY WORD COMPARISON VISUALISATION
# ============================================================

figure2 <- ggplot(
  distinctive_words,
  aes(
    x = reorder(word, difference),
    y = difference,
    fill = more_common_in
  )
) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Higher popularity" = "#7A1538",
      "Lower popularity" = "#D4A373"
    )
  ) +
  geom_hline(
    yintercept = 0,
    colour = "grey40",
    linewidth = 0.5
  ) +
  labs(
    title = "Words Associated with Movie Popularity",
    subtitle = "Difference in percentage of movies mentioning each word",
    x = NULL,
    y = "Difference (percentage points)",
    fill = "More common in",
    caption = "Source: TMDb | 500 movies (2015–2024)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

# Display graph
print(figure2)

# Save graph
ggsave(
  filename = "figures/popularity_word_comparison.png",
  plot = figure2,
  width = 11,
  height = 8,
  dpi = 300
)


print(median_popularity)

movies_groups %>%
  count(popularity_group)