# install.packages("topicmodels")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("syuzhet")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidytext")
# install.packages("forcats")
# install.packages("reshape2")
# install.packages("readxl")
# install.packages("readr")
# install.packages("LDAvis")
# install.packages("textmineR")
# install.packages("ldatuning")
# install.packages("Matrix")
# sessionInfo()


# Ngarkojme te gjitha paketat qe na duhen per te kryer topic modelling

library(topicmodels)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)
library(tidytext)
library(forcats)
library(dplyr)
library(reshape2)
library(readxl)
library(readr)
library(LDAvis)
library(stringr)
install.packages("ldatuning")
library(ldatuning)


data <- readr::read_csv("albania_dataset.csv")

skills_data_cleaned <- read.csv("skills_list_cleaned.csv")

# Të gjitha aftësitë duhet të përpunohen fillimisht në skriptin skills_extractor.R.
# Ne kete file R, aftesite jane perpunuar. Output i krijuar pas perpunimit te aftesive, ruhen ne file-n skills_list_cleaned.csv

words_cleaner <- function(vector_of_words){
  vector_of_words <- as.character(vector_of_words)
  
  # FORCE UTF-8 (this fixes the error)
  vector_of_words <- iconv(vector_of_words, from = "", to = "UTF-8", sub = "")
  
  cleaned <- vector_of_words %>%
    stringr::str_to_lower() %>%
    stringr::str_trim()
  
  cleaned <- stringr::str_replace_all(cleaned, "[[:punct:]]", " ")
  cleaned <- tm::stemDocument(cleaned)
  
  cleaned
}

#___________________


data$cleaned_description <- words_cleaner(data$Description) 

res <- sapply(skills_data_cleaned$cleaned_skills, grepl, x = data$Description)
sum(res) # Shfaq nr e rreshtave qe kane te pakten 1 aftesi qe "match"

data <- data %>%
  mutate(
    doc_id = row_number(),
    cleaned_description = words_cleaner(Description)
  )
names(data)

# tokenize from cleaned_description
data_skills_filtered <- data %>%
  select(doc_id, Title, Category, CompanyName, cleaned_description) %>%
  unnest_tokens(word, cleaned_description) %>%
  mutate(word = words_cleaner(word)) %>%
  filter(word %in% skills_data_cleaned$cleaned_skills)


# Filtrojme sipas listes se aftesive te vendosur ne file skills_list_cleaned.csv
data_skills_filtered$word <- data_skills_filtered$word %>% words_cleaner()

data_skills_filtered <- data_skills_filtered %>% filter(word %in% skills_data_cleaned$cleaned_skills)

# Numerojme frekuencat e cdo aftesie

nrow(data_skills_filtered) #ketu shfaqet nr i aftesive qe behen "match" ne te gjithe dataset-in

skills_count <- data_skills_filtered %>%
  count(word, sort = TRUE)

stopifnot("cleaned_skills" %in% names(skills_data_cleaned))

skills_count <- data_skills_filtered %>%
  count(word, sort = TRUE) %>%
  left_join(skills_data_cleaned, by = c("word" = "cleaned_skills")) %>%
  select(-any_of("X"))

# Shikojme aftesite me te larta sipas frekuences
print(skills_count)

# -----------------------------
# 3) PREPARE DOCUMENTS
# -----------------------------
data <- data %>%
  mutate(
    doc_id = row_number(), # internal id (not from dataset)
    cleaned_description = words_cleaner(Description)
  )

# Optional: count docs with at least one skill match (fast regex check)
skill_regex <- paste0("\\b(", paste(unique(skills_data_cleaned$cleaned_skills), collapse="|"), ")\\b")
docs_with_match <- sum(str_detect(data$cleaned_description, skill_regex))
docs_with_match

# -----------------------------
# 4) TOKENIZE + FILTER TO SKILLS
# -----------------------------
data_skills_filtered <- data %>%
  select(doc_id, Title, Category, CompanyName, cleaned_description) %>%
  unnest_tokens(word, cleaned_description) %>%
  mutate(word = words_cleaner(word)) %>%
  filter(word %in% skills_data_cleaned$cleaned_skills)

# Skills frequency
skills_count <- data_skills_filtered %>%
  count(word, sort = TRUE) %>%
  left_join(skills_data_cleaned, by = c("word" = "cleaned_skills")) %>%
  select(-any_of("X"))

print(head(skills_count, 20))

# Plot frequency (top 30)
ggplot(skills_count %>% slice_max(n, n = 30),
       aes(x = reorder(Skill_En, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Skills", y = "Frequency",
       title = "Top 30 most mentioned skills")

# -----------------------------
# 5) BUILD DTM (GLOBAL)
# -----------------------------
dtm <- data_skills_filtered %>%
  count(document = doc_id, word) %>%
  cast_dtm(document, word, n)

# Remove empty docs
rs <- rowSums(as.matrix(dtm))
dtm <- dtm[rs > 0, ]

print(paste("DTM docs:", nrow(dtm), " | DTM terms:", ncol(dtm)))

# ============================================================
# 6) FindTopicsNumber-style metrics WITHOUT ldatuning
#    (Griffiths2004, CaoJuan2009, Arun2010, Deveaud2014)
# ============================================================
#Automatikisht identifikohet nr of topics qe do te perdorim ne modelin LDA, bazuar ne metrics te meposhtme

library(ldatuning)

num_of_topics <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 35, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = NA,
  verbose = TRUE
)

FindTopicsNumber_plot(num_of_topics) #Nga grafiku shohim qe nr i topics me i pershtatshem eshte 7.


# -----------------------------
# 7) FIT FINAL LDA MODEL
# -----------------------------
lda_model <- LDA(dtm, k = 8, control = list(seed = 1234))

# Perplexity of final model
perplexity_final <- topicmodels::perplexity(lda_model, dtm)
print(paste("Final model perplexity:", perplexity_final))

# -----------------------------
# 8) TIDY TOPICS + JOIN WITH SKILL CATEGORY
# -----------------------------
topics <- tidy(lda_model) %>%
  group_by(topic) %>%
  slice_max(beta, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(skills_data_cleaned, by = c("term" = "cleaned_skills")) %>%
  select(topic, term, beta, Skill_En, Skill_Category)

# Plot topics faceted (like your screenshot)
ggplot(topics, aes(x = reorder(Skill_En, beta), y = beta, fill = Skill_Category)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = "Skill", y = "Beta",
       title = "Top skills per topic (color = Skill Category)")

# Plot categories faceted (like your screenshot)
ggplot(topics, aes(x = reorder(Skill_En, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~ Skill_Category, scales = "free") +
  coord_flip() +
  labs(x = "Skill", y = "Beta", fill = "Topic",
       title = "Top skills by category (facets) and topic (color)")

# -----------------------------
# 9) WORDCLOUDS
# -----------------------------
wordcloud(
  words = topics$Skill_En,
  freq  = topics$beta,
  max.words = 50,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

plot_wordcloud_category <- function(cat_name){
  df <- topics %>% filter(Skill_Category == cat_name)
  if(nrow(df) == 0) return(invisible(NULL))
  wordcloud(
    words = df$Skill_En,
    freq  = df$beta,
    max.words = 150,
    random.order = FALSE,
    colors = brewer.pal(8, "Set2")
  )
}

# Example usage:
plot_wordcloud_category("Social skills")
plot_wordcloud_category("Digital skills")
plot_wordcloud_category("Self-management skills")
plot_wordcloud_category("Cognitive skills")

# -----------------------------
# 10) CORRELATION GRAPH (widyr + ggraph)
# -----------------------------
skill_pairs <- data_skills_filtered %>%
  count(doc_id, word) %>%
  filter(n > 0) %>%
  group_by(word) %>%
  filter(n() >= 50) %>%
  ungroup() %>%
  widyr::pairwise_cor(word, doc_id, sort = TRUE, upper = FALSE)

set.seed(1234)
skill_pairs %>%
  filter(correlation > 0.12) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation), colour = "darkred") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# -----------------------------
# 11) SKILLS NETWORK GRAPH (topic -> skill)
# -----------------------------
edges <- topics %>%
  filter(beta > 0.03) %>%
  transmute(from = paste0("Topic_", topic), to = Skill_En, weight = beta)

g <- graph_from_data_frame(edges, directed = FALSE)

set.seed(1234)
ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4, colour = "darkred") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void()

# -----------------------------
# 12) LDAvis (SAFE)
# -----------------------------
post <- topicmodels::posterior(lda_model)
phi  <- post$terms
theta <- post$topics
K <- nrow(phi)

if (K < 3) {
  message("LDAvis skipped: K < 3 (need K >= 3).")
} else {
  doc_length <- rowSums(as.matrix(dtm))
  term_freq  <- colSums(as.matrix(dtm))
  vocab      <- colnames(phi)
  
  json <- LDAvis::createJSON(
    phi = phi,
    theta = theta,
    doc.length = doc_length,
    vocab = vocab,
    term.frequency = term_freq
  )
  LDAvis::serVis(json)
}

# Krijimi i nje objekti JSON per vizualizimin e temave

json_lda <- createJSON(phi = posterior(lda_model)$terms,
                       theta = posterior(lda_model)$topics,
                       doc.length = rowSums(as.matrix(dtm)),
                       vocab = colnames(as.matrix(dtm)),
                       term.frequency = colSums(as.matrix(dtm)))

# Vizualizimi i temave
serVis(json_lda)
rowSums(posterior(lda_model)$terms)


# -----------------------------
# 13) TOPIC COHERENCE (UMass)
# -----------------------------
umass_coherence <- function(dtm, phi, top_m = 10, epsilon = 1e-12){
  mat <- as.matrix(dtm)
  term_names <- colnames(mat)
  bin <- (mat > 0) * 1
  df <- colSums(bin)
  
  coherences <- numeric(nrow(phi))
  for (t in seq_len(nrow(phi))) {
    top_terms <- term_names[order(phi[t, ], decreasing = TRUE)][1:top_m]
    score <- 0
    for (i in 2:length(top_terms)) {
      wi <- top_terms[i]
      for (j in 1:(i-1)) {
        wj <- top_terms[j]
        Dij <- sum(bin[, wi] * bin[, wj])
        Dj  <- df[wj]
        score <- score + log((Dij + 1) / (Dj + epsilon))
      }
    }
    coherences[t] <- score
  }
  coherences
}

topic_coh <- umass_coherence(dtm, phi, top_m = 10)
print(topic_coh)
print(paste("Mean topic coherence (UMass):", mean(topic_coh)))

# -----------------------------
# 14) EXPORTS
# -----------------------------
write.csv(skills_count, "skills_frequency_overall.csv", row.names = FALSE)
write.csv(topics, "topics_top_terms.csv", row.names = FALSE)
write.csv(num_of_topics, "FindTopicsNumber_metrics.csv", row.names = FALSE)
write.csv(data.frame(topic = 1:length(topic_coh), coherence = topic_coh),
          "topic_coherence_umass.csv", row.names = FALSE)

saveRDS(list(lda_model = lda_model, dtm = dtm, topics = topics, metrics = num_of_topics),
        "lda_outputs.rds")

# =============================
# 14) SKILL PREDICTION FROM TEXT PROMPTS  (FIXED)
# =============================

predict_skills <- function(
    new_text,
    top_n = 10,
    lda_model_obj = lda_model,   # default: uses your trained model in workspace
    dtm_ref = dtm,               # default: uses your training dtm in workspace
    topics_df = topics           # default: uses your tidy topics table in workspace
){
  # 1) Clean text (same pipeline as training)
  new_text_clean <- words_cleaner(new_text)
  
  # 2) Build corpus
  new_corpus <- tm::Corpus(tm::VectorSource(new_text_clean))
  new_corpus <- tm::tm_map(new_corpus, tm::content_transformer(tolower))
  new_corpus <- tm::tm_map(new_corpus, tm::removePunctuation)
  new_corpus <- tm::tm_map(new_corpus, tm::removeWords, tm::stopwords("english"))
  new_corpus <- tm::tm_map(new_corpus, tm::stripWhitespace)
  
  # 3) Create DTM using the SAME vocabulary as training dtm
  new_dtm <- tm::DocumentTermMatrix(
    new_corpus,
    control = list(dictionary = tm::Terms(dtm_ref))
  )
  
  # Edge case: if prompt contains none of the training vocab -> empty dtm
  if (ncol(new_dtm) == 0 || sum(new_dtm$v) == 0) {
    return(list(
      prompt = new_text,
      predicted_topic = NA,
      topic_probabilities = NA,
      top_10_skills = tibble::tibble(),
      note = "No terms from the prompt matched the training vocabulary."
    ))
  }
  
  # 4) Topic probabilities
  post <- topicmodels::posterior(lda_model_obj, new_dtm)
  topic_probs <- as.numeric(post$topics[1, ])
  best_topic <- which.max(topic_probs)
  
  # 5) Top skills for predicted topic
  top_skills <- topics_df %>%
    dplyr::filter(topic == best_topic) %>%
    dplyr::arrange(dplyr::desc(beta)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::select(topic, term, Skill_En, Skill_Category, beta)
  
  list(
    prompt = new_text,
    predicted_topic = best_topic,
    topic_probabilities = topic_probs,
    top_10_skills = top_skills
  )
}

# --------- Nice printer (optional but useful) ----------
print_prediction <- function(pred){
  cat("\n==============================\n")
  cat("PROMPT:\n", pred$prompt, "\n\n")
  cat("Predicted Topic:", pred$predicted_topic, "\n\n")
  
  if (!is.null(pred$note)) {
    cat("NOTE:", pred$note, "\n")
    return(invisible(pred))
  }
  
  cat("Top skills:\n")
  pred$top_10_skills %>%
    dplyr::mutate(beta = round(beta, 4)) %>%
    dplyr::select(Skill_En, Skill_Category, beta) %>%
    print(n = Inf)
  
  invisible(pred)
}

# -----------------------------
# TEST PROMPTS (fixed calls)
# -----------------------------
prompt_1 <- "we are looking for a good communication officer to manage social media of computers design company"
test_1 <- predict_skills(prompt_1, top_n = 10)
test_1$predicted_topic
test_1$top_10_skills
print_prediction(test_1)

prompt_2 <- "Progeen Construction Company is looking for an administrative assistant to pursue any matter of an administrative nature"
test_2 <- predict_skills(prompt_2, top_n = 10)
test_2$predicted_topic
test_2$top_10_skills
print_prediction(test_2)

prompt_3 <- "EPPC Albania is looking for a Retail Store Manager. University Degree in Business Administration, or other similar branches"
test_3 <- predict_skills(prompt_3, top_n = 10)
test_3$predicted_topic
test_3$top_10_skills
print_prediction(test_3)

