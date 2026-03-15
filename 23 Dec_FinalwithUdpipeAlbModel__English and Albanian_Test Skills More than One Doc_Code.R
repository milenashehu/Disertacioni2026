# app.R — Single-org Skills App
# UPDATED (Phrase-level matching):
# - Extracts bigrams/trigrams from the REQUIRED section (POS-filtered UDPIPE tokens)
# - Fuzzy-matches phrases against skill_sq_clean / skill_en_clean (not unigram tokens)
# - This makes category plots reflect your skills taxonomy (Cognitive/Communication/Management/etc.)

# -------------------- Libraries --------------------
library(shiny)
library(tm)
library(topicmodels)
library(dplyr)
library(stringr)
library(SnowballC)
library(readtext)
library(ggplot2)
library(DT)
library(fuzzyjoin)
library(udpipe)
library(wordcloud)
library(RColorBrewer)
library(purrr)
library(stringdist)
library(igraph)
library(corrplot)
library(glue)
library(pagedown)
library(readr)
library(jsonlite)
library(memoise)
library(digest)
library(tidyr)
library(tibble)
library(officer)
library(tools)
library(readxl)
library(htmltools)
library(LDAvis)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================= BRANDING ======================================
BRAND_NAME <- "Harta e Aftësive"

ensure_brand_assets <- function() {
  if (!dir.exists("www")) dir.create("www", recursive = TRUE)
  dest <- file.path("www", "logo.svg")
  if (!file.exists(dest)) {
    cat(
      '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 320 80">
         <rect width="420" height="100" fill="#004494"/>
         <text x="90" y="50" font-family="system-ui,Arial" font-size="24" fill="#fff">Harta e Aftësive</text>
         <circle cx="46" cy="40" r="26" fill="#0E47CB"/>
         <circle cx="46" cy="40" r="13" fill="#FFCC00"/>
       </svg>',
      file = dest
    )
  }
  invisible(dest)
}
ensure_brand_assets()

# ============================= APP PATH HELPERS ==============================
get_app_root <- function() {
  d <- tryCatch(shiny::getAppDir(), error = function(e) NULL)
  if (!is.null(d) && nzchar(d) && dir.exists(d)) return(normalizePath(d, winslash = "/", mustWork = TRUE))
  
  if (interactive()) {
    wd <- getwd()
    if (file.exists(file.path(wd, "app.R")) || file.exists(file.path(wd, "server.R")) || file.exists(file.path(wd, "ui.R"))) {
      return(normalizePath(wd, winslash = "/", mustWork = TRUE))
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

pick_latest_file <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(character(0))
  info <- file.info(paths)
  paths[which.max(info$mtime)]
}

# ============================= UDPIPE + LANGUAGE =============================
load_udpipe_model <- function(language = c("english", "albanian")) {
  language <- match.arg(language)
  
  app_root   <- get_app_root()
  models_dir <- file.path(app_root, "models")
  
  list_models <- function(dir_path, lang) {
    if (!dir.exists(dir_path)) return(character(0))
    list.files(
      dir_path,
      pattern = paste0("^", lang, ".*\\.udpipe$"),
      ignore.case = TRUE,
      full.names = TRUE
    )
  }
  
  # Albanian: always pick latest local (models/ preferred)
  if (language == "albanian") {
    cand_models <- list_models(models_dir, "albanian")
    model_file  <- if (length(cand_models)) pick_latest_file(cand_models) else pick_latest_file(list_models(app_root, "albanian"))
    
    if (length(model_file) == 1 && nzchar(model_file) && file.exists(model_file)) {
      return(udpipe::udpipe_load_model(model_file))
    }
    
    stop(
      "No Albanian UDPIPE model found.\n",
      "Searched (preferred): ", models_dir, "\n",
      "Fallback: ", app_root, "\n\n",
      "Expected filename like: 'albanian-*.udpipe' inside models/."
    )
  }
  
  # English: prefer local (offline), else download
  cand_local <- unique(c(list_models(models_dir, "english"), list_models(app_root, "english")))
  if (length(cand_local)) {
    model_file <- pick_latest_file(cand_local)
    return(udpipe::udpipe_load_model(model_file))
  }
  
  dl <- udpipe::udpipe_download_model(language = "english")
  udpipe::udpipe_load_model(dl$file_model)
}

get_ud_model <- memoise::memoise(load_udpipe_model)

# ============================= STOPWORDS =====================================
albanian_stopwords <- tolower(c(
  "dhe","ose","por","po","qe","se","pra","e","ne","në","per","për","nga","me","mbi",
  "te","të","ky","kjo","ai","ajo","ata","ato","i","u",
  "jam","je","është","eshte","janë","jane","kam","ke","ka","kemi","keni","kanë","kane",
  "ishte","kisha","kishte","do","duhet","mund",
  "së","si","kështu","keshtu","shumë","shume","pak","edhe",
  "një","nje","çdo","cdo","këtë","kete","aty","këtu","ketu",
  "kush","pse","kur","ku","sa","as","ndër","nder","gjithë","gjithe","vetëm","vetem"
))

stopwords_lang <- function(lang_code) if (lang_code == "en") tm::stopwords("en") else albanian_stopwords

stem_lang <- function(tokens, lang_code) {
  if (lang_code == "en") SnowballC::wordStem(tokens, "en") else tokens
}

# ====================== Non-skill stoplist (default) =========================
non_skill_stoplist_default <- function() {
  tolower(c(
    "albania","tirana","durres","vlore","prishtina","kosovo","macedonia",
    "europe","balkan","italy","germany","london","paris",
    "company","university","department","ministry","government",
    "foundation","association","hospital","school","bank",
    "shpk","sha","ltd","llc","inc","plc","gmbh","sa","spa",
    "eu","un","usaid","who","unicef"
  ))
}

# =================== Required Section Extractor ==============================
extract_required_section <- function(txt) {
  txt <- tryCatch(as.character(txt)[1], error = function(e) "")
  if (is.null(txt) || is.na(txt) || !nzchar(txt)) return("")
  lines <- unlist(strsplit(txt, "\n", fixed = TRUE), use.names = FALSE)
  norm  <- trimws(lines)
  
  start_patterns <- c(
    "required\\s+(qualifications|skills|competences|experience)",
    "qualifications\\s*(:|\\n|$)",
    "requirements\\s*(:|\\n|$)",
    "what you(?:'|’)ll need","what we(?:'|’)re looking for",
    "must\\s+have","about you","profile",
    "kualifikimet\\s+e\\s+kerkuara","kerkesa[t]? e punes","aftesi(?:t)?\\s+te\\s+kerkuara"
  )
  stop_patterns <- c(
    "responsibilities","duties","tasks","what you(?:'|’)ll do",
    "benefits","perks","why join us","about the role","about us",
    "company overview","how to apply","application","pay|salary|compensation",
    "work environment","our values","diversity","privacy"
  )
  is_heading_like <- function(s) {
    s2 <- gsub("[^A-Za-z ]", "", s)
    nchar(s2) > 0 && (grepl("^[A-Z][A-Za-z ]{0,60}$", s2) || grepl("^[-•*]{0,3}\\s*[A-Za-z].{0,60}$", s))
  }
  
  start_idx <- NA_integer_
  for (pat in start_patterns) {
    hits <- which(grepl(pat, tolower(norm)))
    if (length(hits)) { start_idx <- hits[1]; break }
  }
  if (is.na(start_idx)) {
    n <- length(norm); s <- floor(n/3); e <- min(n, s + floor(n/3))
    return(paste(norm[s:e], collapse = "\n"))
  }
  end_idx <- length(norm)
  for (i in seq.int(start_idx + 1, length(norm))) {
    l <- norm[i]
    if (is_heading_like(l) && any(grepl(paste0("\\b(", paste(stop_patterns, collapse="|"), ")\\b"), tolower(l)))) {
      end_idx <- i - 1; break
    }
  }
  paste(norm[start_idx:end_idx], collapse = "\n")
}

# =================== Tokenization (lemma + stem, POS filtered) ===============
tokenize_udpipe <- function(text, extra_stop = character(0), ud_model, lang_code = "en") {
  ann <- udpipe::udpipe_annotate(ud_model, x = text)
  df  <- as.data.frame(ann)
  
  extra_stop <- tolower(trimws(extra_stop))
  bad_terms  <- unique(c(non_skill_stoplist_default(), extra_stop))
  sw <- stopwords_lang(lang_code)
  
  df %>%
    filter(!is.na(lemma)) %>%
    mutate(
      lemma_lower = tolower(lemma),
      stem        = stem_lang(lemma_lower, lang_code)
    ) %>%
    filter(
      upos %in% c("NOUN","ADJ","PROPN"),
      !lemma_lower %in% sw,
      !lemma_lower %in% bad_terms,
      nchar(stem) >= 3,
      !grepl("[0-9]", lemma_lower),
      grepl("[[:alpha:]]", lemma_lower)
    ) %>%
    select(lemma_lower, stem)
}

# ========================= Skills backend (BILINGUAL) =========================
SKILLS_XLSX_PATH <- "data/skills_list_raw.xlsx"
SKILLS_CSV_PATH  <- "data/skills_list_raw.csv"

normalize_names <- function(x) {
  x <- enc2utf8(x)
  x <- gsub("\uFEFF", "", x, fixed = TRUE)
  x <- gsub("[\u00A0\u2007\u202F]", " ", x)
  x <- tolower(x)
  x <- trimws(gsub("\\s+", " ", x))
  gsub("[^a-z0-9 ]+", "", x)
}

pick_col <- function(raw, patterns, default_idx = 1) {
  n_clean <- normalize_names(names(raw))
  hit <- which(Reduce(`|`, lapply(patterns, function(p) grepl(p, n_clean, perl = TRUE))))
  if (length(hit)) names(raw)[hit[1]] else names(raw)[default_idx]
}

clean_phrase <- function(x) {
  x %>%
    enc2utf8() %>%
    tolower() %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}

load_embedded_skills <- function() {
  raw <- NULL
  
  if (file.exists(SKILLS_XLSX_PATH)) {
    raw <- readxl::read_excel(SKILLS_XLSX_PATH, sheet = 1)
  } else if (file.exists(SKILLS_CSV_PATH)) {
    raw <- readr::read_csv(SKILLS_CSV_PATH, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
  } else {
    stop(sprintf("Missing skills data. Add %s (preferred) or %s.", SKILLS_XLSX_PATH, SKILLS_CSV_PATH))
  }
  
  col_en  <- pick_col(raw, c("^skill[_ ]?en$", "skill.*en", "english"))
  col_sq  <- pick_col(raw, c("^skill[_ ]?(al|sq)$", "skill.*al", "albanian", "shqip"))
  col_cat <- pick_col(raw, c("^skill[_ ]?category$", "category", "type", "dimension", "domain"))
  
  out <- tibble::tibble(
    skill_en      = as.character(raw[[col_en]]),
    skill_sq      = as.character(raw[[col_sq]]),
    oecd_category = as.character(raw[[col_cat]])
  ) %>%
    mutate(
      oecd_category = stringr::str_squish(as.character(oecd_category)),
      oecd_category = dplyr::na_if(oecd_category, ""),
      oecd_category = dplyr::coalesce(oecd_category, "Other"),
      skill_en_clean = clean_phrase(skill_en),
      skill_sq_clean = clean_phrase(skill_sq)
    ) %>%
    filter(nzchar(skill_en_clean) | nzchar(skill_sq_clean)) %>%
    distinct(skill_en_clean, skill_sq_clean, oecd_category, .keep_all = TRUE)
  
  out
}

skills_df_static <- load_embedded_skills()

detect_skills_backend_name <- function(path) {
  nm <- tolower(basename(path))
  if (grepl("esco", nm)) return("ESCO")
  if (grepl("oecd", nm) || grepl("skills[-_]?for[-_]?jobs", nm)) return("OECD Skills-for-Jobs")
  "Skills backend"
}

backend_source_path <- if (file.exists(SKILLS_XLSX_PATH)) SKILLS_XLSX_PATH else SKILLS_CSV_PATH
backend_name_static <- detect_skills_backend_name(backend_source_path)
backend_short_static <- tolower(gsub("[^a-z0-9]+", "_", backend_name_static))

# ===================== PHRASE MATCHING (DROP-IN) =============================
# Build candidate bigrams/trigrams from POS-filtered token sequences.
# Matching is done against skill_en_clean or skill_sq_clean (phrase-level).

build_ngrams <- function(tokens, n = 2L) {
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) < n) return(character(0))
  vapply(seq_len(length(tokens) - n + 1L), function(i) paste(tokens[i:(i+n-1L)], collapse = " "), character(1))
}

candidate_phrases_from_tokens <- function(tokens_list, lang_code = "en", ngram_n = c(2L, 3L),
                                          min_count = 1L, top_n = 200L) {
  # Use lemma sequence (more human / closer to skills phrases)
  all_phr <- character(0)
  
  for (df in tokens_list) {
    if (is.null(df) || nrow(df) == 0) next
    lemmas <- df$lemma_lower
    lemmas <- clean_phrase(lemmas)
    lemmas <- lemmas[nzchar(lemmas)]
    if (length(lemmas) < 2) next
    
    # doc phrases
    doc_phr <- unlist(lapply(ngram_n, function(n) build_ngrams(lemmas, n = n)), use.names = FALSE)
    doc_phr <- clean_phrase(doc_phr)
    doc_phr <- doc_phr[nzchar(doc_phr)]
    all_phr <- c(all_phr, doc_phr)
  }
  
  if (!length(all_phr)) {
    return(tibble::tibble(Phrase = character(0), Beta = numeric(0), phrase_clean = character(0)))
  }
  
  tb <- tibble::tibble(phrase_clean = all_phr) %>%
    count(phrase_clean, name = "n", sort = TRUE) %>%
    filter(n >= min_count) %>%
    slice_head(n = top_n) %>%
    mutate(
      Beta = n / sum(n),
      Phrase = phrase_clean
    ) %>%
    select(Phrase, Beta, phrase_clean)
  
  tb
}

skills_phrase_table <- function(skills_tbl, lang_code = "en") {
  if (lang_code == "sq") {
    skills_tbl %>%
      filter(nzchar(skill_sq_clean)) %>%
      transmute(
        skill_phrase = skill_sq_clean,
        Skill_Category = oecd_category,
        Matched_Skill_Label = skill_sq
      ) %>%
      distinct(skill_phrase, Skill_Category, Matched_Skill_Label)
  } else {
    skills_tbl %>%
      filter(nzchar(skill_en_clean)) %>%
      transmute(
        skill_phrase = skill_en_clean,
        Skill_Category = oecd_category,
        Matched_Skill_Label = skill_en
      ) %>%
      distinct(skill_phrase, Skill_Category, Matched_Skill_Label)
  }
}

match_phrases_to_skills <- function(phr_df, skills_tbl_phr, max_dist_use = 0.25) {
  # JW distance: 0 exact, closer to 1 = far
  # NOTE: With phrases, you usually want stricter thresholds than tokens.
  if (is.null(phr_df) || nrow(phr_df) == 0) {
    return(tibble::tibble(
      Forecasted_Skills = character(0), Beta = numeric(0), term_stem = character(0),
      Matched_OECD_Skill = character(0), Skill_Category = character(0), dist = numeric(0), conf_label = character(0)
    ))
  }
  
  joined <- fuzzyjoin::stringdist_left_join(
    phr_df,
    skills_tbl_phr,
    by = c("phrase_clean" = "skill_phrase"),
    method = "jw",
    max_dist = max_dist_use,
    distance_col = "dist"
  ) %>%
    group_by(phrase_clean) %>%
    slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      Forecasted_Skills = Phrase,
      Matched_OECD_Skill = Matched_Skill_Label,
      Skill_Category = dplyr::if_else(is.na(dist), "Unmatched", Skill_Category),
      conf_label = dplyr::case_when(
        !is.na(dist) & dist <= (max_dist_use * 0.5) ~ "High confidence",
        !is.na(dist) ~ "Matched",
        TRUE ~ ""
      ),
      term_stem = phrase_clean
    ) %>%
    select(Forecasted_Skills, Beta, term_stem, Matched_OECD_Skill, Skill_Category, dist, conf_label)
  
  joined
}

# ===================== DTM helpers (kept for LDAvis tab) ======================
build_dtm <- function(tokens_list) {
  docs_str <- vapply(tokens_list, function(df) paste(df$stem, collapse = " "), "")
  corp <- Corpus(VectorSource(docs_str))
  dtm  <- DocumentTermMatrix(corp, control = list(wordLengths = c(3, Inf)))
  removeSparseTerms(dtm, 0.995)
}

pretty_map_from_tokens <- function(tokens_list) {
  bind_rows(tokens_list, .id = "doc") |>
    count(stem, lemma_lower, sort = TRUE) |>
    group_by(stem) |>
    slice_max(n, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(stem, best_lemma = lemma_lower)
}

global_top_terms_from_lda <- function(lda, n_top = 30) {
  post  <- topicmodels::posterior(lda)
  phi   <- post$terms
  theta <- post$topics
  w_z   <- colMeans(theta)
  p_w   <- as.numeric(w_z %*% phi)
  vocab <- colnames(phi)
  tibble(term = vocab, Beta = p_w) |> arrange(desc(Beta)) |> slice_head(n = n_top)
}

doc_top_terms_from_lda <- function(lda, doc_index, n_top = 30) {
  post  <- topicmodels::posterior(lda)
  phi   <- post$terms
  theta <- post$topics
  p_w   <- as.numeric(theta[doc_index, , drop = FALSE] %*% phi)
  vocab <- colnames(phi)
  tibble(term = vocab, Beta = p_w) |> arrange(desc(Beta)) |> slice_head(n = n_top)
}

# ================================ UI =========================================
ui <- fluidPage(
  tags$head(
    tags$title(BRAND_NAME),
    tags$link(rel="icon", type="image/svg+xml", href="logo.svg"),
    tags$style(HTML("
      body { padding-bottom: 70px !important; }
      .brandbar { display:flex; align-items:center; gap:14px; padding:12px 16px; border-bottom:1px solid #e5e7eb; background:#f8fafc; }
      .brandbar img { height:42px; }
      .brandbar .brandname { font-size:26px; font-weight:700; color:#004494; letter-spacing:.2px; }
      .brandbar .subtitle { font-size:13px; color:#64748b; margin-left:4px; }
      .badge { display:inline-block; margin:6px 0 12px; padding:4px 10px; border-radius:999px; font-size:12px; background:#eef2ff; color:#3730a3; border:1px solid #c7d2fe; }
      .app-footer { position: fixed; left:0; bottom:0; width:100%; height:50px; background:#f9f9f9; border-top:1px solid #ddd; text-align:center; line-height:50px; font-size:0.8em; color:#555; z-index:1000; }
      .app-footer a { color:#555; text-decoration:none; }
      .callout { background:#f5faff; border:1px solid #cfe8ff; border-radius:8px; padding:14px; }
      .mono { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, 'Liberation Mono', monospace; white-space: pre-wrap; }
    "))
  ),
  
  tags$div(class="brandbar",
           tags$img(src="logo.svg", alt=BRAND_NAME),
           tags$span(class="brandname", BRAND_NAME),
           tags$span(class="subtitle", backend_name_static)
  ),
  titlePanel(NULL),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "callout",
          tags$b("Aplikacioni për gjetjen e aftësive"),
          p("Ngarkoni një ose disa Përshkrime Pune (JD). Aplikacioni analizon VETËM seksionet 'Kualifikimet / kompetencat / përvoja e kërkuar'."),
          p(tags$em(sprintf("Lista e aftësive e përfshirë brenda aplikacionit.", backend_name_static)))
      ),
      div(
        class = "model-info",
        tags$b("Model info:"),
        tags$br(),
        HTML("Modeli në gjuhën Anglisht : <code>udpipe_download_model('english')</code> .<br/>
              Modeli në gjuhën Shqip: local <code>albanian-*.udpipe</code> from <code>models/</code> (përditësimi më i fundit është përdorur).")
      ),
      
      selectInput("language_model", "Gjuha e modelit:", choices = c("English", "Albanian"), selected = "English"),
      
      fileInput("input_file","Ngarko JD(s)", accept=c(".docx",".pdf",".txt"), multiple=TRUE),
      
      tags$hr(),
      h5("Opsionale: non-skill stoplist (.csv)"),
      fileInput("non_skill_csv", "non-skill stoplist (.csv)", accept = ".csv"),
      helpText("CSV with a column 'term'."),
      downloadButton("dl_non_skill_template", "Shkarko template (.csv)"),
      
      tags$hr(),
      selectInput("file_select","Zgjidh dokumentin:", choices="All"),
      sliderInput("num_skills","Numri i aftësive që do të parashikosh:",5,80,30),
      numericInput("topics_k", "Topics K (5 = auto):", value = 0, min = 0, max = 30, step = 1),
      
      sliderInput("confidence_threshold","Fuzzy Match Threshold (JW dist):",0.05,0.6,0.20,step=0.05),
      checkboxInput("show_high_conf","Highlight High-Confidence Matches",FALSE),
      
      actionButton("submit_btn","Run"),
      
      br(), br(),
      downloadButton("download_summary_pdf","Shkarko raportin përmbledhës (PDF)"),
      downloadButton("download_results","Shkarko dokumentin CSV të aftësive")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Ngarko & Shiko paraprakisht",
                 fluidRow(
                   column(6, h4("Ngarko dokumentat"),
                          div(style = "overflow-x:auto; width:100%;", DTOutput("upload_table", width = "100%"))),
                   column(6, h4("Combined Required Section (preview)"),
                          div(style = "border:1px solid #e5e7eb; border-radius:8px; padding:10px; background:#f9fafb;",
                              verbatimTextOutput("combined_required_preview", placeholder = FALSE)))
                 )
        ),
        
        tabPanel("JD Template",
                 div(class = "callout",
                     p("Përdorni këtë strukturë të standardizuar JD për të përmirësuar qartësinë e përshkrimit të vendit të punës. Aplikacioni lexon vetëm seksionin e Kërkesave.")
                 ),
                 h3("Standard Structure of JD"),
                 tags$ol(
                   tags$li("JD overview (2–3 sentences)"),
                   tags$li("Team & Reporting Line"),
                   tags$li("Key Responsibilities (5–8 bullets, outcomes)"),
                   tags$li("Required Qualifications / Competences / Experience"),
                   tags$li("Preferred (Nice-to-have)"),
                   tags$li("Working Conditions"),
                   tags$li("Compensation & Benefits"),
                   tags$li("Equal Opportunity Statement & Data Privacy"),
                   tags$li("Application Instructions & Deadline")
                 ),
                 br(),
                 downloadButton("download_jd_template_docx", "Shkarko Template e JD (DOCX)")
        ),
        
        tabPanel("Analiza e të gjithë dokumentacionit",
                 div(class="badge", sprintf("Using %s skills backend", backend_name_static)),
                 tabsetPanel(
                   tabPanel("Seksionet e kërkuara", verbatimTextOutput("overall_required_text", placeholder = FALSE)),
                   tabPanel("Aftësitë e gjetura", plotOutput("overall_skills_plot"), DTOutput("overall_skills_table")),
                   tabPanel("Shpërndarja e temave (LDA në tokens)", htmlOutput("overall_ldavis"), plotOutput("overall_topic_plot")),
                   tabPanel("Përmbledhja e kategorive", tableOutput("overall_category_summary"), plotOutput("overall_wordcloud", height="400px")),
                   tabPanel("Korrelacioni i aftësive",
                            fluidRow(
                              column(6, h4("Network"),  plotOutput("overall_skills_network",height="350px")),
                              column(6, h4("Heatmap"),  plotOutput("overall_skills_corrplot",height="350px"))
                            )
                   )
                 )
        ),
        
        tabPanel("Analiza e çdo dokumenti të ngarkuar",
                 tabsetPanel(
                   tabPanel("Seksionet e kërkuara", verbatimTextOutput("doc_required_text", placeholder = FALSE)),
                   tabPanel("Aftësitë e gjetura", plotOutput("doc_skills_plot"), DTOutput("doc_skills_table")),
                   tabPanel("Shpërndarja e temave (LDA në tokens)", htmlOutput("doc_ldavis"), plotOutput("doc_topic_plot")),
                   tabPanel("Përmbledhja e kategorive", tableOutput("doc_category_summary"), plotOutput("doc_wordcloud",height="400px")),
                   tabPanel("Korrelacioni i aftësive",
                            fluidRow(
                              column(6, h4("Network"),  plotOutput("doc_skills_network",height="350px")),
                              column(6, h4("Heatmap"),  plotOutput("doc_skills_corrplot",height="350px"))
                            )
                   )
                 )
        )
      )
    )
  ),
  
  tags$footer(
    class="app-footer",
    HTML("&copy; 2025 Milena Shehu &#8211; Licensed <a href='https://opensource.org/licenses/MIT' target='_blank'>MIT</a>")
  )
)

# =========================== Report builder ==================================
build_summary_html <- function(df_overall, df_doc, input, scope, req_overall_txt, req_doc_txt) {
  safe_tbl <- function(df, n=20) {
    if (is.null(df) || nrow(df)==0) return("<em>No data available.</em>")
    head_df <- df %>% arrange(desc(Beta)) %>% head(n) %>% mutate(Beta = sprintf("%.4f", as.numeric(Beta)))
    rows <- apply(head_df[, c("Forecasted_Skills","Beta","Matched_OECD_Skill","Skill_Category")], 1, function(r) {
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>", r[[1]], r[[2]], r[[3]] %||% "", r[[4]] %||% "")
    })
    paste0(
      '<table border="1" cellspacing="0" cellpadding="6" style="border-collapse:collapse;width:100%;">
        <thead style="background:#f3f4f6"><tr>
          <th>Forecasted Skills (phrases)</th><th>Beta</th><th>Matched Skill</th><th>Skill Category</th>
        </tr></thead><tbody>', paste(rows, collapse=""), '</tbody></table>'
    )
  }
  
  glue::glue('
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8"/>
<title>{BRAND_NAME} — Summary</title>
<style>
  body {{ font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Arial, sans-serif; margin:24px; }}
  h1,h2,h3 {{ margin-top: 1.2em; }}
  .meta {{ background:#f5faff; border:1px solid #cfe8ff; border-radius:8px; padding:12px; }}
  .mono {{ font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace; white-space: pre-wrap; }}
  @page {{ size: A4; margin: 16mm; }}
</style>
</head>
<body>
  <h1>{BRAND_NAME} — Skills Forecast Report</h1>
  <div class="meta">
    <p><strong>Scope:</strong> {scope}</p>
    <p><strong>Parameters:</strong> language = {input$language_model}, num_skills = {input$num_skills}, phrase-threshold = {input$confidence_threshold}</p>
  </div>

  <h2>Overall (All Documents) — Required Section Extract</h2>
  <div class="mono">{htmltools::htmlEscape(req_overall_txt)}</div>

  <h2>Overall — Top Skill Phrases</h2>
  {safe_tbl(df_overall, n=25)}

  <h2>Selected Document — Required Section Extract</h2>
  <div class="mono">{htmltools::htmlEscape(req_doc_txt)}</div>

  <h2>Selected Document — Top Skill Phrases</h2>
  {safe_tbl(df_doc, n=25)}

  <p style="margin-top:2em; color:#666;">Generated on {as.character(Sys.time())}</p>
</body></html>
  ')
}

# =============================== SERVER ======================================
server <- function(input, output, session) {
  
  # ---------- Language / UDPIPE selection ----------
  current_lang_code <- reactive({
    lbl <- input$language_model %||% "English"
    if (tolower(lbl) %in% c("albanian","shqip")) "sq" else "en"
  })
  
  current_ud_model <- reactive({
    code    <- current_lang_code()
    ud_lang <- if (code == "sq") "albanian" else "english"
    
    tryCatch(
      get_ud_model(ud_lang),
      error = function(e) {
        showNotification(paste("Error loading", ud_lang, "UDPIPE model:", e$message),
                         type = "error", duration = NULL)
        NULL
      }
    )
  })
  
  # ---------- Skills backend ----------
  skills_df <- reactive({ skills_df_static })
  
  skills_phr_tbl <- reactive({
    skills_phrase_table(skills_df(), lang_code = current_lang_code())
  })
  
  all_skill_categories <- reactive({
    sort(unique(skills_df()$oecd_category))
  })
  
  # ---------- Non-skill stoplist ----------
  output$dl_non_skill_template <- downloadHandler(
    filename = function() "non_skill_stoplist_template.csv",
    content  = function(file) {
      tmp <- tibble(
        term = c("tirana","albania","company","university","llc","ltd","ministry","bank","europe","usaid"),
        note = c("city","country","generic org","generic org","legal form","legal form","gov org","financial org","region","acronym")
      )
      write_csv(tmp, file)
    }
  )
  
  user_non_skill <- reactive({
    req(input$non_skill_csv)
    out <- tryCatch({
      df <- read_csv(input$non_skill_csv$datapath, show_col_types = FALSE)
      cand <- names(df)[tolower(names(df)) %in% c("term","word","token")]
      if (!length(cand)) return(character(0))
      terms <- tolower(trimws(df[[cand[1]]]))
      terms[nzchar(terms)]
    }, error = function(e) character(0))
    unique(out)
  })
  
  combined_non_skill <- reactive({
    if (is.null(input$non_skill_csv)) non_skill_stoplist_default()
    else unique(c(non_skill_stoplist_default(), user_non_skill()))
  })
  
  # ---------- Documents & Required Sections ----------
  observeEvent(input$input_file, {
    req(input$input_file)
    updateSelectInput(session,"file_select", choices = c("All", input$input_file$name), selected = "All")
  })
  
  docs_df <- eventReactive(input$submit_btn, {
    req(input$input_file)
    rx <- readtext::readtext(input$input_file$datapath)
    tibble(
      name = as.character(input$input_file$name),
      full_text = as.character(rx$text)
    ) %>%
      mutate(required_text = vapply(full_text, extract_required_section, FUN.VALUE = character(1)))
  })
  
  required_text_all <- reactive({
    req(docs_df())
    as.character(paste(docs_df()$required_text, collapse = "\n\n---\n\n"))
  })
  
  required_text_one <- reactive({
    req(docs_df(), input$file_select)
    out <- if (identical(input$file_select, "All")) docs_df()$required_text[1]
    else docs_df()$required_text[match(input$file_select, docs_df()$name)]
    as.character(out %||% "")
  })
  
  # ---------- Upload & Preview: table of files ----------
  upload_table_df <- reactive({
    req(input$input_file)
    rx <- readtext::readtext(input$input_file$datapath)
    
    tibble::tibble(
      name  = as.character(input$input_file$name),
      type  = tolower(tools::file_ext(name)),
      size_kb = round(as.numeric(input$input_file$size) / 1024, 1),
      full_text = as.character(rx$text)
    ) |>
      dplyr::mutate(
        required_text  = vapply(full_text, extract_required_section, FUN.VALUE = character(1)),
        words          = vapply(strsplit(full_text, "\\s+"), function(v) sum(nzchar(v)), integer(1)),
        required_chars = nchar(required_text, allowNA = TRUE, keepNA = FALSE)
      ) |>
      dplyr::select(name, type, size_kb, words, required_chars, required_text)
  })
  
  output$upload_table <- DT::renderDT({
    df <- upload_table_df()
    DT::datatable(
      df |> dplyr::select(name, type, size_kb, words, required_chars),
      rownames = FALSE,
      class   = "stripe hover order-column nowrap",
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 25, 50),
        order = list(list(0, 'asc')),
        autoWidth = TRUE,
        scrollX   = TRUE
      )
    )
  })
  
  output$combined_required_preview <- renderText({
    req(upload_table_df())
    paste(upload_table_df()$required_text, collapse = "\n\n---\n\n")
  })
  
  # ---------- JD Template DOCX ----------
  output$download_jd_template_docx <- downloadHandler(
    filename = function() paste0("JD_Template_EU_", Sys.Date(), ".docx"),
    content = function(file) {
      bd <- function(doc, txt, lvl = 1) officer::body_add_par(doc, txt, style = paste0("heading ", lvl))
      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, "Job Title", style = "heading 1")
      doc <- officer::body_add_par(doc, "Organization / Department", style = "Normal")
      doc <- bd(doc, "Required Qualifications / Competences / Experience", 1)
      doc <- officer::body_add_par(doc, "List only must-haves; align to ESCO/OECD where possible.", style = "Normal")
      print(doc, target = file)
    }
  )
  
  # ---------- Tokens / DTM ----------
  tokens_list <- reactive({
    req(docs_df())
    model <- current_ud_model()
    req(!is.null(model))
    
    extra     <- combined_non_skill()
    lang_code <- current_lang_code()
    
    lapply(
      docs_df()$required_text,
      tokenize_udpipe,
      extra_stop = extra,
      ud_model   = model,
      lang_code  = lang_code
    )
  })
  
  dtm_all <- reactive({ req(tokens_list()); build_dtm(tokens_list()) })
  
  # ---------- LDA fit (kept for LDAvis tab) ----------
  lda_fit <- reactive({
    req(dtm_all())
    k_input <- suppressWarnings(as.integer(input$topics_k %||% 0))
    k_auto  <- 5L
    k_raw   <- if (!is.na(k_input) && k_input > 0) k_input else k_auto
    k <- max(2, min(20, round(k_raw)))
    
    if (nrow(dtm_all()) < 1 || ncol(dtm_all()) < 2) return(NULL)
    topicmodels::LDA(dtm_all(), k = min(k, ncol(dtm_all())), control = list(seed = 123))
  })
  
  phi_theta <- reactive({
    fit <- lda_fit(); if (is.null(fit)) return(NULL)
    post <- topicmodels::posterior(fit)
    list(phi = post$terms, theta = post$topics)
  })
  
  # ===================== PHRASE-BASED "Found Skills" =========================
  make_found_skills_phrases <- function(scope = c("overall","doc"), doc_name = NULL, lang_code = "en") {
    scope <- match.arg(scope)
    
    tok <- tokens_list()
    req(length(tok) >= 1)
    
    # choose tokens subset for document scope
    tok_use <- tok
    if (scope == "doc") {
      req(doc_name, docs_df())
      idx <- match(doc_name, docs_df()$name)
      if (is.na(idx)) idx <- 1L
      tok_use <- list(tok[[idx]])
    }
    
    # candidate phrases
    cand <- candidate_phrases_from_tokens(
      tokens_list = tok_use,
      lang_code   = lang_code,
      ngram_n     = c(2L, 3L),
      min_count   = 1L,
      top_n       = max(200L, as.integer(input$num_skills) * 10L)
    )
    
    # keep top N for display (but matching on larger pool is better)
    # We'll match on cand pool, then return top N by Beta after matching.
    max_dist_use <- min(max(0.05, input$confidence_threshold), 0.60)
    
    matched <- match_phrases_to_skills(
      phr_df = cand,
      skills_tbl_phr = skills_phr_tbl(),
      max_dist_use = max_dist_use
    )
    
    # If user wants only N rows in UI, slice here:
    matched %>%
      arrange(desc(Beta)) %>%
      slice_head(n = as.integer(input$num_skills))
  }
  
  overall_df <- reactive({
    req(docs_df(), tokens_list(), skills_df())
    make_found_skills_phrases("overall", lang_code = current_lang_code())
  })
  
  doc_df <- reactive({
    req(docs_df(), tokens_list(), input$file_select, skills_df())
    nm <- if (identical(input$file_select, "All")) docs_df()$name[1] else input$file_select
    make_found_skills_phrases("doc", doc_name = nm, lang_code = current_lang_code())
  })
  
  # ---------- Similarity (for network/heatmap) ----------
  make_similarity <- function(labels, stems, threshold) {
    clean_df <- tibble(label = labels, stem = stems) %>%
      filter(!is.na(stem) & !is.na(label)) %>%
      distinct(stem, .keep_all = TRUE)
    if (nrow(clean_df) < 2) {
      return(list(sim = matrix(1, nrow = 1, ncol = 1,
                               dimnames = list(clean_df$label, clean_df$label)),
                  adj = matrix(FALSE, nrow = 1, ncol = 1)))
    }
    raw_dist <- stringdist::stringdistmatrix(clean_df$stem, clean_df$stem, method = "jw")
    sim_mat  <- 1 - as.matrix(raw_dist)
    rownames(sim_mat) <- clean_df$label
    colnames(sim_mat) <- clean_df$label
    adj <- sim_mat >= (1 - threshold); diag(adj) <- FALSE
    list(sim = sim_mat, adj = adj)
  }
  
  # =================== OVERALL TAB OUTPUTS ===================================
  output$overall_required_text <- renderText(as.character(required_text_all()))
  
  output$overall_skills_table <- renderDT({
    df <- overall_df()
    datatable(df %>% arrange(desc(Beta)), options = list(pageLength=10), rownames = FALSE)
  })
  
  output$overall_skills_plot  <- renderPlot({
    df <- overall_df(); req(nrow(df)>0)
    
    # keep all categories in legend (optional)
    df$Skill_Category <- factor(df$Skill_Category, levels = c(all_skill_categories(), "Unmatched"))
    
    ggplot(df, aes(reorder(Forecasted_Skills, Beta), Beta, fill = Skill_Category)) +
      geom_col() + coord_flip() + theme_minimal() +
      scale_fill_discrete(drop = FALSE) +
      labs(title="Overall: Top Skill Phrases (bigrams/trigrams) — Required section only", x=NULL, y="Beta")
  })
  
  output$overall_ldavis <- renderUI({
    pt <- phi_theta()
    if (is.null(pt)) return(HTML("<p><strong>Not enough content to build topics.</strong></p>"))
    phi <- pt$phi; theta <- pt$theta
    m   <- as.matrix(dtm_all())
    json <- LDAvis::createJSON(phi, theta, colnames(phi),
                               doc.length = rowSums(m), term.frequency = colSums(m))
    folder_name    <- paste0("ldavis_", digest::digest(Sys.time()))
    target_www_dir <- file.path("www", folder_name); dir.create(target_www_dir, showWarnings = FALSE, recursive = TRUE)
    LDAvis::serVis(json, out.dir = target_www_dir, open.browser = FALSE)
    tags$a(href = paste0(folder_name, "/index.html"), target = "_blank", "Open interactive topic visualization")
  })
  
  output$overall_topic_plot <- renderPlot({
    # Keep a simple LDA top-terms plot for reference (tokens-based)
    fit <- lda_fit()
    if (is.null(fit)) { plot.new(); text(0.5, 0.5, "No topics to display.\nAdjust inputs.", cex = 1.2); return() }
    df <- global_top_terms_from_lda(fit, n_top = as.integer(input$num_skills))
    ggplot(df, aes(x = reorder(term, Beta), y = Beta)) +
      geom_col() + coord_flip() + theme_minimal() +
      scale_fill_discrete(drop = FALSE) +
      labs(x="Token (stem)", y="Probability (β)", title="Overall: LDA top terms (tokens) — reference")
  })
  
  output$overall_category_summary <- renderTable({
    overall_df() %>% count(Skill_Category, name="Count") %>% arrange(desc(Count))
  })
  
  output$overall_wordcloud <- renderPlot({
    df <- overall_df(); req(nrow(df)>0)
    wordcloud(df$Forecasted_Skills, df$Beta, scale=c(5,0.7),
              colors=brewer.pal(8,"Dark2"), random.order=FALSE)
  })
  
  output$overall_skills_network <- renderPlot({
    df <- overall_df()
    if (nrow(df) < 2 || !"Beta" %in% names(df)) {
      plot.new(); text(0.5, 0.5, "Need ≥ 2 skills to build network", cex=1.2); return()
    }
    tryCatch({
      sims <- make_similarity(df$Forecasted_Skills, df$term_stem, input$confidence_threshold)
      g <- igraph::graph_from_adjacency_matrix(sims$adj, mode = "undirected", diag = FALSE)
      node_labels <- rownames(sims$sim)
      df_filtered <- dplyr::filter(df, Forecasted_Skills %in% node_labels)
      sizes <- setNames(df_filtered$Beta, df_filtered$Forecasted_Skills)
      sizes <- sizes[V(g)$name]
      V(g)$label <- V(g)$name
      V(g)$size  <- 5 + 10 * (sizes / max(sizes, na.rm = TRUE))
      plot(g, layout = layout_with_fr(g), vertex.label.cex=0.8,
           vertex.label.color="black", edge.color="gray70",
           main=paste0("Overall: network (sim ≥ ", round(1 - input$confidence_threshold, 2), ")"))
    }, error = function(e) {
      plot.new(); text(0.5, 0.5, paste("Network Error:\n", e$message), cex = 1.1, col = "red")
    })
  })
  
  output$overall_skills_corrplot <- renderPlot({
    df <- overall_df()
    if (nrow(df) < 2) { plot.new(); text(0.5, 0.5, "Need ≥ 2 skills for heatmap", cex=1.2); return() }
    tryCatch({
      sims <- make_similarity(df$Forecasted_Skills, df$term_stem, input$confidence_threshold)
      corrplot::corrplot(sims$sim, method="color", type="upper",
                         tl.col="black", tl.cex=0.6, tl.srt=45,
                         main="Overall: Skill Similarity (1−JW dist)",
                         mar=c(0,0,1,0))
    }, error = function(e) {
      plot.new(); text(0.5, 0.5, paste("Heatmap Error:\n", e$message), cex=1.1, col="red")
    })
  })
  
  # =================== PER-DOC TAB OUTPUTS ===================================
  output$doc_required_text <- renderText(as.character(required_text_one()))
  
  output$doc_skills_table <- renderDT({
    df <- doc_df()
    datatable(df %>% arrange(desc(Beta)), options = list(pageLength=10), rownames = FALSE)
  })
  
  output$doc_skills_plot  <- renderPlot({
    df <- doc_df(); req(nrow(df)>0)
    df$Skill_Category <- factor(df$Skill_Category, levels = c(all_skill_categories(), "Unmatched"))
    ggplot(df, aes(reorder(Forecasted_Skills, Beta), Beta, fill=Skill_Category))+
      geom_col()+coord_flip()+theme_minimal()+
      scale_fill_discrete(drop = FALSE) +
      labs(title=paste("Skill phrases (bigrams/trigrams):", input$file_select), x=NULL, y="Beta")
  })
  
  output$doc_ldavis <- renderUI({
    pt <- phi_theta()
    if (is.null(pt)) return(HTML("<p><strong>Not enough content to build topics for this document.</strong></p>"))
    phi <- pt$phi; theta <- pt$theta
    m   <- as.matrix(dtm_all())
    json <- LDAvis::createJSON(phi, theta, colnames(phi),
                               doc.length = rowSums(m), term.frequency = colSums(m))
    folder_name    <- paste0("ldavis_doc_", digest::digest(Sys.time()))
    target_www_dir <- file.path("www", folder_name); dir.create(target_www_dir, showWarnings = FALSE, recursive = TRUE)
    LDAvis::serVis(json, out.dir = target_www_dir, open.browser = FALSE)
    tags$a(href = paste0(folder_name, "/index.html"), target = "_blank", "Open interactive LDAvis (document)")
  })
  
  output$doc_topic_plot <- renderPlot({
    fit <- lda_fit()
    if (is.null(fit)) { plot.new(); text(0.5, 0.5, "No topics to display.\nAdjust inputs.", cex = 1.2); return() }
    nm <- if (identical(input$file_select, "All")) docs_df()$name[1] else input$file_select
    idx <- match(nm, docs_df()$name); if (is.na(idx)) idx <- 1L
    df <- doc_top_terms_from_lda(fit, doc_index = idx, n_top = as.integer(input$num_skills))
    ggplot(df, aes(x = reorder(term, Beta), y = Beta)) +
      geom_col() + coord_flip() + theme_minimal() +
      scale_fill_discrete(drop = FALSE) +
      labs(x="Token (stem)", y="Probability (β)", title=paste("Doc: LDA top terms (tokens) — reference:", input$file_select))
  })
  
  output$doc_category_summary <- renderTable({
    doc_df() %>% count(Skill_Category, name="Count") %>% arrange(desc(Count))
  })
  
  output$doc_wordcloud <- renderPlot({
    df <- doc_df(); req(nrow(df)>0)
    wordcloud(df$Forecasted_Skills, df$Beta, scale=c(5,0.7),
              colors=brewer.pal(8,"Dark2"), random.order=FALSE)
  })
  
  output$doc_skills_network <- renderPlot({
    df <- doc_df()
    if (nrow(df) < 2 || !"Beta" %in% names(df)) {
      plot.new(); text(0.5, 0.5, "Need ≥ 2 skills to build network", cex=1.2); return()
    }
    tryCatch({
      sims <- make_similarity(df$Forecasted_Skills, df$term_stem, input$confidence_threshold)
      g <- igraph::graph_from_adjacency_matrix(sims$adj, mode = "undirected", diag = FALSE)
      node_labels <- rownames(sims$sim)
      df_filtered <- dplyr::filter(df, Forecasted_Skills %in% node_labels)
      sizes <- setNames(df_filtered$Beta, df_filtered$Forecasted_Skills)
      sizes <- sizes[V(g)$name]
      V(g)$label <- V(g)$name
      V(g)$size  <- 5 + 10 * (sizes / max(sizes, na.rm = TRUE))
      plot(g, layout = layout_with_fr(g), vertex.label.cex=0.8,
           vertex.label.color="black", edge.color="gray70",
           main=paste0("Doc network (sim ≥ ", round(1 - input$confidence_threshold, 2), ")"))
    }, error = function(e) {
      plot.new(); text(0.5, 0.5, paste("Network Error:\n", e$message), cex = 1.1, col = "red")
    })
  })
  
  output$doc_skills_corrplot <- renderPlot({
    df <- doc_df()
    if (nrow(df) < 2) { plot.new(); text(0.5, 0.5, "Need ≥ 2 skills for heatmap", cex=1.2); return() }
    tryCatch({
      sims <- make_similarity(df$Forecasted_Skills, df$term_stem, input$confidence_threshold)
      corrplot::corrplot(sims$sim, method="color", type="upper",
                         tl.col="black", tl.cex=0.6, tl.srt=45,
                         main="Per-Document: Skill Similarity (1−JW dist)",
                         mar=c(0,0,1,0))
    }, error = function(e) {
      plot.new(); text(0.5, 0.5, paste("Heatmap Error:\n", e$message), cex=1.1, col="red")
    })
  })
  
  # ---------- Downloads ----------
  output$download_results <- downloadHandler(
    filename=function() paste0("forecasted_skill_phrases_overall_", Sys.Date(), ".csv"),
    content=function(file) write.csv(overall_df(), file, row.names=FALSE)
  )
  
  output$download_summary_pdf <- downloadHandler(
    filename = function() paste0("skills_forecast_single_org_", backend_short_static, "_", Sys.Date(), ".pdf"),
    content  = function(file) {
      scope <- if (input$file_select == "All") "Overall (All documents)" else paste("Document:", input$file_select)
      html  <- build_summary_html(overall_df(), doc_df(), input, scope, required_text_all(), required_text_one())
      tmp_html <- tempfile(fileext = ".html")
      writeLines(as.character(html), tmp_html, useBytes = TRUE)
      tryCatch(
        pagedown::chrome_print(input = tmp_html, output = file),
        error = function(e) {
          fallback <- sub("\\.pdf$", ".html", file, ignore.case = TRUE)
          file.copy(tmp_html, fallback, overwrite = TRUE)
          stop("PDF rendering failed (Chromium missing). Saved HTML instead: ", basename(fallback))
        }
      )
    }
  )
}

shinyApp(ui, server)
