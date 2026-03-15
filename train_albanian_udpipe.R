# ---------------- One-time training of an Albanian UDPipe model ----------------
# Run this script from the folder where your app.R lives.

library(udpipe)

# 1. Make sure we are in the app directory
# setwd("C:/path/to/your/app/folder")   # <- adjust once if needed

# 2. Ensure a 'models' folder exists (this is where your app already looks)
dir.create("models", showWarnings = FALSE)

# 3. Download the UD_Albanian-STAF treebank (train + dev)
base_url <- "https://raw.githubusercontent.com/UniversalDependencies/UD_Albanian-STAF/master"

train_conllu <- file.path("models", "sq_staf-ud-train.conllu")
dev_conllu   <- file.path("models", "sq_staf-ud-dev.conllu")

download.file(paste0(base_url, "/sq_staf-ud-train.conllu"),
              destfile = train_conllu, mode = "wb")
download.file(paste0(base_url, "/sq_staf-ud-dev.conllu"),
              destfile = dev_conllu, mode = "wb")

# (Optional) you can also download the test file just to have it locally
# test_conllu <- file.path("models", "sq_staf-ud-test.conllu")
# download.file(paste0(base_url, "/sq_staf-ud-test.conllu"),
#               destfile = test_conllu, mode = "wb")

# 4. Train the UDPipe model
#    This can take a while – it uses default hyperparameters.
files_training <- c(train_conllu, dev_conllu)

# Run this from the app folder
setwd("C:/Users/HP/Desktop/Doktoratura/Topic Modelling per Skills_BERT Algorithm/Finalet/Skills_Forecast_app/skills")

library(udpipe)

dir.create("models", showWarnings = FALSE)

model_path <- file.path("models", "albanian-staf-ud-2.15.udpipe")

m <- udpipe_train(
  file = model_path,
  files_conllu_training = files_training,
  annotation_tokenizer = "default",
  annotation_tagger    = "default",
  annotation_parser    = "default"
)

cat("Model trained and saved to:", m$file_model, "\n")
