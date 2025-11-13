#
# Chatbot competition data
#
# Citation/data source:
# https://huggingface.co/datasets/lmarena-ai/arena-human-preference-140k
#
# FILES: Multiple parquet data files (see package arrow) and a README.md file
#

# Reading in one as an example, reducing to key variables, though we
# won't use the category_tag (initially, TBD).

library(arrow)
x <- read_parquet('train-00000-of-00007.parquet')

x <- as.data.frame(x[, c("id", "model_a", "model_b",
                         "winner", "category_tag")])
dim(x)
head(x[, 1:4])

saveRDS(x, file = 'chatbot.data.rds')

################################################################################
# Check the save with a read because, why not!?

rm(list = ls())
x <- readRDS(file = 'chatbot.data.rds')

dim(x)
head(x[, 1:4])


################################################################################
# Goal: Evaluate chatbot strength (or relative strength) based on these
# results, but we want more than just an ad hoc leaderboard.  For a
# yet-to-be-played competition between any two bots, predict outcome
# probabilities.  Ultimately, there could be additional nuance based on the
# competition categorization, but that should not be part of your initial work.
# 
# Suggestion: Start with a simple model that ignores ties and the so-called
# "both_bad" outcomes.  Perhaps draw from models commonly used in sports
# analyses (this would be like a sport where only win/loss outcomes are
# observed, without any margin of victory).
#