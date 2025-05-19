# Creates embeddings via openai's API

##### PRELIMINARIES #####

library(magrittr)
library(openai)

OPENAI_API_KEY= "<KEY>"

##### GET EMBEDDINGS FOR GPT 3.5 ##### 

essays <- tar_read(ncds_essays)

group_index <- ceiling(seq_along(1:nrow(essays))/100)

# Splitting the dataframe into groups of 100 rows
split_df <- split(essays, group_index)

embeddings_raw <- split_df %>%
  purrr::map(function(x){
    create_embedding(
      model = "text-embedding-ada-002",
      input = x$text,
      openai_api_key = OPENAI_API_KEY
    )
    
  })

saveRDS(embeddings_raw, "data/embeddings_gpt35_raw.rds")

##### GET EMBEDDINGS FOR GPT 4 ##### 

essays <- tar_read(ncds_essays)

group_index <- ceiling(seq_along(1:nrow(essays))/100)

# Splitting the dataframe into groups of 100 rows
split_df <- split(essays, group_index)

embeddings_raw_gpt_4 <- split_df %>%
  purrr::map(function(x){
    create_embedding(
      model = "text-embedding-3-large",
      input = x$text,
      openai_api_key = OPENAI_API_KEY
    )
    
  })

saveRDS(embeddings_raw_gpt_4, "data/embeddings_gpt4_raw.rds")


