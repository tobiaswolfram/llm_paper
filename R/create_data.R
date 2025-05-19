library(magrittr)
library(targets)
library(ggplot2)
library(latex2exp)
library(ggpubr)
library(SuperLearner)
library(data.table)
library(ggsci)


mapping <- tibble::tribble(
  ~variable,                             ~category,                  ~name,
  "s2_co_factor_ability",                "cognitive_abilities", "General Factor of Cognitive Ability",
  "s2_co_reading_ability",               "cognitive_abilities", "Reading Ability",
  "s2_co_verbal_ability",                "cognitive_abilities", "Verbal Ability",
  "s2_co_nonverbal_ability",             "cognitive_abilities", "Nonverbal Ability",
  "s2_co_mathematics_ability",           "cognitive_abilities", "Mathematical Ability",
  "s3_co_reading_ability",             "cognitive_abilities", "Reading Ability",
  "s3_co_mathematics_ability",           "cognitive_abilities", "Mathematical Ability",
  "s3_co_factor_scholastic_motivation",  "motivation_aspiration", "Scholastic Motivation",
  "s2_co_aspiration_camsis",             "motivation_aspiration",  "Occupational Aspirations",
  "s5_co_highest_edu",                   "life_outcomes",       "Highest Education",
  "s3_te_factor_externalizing",          "personality", "Externalizing Behavior",
  "s3_te_factor_internalizing",          "personality", "Internalizing Behavior",
  "s3_pa_edu",          "confounder", "Parental SES",
  "s3_co_height",          "confounder", "Height",
  "s0_co_male",          "confounder", "Sex",
  "s0_mo_birthweight",          "confounder", "Birthweight"
  
) %>% 
  dplyr::mutate(category_name = dplyr::case_when(category == "cognitive_abilities" ~ "Cognitive Abilities",
                                                 category == "motivation_aspiration" ~ "Non-cognitive Traits",
                                                 category == "personality" ~ "Non-cognitive Traits",
                                                 category == "life_outcomes" ~ "Life Outcomes"),
                name = dplyr::case_when(grepl("^s2", variable) ~ paste(name, "(Age 11)"),
                                        grepl("^s3", variable) ~ paste(name, "(Age 16)"),
                                        grepl("^s4", variable) ~ paste(name, "(Age 23)"),
                                        grepl("^s5", variable) ~ paste(name, "(Age 33)"),
                                        grepl("^s6", variable) ~ paste(name, "(Age 42)"),
                                        grepl("^s7", variable) ~ paste(name, "(Age 46)"),
                                        grepl("^s8", variable) ~ paste(name, "(Age 50)"),
                                        grepl("^s9", variable) ~ paste(name, "(Age 55)"),
                                        TRUE ~ name
                ))

essay_full_metrics <-tar_read(essay_superlearner_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Prediction based on ~250 Word Essay") 
genes_full_metrics <-tar_read(gene_superlearner_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Prediction based on Combination of various Polygenic Scores") 
teacher_full_metrics <-tar_read(teacher_superlearner_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Prediction based on Teacher Evaluation") 
teacher_genes_essay_full_metrics <-tar_read(teacher_genes_essay_superlearner_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 



predictions <-dplyr::bind_rows(
  essay_full_metrics, 
  genes_full_metrics,
  teacher_full_metrics
) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 15),
                type = stringr::str_wrap(.$type, width = 50)) %>%
  dplyr::filter(category != "Life Outcomes" & name != "General Factor\nof Cognitive\nAbility (Age\n11)") %>%
  dplyr::mutate(type = stringr::str_wrap(type, 20))



essay_metrics <-tar_read(essay_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay") 
genes_metrics <-tar_read(gene_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores") 
teacher_metrics <-tar_read(teacher_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Teacher Evaluation") 
essay_genes_metrics <-tar_read(essay_genes_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & ~250 Word Essay") 
essay_teacher_metrics <-tar_read(essay_teacher_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay & Teacher Evaluation") 
teacher_genes_metrics <-tar_read(teacher_genes_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & Teacher Evaluation") 
teacher_genes_essay_metrics <-tar_read(teacher_genes_essay_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 



cog_full_metrics <- get_cv_superlearner_metrics(cog_superlearner_social_lm)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Cognitive Abilities") 
noncog_full_metrics <- tar_read(noncog_superlearner_social_lm_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Non-cognitive Traits") 
birthweight_full_metrics <-tar_read(birthweight_superlearner_social_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Birthweight") 
height_full_metrics <-tar_read(height_superlearner_social_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Height") 
pedu_full_metrics <-tar_read(pedu_superlearner_social_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Parental Education") 
teacher_genes_essay_full_metrics <- teacher_genes_essay_metrics %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 
sociological_model_full_metrics <-  tar_read(sociological_superlearner_metrics) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(type = "Sociological Baseline Model")
sociological_model_lm_full_metrics <-  tar_read(sociological_superlearner_social_lm_metrics) %>% 
  dplyr::bind_rows() %>% 
  dplyr::mutate(type = "Sociological Baseline Model")


plot_3_data <-dplyr::bind_rows(
  cog_full_metrics,
  noncog_full_metrics,
  birthweight_full_metrics,
  height_full_metrics,
  sociological_model_lm_full_metrics,
  teacher_genes_essay_full_metrics
) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 20),
                type = stringr::str_wrap(.$type, width = 20)) %>%
  dplyr::filter(category == "Life Outcomes") %>%
  dplyr::mutate(naming = "Educational Attainment")

essay_metrics_cog <- tar_read(essay_superlearner_cog_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay") 
genes_metrics_cog <- tar_read(gene_superlearner_cog_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores") 
teacher_metrics_cog <- tar_read(teacher_superlearner_cog_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Teacher Evaluation") 
essay_genes_metrics_cog <- tar_read(essay_genes_superlearner_cog_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & ~250 Word Essay") 
essay_teacher_metrics_cog <- tar_read(essay_teacher_superlearner_cog_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay & Teacher Evaluation") 
teacher_genes_metrics_cog <- tar_read(teacher_genes_superlearner_cog_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & Teacher Evaluation") 
teacher_genes_essay_metrics_cog <- tar_read(teacher_genes_essay_superlearner_cog_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 




text_length_lm <- tar_read(text_length)  %>% purrr::map_dfr(get_cv_superlearner_metrics) %>% dplyr::mutate(type = "Number of Words")
salat_metrics_superlearner <- tar_read(salat_metrics_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics) %>% dplyr::mutate(type = "Linguistic Metrics of Lexical Diversity, Sophistication and Sentiment")
readability_metrics_superlearner  <- tar_read(readability_metrics_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics)  %>% dplyr::mutate(type = "Simple Indices of Readability")
spelling_errors_superlearner  <- tar_read(spelling_errors_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics)  %>% dplyr::mutate(type = "Grammatical and Typographical Errors")
all_except_gpt <- tar_read(spelling_salat_readability_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics) %>% dplyr::mutate(type = "All Textual Information (except Embeddings)")
gpt_embeddings_superlearner <- tar_read(gpt_embeddings_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics)  %>% dplyr::mutate(type = "GPT 3.5-based Embeddings")
roberta_embeddings_superlearner <- tar_read(roberta_embeddings_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics)  %>% dplyr::mutate(type = "RoBERTa-based Embeddings")
gpt4_embeddings_superlearner <- tar_read(gpt4_embeddings_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics)  %>% dplyr::mutate(type = "GPT 4-based Embeddings")


appendix_11_data <- roberta_embeddings_superlearner %>% 
  dplyr::bind_rows(gpt_embeddings_superlearner) %>%
  dplyr::bind_rows(gpt4_embeddings_superlearner) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) 
dplyr::mutate(name = stringr::str_wrap(.$name, width = 20)) %>%
  dplyr::mutate(type = factor(type, levels = c("RoBERTa-based Embeddings", "GPT 3.5-based Embeddings", "GPT 4-based Embeddings"), 
                              labels = c("RoBERTa", "GPT 3.5", "GPT 4"), ordered = TRUE))

combination <- tar_read(essay_superlearner) %>% purrr::map_dfr(get_cv_superlearner_metrics) %>% dplyr::mutate(type = "All Textual Information")


all_text <- salat_metrics_superlearner %>% 
  dplyr::bind_rows(readability_metrics_superlearner) %>% 
  dplyr::bind_rows(spelling_errors_superlearner) %>%
  dplyr::bind_rows(all_except_gpt) %>%
  dplyr::bind_rows(gpt_embeddings_superlearner) %>%
  dplyr::bind_rows(combination) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::left_join(text_length_lm %>% dplyr::select(var, lm_performance = mean_r2)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 20),
                type = stringr::str_wrap(.$type, width = 20),
                relative_performance = mean_r2/lm_performance)






plot_1_data <- dplyr::bind_rows(
  essay_full_metrics, 
  genes_full_metrics,
  teacher_full_metrics
) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 15),
                type = stringr::str_wrap(.$type, width = 50)) %>%
  dplyr::filter(category != "Life Outcomes" & name != "General Factor\nof Cognitive\nAbility (Age\n11)") %>%
  dplyr::mutate(type = stringr::str_wrap(type, 20))


plot_2_data <- essay_metrics %>% 
  dplyr::mutate(type = "~250 Word Essay") %>%
  dplyr::bind_rows(teacher_metrics %>% 
                     dplyr::mutate(type = "Teacher Evaluation")) %>%
  dplyr::bind_rows(genes_metrics %>% 
                     dplyr::mutate(type = "Polygenic Scores")) %>% 
  dplyr::bind_rows(essay_genes_metrics) %>% 
  dplyr::bind_rows(essay_teacher_metrics) %>%
  dplyr::bind_rows(teacher_genes_metrics) %>% 
  dplyr::bind_rows(teacher_genes_essay_metrics)  %>% 
  dplyr::bind_rows(essay_metrics_cog %>% 
                     dplyr::mutate(type = "~250 Word Essay")) %>%
  dplyr::bind_rows(teacher_metrics_cog %>% 
                     dplyr::mutate(type = "Teacher Evaluation")) %>%
  dplyr::bind_rows(genes_metrics_cog %>% 
                     dplyr::mutate(type = "Polygenic Scores")) %>% 
  dplyr::bind_rows(essay_genes_metrics_cog) %>% 
  dplyr::bind_rows(essay_teacher_metrics_cog) %>%
  dplyr::bind_rows(teacher_genes_metrics_cog) %>% 
  dplyr::bind_rows(teacher_genes_essay_metrics_cog) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 20),
                type = stringr::str_wrap(.$type, width = 20)) 


plot_4_data <- all_text %>% 
  dplyr::filter(!name %in% c("Highest Education\n(Age 33)", "General Factor of\nCognitive Ability\n(Age 11)")) 

appendix_1_data <- targets::tar_read(ncds_essays) %>%
  dplyr::mutate(words = as.numeric(words)) 

appendix_2_data <- tar_read(ncds_1_to_9) %>%
  dplyr::select(ncdsid, 
                'General Knowledge' = n876, 
                'Number Work' = n877, 
                'Use of Books' = n878, 
                'Oral Ability' = n879) %>%
  dplyr::mutate_if(is.numeric, function(x) haven::as_factor(x)) %>%
  tidyr::pivot_longer(cols = colnames(.)[-1]) %>%
  dplyr::group_by(name) %>%
  dplyr::count(value) %>%
  dplyr::mutate(n = n/sum(n)) %>%
  na.omit()

appendix_3_data <- tar_read(ncds_1_to_9) %>%
  dplyr::select(ncdsid, 
                'Poor Hand Control' = n880,
                'Squirmy, Fidgety' = n881, 
                'Poor Physical Coordination' = n882, 
                'Hardly Ever Still' = n883, 
                'Speech Difficulties' = n884, 
                'Imperfect Grasp of English' = n885) %>%
  dplyr::mutate_if(is.numeric, function(x) haven::as_factor(x)) %>%
  tidyr::pivot_longer(cols = colnames(.)[-1]) %>%
  dplyr::group_by(name) %>%
  dplyr::count(value) %>%
  dplyr::mutate(n = n/sum(n)) %>%
  na.omit() 


appendix_4_data <- tar_read(ncds_1_to_9) %>%
  dplyr::select(ncdsid, 
                'Inconsequential Behavior' = n1001,
                'Nervous Symptoms' = n1005,
                'Anxiety for Acceptance by Adults' = n983,
                'Anxiety for Acceptance by Children' = n992,
                'Hostility towards Children' = n995,
                'Writing off of Adults and Adult Standards' = n989,
                'Hostility towards Adults' = n986,
                'Miscellaneous Symptoms' = n1004,
                'Restlessness' = n998,
                'Unforthcomingness' = n974,
                'Depression' = n980,
                'Withdrawal' = n977) %>%
  dplyr::mutate_if(is.numeric, function(x) as.numeric(x)) %>%
  tidyr::pivot_longer(cols = colnames(.)[-1])

appendix_5_data <- tar_read(ncds_1_to_9) %>% 
  dplyr::mutate(aspired_job = haven::as_factor(n2771)) %>%     
  dplyr::select(aspired_job) %>% 
  na.omit() %>%
  dplyr::filter(!aspired_job %in% c(47, 20.5)) %>%
  dplyr::group_by(aspired_job) %>% 
  dplyr::count() %>% 
  dplyr::arrange(dplyr::desc(n))


appendix_6_data <- dplyr::bind_rows(
  essay_full_metrics %>% dplyr::mutate(method = "SuperLearner"), 
  genes_full_metrics %>% dplyr::mutate(method = "SuperLearner"),
  teacher_full_metrics %>% dplyr::mutate(method = "SuperLearner"),
  essay_full_metrics_lm, 
  genes_full_metrics_lm,
  teacher_full_metrics_lm,
) %>% 
  dplyr::select(mean_r2, var, type, method) %>%
  dplyr::filter(var != "s2_co_factor_ability") %>%
  tidyr::pivot_wider(names_from = "method", values_from = "mean_r2") %>%
  dplyr::mutate(diff = SuperLearner - pmax(`Linear Model`, 0)) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 15),
                type = stringr::str_wrap(.$type, width = 50),
                category = factor(category, levels = c("Cognitive Abilities", "Non-cognitive Traits", "Life Outcomes"), ordered = TRUE)) %>% 
  dplyr::filter(category != "Life Outcomes")


essay_metrics <- tar_read(essay_superlearner_bfi_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Prediction based on ~250 Word Essay") 
genes_metrics <- tar_read(gene_superlearner_bfi_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Prediction based on Combination\nof various Polygenic Scores") 
teacher_metrics <- tar_read(teacher_superlearner_bfi_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Prediction based on Teacher Evaluation") 
teacher_genes_essay_metrics <- tar_read(teacher_genes_essay_superlearner_bfi_metrics)    %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 


appendix_7_data <- dplyr::bind_rows(
  essay_metrics, 
  genes_metrics,
  teacher_metrics
) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 30),
                type = stringr::str_wrap(.$type, width = 50),
                sample = "Complete Information on all Variables")  %>% 
  dplyr::mutate(name = paste0("Big 5: ", stringr::str_to_title(gsub("s8_co_", "", var)), "\n (Age 50)"))

cog_overlap_metrics <- get_cv_superlearner_metrics(tar_read(cog_superlearner_social_lm_overlap)[[1]])  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Cognitive Abilities") 
noncog_overlap_metrics <- get_cv_superlearner_metrics(tar_read(noncog_superlearner_social_lm_overlap)[[1]])  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Non-cognitive Traits") 
birthweight_overlap_metrics <-tar_read(birthweight_superlearner_social_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Birthweight") 
height_overlap_metrics <-tar_read(height_superlearner_social_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Height") 
pedu_overlap_metrics <-tar_read(pedu_superlearner_social_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Parental Education") 
teacher_genes_essay_overlap_metrics <- teacher_genes_essay_overlap_metrics %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 





cog_full_metrics <- get_cv_superlearner_metrics(cog_superlearner_social_lm)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Cognitive Abilities") 
noncog_full_metrics <- tar_read(noncog_superlearner_social_lm_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Non-cognitive Traits") 
birthweight_full_metrics <-tar_read(birthweight_superlearner_social_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Birthweight") 
height_full_metrics <-tar_read(height_superlearner_social_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Height") 
pedu_full_metrics <-tar_read(pedu_superlearner_social_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Parental Education") 
teacher_genes_essay_full_metrics <- teacher_genes_essay_metrics %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 




predictions_full <- dplyr::bind_rows(
  cog_full_metrics,
  noncog_full_metrics,
  pedu_full_metrics,
  birthweight_full_metrics,
  height_full_metrics,
  teacher_genes_essay_full_metrics
) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 30),
                type = stringr::str_wrap(.$type, width = 30),
                sample = "Maximum Observations") %>%
  dplyr::filter(category == "Life Outcomes")


predictions <- dplyr::bind_rows(
  cog_overlap_metrics,
  noncog_overlap_metrics,
  pedu_overlap_metrics,
  birthweight_overlap_metrics,
  height_overlap_metrics,
  teacher_genes_essay_overlap_metrics
) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 30),
                type = stringr::str_wrap(.$type, width = 30),
                sample = "Complete Information on all Variables") %>%
  dplyr::filter(category == "Life Outcomes")


appendix_8_data <- predictions %>%
  dplyr::bind_rows(predictions_full) 

essay_metrics <- tar_read(essay_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay") 
genes_metrics <- tar_read(gene_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores") 
teacher_metrics <- tar_read(teacher_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Teacher Evaluation") 
essay_genes_metrics <- tar_read(essay_genes_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & ~250 Word Essay") 
essay_teacher_metrics <- tar_read(essay_teacher_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay & Teacher Evaluation") 
teacher_genes_metrics <- tar_read(teacher_genes_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & Teacher Evaluation") 
teacher_genes_essay_metrics <- tar_read(teacher_genes_essay_superlearner_mmg_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 

essay_overlap_metrics <- tar_read(essay_superlearner_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay") 
genes_overlap_metrics <- tar_read(gene_superlearner_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores") 
teacher_overlap_metrics <- tar_read(teacher_superlearner_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Teacher Evaluation") 
essay_genes_overlap_metrics <- tar_read(essay_genes_superlearner_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & ~250 Word Essay") 
essay_teacher_overlap_metrics <- tar_read(essay_teacher_superlearner_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "~250 Word Essay & Teacher Evaluation") 
teacher_genes_overlap_metrics <- tar_read(teacher_genes_superlearner_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores & Teacher Evaluation") 
teacher_genes_essay_overlap_metrics <- tar_read(teacher_genes_essay_superlearner_overlap_metrics)  %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation") 

metrics <- essay_metrics %>% 
  dplyr::mutate(type = "~250 Word Essay") %>%
  dplyr::bind_rows(teacher_metrics %>% 
                     dplyr::mutate(type = "Teacher Evaluation")) %>%
  dplyr::bind_rows(genes_metrics %>% 
                     dplyr::mutate(type = "Polygenic Scores")) %>% 
  dplyr::bind_rows(essay_genes_metrics) %>% 
  dplyr::bind_rows(essay_teacher_metrics) %>%
  dplyr::bind_rows(teacher_genes_metrics) %>% 
  dplyr::bind_rows(teacher_genes_essay_metrics) %>% 
  dplyr::mutate(sample =  "Maximum Observations")

overlap_metrics <- essay_overlap_metrics %>% 
  dplyr::mutate(type = "~250 Word Essay") %>%
  dplyr::bind_rows(teacher_overlap_metrics %>% 
                     dplyr::mutate(type = "Teacher Evaluation")) %>%
  dplyr::bind_rows(genes_overlap_metrics %>% 
                     dplyr::mutate(type = "Polygenic Scores")) %>% 
  dplyr::bind_rows(essay_genes_overlap_metrics) %>% 
  dplyr::bind_rows(essay_teacher_overlap_metrics) %>%
  dplyr::bind_rows(teacher_genes_overlap_metrics) %>% 
  dplyr::bind_rows(teacher_genes_essay_overlap_metrics) %>% 
  dplyr::mutate(sample = "Complete Information on all Variables")

appendix_9_data <- metrics %>%
  dplyr::bind_rows(overlap_metrics) %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 20),
                type = stringr::str_wrap(.$type, width = 20)) %>%
  dplyr::filter(category == "Life Outcomes")

appendix_12_data <- genes_full_metrics %>%
  dplyr::left_join(dplyr::select(mapping, var = variable, name, category = category_name)) %>%
  dplyr::mutate(name = stringr::str_wrap(.$name, width = 15),
                type = stringr::str_wrap(.$type, width = 50))  %>%
  dplyr::mutate(type = stringr::str_wrap(type, 20)) 


name_mapping <- data.frame(
  model = c("SL.mean_All", 
            "SL.ranger_screen.glmnet", 
            "SL.nnet_screen.glmnet", 
            "SL.xgboost.hist_screen.glmnet",
            "SL.ksvm_screen.glmnet", 
            "SL.lm_screen.glmnet"),
  model_name = c("Mean", 
                 "Random_Forest", 
                 "Neural_Network", 
                 "XGBoost",
                 "SVM", 
                 "Linear_Model")
)

essay <- tar_read(essay_superlearner) %>%
  purrr::map_dfr( 
    function(x){ 
      df <- x$fit$coef
      result <- data.frame(
        type = "Prediction based on ~250 Word Essay",
        outcome = x[[2]],
        model = colnames(df),
        mean = colMeans(df),
        sd = apply(df, 2, sd)
      )
    }) 

gene <- tar_read(gene_superlearner) %>%
  purrr::map_dfr( 
    function(x){ 
      df <- x$fit$coef
      result <- data.frame(
        type = "Prediction based on Combination of various Polygenic Scores",
        outcome = x[[2]],
        model = colnames(df),
        mean = colMeans(df),
        sd = apply(df, 2, sd)
      )
    })

teacher <- tar_read(teacher_superlearner) %>%
  purrr::map_dfr( 
    function(x){ 
      df <- x$fit$coef
      result <- data.frame(
        type = "Prediction based on Teacher Evaluation",
        outcome = x[[2]],
        model = colnames(df),
        mean = colMeans(df),
        sd = apply(df, 2, sd)
      )
    })

teacher_genes_essay <- tar_read(teacher_genes_essay_superlearner) %>%
  purrr::map_dfr( 
    function(x){ 
      df <- x$fit$coef
      result <- data.frame(
        type = "Polygenic Scores, ~250 Word Essay & Teacher Evaluation",
        outcome = x[[2]],
        model = colnames(df),
        mean = colMeans(df),
        sd = apply(df, 2, sd)
      )
    })


appendix_10_data <- essay %>% 
  dplyr::bind_rows(gene) %>% 
  dplyr::bind_rows(teacher) %>% 
  dplyr::bind_rows(teacher_genes_essay) %>%
  dplyr::left_join(dplyr::select(mapping, outcome = variable, name, category = category_name))  %>%
  dplyr::left_join(name_mapping) %>% 
  dplyr::select(type, name, model = model_name, mean_weight = mean, sd)

rownames(appendix_10_data) <- NULL




fwrite(appendix_1_data, "appendix_D4_data.csv")
fwrite(appendix_2_data, "appendix_D5_data.csv")
fwrite(appendix_3_data, "appendix_D6_data.csv")
fwrite(appendix_4_data, "appendix_D7_data.csv")
fwrite(appendix_5_data, "appendix_D8_data.csv")
fwrite(appendix_6_data, "appendix_D9_data.csv")
fwrite(appendix_7_data, "appendix_D10_data.csv")
fwrite(appendix_8_data, "appendix_D11_data.csv")
fwrite(appendix_9_data, "appendix_D12_data.csv")
fwrite(appendix_10_data, "appendix_D3_data.csv")
fwrite(appendix_11_data, "appendix_D2_data.csv")
fwrite(appendix_12_data, "appendix_D1_data.csv")

fwrite(plot_1_data, "fig_2_data.csv")
fwrite(plot_2_data, "fig_3_data.csv")
fwrite(plot_3_data, "fig_4_data.csv")
fwrite(plot_4_data, "fig_5_data.csv")
