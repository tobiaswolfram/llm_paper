
get_gpt_embeddings <- function(essays){
  readRDS(essays) %>% 
    purrr::map_dfr(function(x) x$data %>% tidyr::unnest_wider(embedding, names_sep = "_")) %>% 
    dplyr::select(-1, -2) %>%
    dplyr::bind_cols(essays, .) %>% 
    dplyr::select(-1,-3,-4) %>% 
    dplyr::as.tbl() %>% 
    dplyr::rename(id = ncdsid)
}

get_gpt4_embeddings <- function(essays){
  readRDS(essays) %>% 
    purrr::map_dfr(function(x) x$data %>% tidyr::unnest_wider(embedding, names_sep = "_")) %>% 
    dplyr::select(-1, -2) %>%
    dplyr::bind_cols(essays, .) %>% 
    dplyr::select(-1,-3,-4) %>% 
    dplyr::as.tbl() %>% 
    dplyr::rename(id = ncdsid)
}

read_datalist <- function(path){
  read_excel(path)
}

read_essays <- function(folder){
  readtext::readtext(paste0(folder)) %>%
    tidyr::separate(text, sep = "\n----------------------\n", into = c("ncdsid", "text")) %>%
    tidyr::separate(text, sep = "  Words: ", into = c("text", "words")) %>%
    dplyr::mutate(ncdsid = gsub("ID: ", "", ncdsid))
}


read_gene_data <- function(path){
  #PLACEHOLDER
}


read_camsis <- function(path){
  haven::read_dta(path)
}

read_occupation_aspiration_mapping <- function(path){
  read_excel(path)
}

read_ncds <- function(file, varlist){
  haven::read_dta(file) %>%
    setNames(tolower(colnames(.))) %>%
    dplyr::select(dplyr::one_of(tolower(c("ncdsid", varlist)))) %>%
    sjlabelled::set_na(na = -99:-1) %>%
    #haven::as_factor() %>%
    #dplyr::mutate_if(is.factor, as.character) %>%
    setNames(tolower(colnames(.)))
}

combine_ncds <- function(..., varlist){

  list(...) %>% 
    plyr::join_all(by= "ncdsid", type= "full") %>%
    tibble::as_tibble()
}

clean_ncds <- function(ncds_complete, mapping_df){

  rename_by_mapping <- function(data, mapping_df, var_old, var_new, id){
    
    names(data) <- mapping_df[[var_new]][match(names(data), mapping_df[[var_old]])]
    
    names(data)[1] <-id
    
    data
  }
  
  variables <-  mapping_df %>%
    dplyr::mutate(full_name = paste0("s", sweep, "_", 
                                     substr(respondent, 1,2), "_", new_varname),
                  variable = tolower(variable)) 
  
  data_renamed <- ncds_complete %>% 
    dplyr::mutate_all(function(x) ifelse(sjlabelled::to_character(x) %in% c("Dont know", "Inapplicable", "Not known",
                                                                                      "Not answered", "Do not know, DNA", 
                                                                                      "Do not know", "Refused", "Not applicable",
                                                                                      "Self completion qnaire not completed", 
                                                                                      "Item not applicable", "Misrouted - incomplete interview",
                                                                                      "N/a: proxy/block not entered",
                                                                                      "No usual pay", " Cant say,inappl",
                                                                                      "Cant say", "Inapplicable", "Self completion qnaire not completed", 
                                                                                      "Unclassifiable", "Too vague",
                                                                                      "Imprecise"), NA, x)) %>% 
    dplyr::select(ncdsid, dplyr::one_of(tolower(variables$variable))) %>%
    rename_by_mapping(mapping_df = variables, 
                      var_old = "variable", 
                      var_new = "full_name",
                      id = "ncdsid") %>%
    # recode some numeric missings
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("year")), function(x) ifelse(x %in% c(9998, 9999), NA, x)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("month")), function(x) ifelse(x %in% c(9998, 9999), NA, x))
  
  ##### PARENTS #####
  
  parents <- data_renamed %>%
    dplyr::select(ncdsid, dplyr::one_of(dplyr::filter(variables, type %in% c("parental class"))$full_name)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("nssec")), 
                     function(x){
                       dplyr::case_when((x>=1)&(x<=2) ~ 1,
                                        (x>=3.1)&(x<=3.4) ~ 2, 
                                        (x>=4.1)&(x<=6) ~ 3,
                                        (x>=7.1)&(x<=7.4) ~ 4,
                                        (x>=8.1)&(x<=9.2) ~ 5,
                                        (x>=10)&(x<=11.2) ~ 6,
                                        (x>=12.1)&(x<=12.7) ~ 7,
                                        (x>=13.1)&(x<=13.5) ~8,
                                        TRUE  ~ NA_real_) %>%
                         as.numeric()}) %>%
    dplyr::mutate(s3_pa_mother_edu = dplyr::case_when( as.numeric(s3_pa_age_edu_left_mother) < 4 ~ "No Qualifications",
                                                       as.numeric(s3_pa_age_edu_left_mother) < 6 ~ "Lower Secondary",
                                                       as.numeric(s3_pa_age_edu_left_mother) < 8 ~ "Upper Secondary",
                                                       as.numeric(s3_pa_age_edu_left_mother) < 11 ~ "Degree",
                                                       TRUE ~ NA_character_),
                  s3_pa_father_edu = dplyr::case_when( as.numeric(s3_pa_age_edu_left_father) < 4 ~ "No Qualifications",
                                                       as.numeric(s3_pa_age_edu_left_father) < 6 ~ "Lower Secondary",
                                                       as.numeric(s3_pa_age_edu_left_father) < 8 ~ "Upper Secondary",
                                                       as.numeric(s3_pa_age_edu_left_father) < 11 ~ "Degree",
                                                       TRUE ~ NA_character_),
                  s3_pa_mother_edu = factor(s3_pa_mother_edu, levels = c("No Qualifications", "Lower Secondary", 
                                                                         "Upper Secondary", "Degree"), ordered = TRUE),
                  s3_pa_father_edu = factor(s3_pa_father_edu, levels = c("No Qualifications", "Lower Secondary", 
                                                                         "Upper Secondary", "Degree"), ordered = TRUE),
                  s3_pa_edu = dplyr::case_when(pmin(s3_pa_mother_edu, s3_pa_father_edu) == "No Qualifications" &
                                                 pmax(s3_pa_mother_edu, s3_pa_father_edu) == "No Qualifications" ~ 1,
                                               pmin(s3_pa_mother_edu, s3_pa_father_edu) == "No Qualifications" &
                                                 pmax(s3_pa_mother_edu, s3_pa_father_edu) == "Lower Secondary" ~ 2,
                                               pmin(s3_pa_mother_edu, s3_pa_father_edu) == "Lower Secondary" &
                                                 pmax(s3_pa_mother_edu, s3_pa_father_edu) == "Lower Secondary" ~ 3,
                                               pmin(s3_pa_mother_edu, s3_pa_father_edu) < "Upper Secondary" &
                                                 pmax(s3_pa_mother_edu, s3_pa_father_edu) ==  "Upper Secondary" ~ 4,
                                               pmin(s3_pa_mother_edu, s3_pa_father_edu) == "Upper Secondary" &
                                                 pmax(s3_pa_mother_edu, s3_pa_father_edu) ==  "Upper Secondary" ~ 5,
                                               pmin(s3_pa_mother_edu, s3_pa_father_edu) < "Degree" &
                                                 pmax(s3_pa_mother_edu, s3_pa_father_edu) ==  "Degree" ~ 6,
                                               pmin(s3_pa_mother_edu, s3_pa_father_edu) == "Degree" &
                                                 pmax(s3_pa_mother_edu, s3_pa_father_edu) ==  "Degree" ~ 7,
                                               TRUE ~ NA_real_),
                  s3_pa_edu = factor(s3_pa_edu)) 
  
  
  ##### SEX #####
  
  sex <- data_renamed %>%
    dplyr::mutate(s0_co_male = ifelse(s0_co_sex == 1, TRUE, FALSE)) %>%
    dplyr::select(ncdsid, s0_co_male)
  
  ##### HEIGHT #####
  
  height <- data_renamed %>% 
    dplyr::select(ncdsid, s3_co_height)
  
  ##### BIRTHWEIGHT #####
  
  birthweight <- data_renamed %>% 
    dplyr::select(ncdsid, s0_mo_birthweight)
  
  ##### TEACHER #####
  
  teacher <- data_renamed %>%
    dplyr::select(ncdsid, s2_te_general_knowledge,
                  s2_te_number_work,
                  s2_te_use_of_books,
                  s2_te_oral_ability,
                  s2_te_poor_hand_control,
                  s2_te_squirmy,
                  s2_te_poor_coordination,
                  s2_te_hardly_ever_still,
                  s2_te_poor_speech,
                  s2_te_inconsequential_behavior,
                  s2_te_miscellneous_symptoms,
                  s2_te_anxiety_adults,
                  s2_te_anxiety_children,
                  s2_te_hostility_children,
                  s2_te_writing_off_adults,
                  s2_te_misc,
                  s2_te_hostility_adults,
                  s2_te_restlessness,
                  s2_te_unforthcomingness,
                  s2_te_depression,
                  s2_te_withdrawal
                  
    ) %>%
    dplyr::mutate_if(is.numeric, haven::as_factor) %>%
    dplyr::mutate_all(function(x) ifelse(x == "Dont know", NA, x))
  
  ##### BEHAVIOR #####
  
  behavior <- data_renamed %>%
    dplyr::select(ncdsid, dplyr::one_of(dplyr::filter(variables, type %in% c("behavior"))$full_name))
  
  ##### ABILITY #####
  
  ability <- data_renamed %>%
    dplyr:::select(ncdsid, dplyr::one_of(dplyr::filter(variables, type %in% c("ability"))$full_name)) %>%
    dplyr::select(-s2_co_total_ability)
  
  ##### PERSONALITY #####
  
  personality <- data_renamed %>%
    dplyr::select(ncdsid, dplyr::one_of(dplyr::filter(variables, type %in% c("personality"))$full_name)) 
  
  ##### MOTIVATION #####
  
  motivation <- data_renamed %>%
    dplyr::select(ncdsid, dplyr::one_of(dplyr::filter(variables, type %in% c("academic_motivation"))$full_name))
  
  ##### HIGHEST EDUCATION ####
  
  highest_edu <- data_renamed %>%
    dplyr::select(ncdsid, dplyr::one_of(dplyr::filter(variables, type %in% c("education"))$full_name))
  

  
  ##### COMBINATION #####
  # combine the important variables from each wave for joint analyses
  
  all_variables <- plyr::join_all(list(teacher, parents, height, birthweight,
                                       bsag, behavior, ability,  aspirations, personality,
                                       motivation, parenting, highest_edu, sex, camsis), 
                                  by = "ncdsid", type = "full") %>%
    dplyr::select(ncdsid, 
                  colnames(sex),
                  colnames(birthweight),
                  colnames(height),
                  colnames(teacher),
                  colnames(parents),
                  colnames(personality),
                  colnames(behavior),
                  colnames(ability),
                  colnames(motivation), 
                  colnames(highest_edu), 
    ) %>%
    tibble::as_tibble()
  
}

create_aspirations <- function(ncds_complete, camsis, occupation_aspiration_mapping){
  # the camsis-dataset contains mappings from soc70 (job groups in gb in the 70s)
  # to various job-based social stratification measures, but none of those besides
  # camsis are usable if information on employment-status is missing 
  aspiration_camsis <- camsis %>%
    dplyr::select(occupation_1970 = co1970, camsis_male = mcamsis, camsis_female = fcamsis, stdempst) %>%
    # employment status is unknown   
    dplyr::filter(stdempst  == 0) %>%
    dplyr::mutate(occupation_1970 = haven::as_factor(occupation_1970)) %>%
    dplyr::select(-stdempst) %>%
    unique() %>%
    # merge the mapping we crafted by hand
    dplyr::full_join(occupation_aspiration_mapping) %>%
    na.omit()
  
  ncds_complete %>%
    dplyr::select(ncdsid, aspiration_n2771   = n2771, sex = n622) %>%
    dplyr::mutate(aspiration_n2771 = as.character(haven::as_factor(aspiration_n2771)),
                  sex = as.character(haven::as_factor(sex))) %>%
    dplyr::left_join(aspiration_camsis)  %>%
    # as camsis is sex-specific we create a joint camsis-score for males and females
    dplyr::mutate(camsis = ifelse(sex == 1, camsis_male, camsis_female)) %>%
    dplyr::select(ncdsid, 
                  s2_co_aspiration_camsis_male = camsis_male, 
                  s2_co_aspiration_camsis_female = camsis_female, 
                  s2_co_aspiration_camsis = camsis)
}

create_factors <- function(ncds_cleaned){
  # create factors according to the results of the explorative analysis of 
  # structures in the report
  
  
  s2_co_factor_ability <- list(vars = c("s2_co_verbal_ability", "s2_co_nonverbal_ability", 
                                        "s2_co_reading_ability", "s2_co_mathematics_ability"), 
                               type = "cor")
  s3_co_factor_scholastic_motivation <- list(vars = c("s3_co_school_waste_of_time", 
                                                      "s3_co_homework_a_bore", 
                                                      "s3_co_never_take_work_seriously", 
                                                      "s3_co_not_like_schol"), 
                                             type = "poly")
  s3_te_factor_internalizing <- list(vars = c("s3_te_worried", "s3_te_solitary", 
                                              "s3_te_miserable", "s3_te_fearful", 
                                              "s3_te_cries_in_school"), 
                                     type = "poly")
  s3_te_factor_externalizing <- list(vars = c("s3_te_restlessness", "s3_te_squirmy", 
                                              "s3_te_destructive", "s3_te_fight_others",  
                                              "s3_te_irritable", "s3_te_disobedient", 
                                              "s3_te_cannot_settle", "s3_te_lying", 
                                              "s3_te_steals", "s3_te_resentful", 
                                              "s3_te_bully"), 
                                     type = "poly")
  
  dplyr::lst(s2_co_factor_ability, s3_co_factor_scholastic_motivation, s3_te_factor_internalizing, s3_te_factor_externalizing) %>%
    purrr::imap_dfc(function(x,y){
      ncds_cleaned %>%
        dplyr::select(x$vars) %>%
        psych::fa(1, cor = x$type) %>%
        .$score %>%
        tibble::as_tibble() %>%
        setNames(y)
    }) %>%
    dplyr::bind_cols(dplyr::select(ncds_cleaned, ncdsid), .)
}

get_complete_ncds <- function(ncds_cleaned, ncds_factors, ncds_aspirations, ncds_essay){
  ncds_cleaned %>%
    dplyr::left_join(ncds_factors) %>%
    dplyr::left_join(ncds_aspirations) %>%
    dplyr::left_join(ncds_essay) 
}


create_essay_variables <- function(salat_metrics, readability_metrics, spelling_errors, roberta_embeddings){
  
  salat_metrics %>%
    dplyr::left_join(readability_metrics) %>%
    dplyr::left_join(spelling_errors) %>%
    dplyr::left_join(roberta_embeddings, by = c("ncdsid" = "id")) %>%
    dplyr::select(-filename) %>%
    dplyr::mutate_at(dplyr::vars(-ncdsid), as.numeric) %>%
    #exclude all metrics which have NAs 
    dplyr::select_if(function(x) is.character(x) || (all(!is.na(x) &  max(x) != Inf & min(x) != -Inf & var(x) > 0))) 
  
}

find_essay_teacher_genetics_overlap <- function(ncds_complete_genetics, ncds_essays, ncds_1_to_9){
  
  extended_teacher_evaluations <- ncds_1_to_9  %>%
    dplyr::select(ncdsid, n876, n877, n878, n879, n880, n881, n882, n883, n884, n885) %>%
    dplyr::mutate_if(is.numeric, haven::as_factor) %>%
    dplyr::mutate_all(function(x) ifelse(x == "Dont know", NA, x)) %>%
    na.omit()
  
  ncds_complete_genetics %>% 
    dplyr::filter(!is.na(s2_co_factor_ability) & 
                    !is.na(s2_co_verbal_ability) & 
                    !is.na(s2_co_nonverbal_ability) &
                    !is.na(s2_co_reading_ability) & 
                    !is.na(s2_co_mathematics_ability)) %>% 
    dplyr::inner_join(ncds_essays) %>%
    dplyr::inner_join(extended_teacher_evaluations)
}

find_full_overlap <- function(ncds_complete, varlist){
  
  ncds_complete %>% 
    dplyr::select(ncdsid,varlist) %>%
    na.omit()
  
}

tokenize_essays <- function(ncds_essays){
  ncds_essays %>% 
    dplyr::mutate(count = 1:dplyr::n()) %>%
    split(1:nrow(.)) %>%
    purrr::map(function(x){
      tokenization <- treetag(file = x$text, treetagger = "manual", lang = "en", format = "obj", TT.options=list(
        path="C:/TreeTagger", preset = "en"))
      
      tokenization
      
    })
}

calculate_readability_metrics <- function(ncds_essays, tokenized_essays){
  
  readability_metrics <- tokenized_essays %>%
    purrr::map_dfr(function(x){
      x %>% 
        readability() %>% 
        summary() %>%
        dplyr::mutate(metric = ifelse(raw == "", grade, raw)) %>% 
        dplyr::select(index, metric) %>%
        t() %>% 
        janitor::row_to_names(1) %>%
        as.data.frame() %>%
        tibble::remove_rownames()
    }) 
  
  ncds_essays %>%
    as.data.frame() %>%
    dplyr::select(filename = doc_id, ncdsid) %>%
    dplyr::bind_cols(readability_metrics)
}

get_spelling_error_metrics <- function(ncds_essays, path){
  
  spelling_mistakes <- readr::read_csv(path)
  
  spelling_mistakes_count <- spelling_mistakes %>% 
    dplyr::group_by(ncdsid, rule_issue_type) %>% 
    dplyr::count() 
  
  spelling_error_metrics <- ncds_essays %>% 
    dplyr::select(ncdsid, words) %>%
    dplyr::left_join(spelling_mistakes_count, by = "ncdsid") %>% 
    dplyr::select(ncdsid, rule_issue_type, n, words) %>% 
    dplyr::mutate(error_per_words = n/as.numeric(words)) %>% 
    dplyr::select(ncdsid, rule_issue_type, error_per_words) %>% 
    tidyr::pivot_wider(names_from = rule_issue_type, values_from = c(error_per_words)) %>% 
    dplyr::mutate_at(dplyr::vars(dplyr::one_of("grammar", "misspelling", "typographical", 
                                               "locale-violation", "duplication",   "style",
                                               "whitespace", "uncategorized", "inconsistency")), 
                     function(x) ifelse(is.na(x), 0, x)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(total = rowSums(dplyr::select(., grammar, misspelling, typographical, 
                                                `locale-violation`, duplication,  style, 
                                                whitespace, uncategorized, inconsistency)),
                  other = rowSums(dplyr::select(., `locale-violation`, 
                                                whitespace, uncategorized, inconsistency))) %>%
    dplyr::select(-"NA")
  
}

get_salat_metrics <- function(ncds_essays, 
                              taaled_1, taaled_2, taaled_3, 
                              taales_1, taales_2, taales_3,
                              seance_1, seance_2, seance_3){
  
  taaled <- dplyr::bind_rows(readr::read_csv(taaled_1),
                             readr::read_csv(taaled_2),
                             readr::read_csv(taaled_3))
  
  taales <- dplyr::bind_rows(readr::read_csv(taales_1),
                             readr::read_csv(taales_2),
                             readr::read_csv(taales_3)) %>%
    dplyr::rename(filename = Filename)
  
  seance <- dplyr::bind_rows(readr::read_csv(seance_1),
                             readr::read_csv(seance_2),
                             readr::read_csv(seance_3))
  
  ncds_essays %>%
    as.data.frame() %>%
    dplyr::select(filename = doc_id, ncdsid) %>%
    dplyr::left_join(taaled) %>%
    dplyr::left_join(taales) %>%
    dplyr::left_join(seance) %>%
    dplyr::select(-filename)
}

get_roberta_embeddings <- function(essays, max_len = 250L){
  
  prepare_data <- function(data, data_col, max_length = 512L) {
    
    text = list()
    
    for (i in 1:nrow(data)) {
      print(i)
      txt <- tokenizer$encode(data[[data_col]][i], max_length = max_length, 
                              truncation = T, pad_to_max_length = TRUE) %>% 
        t() %>% 
        as.matrix() %>% list()
      text <- text %>% append(txt)
    }
    do.call(plyr::rbind.fill.matrix,text)
  }
  
  reticulate::use_python("C:/Users/usr/anaconda3/python.exe", required = TRUE)
  transformer <- reticulate::import('transformers', delay_load = TRUE)
  
  # get Tokenizer
  tokenizer <- transformer$RobertaTokenizer$from_pretrained('roberta-base', 
                                                            do_lower_case=TRUE)
  
  # tokenize data and bring it into right shape
  data_prepared <- prepare_data(essays, data_col = "text", max_length = as.integer(max_len))
    
  tf_data <- tensor_slices_dataset(list(data_prepared)) 
  
  # get Model with weights
  roberta_model <- transformer$TFRobertaModel$from_pretrained('roberta-base')
  # weights should stay fixed 
  roberta_model$trainable <- FALSE
  # create an input layer
  input <- layer_input(shape=as.integer(max_len), dtype='int32')
  # we directly want the roberta-embeddings as output
  output <- tf$reduce_mean(roberta_model(input)[[1]], axis=1L)
  # combine input and output
  final_model <- keras_model(inputs=input, outputs = output)
 
  # create the predictions for the data
  cbind.data.frame(essays$ncdsid, predict(final_model, data_prepared)) %>%
     setNames(c("id", paste0("roberta_dim_", 1:768))) %>%
     dplyr::as_tibble()
}

SL.xgboost.hist = function(...) {
  SL.xgboost(..., params = list(tree_method = "hist"))
}

get_general_superlearner_cv_model <- function(outcome_var, predictors, data){
  
  data <- dplyr::select(data, outcome_var, predictors) %>%
    na.omit()
  
  y_train <- dplyr::pull(data, outcome_var)
  x_train <- dplyr::select(data, predictors)
  
  colnames(x_train) <- make.names(colnames(x_train))
  
  cluster <- parallel::makeCluster(10)
  parallel::clusterEvalQ(cluster, library(SuperLearner))
  parallel::clusterExport(cluster,c('SL.xgboost.hist'))
  parallel::clusterSetRNGStream(cluster, 1)

  cv_sl <- CV.SuperLearner(Y = y_train, X = x_train, family = gaussian(),
                           cvControl = list(V = 10), innerCvControl = list(list(V= 5)),
                           parallel = cluster,
                           SL.library = list("SL.mean",
                                             c("SL.ranger", "screen.glmnet"),
                                             c("SL.nnet", "screen.glmnet"),
                                             c("SL.xgboost.hist", "screen.glmnet"),
                                             c("SL.ksvm", "screen.glmnet"),
                                             c("SL.lm", "screen.glmnet")), 
                           saveAll = TRUE, verbose = TRUE)
  
  return(list(fit = cv_sl, var = outcome_var))
  
}

get_lm_cv_model <- function(outcome_var, predictors, data){
  
  data <- dplyr::select(data, outcome_var, predictors) %>%
    na.omit()
  
  y_train <- dplyr::pull(data, outcome_var)
  x_train <- dplyr::select(data, predictors)
  
  colnames(x_train) <- make.names(colnames(x_train))
  
  cluster <- parallel::makeCluster(5)
  parallel::clusterEvalQ(cluster, library(SuperLearner))
  parallel::clusterSetRNGStream(cluster, 1)
  
  cv_sl <- CV.SuperLearner(Y = y_train, X = x_train, family = gaussian(),
                           cvControl = list(V = 10), innerCvControl = list(list(V= 5)),
                           parallel = cluster,
                           SL.library = list("SL.mean", c("SL.lm")), 
                           saveAll = TRUE, verbose = TRUE)
  
  return(list(fit = cv_sl, var = outcome_var))
  
}

get_cv_predictive_r2 <- function(object, obsWeights = NULL){
  
  if ("env" %in% names(object)) {
    env = object$env
  }  else {
    env = parent.frame()
  }
  method <- if (is.null(as.list(object$call)[["method"]])) {
    method <- "method.NNLS"
  } else if (is.symbol(as.list(object$call)[["method"]])) {
    method <- get(paste(as.list(object$call)[["method"]]), 
                  envir = env)
  } else {
    method <- as.list(object$call)[["method"]]
  }
  library.names <- colnames(coef(object))
  V <- object$V
  n <- length(object$SL.predict)
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, length(object$Y))
  }
  folds <- object$folds
  SL.predict <- object$SL.predict
  discreteSL.predict <- object$discreteSL.predict
  library.predict <- object$library.predict
  Y <- object$Y
  Risk.SL <- rep(NA, length = V)
  Risk.dSL <- rep(NA, length = V)
  Risk.library <- matrix(NA, nrow = length(library.names), 
                         ncol = V)
  rownames(Risk.library) <- library.names
  if (method %in% c("method.NNLS", "method.NNLS2", "method.CC_LS")) {
    for (ii in seq_len(V)) {
      Risk.SL[ii] <- mean(obsWeights[folds[[ii]]] * (Y[folds[[ii]]] - 
                                                       SL.predict[folds[[ii]]])^2)
      Risk.dSL[ii] <- mean(obsWeights[folds[[ii]]] * (Y[folds[[ii]]] - 
                                                        discreteSL.predict[folds[[ii]]])^2)
      Risk.library[, ii] <- apply(library.predict[folds[[ii]], 
                                                  , drop = FALSE], 2, function(x) mean(obsWeights[folds[[ii]]] * 
                                                                                         (Y[folds[[ii]]] - x)^2))
    }
    
    predictive_r2 <- 1-Risk.SL/Risk.library[which(rownames(Risk.library) == "SL.mean_All"),] 
    
    
    
  }
  
  data.frame(mean_r2 = mean(predictive_r2), min_r2 = min(predictive_r2), max_r2 = max(predictive_r2))
  
}

get_cv_rmse <- function(object, obsWeights = NULL){
  
  if ("env" %in% names(object)) {
    env = object$env
  }  else {
    env = parent.frame()
  }
  method <- if (is.null(as.list(object$call)[["method"]])) {
    method <- "method.NNLS"
  } else if (is.symbol(as.list(object$call)[["method"]])) {
    method <- get(paste(as.list(object$call)[["method"]]), 
                  envir = env)
  } else {
    method <- as.list(object$call)[["method"]]
  }
  library.names <- colnames(coef(object))
  V <- object$V
  n <- length(object$SL.predict)
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, length(object$Y))
  }
  folds <- object$folds
  SL.predict <- object$SL.predict
  discreteSL.predict <- object$discreteSL.predict
  library.predict <- object$library.predict
  Y <- object$Y
  Risk.SL <- rep(NA, length = V)
  Risk.dSL <- rep(NA, length = V)
  Risk.library <- matrix(NA, nrow = length(library.names), 
                         ncol = V)
  rownames(Risk.library) <- library.names
  if (method %in% c("method.NNLS", "method.NNLS2", "method.CC_LS")) {
    for (ii in seq_len(V)) {
      Risk.SL[ii] <- sqrt(mean(obsWeights[folds[[ii]]] * (Y[folds[[ii]]] - 
                                                       SL.predict[folds[[ii]]])^2))
    }
    
    rmse <- Risk.SL
    
    
    
  }
  
  data.frame(mean_rmse = mean(rmse), min_rmse = min(rmse), max_rmse = max(rmse))
  
}

get_cv_mad <- function(object, obsWeights = NULL){
  
  if ("env" %in% names(object)) {
    env = object$env
  }  else {
    env = parent.frame()
  }
  method <- if (is.null(as.list(object$call)[["method"]])) {
    method <- "method.NNLS"
  } else if (is.symbol(as.list(object$call)[["method"]])) {
    method <- get(paste(as.list(object$call)[["method"]]), 
                  envir = env)
  } else {
    method <- as.list(object$call)[["method"]]
  }
  library.names <- colnames(coef(object))
  V <- object$V
  n <- length(object$SL.predict)
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, length(object$Y))
  }
  folds <- object$folds
  SL.predict <- object$SL.predict
  discreteSL.predict <- object$discreteSL.predict
  library.predict <- object$library.predict
  Y <- object$Y
  Risk.SL <- rep(NA, length = V)
  Risk.dSL <- rep(NA, length = V)
  Risk.library <- matrix(NA, nrow = length(library.names), 
                         ncol = V)
  rownames(Risk.library) <- library.names
  if (method %in% c("method.NNLS", "method.NNLS2", "method.CC_LS")) {
    for (ii in seq_len(V)) {
      Risk.SL[ii] <- mean(abs(obsWeights[folds[[ii]]] * (Y[folds[[ii]]] - 
                                                       SL.predict[folds[[ii]]])))
    }
    
    mad = Risk.SL
    
    
    
  }
  
  data.frame(mean_mad = mean(mad), min_mad = min(mad), max_mad = max(mad))
  
}

get_cv_superlearner_metrics <- function(cv_fit_list){
  
  cv_fit <- cv_fit_list[[1]]
  varname <- cv_fit_list[[2]]
  
  # baseline <- summary(cv_fit)$Table %>%
  #   dplyr::filter(Algorithm == "SL.mean_All")
  
  superlearner <- summary(cv_fit)$Table %>%
    dplyr::filter(Algorithm == "Super Learner")
  
  cv_fit$SL.predict[abs(cv_fit$SL.predict) > 10*sd(abs(cv_fit$SL.predict))+mean(cv_fit$SL.predict)] <- mean(cv_fit$SL.predict)
  
  predictive_r2 <- get_cv_predictive_r2(cv_fit)
  
  mad <- get_cv_mad(cv_fit)
  
  rmse <- get_cv_rmse(cv_fit)
  
  
  superlearner %>%
    dplyr::rename(mean_mse = Ave, min_mse = Min, max_mse = Max) %>%
    dplyr::bind_cols(predictive_r2) %>%
    dplyr::bind_cols(mad) %>%
    dplyr::bind_cols(rmse) %>%
    dplyr::mutate(var = varname,
                  n = length(cv_fit$Y)) 
}

get_cv_lm_metrics <- function(cv_fit_list){
  
  cv_fit <- cv_fit_list[[1]]
  varname <- cv_fit_list[[2]]
  

  superlearner <- summary(cv_fit)$Table %>%
    dplyr::filter(Algorithm == "SL.lm_All")
  
  cv_fit$SL.predict[abs(cv_fit$SL.predict) > 10*sd(abs(cv_fit$SL.predict))+mean(cv_fit$SL.predict)] <- mean(cv_fit$SL.predict)
  
  predictive_r2 <- get_cv_lm_r2(cv_fit)
  
  mad <- get_cv_mad(cv_fit)
  
  rmse <- get_cv_rmse(cv_fit)
  
  superlearner %>%
    dplyr::rename(mean_mse = Ave, min_mse = Min, max_mse = Max) %>%
    dplyr::bind_cols(predictive_r2) %>%
    dplyr::bind_cols(mad) %>%
    dplyr::bind_cols(rmse) %>%
    dplyr::mutate(var = varname,
                  length(cv_fit$Y)) 
}

get_cv_lm_r2 <- function(object, obsWeights = NULL){
  
  if ("env" %in% names(object)) {
    env = object$env
  }  else {
    env = parent.frame()
  }
  method <- if (is.null(as.list(object$call)[["method"]])) {
    method <- "method.NNLS"
  } else if (is.symbol(as.list(object$call)[["method"]])) {
    method <- get(paste(as.list(object$call)[["method"]]), 
                  envir = env)
  } else {
    method <- as.list(object$call)[["method"]]
  }
  library.names <- colnames(coef(object))
  V <- object$V
  n <- length(object$SL.predict)
  if (is.null(obsWeights)) {
    obsWeights <- rep(1, length(object$Y))
  }
  folds <- object$folds
  SL.predict <- object$SL.predict
  discreteSL.predict <- object$discreteSL.predict
  library.predict <- object$library.predict
  Y <- object$Y
  Risk.SL <- rep(NA, length = V)
  Risk.dSL <- rep(NA, length = V)
  Risk.library <- matrix(NA, nrow = length(library.names), 
                         ncol = V)
  rownames(Risk.library) <- library.names
  if (method %in% c("method.NNLS", "method.NNLS2", "method.CC_LS")) {
    for (ii in seq_len(V)) {
      
      Risk.library[, ii] <- apply(library.predict[folds[[ii]], 
                                                  , drop = FALSE], 2, function(x) mean(obsWeights[folds[[ii]]] * 
                                                                                         (Y[folds[[ii]]] - x)^2))
    }
    
    predictive_r2 <- 1-Risk.library[which(rownames(Risk.library) == "SL.lm_All"),] /Risk.library[which(rownames(Risk.library) == "SL.mean_All"),] 
    
    
  }
  
  data.frame(mean_r2 = mean(predictive_r2), min_r2 = min(predictive_r2), max_r2 = max(predictive_r2))
  
}

