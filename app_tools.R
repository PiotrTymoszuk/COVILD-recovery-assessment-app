# This script provides plotting, risk modeling and prediction tools for the app

# tools ----

  require(plyr)
  require(tidyverse)
  require(stringi)
  require(caret)
  require(caretEnsemble)
  require(readxl)
  require(plotly)
  require(writexl)
  require(cowplot)
  require(C50)
  require(randomForest)
  require(kernlab)
  require(nnet)
  require(glmnet)
  

  ## auxiliary scripts

  scrpt_paths <- c('./tools/clust_tools2.R')

  src_result <- try(scrpt_paths %>% 
                      walk(source), 
                    silent = T) ## if launched via app

  if(any(class(src_result) == 'try-error')) {
    
    scrpt_paths <- stri_replace(scrpt_paths, 
                                fixed = './', 
                                replacement = './CovILD Scorer/') ## if run standalone
    
    src_result <- try(scrpt_paths %>% 
                        walk(source), 
                      silent = T)
    
  }
  
# globals ----
  
  globals <- list()
  
  ## graphics
  
  globals$common_text <- element_text(size = 14, 
                                      face = 'plain', 
                                      color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 5, 
                                           l = 5, 
                                           r = 5, 
                                           unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 16, 
                                                                            face = 'bold'), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 14, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0, 
                                                                          vjust = 1), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  plot.margin = globals$common_margin, 
                                                  panel.grid.major = element_line(color = 'gray90'))
  
  ## ML model labels and colors
  
  globals$model_labels <- c('C5.0' = 'C5.0',
                            'rf' = 'RF', 
                            'svmRadial' = 'SVM-R', 
                            'nnet' = 'NNet', 
                            'glmnet' = 'glmNet', 
                            'ensemble' = 'Ensemble', 
                            'median' = 'Median')
  
  globals$model_colors <- c('C5.0' = 'cadetblue3',
                            'rf' = 'darkolivegreen4', 
                            'svmRadial' = 'steelblue', 
                            'nnet' = 'gray60', 
                            'glmnet' = 'plum2', 
                            'ensemble' = 'coral3', 
                            'median' = 'orangered3')
  
  ## app method text
  
  globals$method_text <- list('Risk Clusters (LR: low risk, IR: intermediate risk, HR: high risk) were defined 
  in the training CovILD cohort (NCT04416100) by 52 binary non-CT and non-lung function parameters recorded during acute 
  SARS-CoV-2 infection and early convalescence (approx. 60 days post clinical onset). The clustering algorithm was 
  based on the combined self-organizing map (SOM) - hierarchical clustering procedure described by Vesanto (DOI: 10.1109/72.846731). 
  Prediction of cluster assignment in the user-provided patient data is done by a k-NN (k-nearest neighbors) label 
  algorithm (Leng et al., DOI: 10.3923/JSE.2014.14.22).', 
                              'Machine learning classifiers: C5.0 (Quinlan, DOI: 10.5555/152181), 
                              Random Forest (RF, Breiman, DOI: 10.1023/A:1010933404324), 
  shallow neural network (NNet, Ripley, DOI: 10.1017/CBO9780511812651), 
  support vector machines with radial kernel (SVM-R, Weston & Watkins, 1998) and 
  elastic net (glmNet, Friedman, DOI: 10.18637/jss.v033.i01) were trained 
  and cross-validated in the CovILD cohort data set including solely non-CT and non-lung function parameters recorded during acute 
  SARS-CoV-2 infection and early convalescence (approx. 60 days post clinical onset). Ensemble model was constructed based 
  on the glmNet algorithm.')


# reading the trained cluster objects and the ML models -----
  
  if(class(try(load('./data/data.RData'), silent = TRUE)) == 'try-error') load('./CovILD Scorer/data/data.RData')
  
  if(class(try(load('./data/clust.RData'), silent = TRUE)) == 'try-error') load('./CovILD Scorer/data/clust.RData')
  
  if(class(try(load('./data/ml.RData'), silent = TRUE)) == 'try-error') load('./CovILD Scorer/data/ml.RData')
  
  if(class(try(load('./data/clust_outcome.RData'), silent = TRUE)) == 'try-error') load('./CovILD Scorer/data/clust_outcome.RData')
  
# generating a plot panel with the prevalence of pulmonary disorders in the risk clusters ----
  
  clust_patho <- clust_outcome$main_plots[c('CT_findings_rec', 
                                            'lung_function_impaired_V3')] %>% 
    map2(., c('CT abnormalities', 'Lung function impairment'), 
         ~.x + labs(title = .y, 
                    x = 'Risk Cluster')) %>% 
    map2(., c('Severity\nscore', ''), 
         ~.x + labs(fill = .y)) %>% 
    map(~.x + globals$common_theme) %>% 
    plot_grid(plotlist = ., 
              ncol = 2, 
              align = 'hv')
  
  
# functions ----
  
  read_input <- function(path = 'input_template.xlsx', 
                         template_names = names(extract(part_clust$train_clust$clust_obj, 'data')$observation)) {
    
    ## reads the input excel file
    
    input <- read_excel(path)
    
    input <- input %>% 
      mutate(ID = `Patient ID`, 
             sex_male_V0 = ifelse(Sex == 'male', 'yes', 'no'), 
             sex_male_V0 = factor(sex_male_V0, c('no', 'yes')), 
             age_65_V0 = ifelse(`Age before CoV, years` > 65, 'yes', 'no'), 
             age_65_V0 = factor(age_65_V0, c('no', 'yes')), 
             obesity_rec_V0 = ifelse(`BMI before CoV, kg/m2` > 30, 'yes', 'no'), 
             obesity_rec_V0 = factor(obesity_rec_V0, c('no', 'yes')), 
             overweight_V0 = ifelse(`BMI before CoV, kg/m2` > 30 & obesity_rec_V0 == 'no', 'yes', 'no'), 
             overweight_V0 = factor(overweight_V0, c('no', 'yes')), 
             current_smoker_V0 = ifelse(smoking == 'active', 'yes', 'no'), 
             current_smoker_V0 = factor(current_smoker_V0, c('no', 'yes')), 
             smoking_ex_V0 = ifelse(smoking == 'ex', 'yes', 'no'), 
             smoking_ex_V0 = factor(smoking_ex_V0, c('no', 'yes')), 
             CVDis_rec_V0 = factor(`Cardiovascular disease`, c('no', 'yes')), 
             hypertension_rec_V0 = factor(Hypertension, c('no', 'yes')), 
             PDis_rec_V0 = factor(`Pulmonary disease`, c('no', 'yes')), 
             COPD_rec_V0 = factor(COPD, c('no', 'yes')), 
             asthma_rec_V0 = factor(Asthma, c('no', 'yes')), 
             endocrine_metabolic_rec_V0 = factor(`Endocrine or metabolic disease`, c('no', 'yes')), 
             hypercholesterolemia_rec_V0 = factor(Hypercholesterolemia, c('no', 'yes')), 
             diabetes_rec_V0 = factor(Diabetes, c('no', 'yes')), 
             CKDis_rec_V0 = factor(`Chronic kidney disease`, c('no', 'yes')), 
             GITDis_rec_V0 = factor(`Gastrointestinal disease`, c('no', 'yes')), 
             malignancy_rec_V0 = factor(Malignancy, c('no', 'yes')), 
             immune_deficiency_rec_V0 = factor(`Immune deficiency`, c('no', 'yes')), 
             weight_change_rec_V0 = factor(`Weight loss acute CoV`, c('no', 'yes')), 
             dyspnoe_rec_V0 = factor(`Dyspnoe acute CoV`, c('no', 'yes')), 
             cough_rec_V0 = factor(`Cough acute CoV`, c('no', 'yes')), 
             fever_rec_V0 = factor(`Fever acute CoV`, c('no', 'yes')), 
             night_sweat_rec_V0 = factor(`Night sweating acute CoV`, c('no', 'yes')), 
             pain_rec_V0 = factor(`Pain acute CoV`, c('no', 'yes')), 
             GI_sympt_rec_V0 = factor(`Gastrointestinal symptoms acute CoV`, c('no', 'yes')), 
             anosmia_rec_V0 = factor(`Hypo/anosmia acute CoV`, c('no', 'yes')), 
             ECOG_imp_rec_V0 = factor(`Impaired physical performance acute CoV`, c('no', 'yes')), 
             sleep_disorder_rec_V0 = factor(`Sleep problems acute CoV`, c('no', 'yes')), 
             treat_antiinfec_rec_V0 = factor(`Anti-infective therapy acute CoV`, c('no', 'yes')), 
             treat_antiplat_rec_V0 = factor(`Anti-platelet therapy acute CoV`, c('no', 'yes')), 
             treat_anticoag_rec_V0 = factor(`Anti-coagulation therapy acute CoV`, c('no', 'yes')), 
             treat_immunosuppr_rec_V0 = factor(`Immunosuppression acute CoV`, c('no', 'yes')), 
             anemia_rec_V1 = ifelse(sex_male_V0 == 'yes', 
                                    ifelse(`Hb, g/dL, 60-day FUP` < 13.5, 'yes', 'no'), 
                                    ifelse(`Hb, g/dL, 60-day FUP` < 12, 'yes', 'no')), 
             anemia_rec_V1 = factor(anemia_rec_V1, c('no', 'yes')), 
             ferr_elv_rec_V1 = ifelse(sex_male_V0 == 'yes', 
                                      ifelse(`Ferritin, ng/mL, 60-day FUP` > 300, 'yes', 'no'), 
                                      ifelse(`Ferritin, ng/mL, 60-day FUP` > 150, 'yes', 'no')), 
             ferr_elv_rec_V1 = factor(ferr_elv_rec_V1, c('no', 'yes')), 
             NTelv_rec_V1 = ifelse(`NT-pro-BNP, pg/mL, 60-day FUP` > 125, 'yes', 'no'), 
             NTelv_rec_V1 = factor(NTelv_rec_V1, c('no', 'yes')), 
             Ddimerelv_rec_V1 = ifelse(`D-dimer, pg/ml FEU, 60-day FUP` > 500, 'yes', 'no'), 
             Ddimerelv_rec_V1 = factor(Ddimerelv_rec_V1, c('no', 'yes')),
             CRP_elv_rec_V1 = ifelse(`CRP, mg/dL, 60-day FUP` > 0.5, 'yes', 'no'), 
             CRP_elv_rec_V1 = factor(CRP_elv_rec_V1, c('no', 'yes')), 
             IL6_elv_rec_V1 = ifelse(`IL6, pg/mL, 60-day FUP` > 7, 'yes', 'no'), 
             IL6_elv_rec_V1 = factor(IL6_elv_rec_V1, c('no', 'yes')), 
             iron_deficiency_30_rec_V1 = ifelse(`TSAT, %, 60-day FUP` < 15, 'yes', 'no'), 
             iron_deficiency_30_rec_V1 = factor(iron_deficiency_30_rec_V1, c('no', 'yes')), 
             hosp_7d_V0 = ifelse(`Hospitalization, days, acute CoV` > 7, 'yes', 'no'), 
             hosp_7d_V0 = factor(hosp_7d_V0, c('no', 'yes')), 
             comorb_sum = as.numeric(CVDis_rec_V0) - 1 + 
               as.numeric(hypertension_rec_V0) - 1 + 
               as.numeric(PDis_rec_V0) - 1 + 
               as.numeric(COPD_rec_V0) - 1 + 
               as.numeric(asthma_rec_V0) - 1 + 
               as.numeric(endocrine_metabolic_rec_V0) - 1 + 
               as.numeric(hypercholesterolemia_rec_V0) - 1 + 
               as.numeric(diabetes_rec_V0) - 1 + 
               as.numeric(CKDis_rec_V0) - 1 + 
               as.numeric(GITDis_rec_V0) - 1 + 
               as.numeric(malignancy_rec_V0) - 1, 
             comorb_present_V0 = ifelse(comorb_sum > 0, 'yes', 'no'), 
             comorb_present_V0 = factor(comorb_present_V0, c('no', 'yes')), 
             comorb_3_V0 = ifelse(comorb_sum > 3, 'yes', 'no'), 
             comorb_3_V0 = factor(comorb_3_V0, c('no', 'yes')), 
             sympt_sum = as.numeric(dyspnoe_rec_V0) - 1 + 
               as.numeric(cough_rec_V0) - 1 + 
               as.numeric(fever_rec_V0) - 1 + 
               as.numeric(night_sweat_rec_V0) - 1 + 
               as.numeric(pain_rec_V0) - 1 + 
               as.numeric(GI_sympt_rec_V0) - 1 + 
               as.numeric(anosmia_rec_V0) - 1 + 
               as.numeric(ECOG_imp_rec_V0) - 1 + 
               as.numeric(sleep_disorder_rec_V0) - 1, 
             sympt_6_V0 = ifelse(sympt_sum > 6, 'yes', 'no'), 
             sympt_6_V0 = factor(sympt_6_V0, c('no', 'yes')), 
             sympt_present_V1 = factor(`Symptoms present, 60-day FUP`, c('no', 'yes')), 
             ab_0_V1 = ifelse(`anti-S1/S2 IgG, BAU, 60-day FUP` <= 312.36, 'yes', 'no'), 
             ab_0_V1 = factor(ab_0_V1, c('no', 'yes')), 
             ab_25_V1 = ifelse(312.36 < `anti-S1/S2 IgG, BAU, 60-day FUP` & `anti-S1/S2 IgG, BAU, 60-day FUP` <= 644.10, 'yes', 'no'), 
             ab_25_V1 = factor(ab_25_V1, c('no', 'yes')), 
             ab_50_V1 = ifelse(644.10 < `anti-S1/S2 IgG, BAU, 60-day FUP` & `anti-S1/S2 IgG, BAU, 60-day FUP` <= 974.70, 'yes', 'no'), 
             ab_50_V1 = factor(ab_50_V1, c('no', 'yes')), 
             ab_75_V1 = ifelse(`anti-S1/S2 IgG, BAU, 60-day FUP` > 974.70, 'yes', 'no'), 
             ab_75_V1 = factor(ab_75_V1, c('no', 'yes')), 
             pat_group_G1_V0 = ifelse(`Hospitalization, days, acute CoV` == 0, 'yes', 'no'), 
             pat_group_G1_V0 = factor(pat_group_G1_V0, c('no', 'yes')), 
             pat_group_G2_V0 = ifelse(`Hospitalization, days, acute CoV` > 0 & `Oxygen need, acute CoV2` == 'no', 'yes', 'no'), 
             pat_group_G2_V0 = factor(pat_group_G2_V0, c('no', 'yes')), 
             pat_group_G3_V0 = ifelse(`Hospitalization, days, acute CoV` > 0 & `Oxygen need, acute CoV2` == 'yes' & `ICU, acute CoV2` == 'no', 'yes', 'no'), 
             pat_group_G3_V0 = factor(pat_group_G3_V0, c('no', 'yes')), 
             pat_group_G4_V0 = factor(`ICU, acute CoV2`, c('no', 'yes'))) %>% 
      select(ID, all_of(template_names))
    
    incomplete_records <- input %>% 
      filter(!complete.cases(.)) %>% 
      .$ID
    
    input <- input %>% 
      filter(complete.cases(.))
    
    clust_tbl <- input %>% 
      select(- ID) %>% 
      map_dfc(~as.numeric(.x) - 1) %>% 
      as.data.frame
    
    rownames(clust_tbl) <- input$ID
    
    ml_tbl <- input %>% 
      column_to_rownames('ID')
    
    list(incomplete_records = incomplete_records, 
         clust_tbl = clust_tbl, 
         ml_tbl = ml_tbl)

    
  }
  
  predict_risk_clust <- function(combi_analysis_object = part_clust$train_clust$clust_obj, 
                                 newdata, 
                                 k = 5) {
    
    ## makes risk cluster predictions based on label propagation
    
    predictions <- predict(part_clust$train_clust$clust_obj, 
                           newdata = newdata, 
                           type = 'propagation', 
                           k = k)
    
    ## baustelle: hm and pca plot
    
    return(predictions)
    
  }
  
  plot_risk_clust <- function(train_analysis_object = part_clust$train_clust$clust_obj, 
                              test_analysis_object, 
                              plot_title = NULL, 
                              plot_subtitle = NULL, 
                              interactive = FALSE) {
    
    ## generates a heat map with the test predictions highlighted
    
    ## cluster/node assignment
    
    cmm_ass <- rbind(extract(train_analysis_object, 'assignment'), 
                     extract(test_analysis_object, 'assignment'))
    
    ## plotting table
    
    training_data <- extract(train_analysis_object, 'data')$observation 
    
    clust_features <- names(training_data)

    plotting_tbl <- rbind(training_data, 
                          extract(test_analysis_object, 'data'))

    ## appending the plotting table with the cluster assignment info
    
    plotting_tbl <- plotting_tbl %>% 
      rownames_to_column('observation') %>% 
      gather(key = 'feature', 
             value = 'present', 
             all_of(clust_features)) %>% 
      left_join(cmm_ass, by = 'observation') %>% 
      mutate(present = ifelse(present == 1, 'yes', 'no'), 
             present = factor(present), 
             feature = part_clust$ft_labels[feature], 
             highlight = ifelse(observation %in% rownames(training_data), 
                                'no', 'yes'))
    
    ## n numbers in the training collective
    
    n_tag <- ngroups(train_analysis_object)
    
    n_tag <- map2_chr(n_tag$clust_id, 
                      n_tag$n, 
                      ~paste(.x, .y, sep = ': n = ')) %>% 
      paste(collapse = ', ') %>% 
      paste0('\n', .)
    
    ## plotting
    
    hm <- plotting_tbl %>% 
      ggplot(aes(x = reorder(observation, as.numeric(node)), 
                 y = reorder(feature, as.numeric(present)), 
                 fill = present, 
                 alpha = highlight)) + 
      geom_tile() + 
      facet_grid(.~clust_id, 
                 scales = 'free', 
                 space = 'free') + 
      scale_fill_manual(values = c(no = 'steelblue4', 
                                   yes = 'indianred3'), 
                        name = 'Feature present') + 
      scale_alpha_manual(values = c(no = 0.5, 
                                    yes = 1)) + 
      guides(alpha = FALSE) + 
      globals$common_theme + 
      theme(axis.title.y = element_blank(), 
            axis.text.x = element_blank(), 
            axis.line.x = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.y = element_text(size = 12)) + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           x = 'Patient', 
           tag = n_tag)
    
    if(interactive) {
 
      ggplotly(hm)
      
    } else {
      
      hm
      
    }

  }
  
  pred_risk_clust_output <- function(test_analysis_object, 
                                     path = NULL) {
    
    ## makes a nice cluster assignment output
    
    out_tbl <- extract(test_analysis_object, 'assignment') %>% 
      set_names(c('Patient', 'SOM node', 'Risk cluster'))
    
    if(!is.null(path)) {
      
      write_xlsx(out_tbl, path = path)
      
    }
    
    out_tbl
    
  }
  
  predict_ml <- function(train_list, newdata) {
    
    ## returns predictions (probabilities) by the single models and the ensemble

    ## a table with predictions
    
    single_preds <- train_list$model_list %>% 
      map(predict, 
          newdata = newdata, 
          type = 'prob') %>% 
      map(as_tibble) %>% 
      map2_dfr(., names(.), ~mutate(.x[, 'yes'], 
                                   ID = rownames(newdata), 
                                   method = .y)) %>% 
      select(ID, method, yes) %>% 
      set_names(c('ID', 'method', 'risk'))

    ensemble_preds <-  predict(train_list$ensemble, 
                               newdata = newdata, 
                               type = 'prob')   
    
    ensemble_preds <- tibble(ID = rownames(newdata), 
                             method = 'ensemble', 
                             risk = ensemble_preds)
    
    pred_tbl <- rbind(single_preds, 
                      ensemble_preds)
    
    ## adding the plot order
    
    order_tbl <- tibble(ID = rownames(newdata), 
                        obs_order = 1:nrow(newdata))
    
    ## the classifier result median
    
    med_tbl <- pred_tbl %>% 
      group_by(ID) %>% 
      summarise(risk = median(risk, na.rm = TRUE)) %>% 
      mutate(method = 'median')
    
    rbind(pred_tbl, med_tbl) %>% 
      left_join(order_tbl, by = 'ID') %>% 
      arrange(obs_order, method)
    
  }
  
  plot_ml_risk <- function(pred_tbl, 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           plot_tag = NULL, 
                           interactive = FALSE) {
    
    ## plots the ml risk estimates given the output of the predict_ml() function
    
    pred_tbl <- pred_tbl %>%  
      mutate(method = globals$model_labels[method])
    
    med_tbl <- pred_tbl %>% 
      filter(method == 'Median')
    
    pred_tbl <- pred_tbl %>% 
      filter(method != 'Median')
    
    if(!interactive) {
      
      base_plot <- pred_tbl %>% 
        ggplot(aes(y = risk, 
                   x = reorder(ID, obs_order)))
      
    } else {
      
      base_plot <- pred_tbl %>% 
        ggplot(aes(y = risk, 
                   x = reorder(ID, obs_order), 
                   text = paste0('Patient: ', ID, 
                                 '\nRisk: ', signif(risk, 2), 
                                 '\nmethod: ', method)))
      
    }
    
    
    full_plot <- base_plot + 
      geom_hline(yintercept = 0.5, 
                 linetype = 'dashed') + 
      geom_violin(aes(group = ID), 
                  alpha = 0.25, 
                  fill = 'cornsilk') + 
      scale_y_continuous(limits = c(0, 1)) + 
      coord_flip() + 
      geom_point(aes(color = method), 
                 shape = 16, 
                 size = 2, 
                 alpha = 0.8, 
                 position = position_jitter(width = 0, height = 0.1)) + 
      geom_point(data = med_tbl, 
                 shape = 23, 
                 size = 3.5, 
                 fill = 'orangered3') + 
      scale_color_manual(values = set_names(globals$model_colors, globals$model_labels), 
                        name = 'ML Algorithm') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           y = 'Risk estimate', 
           x = 'Patient')
    
    if(interactive) {
      
      full_plot %>% 
        ggplotly(tooltip = 'text')
      
    } else {
      
      full_plot
      
    }
    
  }
  
  plot_ml_pred <- function(pred_tbl, 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           plot_tag = NULL, 
                           interactive = FALSE) {
    
    ## plots the ml predictions given the output of the predict_ml() function
    
    pred_tbl <- pred_tbl %>% 
      mutate(method = globals$model_labels[method], 
             prediction = ifelse(risk > 0.5, 'yes', 'no'))
    
    med_tbl <- pred_tbl %>% 
      filter(method == 'Median')
    
    pred_tbl <- pred_tbl %>% 
      filter(method != 'Median')
    
    
    if(!interactive) {
      
      base_plot <- pred_tbl %>% 
        ggplot(aes(y = prediction, 
                   x = reorder(ID, obs_order)))
      
    } else {
      
      base_plot <- pred_tbl %>% 
        ggplot(aes(y = prediction, 
                   x = reorder(ID, obs_order), 
                   text = paste0('Patient: ', ID, 
                                 '\nPrediction: ', prediction, 
                                 '\nRisk: ', signif(risk, 2), 
                                 '\nmethod: ', method)))
      
    }
    
    full_plot <- base_plot + 
      coord_flip() + 
      geom_point(data = med_tbl,
                 aes(color = risk), 
                 shape = 16, 
                 size = 7) + 
      geom_point(shape = 16, 
                 size = 2, 
                 alpha = 0.5, 
                 color = 'gray60', 
                 position = position_jitter(width = 0.1, height = 0.1)) + 
      scale_color_gradient2(low = 'steelblue3', 
                            mid = 'gray80', 
                            high = 'firebrick3', 
                            midpoint = 0.5, 
                            limits = c(0, 1), 
                            name = 'Median risk\nestimate') + 
      globals$common_theme + 
      labs(title = plot_title, 
           subtitle = plot_subtitle, 
           tag = plot_tag, 
           y = 'Prediction', 
           x = 'Patient')
    
    if(interactive) {
      
      full_plot %>% 
        ggplotly(tooltip = 'text')
      
    } else {
      
      full_plot
      
    }
    
  }
  
  plot_ml <- function(pred_tbl, plot_type = c('prediction', 'risk'), ...) {
    
    plot_type <- match.arg(plot_type[1], c('prediction', 'risk'))
    
    if(plot_type == 'prediction') {
      
      plot_ml_pred(pred_tbl = pred_tbl, ...)
      
    } else {
      
      plot_ml_risk(pred_tbl = pred_tbl, ...)
      
    }
    
    
    
  }
  
  format_prediction <- function(pred_tbl, 
                                response_txt = 'Any CT abnormality', 
                                path = NULL) {
    
    ## makes a nice table with the ML predictions
    
    out_tbl <- pred_tbl %>% 
      mutate(method = globals$model_labels[method], 
             response = response_txt, 
             risk = signif(risk, 3), 
             predicted_outcome = ifelse(risk > 0.5, 'present', 'absent')) %>% 
      select(ID, method, response, risk, predicted_outcome) %>% 
      set_names(c('Patient', 'Algoritm', 'Outcome', 'Risk estimate', 'Prediction'))
    
    if(!is.null(path)) {
      
      write_xlsx(out_tbl, path = path)
      
    }
    
    out_tbl
    
  }
  
# END ----