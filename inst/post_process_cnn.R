#install.packages("BiocManager")
#install.packages("EBImage") #sudo apt-get install libfftw3-dev pour installer la library fftw necessaire
#BiocManager::install("EBImage")

library(keras)
library(EBImage)
library(stringr)
library(pbapply)
library(tfruns)
library(data.table)
library(purrr)
library(dementiaproject)
library(ggplot2)

#select 4 png pour chaque MRI sessions / anat
path_root <- "~/GENERIC/dementiaproject/"
path_data <- "/srv/OASIS_DATA/"

#test dashboard
list_dropout_dir <- list.dirs(path = file.path(path_data, "dropout_tuning"), recursive = FALSE)
view_run(list_dropout_dir[3])
#check dernier run
view_run(list_dropout_dir[length(list_dropout_dir)-2])

#compare 2 runs
compare_runs(runs = c(list_dropout_dir[5], list_dropout_dir[6]))

#concat des tables summary
list_result <- list.files(path = file.path(path_data, "results_4_class"), pattern = "summary_result.Rdata",  recursive = TRUE, full.names = TRUE)
all_res <- rbindlist(map(list_result, ~loadRData(.)[, .(loss_function, optimizer, good_classif_rate, sensitivity, specificity)]))
all_res[,id :=.I]
all_res[order(-good_classif_rate)]

#best good classif actuellement
dir_res <- "results_8"
prev_res <- loadRData(file.path(path_data, "results", dir_res, "resultat_test.Rdata"))
conf_mat <- loadRData(file.path(path_data, "results", dir_res, "conf_matrix.Rdata"))
sum <- loadRData(file.path(path_data, "results", dir_res, "summary_result.Rdata"))
dt_distrib <- loadRData(file.path(path_data, "results", dir_res, "data_distrib.Rdata"))
prev_res$cdr <- dt_distrib[set == "test"]$cdr_ref
prev_res[pred == obs, res := "ok"]
prev_res[pred != obs, res := "nok"]
prev_res[, nb_ok := nrow(.SD[res == "ok"]), by = "cdr"]
prev_res[, nb_nok := nrow(.SD[res == "nok"]), by = "cdr"]
prev_cat <- unique(prev_res[,.(cdr, nb_ok, nb_nok)])
prev_cat <- melt(prev_cat, "cdr")
prev_cat[, val_per := value*100 / sum(.SD$value), by = cdr ]
ggplot(prev_cat,
       aes(x = factor(cdr), y =  value, fill = variable)) +
  geom_bar(stat="identity", position="stack") + scale_fill_manual(values = c("#3CB371","#F08080") ,name = "Prévision", labels = c("OK", "KO")) +
  geom_label(aes(label = round(val_per, 1)), position =position_stack(vjust = 0.5), show.legend = FALSE, size = 5) + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Nombre de diagnostics")


test <- loadRData(file.path(path_data, "results", "res_tfruns.Rdata"))
setDT(test)
test_gg <- melt(test[ flag_loss_function == "sparse_categorical_crossentropy", .(metric_loss, metric_accuracy, metric_val_loss, metric_val_accuracy, flag_learning_rate )], id.vars = "flag_learning_rate")
test_gg[, set := "train"]
test_gg[grepl(pattern = "val", variable), set := "validation"]
test_gg[grepl(pattern = "loss", variable), variable := "loss"]
test_gg[grepl(pattern = "accuracy", variable), variable := "accuracy"]
test_gg$flag_learning_rate <- factor(test_gg$flag_learning_rate, levels = c(0.001, 0.0005, 0.0002, 0.0001))

ggplot(test_gg,
       aes(x = factor(flag_learning_rate), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~set) + scale_fill_discrete(name = "") +
  labs(x = "Learning Rate", y = "Score") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5)


#re post process
list_rep <- list.dirs(path = file.path(path_data, "results"),  recursive = FALSE, full.names = TRUE)
list_rep <- list_rep[!grepl(list_rep, pattern = "15")]
list_rep <- list.files(path = file.path(path_data, "results"), pattern = "summary_result.Rdata",  recursive = TRUE, full.names = TRUE)
all_res <- rbindlist(map(file.path(list_rep, "summary_result.Rdata"), ~loadRData(.)))
list_rep[7]



# map(list_rep, function(x) {
#   cf_mat <- loadRData(fileName = file.path(x, "conf_matrix.Rdata"))
#   proba_pred <- loadRData(fileName = file.path(x, "probabilities.Rdata"))
#   resultat_test  <- loadRData(fileName = file.path(x, "resultat_test.Rdata"))
#   resultat_test$pred_proba <- max.col(proba_pred)
#   resultat_test[,pred := NA]
#   resultat_test[pred_proba == 1, pred := 0]
#   resultat_test[pred_proba == 2, pred := 0.5]
#   resultat_test[pred_proba == 3, pred := 1]
#   resultat_test[pred_proba == 4, pred := 2]
#   resultat_test$pred_proba <- NULL
#   conf_matrix <- confusionMatrix(data = factor(resultat_test$pred, levels = c(0, 0.5, 1, 2)), reference = factor(resultat_test$obs,levels = c(0, 0.5, 1, 2)))
#   summary_result <- loadRData(fileName = file.path(x, "summary_result.Rdata"))
#   sensitivity_mean <- mean(conf_matrix$byClass[,1], na.omit= TRUE)
#   specificity_mean <- mean(conf_matrix$byClass[,2], na.omit= TRUE)
#   #summary_result[, Accuracy := conf_matrix$overall["Accuracy"]]
#   summary_result[, good_classif_rate := nrow(resultat_test[pred == obs]) / nrow(resultat_test)]
#   summary_result[, ":=" (sensitivity = sensitivity_mean,
#                           specificity = specificity_mean)]
#
#   save(resultat_test, file = file.path(x, "resultat_test.Rdata"))
#   save(conf_matrix, file = file.path(x, "conf_matrix.Rdata"))
#   save(summary_result, file = file.path(x, "summary_result.Rdata"))
#   print("Score recalcultation")
#
# })


##### POST PROCESS 2 CLASS #####


list_rep <- list.dirs(path = file.path(path_data, "results_2_class"),  recursive = FALSE, full.names = TRUE)

res_2_class <- rbindlist(map(list_rep, function(x) {
  print(x)
  summary_result <- loadRData(fileName = file.path(x, "summary_result.Rdata"))
  model <- load_model_hdf5(filepath = file.path(x, "model.h5"))
  history <-  loadRData(fileName = file.path(x, "history.Rdata"))
  conf_matrix <- loadRData(fileName = file.path(x, "conf_matrix.Rdata"))
  summary_result[, loss_train := history$metrics$loss[length(history$metrics$loss)]]
  summary_result[, accuracy_train := history$metrics$accuracy[length(history$metrics$accuracy)]]
  summary_result[, loss_val := history$metrics$val_loss[length(history$metrics$val_loss)]]
  summary_result[, accuracy_val := history$metrics$val_accuracy[length(history$metrics$val_accuracy)]]
  summary_result[, learning_rate := get_config(model$optimizer)$learning_rate]
  summary_result[, nb_class := 2]
  summary_result[, accuracy_test := conf_matrix$overall['Accuracy']]
  return(summary_result[, .(loss_function , optimizer, learning_rate, cut_selection, nb_train_img, nb_test_img, loss_train, accuracy_train, loss_val, accuracy_val, sensitivity , specificity , accuracy_test, nb_class)])
}))

res_2_class_gg <- melt(res_2_class[ loss_function  == "binary_crossentropy", .(learning_rate   , loss_train, accuracy_train, loss_val, accuracy_val, sensitivity, specificity, accuracy_test )], id.vars = "learning_rate")
setDT(res_2_class_gg)
res_2_class_gg[, set := "test"]
res_2_class_gg[grepl(pattern = "val", variable), set := "validation"]
res_2_class_gg[grepl(pattern = "train", variable), set := "train"]
res_2_class_gg[grepl(pattern = "loss", variable), variable := "loss"]
res_2_class_gg[grepl(pattern = "accuracy", variable), variable := "accuracy"]
res_2_class_gg$learning_rate <- factor(formatC(res_2_class_gg$learning_rate, format = "e", digits = 1), levels = c("1.0e-03","5.0e-04", "2.0e-04", "1.0e-04", "5.0e-05", "2.0e-05"))
res_2_class_gg$set <- factor(res_2_class_gg$set, levels = c("train", "validation", "test"))

#plot loss acc vs lr
ggplot(res_2_class_gg[!variable %in% c("sensitivity", "specificity")],
       aes(x = factor(learning_rate), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~set) + scale_fill_discrete(name = "") +
  labs(x = "Learning Rate", y = "Score") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5)

#plot sensitivity specificity vs lr on test group
ggplot(res_2_class_gg[variable %in% c("sensitivity", "specificity")],
       aes(x = factor(learning_rate), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_discrete(name = "") +
  labs(x = "Learning Rate", y = "Score") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5)


res_history <- rbindlist(map(list_rep, function(x) {
  print(x)
  summary_result <- loadRData(fileName = file.path(x, "summary_result.Rdata"))
  model <- load_model_hdf5(filepath = file.path(x, "model.h5"))
  history <-  loadRData(fileName = file.path(x, "history.Rdata"))
  dt_history <- data.table(epoch = seq(1, length(history$metrics$loss), 1),
                           loss = history$metrics$loss,
                           accuracy     = history$metrics$accuracy    ,
                           val_loss     = history$metrics$val_loss    ,
                           val_accuracy = history$metrics$val_accuracy,
                           loss_function = summary_result$loss_function,
                           optimizer = summary_result$optimizer,
                           learning_rate = get_config(model$optimizer)$learning_rate,
                           id = basename(x))
  return(dt_history)
}))

res_history_gg <- melt(res_history[ loss_function  == "binary_crossentropy", .(epoch, loss , accuracy, accuracy, val_accuracy,val_loss, learning_rate)], id.vars = c("learning_rate", "epoch"))
setDT(res_history_gg)
res_history_gg[, set := "train"]
res_history_gg[grepl(pattern = "val", variable), set := "validation"]
res_history_gg[grepl(pattern = "loss", variable), variable := "loss"]
res_history_gg[grepl(pattern = "accuracy", variable), variable := "accuracy"]
res_history_gg$learning_rate <- factor(formatC(res_history_gg$learning_rate, format = "e", digits = 1), levels = c("1.0e-03","5.0e-04", "2.0e-04", "1.0e-04", "5.0e-05", "2.0e-05"))
res_history_gg$set <- factor(res_history_gg$set, levels = c("train", "validation"))

#plot loss acc vs lr

ggplot(hist) + geom_point(aes(x = epoch, y = loss, col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "Architecture") + ggtitle("Amélioration par 'fine-tuning' du modèle VGG16 - Loss [train]") +
  scale_color_discrete(name = "Méthode : ", labels = c("Feature-extraction", "Fine-tuning (1 couche)", "Fine-tuning (2 couches)", "Fine-tuning (3 couches)")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +
  geom_text(aes(x=105, label="Fine-tuning", y=0.4), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  geom_text(aes(x=95, label="Feature-extraction", y=0.4), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)




ggplot(res_history_gg[set %in% c("train", "validation") & variable == "loss"],
       aes(x = epoch, y =  value, col = learning_rate )) + geom_point() + geom_line() +
  scale_color_discrete(name = "Learning Rate : ") +  facet_wrap(~set, scale = "free") +
  labs(x = "Epoch", y = "Loss") +   theme(legend.position = "bottom")


ggplot(res_history_gg[set %in% c("train", "validation") & variable == "accuracy"],
       aes(x = epoch, y =  value, col = learning_rate )) + geom_point() + geom_line() +
  scale_color_discrete(name = "Learning Rate : ") +  facet_wrap(~set, scale = "free") +
  labs(x = "Epoch", y = "Accuracy") +   theme(legend.position = "bottom")


dir_res <- unique(res_history[loss_function == "binary_crossentropy" & learning_rate == min(res_history$learning_rate)]$id)
prev_res <- loadRData(file.path(path_data, "results_2_class", dir_res, "resultat_test.Rdata"))
conf_mat <- loadRData(file.path(path_data, "results_2_class", dir_res, "conf_matrix.Rdata"))
sum <- loadRData(file.path(path_data, "results_2_class", dir_res, "summary_result.Rdata"))
dt_distrib <- loadRData(file.path(path_data, "results_2_class", dir_res, "data_distrib.Rdata"))
prev_res$cdr <- dt_distrib[set == "test"]$cdr_ref
prev_res[pred == obs, res := "ok"]
prev_res[pred != obs, res := "nok"]
prev_res[, nb_ok := nrow(.SD[res == "ok"]), by = "cdr"]
prev_res[, nb_nok := nrow(.SD[res == "nok"]), by = "cdr"]
prev_cat <- unique(prev_res[,.(cdr, nb_ok, nb_nok)])
prev_cat <- melt(prev_cat, "cdr")
prev_cat[, val_per := value*100 / sum(.SD$value), by = cdr ]
ggplot(prev_cat,
       aes(x = factor(cdr), y =  value, fill = variable)) +
  geom_bar(stat="identity", position="stack") + scale_fill_manual(values = c("#3CB371","#F08080") ,name = "Prévision", labels = c("OK", "KO")) +
  geom_label(aes(label = round(val_per, 1)), position =position_stack(vjust = 0.5), show.legend = FALSE, size = 5) + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Nombre de diagnostics")


table <- data.frame(conf_mat$table)

library(dplyr)
plotTable <- table %>%
  mutate(goodbad = ifelse(table$Prediction == table$Reference, "good", "bad")) %>%
  group_by(Reference) %>%
  mutate(prop = Freq/sum(Freq))

# fill alpha relative to sensitivity/specificity by proportional outcomes within reference groups (see dplyr code above as well as original confusion matrix for comparison)
ggplot(data = plotTable, mapping = aes(x = Reference, y = Prediction, fill = goodbad, alpha = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), vjust = .5, fontface  = "bold", alpha = 1) +
  scale_fill_manual(values = c(good = "#009194", bad = "red")) +
  theme_bw() +
  xlim(rev(levels(table$Reference)))


cm <- confusionMatrix(factor(y.pred), factor(y.test), dnn = c("Prediction", "Reference"))

ggplot(as.data.frame(cm$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("Class_1","Class_2","Class_3","Class_4")) +
  scale_y_discrete(labels=c("Class_4","Class_3","Class_2","Class_1"))








##### POST PROCESS 2 CLASS ANALYSE CUT #####


list_rep <- list.dirs(path = file.path(path_data, "results_cut_2_class"),  recursive = FALSE, full.names = TRUE)

res_2_class <- rbindlist(map(list_rep, function(x) {
  print(x)
  summary_result <- loadRData(fileName = file.path(x, "summary_result.Rdata"))
  model <- load_model_hdf5(filepath = file.path(x, "model.h5"))
  history <-  loadRData(fileName = file.path(x, "history.Rdata"))
  conf_matrix <- loadRData(fileName = file.path(x, "conf_matrix.Rdata"))
  summary_result[, loss_train := history$metrics$loss[length(history$metrics$loss)]]
  summary_result[, accuracy_train := history$metrics$accuracy[length(history$metrics$accuracy)]]
  summary_result[, loss_val := history$metrics$val_loss[length(history$metrics$val_loss)]]
  summary_result[, accuracy_val := history$metrics$val_accuracy[length(history$metrics$val_accuracy)]]
  summary_result[, learning_rate := get_config(model$optimizer)$learning_rate]
  summary_result[, nb_class := 2]
  summary_result[, accuracy_test := conf_matrix$overall['Accuracy']]
  return(summary_result[, .(loss_function , optimizer, learning_rate, cut_selection, nb_train_img, nb_test_img, loss_train, accuracy_train, loss_val, accuracy_val, sensitivity , specificity , accuracy_test, nb_class)])
}))

res_2_class_gg <- melt(res_2_class[ loss_function  == "binary_crossentropy", .(cut_selection   ,nb_train_img, nb_test_img , loss_train, accuracy_train, loss_val, accuracy_val, sensitivity, specificity, accuracy_test )], id.vars = c("cut_selection", "nb_train_img", "nb_test_img"))
setDT(res_2_class_gg)
res_2_class_gg[, set := "test"]
res_2_class_gg[grepl(pattern = "val", variable), set := "validation"]
res_2_class_gg[grepl(pattern = "train", variable), set := "train"]
res_2_class_gg[grepl(pattern = "loss", variable), variable := "loss"]
res_2_class_gg[grepl(pattern = "accuracy", variable), variable := "accuracy"]
res_2_class_gg$set <- factor(res_2_class_gg$set, levels = c("train", "validation", "test"))
res_2_class_gg[, nb_cut := length(strsplit(cut_selection, split = "_")[[1]]), by = cut_selection]

#plot loss acc vs lr
ggplot(res_2_class_gg[!variable %in% c("sensitivity", "specificity")],
       aes(x = factor(nb_cut), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~set) + scale_fill_discrete(name = "") +
  labs(x = "Nombre de coupes selectionnées par diagnostic", y = "Score") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5)

#plot sensitivity specificity vs lr on test group
ggplot(res_2_class_gg[variable %in% c("sensitivity", "specificity")],
       aes(x = factor(nb_cut), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_discrete(name = "") +
  labs(x = "Nombre de coupes selectionnées par diagnostic", y = "Score") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5)


#plot sensitivity specificity vs lr on test group
ggplot(melt(unique(res_2_class_gg[, .(nb_cut, nb_train_img, nb_test_img)]), id.vars = "nb_cut"),
       aes(x = factor(nb_cut), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_discrete(name = "Echantillon", labels = c("Entrainement", "Test")) + theme_minimal() +
  labs(x = "Nombre de coupes selectionnées par diagnostic", y = "Nombre d'images") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5)




res_history <- rbindlist(map(list_rep, function(x) {
  print(x)
  summary_result <- loadRData(fileName = file.path(x, "summary_result.Rdata"))
  model <- load_model_hdf5(filepath = file.path(x, "model.h5"))
  history <-  loadRData(fileName = file.path(x, "history.Rdata"))
  dt_history <- data.table(epoch = seq(1, length(history$metrics$loss), 1),
                           loss = history$metrics$loss,
                           accuracy     = history$metrics$accuracy    ,
                           val_loss     = history$metrics$val_loss    ,
                           val_accuracy = history$metrics$val_accuracy,
                           loss_function = summary_result$loss_function,
                           optimizer = summary_result$optimizer,
                           learning_rate = get_config(model$optimizer)$learning_rate,
                           cut_selection = summary_result$cut_selection,
                           id = basename(x))
  return(dt_history)
}))

res_history_gg <- melt(res_history[ loss_function  == "binary_crossentropy", .(epoch, cut_selection , accuracy, loss, val_accuracy, val_loss)], id.vars = c("cut_selection", "epoch"))
setDT(res_history_gg)
res_history_gg[, set := "train"]
res_history_gg[grepl(pattern = "val", variable), set := "validation"]
res_history_gg[grepl(pattern = "loss", variable), variable := "loss"]
res_history_gg[grepl(pattern = "accuracy", variable), variable := "accuracy"]
res_history_gg$set <- factor(res_history_gg$set, levels = c("train", "validation"))
res_history_gg[, nb_cut := length(strsplit(cut_selection, split = "_")[[1]]), by = cut_selection]

#plot loss acc vs lr
ggplot(res_history_gg[variable == "loss" & set == "train"],
       aes(x = epoch, y =  value, col = as.factor(nb_cut))) + geom_point() + geom_line() +
  scale_color_discrete(name = "Nombre de coupes") +
  labs(x = "Epoch", y = "Loss") + ggtitle("Fonction de coût sur la partie train")

ggplot(res_history_gg[variable == "loss" & set == "validation"],
       aes(x = epoch, y =  value, col = as.factor(nb_cut))) + geom_point() + geom_line() + facet_wrap(~nb_cut, scale = "free") +
  scale_color_discrete(name = "") +
  labs(x = "Epoch", y = "Loss")  + ggtitle("Fonction de coût sur la partie validation")

ggplot(res_history_gg[variable == "accuracy" & set == "train"],
       aes(x = epoch, y =  value, col = as.factor(nb_cut))) + geom_point() + geom_line() +
  scale_color_discrete(name = "Nombre de coupes") +
  labs(x = "Epoch", y = "Accuracy")  + ggtitle("Accuracy sur la partie train")

ggplot(res_history_gg[variable == "accuracy" & set == "validation"],
       aes(x = epoch, y =  value, col = as.factor(nb_cut))) + geom_point() + geom_line() + facet_wrap(~nb_cut, scale = "free") +
  scale_color_discrete(name = "") +
  labs(x = "Epoch", y = "Accuracy")  + ggtitle("Accuracy sur la partie validation")



#relaunch on unique test batch
path_raw <- file.path(path_data, "data_raw_t1")
path_selected <- file.path(path_data, "batch_test_selected")
path_processed <- file.path(path_data, "batch_test_processed")
# batch_test <- dementiaproject::cut_selection(path_raw, path_selected, path_processed, cut_list = c(143))
batch_test <- dementiaproject::loadRData(file.path(path_processed, "dataset.Rdata"))

testData  <- dementiaproject::extract_feature(path_dir = path_processed, data_ref = batch_test, width = 224, height = 224, labelsExist = TRUE, nb_class = 2)
test_array <- t(testData$X)
dim(test_array) <- c(224, 224, nrow(testData$X), 1)
test_array <- aperm(test_array, c(3,1,2,4)) # Reorder dimensions
test_array_y <- as.numeric(as.logical(testData$y))
rm(testData)

library(caret)
list_rep <- list.dirs(path = file.path(path_data, "results_cut_2_class"),  recursive = FALSE, full.names = TRUE)
relaunch_batch_test <- rbindlist(map(list_rep, function(x) {
  print(x)
  model <- load_model_hdf5(filepath = file.path(x, "model.h5"))
  summary_result <- loadRData(fileName = file.path(x, "summary_result.Rdata"))
  predictions <-  predict_classes(model, test_array)
  resultat_test <- data.table()
  resultat_test$obs <- as.numeric(test_array_y)
  resultat_test$pred <- as.numeric(predictions)
  conf_matrix <- confusionMatrix(data = as.factor(resultat_test$pred), reference = as.factor(resultat_test$obs))
  sensitivity_mean <- conf_matrix$byClass["Sensitivity"]
  specificity_mean <- conf_matrix$byClass["Specificity"]
  accuracy <- conf_matrix$overall["Accuracy"]
  precision <- conf_matrix$byClass["Precision"]
  recall <- conf_matrix$byClass["Recall"]

  data.table(cut = summary_result$cut_selection, sens = sensitivity_mean, spec = specificity_mean, accuracy = accuracy, precision = precision, recall = recall)
  }))

relaunch_batch_test[, nb_cut := length(strsplit(cut, split = "_")[[1]]), by = cut]
setnames(relaunch_batch_test, c("sens", "spec"),  c("sensitivity", "specificity"))
relaunch_batch_test_gg <- melt(relaunch_batch_test, id.vars = c("cut", "nb_cut"))

#plot sensitivity specificity vs lr on test group
ggplot(relaunch_batch_test_gg[variable %in% c("sensitivity", "specificity")],
       aes(x = factor(nb_cut), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_discrete(name = "") +
  labs(x = "Nombre de coupes selectionnées par diagnostic", y = "Score") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5) + ggtitle("Scores à echantillon test constant (2775 images)")

ggplot(relaunch_batch_test_gg[!variable %in% c("sensitivity", "specificity")],
       aes(x = factor(nb_cut), y =  value, fill = variable)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_discrete(name = "") + facet_wrap(~variable) +
  labs(x = "Nombre de coupes selectionnées par diagnostic", y = "Score") +  geom_label(aes(label = round(value, 2)), position=position_dodge(.9), show.legend = FALSE, size = 5) + ggtitle("Scores à echantillon test constant (2775 images)")




ggplot(res_history_gg[set %in% c("train", "validation") & variable == "loss"],
       aes(x = epoch, y =  value, col = as.factor(nb_cut) )) + geom_point() + geom_line() +
  scale_color_discrete(name = "Nombre de coupes par IRM : ") +  facet_wrap(~set, scale = "free") +
  labs(x = "Epoch", y = "Loss") +   theme(legend.position = "bottom")


ggplot(res_history_gg[set %in% c("train", "validation") & variable == "accuracy"],
       aes(x = epoch, y =  value, col = as.factor(nb_cut)  )) + geom_point() + geom_line() +
  scale_color_discrete(name = "Nombre de coupes par IRM  : ") +  facet_wrap(~set, scale = "free") +
  labs(x = "Epoch", y = "Accuracy") +  theme(legend.position = "bottom")



# POST PROCESS 4 CLASS


#best good classif actuellement
dir_res <- "results_6"
prev_res <- loadRData(file.path(path_data, "results", dir_res, "resultat_test.Rdata"))
conf_mat <- loadRData(file.path(path_data, "results", dir_res, "conf_matrix.Rdata"))
sum <- loadRData(file.path(path_data, "results", dir_res, "summary_result.Rdata"))
dt_distrib <- loadRData(file.path(path_data, "results", dir_res, "data_distrib.Rdata"))
model <- load_model_hdf5(filepath = file.path(path_data, "results", dir_res,  "model.h5"))

library(tensorflow)
library(keras)

fresh_model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                input_shape = c(224,224,1)) %>% #format image width_target x height_target
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 4, activation = "softmax") #car 2 catégories #softmax

summary(fresh_model)

fresh_model %>% load_model(filepath = file.path(path_data, "results", dir_res, "saved_model.pb"))
fresh_model %>% evaluate(test_images, test_labels, verbose = 0)


#resultats finaux




