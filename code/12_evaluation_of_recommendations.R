
#---------------------------------------------------------------------------------------
# 12 - EVALUATION OF THE RECOMMENDATIONS
#---------------------------------------------------------------------------------------

# comparing recommendations (predictions) with purchases (true values)
results <- evaluate(x = kfold_eval, method = "IBCF", n = seq(10, 100, 10))

# extracting confusion matrix from evaluation results
head(getConfusionMatrix(results)[[1]])

# creating avg version
columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)

# ROC curve
plot(results, annotate = TRUE, main = "ROC curve")

# plot precision/recall curve

plot(results, "prec/rec", annotate = TRUE, main = "Precision-recall")

