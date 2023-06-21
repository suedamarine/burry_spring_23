# calculate min/max correlation


# manually enter data - according to 250 and 100 grid numbers
temp_a <- c(172970000, 74970000, 10192000, 255290000, 31213000)
temp_b <- c(251437500, 129375000, 12487500, 289125000, 50625000)
cor(temp_a, temp_b)
linearMod <- lm(temp_a ~temp_b)
print(linearMod)
summary(linearMod)
actuals_preds <- data.frame(cbind(actuals=temp_a, predicts=temp_b))
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min)/ apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicts - actuals_preds$actuals))/actuals_preds$actuals)

# manually enter data - according to 250 and 100 grid numbers

temp_c <- c(213.64, 197.96, 58.31, 239.12, 61.74)
temp_d <- c(317, 326, 69, 348, 177)
cor(temp_c, temp_d)
linearMod_2 <- lm(temp_c ~temp_d)
print(linearMod_2)
summary(linearMod_2)
actuals_preds_2 <- data.frame(cbind(actuals_2=temp_c, predicts_2=temp_d))
correlation_accuracy_2 <- cor(actuals_preds_2)
min_max_accuracy_2 <- mean(apply(actuals_preds_2, 1, min)/ apply(actuals_preds_2, 1, max))
mape_2 <- mean(abs((actuals_preds_2$predicts_2 - actuals_preds_2$actuals_2))/actuals_preds_2$actuals_2)






