library(mRMRe)
file_n<-paste0("F:/Thesis/DataMing+MachieLeaning/Diabetes/Normalized_Dataset_Train80p.csv")
df <- read.csv(file_n, header = TRUE)
for (i in 1:17){
  df[[i]] <- as.numeric(df[[i]])
}
f_data <- mRMR.data(data = data.frame(df))
results <- mRMR.classic("mRMRe.Filter", data = f_data, target_indices = 17,
                        feature_count = 10)
print(solutions(results))

print(results@scores)