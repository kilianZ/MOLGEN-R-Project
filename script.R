# MOLGEN R Project 
# Citrate Concentration Data Analysis Script 
# Written By: Kilian Zindel
# 
# ___
# TODO: 
# - [x] Reorganize CSV file
# - [x] Read CSV file into data.frame
# - [x] Calcualte mean and sd for each datapoint 
# - [ ] Output to CSV file, format tables in excel 
# - [ ] Plot: time on the x-axis, citrate concentration [OD420] on the y-axis 


# read in data from .csv file
data <- read.csv("Data_Citrate_concentrations_reorganized.csv", header=TRUE, sep=",")

# rename the columns to make things easier 
colnames(data) <- c("Gen", "time", "repl", "OD420")

# calculate mean and standard deviation for each data points
# create table to store results 
table <- data.frame(Generation=character(), Replicant=numeric(), mean=numeric(), stddev=numeric())
Generations <- list('A', 'B', 'C', 'D')

# get mean and stddev for OD420 values of each generation in each replicant
for (generation in Generations) {
  for (replicant in 1:5) {
    OD420 = subset(data, repl == replicant & Gen == generation)[['OD420']]
    dpmean = mean(OD420)
    dpsd = sd(OD420)
    # print(paste("For replicant", replicant, "and generation", generation, "the mean is", dpmean, "and the standard deviation is", dpsd))
    # add to table 
    table <- rbind(table, data.frame(Generation=generation, Replicant=replicant, mean=dpmean, stddev=dpsd))
  }
}

print("TABLE 1: mean and standard deviation for each generation in each replicant.")
print(table)

table2 <- data.frame(Generation=character(), mean=numeric(), stddev=numeric())

# get mean and stddev for OD420 valyes of each generation across all replicants 
for (generation in Generations) {
  datapoint = subset(data, Gen == generation)[['OD420']]
  dpmean = mean(datapoint)
  dpsd = sd(datapoint) 
  table2 <- rbind(table2, data.frame(Generation=generation, mean=dpmean, stddev=dpsd))
}

print("TABLE 2: mean and standard deviation for each generation across all replicants combined.")
print(table2)

# make plots
for (replicant in 1:5) {
  # open png device
  png(paste("plots/plot", 1, replicant, sep='-'), width=800, height=600)
  
  genA = subset(data, repl == replicant & Gen == 'A')[['OD420']]
  genB = subset(data, repl == replicant & Gen == 'B')[['OD420']]
  genC = subset(data, repl == replicant & Gen == 'C')[['OD420']]
  genD = subset(data, repl == replicant & Gen == 'D')[['OD420']]
  
  title = paste(paste("plot", 1, replicant, sep='-'), "Citrate Concentrations in the culture medium of E. coli", sep=": ")
  plot(genA, type = 'l', ylim=c(0,400), col='blue', main=title, ylab='OD420', xlab='Time (h)')
  lines(genB, type = 'l', col='red')  
  lines(genC, type = 'l', col='purple')
  lines(genD, type = 'l', col='green')
  legend("bottomleft", legend=c("Gen A", "Gen B", "Gen C", "Gen D"), col=c("blue", "red", "purple", "green"), lty=1, pch=1)
  
  # close png device (write) 
  dev.off()
}

# make plot with OD420 averaged across all 5 replicants
# open png device
png("plots/plot-2", width=800, height=600)

# Subset Gen A data across all replicants, group by time, calculate mean
genA_data <- subset(data, Gen == 'A')
genA_mean <- aggregate(OD420 ~ time, data = genA_data, mean, na.rm = TRUE)
genB_data <- subset(data, Gen == 'B')
genB_mean <- aggregate(OD420 ~ time, data = genB_data, mean, na.rm = TRUE)
genC_data <- subset(data, Gen == 'C')
genC_mean <- aggregate(OD420 ~ time, data = genC_data, mean, na.rm = TRUE)
genD_data <- subset(data, Gen == 'D')
genD_mean <- aggregate(OD420 ~ time, data = genD_data, mean, na.rm = TRUE)

title = "Plot-2: Citrate Concentrations in the culture medium of E. coli"
plot(genA_mean, type = 'l', ylim=c(0,400), col='blue', main=title, ylab='OD420', xlab='Time (h)')
lines(genB_mean, type = 'l', col='red')  
lines(genC_mean, type = 'l', col='purple')
lines(genD_mean, type = 'l', col='green')
legend("bottomleft", legend=c("Gen A", "Gen B", "Gen C", "Gen D"), col=c("blue", "red", "purple", "green"), lty=1, pch=1)

# close png device (write) 
dev.off()


