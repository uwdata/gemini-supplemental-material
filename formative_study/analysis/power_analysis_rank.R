#Set working directory
# setwd("")
df <- fromJSON("../data-pilot.json", flatten=TRUE) #Pilot Data


#Exclude the participants who self-reported color deficiency
#df = df[df$vision_deficiency=='no',];
#Exclude the 4 who responsed inappropriate text as their rationales (e.g., random text, ...)
df = df[df$userKey != 'WtDRM2RiuXcZcuS2Dhypf0OwGq13',];
df = df[df$userKey != 'aAMc8mmmzzgqxyHWxDUFDHUtHT02',];
df = df[df$userKey != '9mU8fcUNGxeRbjiuvgWDhGXPApA3',];
df = df[df$userKey != 'nLm0CjOZj4V2beGj4V464KsM6K83',];
df = df[df$userKey != '1R8bRFWzuIhNiRandt7SpXabpDp2',];
df = df[df$userKey != 'VdD0a9nNQZgnQGriLXNaXgi0WMC2',];
df = df[df$userKey != 'EOMSL7mV9NgaDpYo3dzRwo5jiGC2',];





#Exclude 2 responses that took more than an hour.
df = df[df$compTime <= 15*60*1000,];

s1 = df[df$designName=='stage1-rank1',]$rank
s2r1 = df[df$designName=='stage2-rank1',]$rank
s2r2 = df[df$designName=='stage2-rank2',]$rank
s2r3 = df[df$designName=='stage2-rank3',]$rank
s3 = df[df$designName=='stage3-rank1',]$rank
df = cbind(s2r1, s2r2, s2r3, s1, s3)

set.seed(123)  # to make the results reproducible

possible.ns <- seq(from=50, to=100, by=10)     # The sample sizes we'll be considering
alpha <- 0.05                                    # Standard significance level
sims <- 1000                               # Number of simulations to conduct for each N

#compute power for each test assuming observed staged animations' sd and non staged animations' sd, and mean for staged animations which is effect_size more than mean of average non-staged animations
# effect_size = 2.0

#calculate sds and mean
means <- colMeans(df)
sds <- (apply(df, 2, sd))
# s2r1Mean <- means['s2r3'] - effect_size



#will find power for seeing at least a significant friendman test, and at least n significant posthoc comparison, at lesat two significant post hoc comparisons, etc
#only posthoc comparisons we care about are the ones that compare staged animations to others
pairN = 10
results <- data.frame(condition= c("result"))
results.posthoc <- vector(length=pairN);
for (i in 1:pairN){
  results.posthoc[[i]] <- data.frame(condition= c("result"))
}

# first create the dataframe with NA in all the cells
for (k in 1:length(possible.ns)){
  results[, as.character(possible.ns[k])] = rep(NA, 1)
  for (i in 1:pairN){
    results.posthoc[[i]][, as.character(possible.ns[k])] = rep(NA, 1)
  }
}

significant.experiments.posthoc <- vector(length=pairN);
for (i in 1:pairN){
  significant.experiments.posthoc[[i]] <- data.frame(condition= c("result"))
}


for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  print (N)
  significant.experiments <- rep(NA, sims)
  for (i in 1:pairN){
    significant.experiments.posthoc[[i]] <- rep(NA, sims)
  }
  for (i in 1:sims){ #for each simulation of that N
    s1 <- round(rnorm(N, means['s1'], sds['s1'])) #simulate ranking for s1 transitions
    # s2r1 <- round(rnorm(N, s2r1Mean, sds['s2r1'])) #simulate ranking for s2r1 transitions
    s2r1 <- round(rnorm(N, means['s2r1'], sds['s2r1'])) #simulate ranking for s2r1 transitions
    s2r2 <- round(rnorm(N, means['s2r2'], sds['s2r2'])) #simulate ranking for s2r2 transitions
    s2r3 <- round(rnorm(N, means['s2r3'], sds['s2r3'])) #simulate ranking for s2r3 transitions
    s3 <- round(rnorm(N, means['s3'], sds['s3'])) #simulate ranking for s2r3 transitions

    s1 <- ifelse(s1 > 5, 5, s1) #make sure they aren't below 1 or over 6
    s1 <- ifelse(s1 < 1, 1, s1) #make sure they aren't below 1 or over 6
    s2r1 <- ifelse(s2r1 < 1, 1, s2r1)
    s2r1 <- ifelse(s2r1 > 5, 5, s2r1)
    s2r2 <- ifelse(s2r2 < 1, 1, s2r2)
    s2r2 <- ifelse(s2r2 > 5, 5, s2r2)
    s2r3 <- ifelse(s2r3 < 1, 1, s2r3)
    s2r3 <- ifelse(s2r3 > 5, 5, s2r3)
    s3 <- ifelse(s3 < 1, 1, s3)
    s3 <- ifelse(s3 > 5, 5, s3)


    #put the data together
    fake_ranking <- cbind(s1, s2r1, s2r2, s2r3, s3)
    significant.experiments[i] <- (friedman.test(fake_ranking)$p.value <= alpha)

    Value <- c(s1, s2r1, s2r2, s2r3, s3)
    Group <- factor(c(rep("rank.s1",N),rep("rank.s2r1",N),rep("rank.s2r2",N),rep("rank.s2r3",N),rep("rank.s3",N)))
    p <- pairwise.wilcox.test(Value, Group, p.adj="bonferroni", exact=F, paired=T)

    count = sum(c(p$p.value) <= alpha, na.rm=TRUE)
    for (k in 1:pairN){
      if(count > k-1) {
        significant.experiments.posthoc[[k]][i] = TRUE
      }else{
        significant.experiments.posthoc[[k]][i] = FALSE
      }
    }

  }

  results[1,as.character(N)] <- mean(significant.experiments)
  for (k in 1:pairN){
    results.posthoc[[k]][1,as.character(N)] <- mean(significant.experiments.posthoc[[k]])
  }
}

print("Probability of detecting significant friedman test")
print(results)
print("Probability of detecting significant friedman test and k posthoc differences between staged animations and the others.")
print(results.posthoc)



