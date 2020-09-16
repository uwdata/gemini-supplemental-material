library(jsonlite)

df <- fromJSON("./data-final.json", flatten=TRUE)
df[, 'gender'] <- as.factor((df[, 'gender']))

#Exclude the participants who self-reported color deficiency
#df = df[df$vision_impairment=='no',];
# So far, there is no participants having vision impairment.

#Exclude the responses that took more than an hour (avg = 330048.3 ms)
df = df[df$compTime <= 60*60*1000,];

df = df[!df$excluded,];


print('# of Participants: ')
length(unique(df$userKey))



newdf = df[,c("stimulusName", "designName", "stimulusOrder", "designOrder", "rank", "userKey", "designCost")]
newdf[, 'rank'] <- as.numeric((df[, 'rank']))
newdf[, 'stimulusName'] <- as.factor((df[, 'stimulusName']))
newdf[, 'designName'] <- as.factor((df[, 'designName']))
newdf[, 'stimulusOrder'] <- as.numeric((df[, 'stimulusOrder']))
newdf[, 'designOrder'] <- as.numeric((df[, 'designOrder']))
newdf[, 'userKey'] <- as.factor((df[, 'userKey']))
newdf[, 'designCost'] <- as.numeric((df[, 'designCost']))
summary(newdf)


#### Overall

s2R1 = newdf[newdf$designName=='stage2-rank1',]$rank
s2R2 = newdf[newdf$designName=='stage2-rank2',]$rank
s2R3 = newdf[newdf$designName=='stage2-rank3',]$rank
s1R1 = newdf[newdf$designName=='stage1-rank1',]$rank
s3R1 = newdf[newdf$designName=='stage3-rank1',]$rank

data = cbind(s2R1, s2R2, s2R3, s1R1, s3R1)

t <- friedman.test(data)
x <- colMeans(data)
print(x[order(x)])
N = nrow(newdf)
Value <- newdf$rank
Group <- factor(newdf$designName)
pairwise.wilcox.test(Value, Group, p.adj="bonferroni", exact=F, paired=T)

#s1R1     s2R1     s2R2     s3R1     s2R3
#2.137441 2.672986 3.236967 3.398104 3.554502

# stage1-rank1 > *
# stage2-rank1 > * (except stage1-rank1)


#### Stimulus: Filtering Points

df_stim1 = newdf[newdf$stimulusName=='Stimulus 1',]
s2R1 = df_stim1[df_stim1$designName=='stage2-rank1',]$rank
s2R2 = df_stim1[df_stim1$designName=='stage2-rank2',]$rank
s2R3 = df_stim1[df_stim1$designName=='stage2-rank3',]$rank
s1R1 = df_stim1[df_stim1$designName=='stage1-rank1',]$rank
s3R1 = df_stim1[df_stim1$designName=='stage3-rank1',]$rank

data = cbind(s2R1, s2R2, s2R3, s1R1, s3R1)
x <- colMeans(data)
print(x[order(x)])
friedman.test(data)

N = nrow(df_stim1)
Value <- df_stim1$rank
Group <- factor(df_stim1$designName)
pairwise.wilcox.test(Value, Group, p.adj="bonferroni", exact=F, paired=T)

#s2R1     s1R1     s3R1     s2R2     s2R3
#2.207547 2.396226 3.377358 3.490566 3.528302
#stage1-rank1 > stage3-rank1, stage2-rank2, stage2-rank3
#stage2-rank1 > stage3-rank1, stage2-rank2, stage2-rank3

#### Stimulus: Expanding Lines

df_stim3 = newdf[newdf$stimulusName=='Stimulus 2',]
s2R1 = df_stim3[df_stim3$designName=='stage2-rank1',]$rank
s2R2 = df_stim3[df_stim3$designName=='stage2-rank2',]$rank
s2R3 = df_stim3[df_stim3$designName=='stage2-rank3',]$rank
s1R1 = df_stim3[df_stim3$designName=='stage1-rank1',]$rank
s3R1 = df_stim3[df_stim3$designName=='stage3-rank1',]$rank

data = cbind(s2R1, s2R2, s2R3, s1R1, s3R1)
x <- colMeans(data)
print(x[order(x)])
friedman.test(data)

N = nrow(df_stim3)
Value <- df_stim3$rank
Group <- factor(df_stim3$designName)
pairwise.wilcox.test(Value, Group, p.adj="bonferroni", exact=F, paired=T)

#s1R1     s2R1     s2R3     s3R1     s2R2
#1.788462 2.653846 3.442308 3.538462 3.576923
# stage1-rank1 > *
# stage2-rank1 > stage2-rank2, stage3-rank1

#### Stimulus: Sorting & Updating Bars

df_stim2 = newdf[newdf$stimulusName=='Stimulus 3',]
s2R1 = df_stim2[df_stim2$designName=='stage2-rank1',]$rank
s2R2 = df_stim2[df_stim2$designName=='stage2-rank2',]$rank
s2R3 = df_stim2[df_stim2$designName=='stage2-rank3',]$rank
s1R1 = df_stim2[df_stim2$designName=='stage1-rank1',]$rank
s3R1 = df_stim2[df_stim2$designName=='stage3-rank1',]$rank

data = cbind(s2R1, s2R2, s2R3, s1R1, s3R1)
x <- colMeans(data)
print(x[order(x)])
friedman.test(data)

N = nrow(df_stim2)
Value <- df_stim2$rank
Group <- factor(df_stim2$designName)
pairwise.wilcox.test(Value, Group, p.adj="bonferroni", exact=F, paired=T)

#s1R1     s2R2     s2R1     s3R1     s2R3
#2.018868 2.867925 2.924528 3.584906 3.603774
#s1R1 > *


#### Stimulus: Aggregating


df_stim4 = newdf[newdf$stimulusName=='Stimulus 4',]
s2R1 = df_stim4[df_stim4$designName=='stage2-rank1',]$rank
s2R2 = df_stim4[df_stim4$designName=='stage2-rank2',]$rank
s2R3 = df_stim4[df_stim4$designName=='stage2-rank3',]$rank
s1R1 = df_stim4[df_stim4$designName=='stage1-rank1',]$rank
s3R1 = df_stim4[df_stim4$designName=='stage3-rank1',]$rank

data = cbind(s2R1, s2R2, s2R3, s1R1, s3R1)
x <- colMeans(data)
print(x[order(x)])
friedman.test(data)

N = nrow(df_stim4)
Value <- df_stim4$rank
Group <- factor(df_stim4$designName)
pairwise.wilcox.test(Value, Group, p.adj="bonferroni", exact=F, paired=T)

# s1R1     s2R1     s2R2     s3R1     s2R3
# 2.339623 2.905660 3.018868 3.094340 3.641509
# stage1-rank1 > stage2-rank3





############################################################################################
# Bootstrap 95% CIs
library(boot)

bs <- function(data, indices) {
  return(mean(data[indices]))
}

bootstrapped_cis = data.frame(stimulusName = character(), designName=character(), lo=numeric(), hi=numeric())
for (stimulusName in c('Stimulus 1', 'Stimulus 2', 'Stimulus 3', 'Stimulus 4')) {
  sub_df = newdf[newdf$stimulusName==stimulusName,]

  result_s1r1 <- boot(data=sub_df[sub_df$designName=='stage1-rank1','rank'], statistic=bs, R=1000)
  result_s2r1 <- boot(data=sub_df[sub_df$designName=='stage2-rank1','rank'], statistic=bs, R=1000)
  result_s2r2 <- boot(data=sub_df[sub_df$designName=='stage2-rank2','rank'], statistic=bs, R=1000)
  result_s2r3 <- boot(data=sub_df[sub_df$designName=='stage2-rank3','rank'], statistic=bs, R=1000)
  result_s3r1 <- boot(data=sub_df[sub_df$designName=='stage3-rank1','rank'], statistic=bs, R=1000)

  boot_ci_s1r1 = boot.ci(result_s1r1, type="bca")$bca[4:5]
  boot_ci_s2r1 = boot.ci(result_s2r1, type="bca")$bca[4:5]
  boot_ci_s2r2 = boot.ci(result_s2r2, type="bca")$bca[4:5]
  boot_ci_s2r3 = boot.ci(result_s2r3, type="bca")$bca[4:5]
  boot_ci_s3r1 = boot.ci(result_s3r1, type="bca")$bca[4:5]

  bootstrapped_cis <- rbind(bootstrapped_cis,
                            data.frame(
                              rbind(
                                c(stimulusName, 'stage1-rank1', boot_ci_s1r1[1], boot_ci_s1r1[2]),
                                c(stimulusName, 'stage2-rank1', boot_ci_s2r1[1], boot_ci_s2r1[2]),
                                c(stimulusName, 'stage2-rank2', boot_ci_s2r2[1], boot_ci_s2r2[2]),
                                c(stimulusName, 'stage2-rank3', boot_ci_s2r3[1], boot_ci_s2r3[2]),
                                c(stimulusName, 'stage3-rank1', boot_ci_s3r1[1], boot_ci_s3r1[2])
                              )
                            )
  )

}

names(bootstrapped_cis) <- c("stimulusName", "designName", "lo", "hi")
toJSON(bootstrapped_cis)
