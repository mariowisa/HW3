Mario, Adeel, Minghao



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r}
load('Household_Pulse_data.RData')
attach(Household_Pulse_data)
```

I want to see if there's a trend associated with higher education vs vaccination status

```{r}
all_doses <- data.frame(matrix(ncol=1+length(summary(EEDUC)),nrow=0))
colnames(all_doses) <- levels(unique(EEDUC))
for (i in 1:length(summary(EEDUC))){
  all_doses[1,i] <- summary(EEDUC[DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'])[i] / summary(EEDUC)[i]
}
all_doses
```

I want to also include people who "claim" they are going to get vaccinated. \
Some people might say vaccination is a good thing but never do it. Let's see what percentage of people actually got vaccinated.
```{r}
for (i in 1:length(summary(EEDUC))){
  all_doses[2,i] <- summary(EEDUC[DOSESRV=='yes got all doses'])[i] / summary(EEDUC)[i]
}
all_doses
all_doses[3,] <- all_doses[1,] - all_doses[2,] #for percentage difference
```


Some cleaning -

```{r}
all_doses[8] <- NULL
all_doses_t <- t(all_doses) #For better viewing
colnames(all_doses_t) <- c('Included','Not Included','percentage_difference')
all_doses_t
```
It appears from the probability that people with higher educations are more likely to get vaccinated. People with higher educations are also more likely to already be vaccinated. This could have a lot to do with distribution of time for higher education groups: with a better salary and work-life balance, people with higher education tends to spend more time worrying about well-being and be more educated around getting vaccinated. \

To reflect on the belief, I also tested people who work remotely might be less likely to get vaccinated vs people who work on-site.

```{r}
onsite_only <- subset(Household_Pulse_data, Works_onsite == 'worked onsite' & works_remote != 'worked remotely')
remote_only <- subset(Household_Pulse_data, works_remote == 'worked remotely' & Works_onsite != 'worked onsite')
hybrid <- subset(Household_Pulse_data, works_remote == 'worked remotely' & Works_onsite == 'worked onsite')
vaccinated1 <- nrow(subset(onsite_only,DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'))/nrow(onsite_only)
vaccinated2 <- nrow(subset(remote_only,DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'))/nrow(remote_only)
vaccinated3 <- nrow(subset(hybrid,DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses'))/nrow(hybrid)
vaccination_rate <- c(vaccinated1,vaccinated3,vaccinated2)
work_related <- data.frame(vaccination_rate,index = c('onsite','hybrid','remote'))
work_related
```

It seems that people who work remotely is more likely to get vaccinated, which can be a result of two factors:
1. As I previously mentioned, higher education groups tend to care more about their well-being and be less likely misinformed about vaccination. Higher education people also tend to have higher paying jobs and jobs require less on-site (hybrid and remote)
2. People who work remotely and hybrid are more likely people who worry about virus and will more likely get vaccinated.

Second factor lacks context, because people often have no choice over whether to work hybrid/remote/onsite based on their job specifications. However, I can test on the first one: Higher education people tend to have more hybrid jobs.

```{r}
hybrid_or_remote <- rbind(hybrid,remote_only)
education_level <- summary(hybrid_or_remote$EEDUC)
education_level/sum(education_level)#hybrid
summary(onsite_only$EEDUC)  / sum(summary(onsite_only$EEDUC)) #onsite
summary(remote_only$EEDUC)  / sum(summary(remote_only$EEDUC)) #remote
```
A majority of people (over 70%) who are working remotely are from higher education background, compared to less than 60%. This seems to be the reason for the difference, and we can confirm that by seeing how many of those higher degree individuals actually had vaccination.
```{r}
dose_data_hybrid <- subset(hybrid_or_remote, DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses')
summary(dose_data_hybrid$EEDUC)
summary(dose_data_hybrid$EEDUC)/nrow(dose_data_hybrid)
summary(dose_data_hybrid$EEDUC)/nrow(dose_data_hybrid) -summary(hybrid_or_remote$EEDUC)  / sum(summary(hybrid_or_remote$EEDUC))
```
This is an approximately matching result compared to what we are seeing previously. The difference is so small that it is not necessary to conduct a hypothesis testing. \
Let's see if things change for onsite workers.
```{r}
dose_data_onsite <- subset(onsite_only, DOSESRV=='yes got all doses' | DOSESRV == 'yes plan to get all doses')
summary(dose_data_onsite$EEDUC)
summary(dose_data_onsite$EEDUC)/nrow(dose_data_onsite)
summary(dose_data_onsite$EEDUC)/nrow(dose_data_onsite) - summary(onsite_only$EEDUC)  / sum(summary(onsite_only$EEDUC))
```
I am not seeing a significant difference between education distribution and vaccination rate. It is likely not a result education levels based on the data. 

If the data show some significant difference (at least 5% or higher on any of the categories) then it is worthwhile to test for deviation from the mean. I think on this case, the best approach is to test for difference in means between remote/onsite/hybrid with the overall survey results. Since the data is large enough, I can use 95% confidence interval to test the result. However, the difference is too small and not consistent (less than 3% across the board), it is not worthwhile to continue and I can safely conclude that factor 1 is not a significant factor.
