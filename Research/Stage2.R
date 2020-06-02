#############   STAGE 2

######    PART 1

# First read data
DS <- read.csv2("DataSets/DS2.csv")[-1]

# It might be NAs in data. Need to check
NA %in% DS$x..1
# True, so we need to divide data into 2 groups
DS_inv <- DS[is.na(DS$x..1),]
DS_val <- DS[!is.na(DS$x..1),]

# What the percentage of invalid records
N <- nrow(DS)
N_inv <- nrow(DS_inv)
N_val <- nrow(DS_val)
N_inv/N *100
# Almost 20%! Pretty much...

# Then we will devide valid data into 2 groups: correct and uncorrect records 
DS_cor <- DS_val[DS_val$reached.accuracy <= DS_val$accuracy,]
DS_unc <- DS_val[DS_val$reached.accuracy > DS_val$accuracy,]

# And now check percentage of correct and uncorrect records in general 
N_cor <- nrow(DS_cor)
N_unc <- nrow(DS_unc)
N_cor/N *100
N_unc/N *100
# Looks sadly
# only 35% of correct records
# and more than 45% of uncorrect records
# So first we need to find out what called such situation

# First quastion is that could we merge uncorrect and invalid 
# records to work with one dataset. We might look for similar parameters
# in each set and comare them if they are has simillar affect from 
# simillar parameters
# Let's compare firstly histograms
# First will be pair of derivative step histograms
hist(log10(DS_inv$derivative.step))
hist(log10(DS_unc$derivative.step))
# As we can see it looks pretty different
hist(log10(DS_cor$derivative.step))
# And we can see almost uniform distribution on correct values
# So derivative step does not affect on correctness

# Comparison of ODS accuracies
hist(log10(DS_inv$onedim.accuracy))
hist(log10(DS_unc$onedim.accuracy))
# Again we observe significant difference
hist(log10(DS_cor$onedim.accuracy))
# There we see small worthness of increasing the ODS accuracy,
# but very big accuracy loses this worthness

# What about alpha parameter
hist(log10(DS_inv$alpha.in.Sven))
hist(log10(DS_unc$alpha.in.Sven))
# Pretty interesting differences
hist(log10(DS_cor$alpha.in.Sven))
# Obvious linear increasing of correctness with decreasing alpha parameter
# So it worth to use smaller alphas

# And so what about binary factors. Need to check ratios
# Ratios for derivative method
der_Oh2_inv <- sum(as.character(DS_inv$derivative.method)=="O(h^2)")/N
der_Oh4_inv <- sum(as.character(DS_inv$derivative.method)=="O(h^4)")/N
der_Oh2_unc <- sum(as.character(DS_unc$derivative.method)=="O(h^2)")/N
der_Oh4_unc <- sum(as.character(DS_unc$derivative.method)=="O(h^4)")/N
der_Oh2_cor <- sum(as.character(DS_cor$derivative.method)=="O(h^2)")/N
der_Oh4_cor <- sum(as.character(DS_cor$derivative.method)=="O(h^4)")/N
pie(c(der_Oh2_inv, der_Oh2_unc, der_Oh2_cor, der_Oh4_cor, der_Oh4_unc, der_Oh4_inv),
    labels = c("O(h^2) invalid", "O(h^2) uncorrect", "O(h^2) correct", 
               "O(h^4) correct", "O(h^4) uncorrect", "O(h^4) invalid"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# As we can see there are little difference but we have no reasons to make 
# any valuable conclusions about "bad habits". Can only say that greater degree 
# of accuracy much fallible

# Next will be ODS method
ods_grm_inv <- sum(as.character(DS_inv$onedim.method)=="GR")/N
ods_dsk_inv <- sum(as.character(DS_inv$onedim.method)=="DSK-P")/N
ods_grm_unc <- sum(as.character(DS_unc$onedim.method)=="GR")/N
ods_dsk_unc <- sum(as.character(DS_unc$onedim.method)=="DSK-P")/N
ods_grm_cor <- sum(as.character(DS_cor$onedim.method)=="GR")/N
ods_dsk_cor <- sum(as.character(DS_cor$onedim.method)=="DSK-P")/N
pie(c(ods_grm_inv, ods_grm_unc, ods_grm_cor, ods_dsk_cor, ods_dsk_unc, ods_dsk_inv),
    labels = c("GRM invalid", "GRM uncorrect", "GRM correct", 
               "DSK-P correct", "DSK-P uncorrect", "DSK-P invalid"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# There we see more clear situation: GRM gives uncorrect values very often
# while correct records met as seldom as invalid
# On the second hand: DSK-Powell method more then in half times returns 
# correct result and uncorrect values met as freequent as invalid
# So we can make a conclusion that using DSK-Powell method gives more profit!

# And what for stop criteria?
stp_nrm_inv <- sum(as.character(DS_inv$criteria.of.stop)=="norm")/N
stp_grd_inv <- sum(as.character(DS_inv$criteria.of.stop)=="grad")/N
stp_nrm_unc <- sum(as.character(DS_unc$criteria.of.stop)=="norm")/N
stp_grd_unc <- sum(as.character(DS_unc$criteria.of.stop)=="grad")/N
stp_nrm_cor <- sum(as.character(DS_cor$criteria.of.stop)=="norm")/N
stp_grd_cor <- sum(as.character(DS_cor$criteria.of.stop)=="grad")/N
pie(c(stp_grd_inv, stp_grd_unc, stp_grd_cor, stp_grd_cor, stp_grd_unc, stp_grd_inv),
    labels = c("norm invalid", "norm uncorrect", "norm correct", 
               "grad correct", "grad uncorrect", "grad invalid"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# So it does't matter wich criteria to choose

# Last what we need to visualise of binary parameters is restart prescence
rst_T_inv <- sum(DS_inv$restarts.presence)/N
rst_F_inv <- sum(!DS_inv$restarts.presence)/N
rst_T_unc <- sum(DS_unc$restarts.presence)/N
rst_F_unc <- sum(!DS_unc$restarts.presence)/N
rst_T_cor <- sum(DS_cor$restarts.presence)/N
rst_F_cor <- sum(!DS_cor$restarts.presence)/N
pie(c(rst_T_inv, rst_T_unc, rst_T_cor, rst_F_cor, rst_F_unc, rst_F_inv),
    labels = c("invalid with restarts", "uncorrect with restarts", "correct with restarts", 
               "correct without restarts", "uncorrect without restarts", "invalid without restarts"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# Somewhy algorithm is less times runned without restarts, but even so we can see 
# that when restarts present it is less faults.

# But still there is question: the percen of correct records?
# Answer for presece of restarts
rst_T_cor/(rst_T_cor+rst_T_unc+rst_T_inv) *100
# And for absence of them
rst_F_cor/(rst_F_cor+rst_F_unc+rst_F_inv) *100
# So almost 35% percents in both cases
# In fact there is new question appears: 
# What is better - to have greater uncorrectness and greater stability
# or vice versa - less uncorrectness and less stability?

# And last: how DFP method accuracy affects on correctnes density?
hist(log10(DS_inv$accuracy))
hist(log10(DS_unc$accuracy))
# Again, accuracy grows causes small invalidance grows # and uncorrectness decrease
hist(log10(DS_cor$accuracy))
# But summary correctness only increases with increasing of method accuracy
# But the most valuable accuracy as we can see is 1e-3.

# Let's see probably the best situations with probably the most correctness coefficient
DS_best <- DS[DS$accuracy %in% c(1e-3,1e-4, 1e-5),]
DS_best <- DS_best[DS_best$onedim.accuracy %in% c(1e-4, 1e-6),]
DS_best <- DS_best[DS_best$alpha.in.Sven %in% c(1e-2, 1e-3),]
DS_best <- DS_best[as.character(DS_best$onedim.method)=="DSK-P",]
# Are there NAs?
NA %in% DS_best$function.value
# True, so need to devide and find out how much
DS_best_inv <- DS_best[is.na(DS_best$x..1),]
DS_best_val <- DS_best[!is.na(DS_best$x..1),]
N_best <- nrow(DS_best)
N_best_inv <- nrow(DS_best_inv)
N_best_inv/N_best *100
# 16% is less then 20% but still to much, it might be the problem of big accuracy

# What about correctness?
DS_best_unc <- DS_best_val[DS_best_val$accuracy < DS_best_val$reached.accuracy,]
DS_best_cor <- DS_best_val[DS_best_val$accuracy >= DS_best_val$reached.accuracy,]
N_best_unc <- nrow(DS_best_unc)
N_best_unc/N_best *100
# 7.32% of uncorrect records - it is much less then 45.4% and it is pretty interesting

# And so what the percentage of correct values&
N_best_cor <- nrow(DS_best_cor)
N_best_cor/N_best *100
# 76.6% is MUCH more then 35% as in general sample

# It is interesting how valuable can be mistake
max(DS_best_val$reached.accuracy)
# It is great deviation, so we need to find out what causes it

# what the percentage of correct values in this cases from general number
# of correct values
N_best_cor/N_cor *100
# 18.5% from all correct values.
# What the part of all this records from all records?
N_best/N *100
# only 8.45% cases from general sample occupy 18.5% of all correct records
# It's really nice.

# And last of this part is checking for changes of binary 
# parameters (excluding ODS method)
# First will be serivative method
der_Oh2_inv <- sum(as.character(DS_best_inv$derivative.method)=="O(h^2)")/N_best
der_Oh4_inv <- sum(as.character(DS_best_inv$derivative.method)=="O(h^4)")/N_best
der_Oh2_unc <- sum(as.character(DS_best_unc$derivative.method)=="O(h^2)")/N_best
der_Oh4_unc <- sum(as.character(DS_best_unc$derivative.method)=="O(h^4)")/N_best
der_Oh2_cor <- sum(as.character(DS_best_cor$derivative.method)=="O(h^2)")/N_best
der_Oh4_cor <- sum(as.character(DS_best_cor$derivative.method)=="O(h^4)")/N_best
pie(c(der_Oh2_inv, der_Oh2_unc, der_Oh2_cor, der_Oh4_cor, der_Oh4_unc, der_Oh4_inv),
    labels = c("O(h^2) invalid", "O(h^2) uncorrect", "O(h^2) correct", 
               "O(h^4) correct", "O(h^4) uncorrect", "O(h^4) invalid"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# We see that correctness is similar, but in case of smaller power of 
# accuracy there are less uncorrect values and more faults, so it is more
# stable due to we has less probability to be uninformed about uncorrect result.

# In this case it is interesting to know how big could be mistake for each method
max(DS_best_val[as.character(DS_best_val$derivative.method)=="O(h^2)",]$reached.accuracy)
# It is much less but still to big

# Second - stop criteria
stp_nrm_inv <- sum(as.character(DS_best_inv$criteria.of.stop)=="norm")/N_best
stp_grd_inv <- sum(as.character(DS_best_inv$criteria.of.stop)=="grad")/N_best
stp_nrm_unc <- sum(as.character(DS_best_unc$criteria.of.stop)=="norm")/N_best
stp_grd_unc <- sum(as.character(DS_best_unc$criteria.of.stop)=="grad")/N_best
stp_nrm_cor <- sum(as.character(DS_best_cor$criteria.of.stop)=="norm")/N_best
stp_grd_cor <- sum(as.character(DS_best_cor$criteria.of.stop)=="grad")/N_best
pie(c(stp_grd_inv, stp_grd_unc, stp_grd_cor, stp_grd_cor, stp_grd_unc, stp_grd_inv),
    labels = c("norm invalid", "norm uncorrect", "norm correct", 
               "grad correct", "grad uncorrect", "grad invalid"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# No difference between factors.

# And third is presence of restarts
rst_T_inv <- sum(DS_best_inv$restarts.presence)/N_best
rst_F_inv <- sum(!DS_best_inv$restarts.presence)/N_best
rst_T_unc <- sum(DS_best_unc$restarts.presence)/N_best
rst_F_unc <- sum(!DS_best_unc$restarts.presence)/N_best
rst_T_cor <- sum(DS_best_cor$restarts.presence)/N_best
rst_F_cor <- sum(!DS_best_cor$restarts.presence)/N_best
pie(c(rst_T_inv, rst_T_unc, rst_T_cor, rst_F_cor, rst_F_unc, rst_F_inv),
    labels = c("invalid with restarts", "uncorrect with restarts", "correct with restarts", 
               "correct without restarts", "uncorrect without restarts", "invalid without restarts"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# Obviously it is more correct result when restarts are present, but 
# it is interesting what the percent of uncorrect records in each case

# Percent of uncorrect records when restarts are present
rst_T_unc/(rst_T_unc+rst_T_inv+rst_T_cor) *100
# And percent of uncorrect recorts when restarts are absent
rst_F_unc/(rst_F_unc+rst_F_inv+rst_F_cor) *100
# Nice, it is more frequent uncorrect results when restarts are absent
# But I can't stop on this!

# We will check the probability to get uncorrect result!
# When restarts are present
rst_T_unc/rst_T_cor *100
# When restarts are absent
rst_F_unc/rst_F_cor *100
# Twice more probability to get mistake when restarts are present

# Let's try ro exclude derivative method with O(h^4) accuracy
DS_better <- DS_best[as.character(DS_best$derivative.method)=="O(h^2)",]
DS_better_inv <- DS_better[is.na(DS_better$x..1),]
DS_better_val <- DS_better[!is.na(DS_better$x..1),]
DS_better_unc <- DS_better_val[DS_better_val$accuracy < DS_better_val$reached.accuracy,]
DS_better_cor <- DS_better_val[DS_better_val$accuracy >= DS_better_val$reached.accuracy,]
N_better <- nrow(DS_better)
# And then look for changes...

# How pie for presence of restarts looks now?
rst_T_inv <- sum(DS_better_inv$restarts.presence)/N_better
rst_F_inv <- sum(!DS_better_inv$restarts.presence)/N_better
rst_T_unc <- sum(DS_better_unc$restarts.presence)/N_better
rst_F_unc <- sum(!DS_better_unc$restarts.presence)/N_better
rst_T_cor <- sum(DS_better_cor$restarts.presence)/N_better
rst_F_cor <- sum(!DS_better_cor$restarts.presence)/N_better
pie(c(rst_T_inv, rst_T_unc, rst_T_cor, rst_F_cor, rst_F_unc, rst_F_inv),
    labels = c("invalid with restarts", "uncorrect with restarts", "correct with restarts", 
               "correct without restarts", "uncorrect without restarts", "invalid without restarts"),
    col=c("red", "yellow", "green", "green", "yellow", "red"))
# It looks much better. We shall look on the new percentages

# Probabilities to make mistake
# When restarts are present
rst_T_unc/rst_T_cor *100
# When restarts are absent
rst_F_unc/rst_F_cor *100


# SUMMARY OF PART 1
#
# - We found that many combinations causes haltings, only 35% of correct records
# - Derivative step does not affect on correctness
# - Increasing the ODS accuracy worth a litttle, 
#   but very big accuracy loses this worthness 
# - Linear dependance between decreasing alpha parameter power 
#   and increasing of correctness
# - Greater degree of derivative accuracy (O(h^4)) causes little more uncorrectness
# - Gold Ratio method gives uncorrect values very often (about 7 per 9 times),
#   in other hand DSK-Powell method more then in half times returns correct result
#   and uncorrect values met as freequent as invalid. So usage od DSK-P is more valuable
# - It is no difference between usage of both stop criterias
# - Presence of restarts a little more stable then absence of them: equal correctness
#   with less failures 
# - The most correct values returned when DFP accuracy is 1e-3 and greater
#
# --- In case when only viewed parameters wich occurs the most correctness ---
#
# - This cases twice more correct indeed: 76.6% of correctness 
#   when in general sample it is only 35%
# - This cases occurs 18.5% of correct values in general sample
# - In additional it is only 8.45% of general saple
# - It is really valuable to use such cases to exclude uncorrectnes and faults
# - Derivative method with O(h^4) accuracy degree causes mistakes more frequently
#   and it can be excluded
# - Stop criteria has no influence on method in this case
# - Prescence of restarts causes correct results much frequent then its' absence
# - Absence of restarts has less probability to cause mistake
# - If derivative method with O(h^4) accuracy degree is excluded, then 
#   presence of restarts has 5.1% of probabily to make mistake, when its' absence
#   has 4% of such probability otherwise when restarts absent it is tree times
#   more probable to get invalid result.


######    PART 2

# There we will build some simple plots, wich have to demonstrate dependencies
# between different parameters and numbers of calls in plain way

# We have mad ejection, so we need to delete it
max(DS$calls.number)
DS <- DS[DS$calls.number<1e9,]
DS_inv <- DS_inv[DS_inv$calls.number<1e9,]
max(DS$calls.number)
# Now it is normal

# First will be plot for all records
# Dependance from derivative step
{
der_steps <- sort(unique(DS$derivative.step))
log_steps <- log10(der_steps)
# Tendencies for invalid records
medns_inv <- NULL # medians for invalid dataset
means_inv <- NULL # averages for invalid dataset
sdevs_inv <- NULL # standard deviations for invalid dataset
# Tendencies for incorrect records
medns_unc <- NULL # medians for incorrect dataset
means_unc <- NULL # averages for incorrect dataset
sdevs_unc <- NULL # standard deviations for incorrect dataset
# Tendencies for correct records
medns_cor <- NULL # medians for correct dataset
means_cor <- NULL # averages for correct dataset
sdevs_cor <- NULL # standard deviations for correct dataset
# Filling tendencies
for (stp in der_steps){
  subsmpl_inv <- DS_inv$calls.number[DS_inv$derivative.step==stp]
  subsmpl_unc <- DS_unc$calls.number[DS_unc$derivative.step==stp]
  subsmpl_cor <- DS_cor$calls.number[DS_cor$derivative.step==stp]

  medns_inv <- c(medns_inv, median(subsmpl_inv))
  means_inv <- c(means_inv, mean(subsmpl_inv))
  sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))

  medns_unc <- c(medns_unc, median(subsmpl_unc))
  means_unc <- c(means_unc, mean(subsmpl_unc))
  sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))

  medns_cor <- c(medns_cor, median(subsmpl_cor))
  means_cor <- c(means_cor, mean(subsmpl_cor))
  sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
}
plot(log10(DS$derivative.step), DS$calls.number, 
     xlab="power of derivative step", 
     ylab="number of calls",
     cex=0.4)

lines(log_steps, medns_inv, type='b', lty="longdash", pch=23, col="red")
lines(log_steps, means_inv, type='b', pch=19, col="red")
lines(log_steps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")

lines(log_steps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
lines(log_steps, means_unc, type='b', pch=19, col="yellow")
lines(log_steps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")

lines(log_steps, medns_cor, type='b', lty="longdash", pch=23, col="green")
lines(log_steps, means_cor, type='b', pch=19, col="green")
lines(log_steps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
# On this plot we see numbers of call dependence of derivative step powers.
# Tendencies for numbers of calls are highlighted:
# - medians are rhombus pointed and linked with dashed lines
# - averages are bold pointed and linked with solid lines
# - one standard deviation over average values is represented with dotted lines
# Colours accordance:
# - Red     - faults
# - Yellow  - when uncorrect values got
# - Green   - when correct values got

# As it is hard to understand all details we will scale it to 10000
{
  der_steps <- sort(unique(DS1$derivative.step))
  log_steps <- log10(der_steps)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (stp in der_steps){
    subsmpl_inv <- DS1_inv$calls.number[DS1_inv$derivative.step==stp]
    subsmpl_unc <- DS1_unc$calls.number[DS1_unc$derivative.step==stp]
    subsmpl_cor <- DS1_cor$calls.number[DS1_cor$derivative.step==stp]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS1$derivative.step), DS1$calls.number, 
       xlab="power of derivative step", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 10000))
  
  lines(log_steps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_steps, means_inv, type='b', pch=19, col="red")
  lines(log_steps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_steps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_steps, means_unc, type='b', pch=19, col="yellow")
  lines(log_steps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_steps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_steps, means_cor, type='b', pch=19, col="green")
  lines(log_steps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
# There is no patterns seen 


# Then we will look on analogue with ODS accuracy 
# instead of derivative accuracy
{
  ods_eps <- sort(unique(DS1$onedim.accuracy))
  log_eps <- log10(ods_eps)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (eps in ods_eps){
    subsmpl_inv <- DS1_inv$calls.number[DS1_inv$onedim.accuracy==eps]
    subsmpl_unc <- DS1_unc$calls.number[DS1_unc$onedim.accuracy==eps]
    subsmpl_cor <- DS1_cor$calls.number[DS1_cor$onedim.accuracy==eps]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS1$onedim.accuracy), DS1$calls.number, 
       xlab="power of ODS accuracy", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 10000))
  
  lines(log_eps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_eps, means_inv, type='b', pch=19, col="red")
  lines(log_eps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_eps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_eps, means_unc, type='b', pch=19, col="yellow")
  lines(log_eps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_eps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_eps, means_cor, type='b', pch=19, col="green")
  lines(log_eps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
# There is grows of calls with increasing accuracy for halting records
# Also decreaseing of calls number for correct values


# Next will be dependance of Sven method parameter alpha
{
  alphas <- sort(unique(DS1$alpha.in.Sven))
  log_alph <- log10(alphas)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (alph in alphas){
    subsmpl_inv <- DS1_inv$calls.number[DS1_inv$alpha.in.Sven==alph]
    subsmpl_unc <- DS1_unc$calls.number[DS1_unc$alpha.in.Sven==alph]
    subsmpl_cor <- DS1_cor$calls.number[DS1_cor$alpha.in.Sven==alph]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS1$alpha.in.Sven), DS1$calls.number, 
       xlab="power of alpha parameter in Sven method", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 10000))
  
  lines(log_alph, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_alph, means_inv, type='b', pch=19, col="red")
  lines(log_alph, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_alph, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_alph, means_unc, type='b', pch=19, col="yellow")
  lines(log_alph, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_alph, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_alph, means_cor, type='b', pch=19, col="green")
  lines(log_alph, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
# Decreasing of alpha parameter power causes decreasing of numbers of calls


# And what about method accuracy?
{
  accuracies <- sort(unique(DS1$accuracy))
  log_eps <- log10(accuracies)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (eps in accuracies){
    subsmpl_inv <- DS1_inv$calls.number[DS1_inv$accuracy==eps]
    subsmpl_unc <- DS1_unc$calls.number[DS1_unc$accuracy==eps]
    subsmpl_cor <- DS1_cor$calls.number[DS1_cor$accuracy==eps]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS1$accuracy), DS1$calls.number, 
       xlab="power of accuracy", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 10000))
  
  lines(log_eps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_eps, means_inv, type='b', pch=19, col="red")
  lines(log_eps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_eps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_eps, means_unc, type='b', pch=19, col="yellow")
  lines(log_eps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_eps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_eps, means_cor, type='b', pch=19, col="green")
  lines(log_eps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
# Nothing interesting found


# After that we will look for boxplots on binary parameters
# in this cases we will exclude invalid cases due to its' superfluity.
# We has already found that failures causes big numbers of calls

# First binary parameter will be derivative method
boxplot(DS_cor[as.character(DS_cor$derivative.method)=='O(h^2)',]$calls.number,
        DS_cor[as.character(DS_cor$derivative.method)=='O(h^4)',]$calls.number,
        DS_cor[as.character(DS_unc$derivative.method)=='O(h^2)',]$calls.number,
        DS_cor[as.character(DS_unc$derivative.method)=='O(h^4)',]$calls.number,
        ylim=c(0,40000),
        names=c("correct O(h^2)", "correct O(h^4)", 
                "incorrect O(h^2)", "incorrect O(h^4)"))
# There is much difference when correct values calculated
# According to this boxplots it is much more efficient to use more accurate 
# method, because it causes much less number of iterations - less then 8000 in 
# 75% of cases, when O(h^2) need not more than 18000 calls in 75% of cases.
# And it is no difference between usage of methods when got value is uncorrect

############################### ODS boxlots
boxplot(DS_cor[as.character(DS_cor$onedim.method)=='GR',]$calls.number,
        DS_cor[as.character(DS_cor$onedim.method)=='DSK-P',]$calls.number,
        DS_cor[as.character(DS_unc$onedim.method)=='GR',]$calls.number,
        DS_cor[as.character(DS_unc$onedim.method)=='DSK-P',]$calls.number,
        ylim=c(0,40000),
        names=c("correct GR", "correct DSK-P", 
                "incorrect GR", "incorrect DSK-P"))
##################################

################################# Stop criteria boxplots
boxplot(DS_cor[as.character(DS_cor$criteria.of.stop)=='norm',]$calls.number,
        DS_cor[as.character(DS_cor$criteria.of.stop)=='grad',]$calls.number,
        DS_cor[as.character(DS_unc$criteria.of.stop)=='norm',]$calls.number,
        DS_cor[as.character(DS_unc$criteria.of.stop)=='grad',]$calls.number,
        ylim=c(0,40000),
        names=c("correct NORM", "correct GRAD", 
                "incorrect NORM", "incorrect GRAD"))
#######################################

####################################### Restarts presence
boxplot(DS_cor[!DS_cor$restarts.presence,]$calls.number,
        DS_cor[DS_cor$restarts.presence,]$calls.number,
        DS_cor[!DS_unc$restarts.presence,]$calls.number,
        DS_cor[DS_unc$restarts.presence,]$calls.number,
        ylim=c(0,40000),
        names=c("correct without\nrestarts", "correct with\nrestarts", 
                "incorrect without\nrestarts", "incorrect with\nrestarts"))
########################################









#########################################################################
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##############
#########<     All the same analysis for filtered sample     >###########
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##############
#########################################################################





# We have mad ejection, so we need to delete it
max(DS_best$calls.number)
# It is already normal
#
#
###########################################################
# Derivative step - scale from 0 to 300000
{
  der_steps <- sort(unique(DS_best$derivative.step))
  log_steps <- log10(der_steps)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (stp in der_steps){
    subsmpl_inv <- DS_best_inv$calls.number[DS_best_inv$derivative.step==stp]
    subsmpl_unc <- DS_best_unc$calls.number[DS_best_unc$derivative.step==stp]
    subsmpl_cor <- DS_best_cor$calls.number[DS_best_cor$derivative.step==stp]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS_best$derivative.step), DS_best$calls.number, 
       xlab="power of derivative step", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 300000))
  
  lines(log_steps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_steps, means_inv, type='b', pch=19, col="red")
  lines(log_steps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_steps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_steps, means_unc, type='b', pch=19, col="yellow")
  lines(log_steps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_steps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_steps, means_cor, type='b', pch=19, col="green")
  lines(log_steps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
###########################################################
#
#
###########################################################
# Derivative step - scale from 0 to 25000
{
  der_steps <- sort(unique(DS_best$derivative.step))
  log_steps <- log10(der_steps)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (stp in der_steps){
    subsmpl_inv <- DS_best_inv$calls.number[DS_best_inv$derivative.step==stp]
    subsmpl_unc <- DS_best_unc$calls.number[DS_best_unc$derivative.step==stp]
    subsmpl_cor <- DS_best_cor$calls.number[DS_best_cor$derivative.step==stp]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS_best$derivative.step), DS_best$calls.number, 
       xlab="power of derivative step", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 25000))
  
  lines(log_steps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_steps, means_inv, type='b', pch=19, col="red")
  lines(log_steps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_steps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_steps, means_unc, type='b', pch=19, col="yellow")
  lines(log_steps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_steps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_steps, means_cor, type='b', pch=19, col="green")
  lines(log_steps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
###########################################################
#
#
###########################################################
# ODS accuracy - scale from 0 to 150000
{
  ods_eps <- sort(unique(DS_best$onedim.accuracy))
  log_eps <- log10(ods_eps)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (eps in ods_eps){
    subsmpl_inv <- DS_best_inv$calls.number[DS_best_inv$onedim.accuracy==eps]
    subsmpl_unc <- DS_best_unc$calls.number[DS_best_unc$onedim.accuracy==eps]
    subsmpl_cor <- DS_best_cor$calls.number[DS_best_cor$onedim.accuracy==eps]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS_best$onedim.accuracy), DS_best$calls.number, 
       xlab="power of ODS accuracy", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 150000))
  
  lines(log_eps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_eps, means_inv, type='b', pch=19, col="red")
  lines(log_eps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_eps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_eps, means_unc, type='b', pch=19, col="yellow")
  lines(log_eps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_eps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_eps, means_cor, type='b', pch=19, col="green")
  lines(log_eps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
###########################################################
#
#
###########################################################
# ODS accuracy - scale from 0 to 20000
{
  ods_eps <- sort(unique(DS_best$onedim.accuracy))
  log_eps <- log10(ods_eps)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (eps in ods_eps){
    subsmpl_inv <- DS_best_inv$calls.number[DS_best_inv$onedim.accuracy==eps]
    subsmpl_unc <- DS_best_unc$calls.number[DS_best_unc$onedim.accuracy==eps]
    subsmpl_cor <- DS_best_cor$calls.number[DS_best_cor$onedim.accuracy==eps]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS_best$onedim.accuracy), DS_best$calls.number, 
       xlab="power of ODS accuracy", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 20000))
  
  lines(log_eps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_eps, means_inv, type='b', pch=19, col="red")
  lines(log_eps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_eps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_eps, means_unc, type='b', pch=19, col="yellow")
  lines(log_eps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_eps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_eps, means_cor, type='b', pch=19, col="green")
  lines(log_eps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
###########################################################
#
#
###########################################################
# Sven alpha - scale from 0 to 15000
{
  alphas <- sort(unique(DS_best$alpha.in.Sven))
  log_alph <- log10(alphas)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (alph in alphas){
    subsmpl_inv <- DS_best_inv$calls.number[DS_best_inv$alpha.in.Sven==alph]
    subsmpl_unc <- DS_best_unc$calls.number[DS_best_unc$alpha.in.Sven==alph]
    subsmpl_cor <- DS_best_cor$calls.number[DS_best_cor$alpha.in.Sven==alph]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS_best$alpha.in.Sven), DS_best$calls.number, 
       xlab="power of alpha parameter in Sven method", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 15000))
  
  lines(log_alph, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_alph, means_inv, type='b', pch=19, col="red")
  lines(log_alph, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_alph, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_alph, means_unc, type='b', pch=19, col="yellow")
  lines(log_alph, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_alph, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_alph, means_cor, type='b', pch=19, col="green")
  lines(log_alph, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
###########################################################
#
#
###########################################################
# DFP method accuracy - scale from 0 to 18000
{
  accuracies <- sort(unique(DS_best$accuracy))
  log_eps <- log10(accuracies)
  # Tendencies for invalid records
  medns_inv <- NULL # medians for invalid dataset
  means_inv <- NULL # averages for invalid dataset
  sdevs_inv <- NULL # standard deviations for invalid dataset
  # Tendencies for incorrect records
  medns_unc <- NULL # medians for incorrect dataset
  means_unc <- NULL # averages for incorrect dataset
  sdevs_unc <- NULL # standard deviations for incorrect dataset
  # Tendencies for correct records
  medns_cor <- NULL # medians for correct dataset
  means_cor <- NULL # averages for correct dataset
  sdevs_cor <- NULL # standard deviations for correct dataset
  # Filling tendencies
  for (eps in accuracies){
    subsmpl_inv <- DS_best_inv$calls.number[DS_best_inv$accuracy==eps]
    subsmpl_unc <- DS_best_unc$calls.number[DS_best_unc$accuracy==eps]
    subsmpl_cor <- DS_best_cor$calls.number[DS_best_cor$accuracy==eps]
    
    medns_inv <- c(medns_inv, median(subsmpl_inv))
    means_inv <- c(means_inv, mean(subsmpl_inv))
    sdevs_inv <- c(sdevs_inv, sd(subsmpl_inv))
    
    medns_unc <- c(medns_unc, median(subsmpl_unc))
    means_unc <- c(means_unc, mean(subsmpl_unc))
    sdevs_unc <- c(sdevs_unc, sd(subsmpl_unc))
    
    medns_cor <- c(medns_cor, median(subsmpl_cor))
    means_cor <- c(means_cor, mean(subsmpl_cor))
    sdevs_cor <- c(sdevs_cor, sd(subsmpl_cor))
  }
  plot(log10(DS_best$accuracy), DS_best$calls.number, 
       xlab="power of accuracy", 
       ylab="number of calls",
       cex=0.4,
       ylim=c(0, 17000))
  
  lines(log_eps, medns_inv, type='b', lty="longdash", pch=23, col="red")
  lines(log_eps, means_inv, type='b', pch=19, col="red")
  lines(log_eps, means_inv+sdevs_inv, type='l', lty="dotted", col="red")
  
  lines(log_eps, medns_unc, type='b', lty="longdash", pch=23, col="yellow")
  lines(log_eps, means_unc, type='b', pch=19, col="yellow")
  lines(log_eps, means_unc+sdevs_unc, type='l', lty="dotted", col="yellow")
  
  lines(log_eps, medns_cor, type='b', lty="longdash", pch=23, col="green")
  lines(log_eps, means_cor, type='b', pch=19, col="green")
  lines(log_eps, means_cor+sdevs_cor, type='l', lty="dotted", col="green")
}
###########################################################
#
#
#
#
#----------------------------------------------------------
# Derivative method
boxplot(DS_cor[as.character(DS_cor$derivative.method)=='O(h^2)',]$calls.number,
        DS_cor[as.character(DS_cor$derivative.method)=='O(h^4)',]$calls.number,
        DS_cor[as.character(DS_unc$derivative.method)=='O(h^2)',]$calls.number,
        DS_cor[as.character(DS_unc$derivative.method)=='O(h^4)',]$calls.number,
        ylim=c(0,40000),
        names=c("correct O(h^2)", "correct O(h^4)", 
                "incorrect O(h^2)", "incorrect O(h^4)"))
#----------------------------------------------------------
#
#
#----------------------------------------------------------
# Stop criteria boxplots
boxplot(DS_cor[as.character(DS_cor$criteria.of.stop)=='norm',]$calls.number,
        DS_cor[as.character(DS_cor$criteria.of.stop)=='grad',]$calls.number,
        DS_cor[as.character(DS_unc$criteria.of.stop)=='norm',]$calls.number,
        DS_cor[as.character(DS_unc$criteria.of.stop)=='grad',]$calls.number,
        ylim=c(0,40000),
        names=c("correct NORM", "correct GRAD", 
                "incorrect NORM", "incorrect GRAD"))
#----------------------------------------------------------
#
#
#----------------------------------------------------------
# Restarts presence
boxplot(DS_cor[!DS_cor$restarts.presence,]$calls.number,
        DS_cor[DS_cor$restarts.presence,]$calls.number,
        DS_cor[!DS_unc$restarts.presence,]$calls.number,
        DS_cor[DS_unc$restarts.presence,]$calls.number,
        ylim=c(0,40000),
        names=c("correct without\nrestarts", "correct with\nrestarts", 
                "incorrect without\nrestarts", "incorrect with\nrestarts"))
#----------------------------------------------------------
#
#
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
#
# Chosen subsample
DS_opt <- DS_best[as.character(DS_best$criteria.of.stop)=="norm",]
DS_opt <- DS_opt[DS_opt$restarts.presence,]

# Deviding invalid, uncorrect and correct values
DS_opt_inv <- DS_opt[is.na(DS_opt$x..1),]
DS_opt_val <- DS_opt[!is.na(DS_opt$x..1),]
DS_opt_unc <- DS_opt_val[DS_opt_val$accuracy<DS_opt_val$reached.accuracy,]
DS_opt_cor <- DS_opt_val[DS_opt_val$accuracy >= DS_opt_val$reached.accuracy,]

# Function calls number
FCN_cor <- DS_opt_cor$calls.number
# What the median and IQR of it?
median(FCN_cor)
IQR(FCN_cor)
