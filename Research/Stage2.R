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
# Again, accuracy grows causes small invalidance grows 
# and uncorrectness decrease
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
# 7.32% is much less then 45.4% and it is pretty interesting

# And so what the percentage of correct values&
N_best_cor <- nrow(DS_best_cor)
N_best_cor/N_best *100
# 76.6% is MUCH more then 35% as in general sample

# what the percentage of correct values in this cases from general number
# of correct values
N_best_cor/N_cor *100
# 18.5% from all correct values.
# What the part of all this records from all records?
N_best/N *100
# only 8.45% cases from general sample occupy 18.5% of all correct records
# It's really nice.

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



######    PART 2

