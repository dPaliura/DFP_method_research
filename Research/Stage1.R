#############   STAGE 1

######    PART 1

# Read Data set for points (1,1) and (0,0)
DF1 <- read.csv2("DataSets/DS1.csv")[,-1]
# Trivial point records
DF1_tr <- DF1[DF1$x0.1==1,]
# Normal point records
DF1_nm <- DF1[DF1$x0.1==0,]


# Check for NA presence in records for trivial pt.
NA %in% DF1_tr$function.value
# Got TRUE, so need to reject such rows
DF1_tr_valid <- DF1_tr[!is.na(DF1_tr$function.value),]

# What the percent of valid values there?
nrow(DF1_tr_valid)/nrow(DF1_tr) * 100
# - Almost 92.2%

# Let's try to find out when this situation occurs
DF1_tr_inval <- DF1_tr[is.na(DF1_tr$function.value),]
unique(DF1_tr_inval$accuracy)           # no patterns
unique(DF1_tr_inval$derivative.step)    # no patterns
unique(DF1_tr_inval$derivative.method)  # only O(h^4) met
unique(DF1_tr_inval$onedim.method)      # only DSK-P met
unique(DF1_tr_inval$onedim.accuracy)    # no patterns
unique(DF1_tr_inval$alpha.in.Sven)      # no patterns
unique(DF1_tr_inval$criteria.of.stop)   # only norm met
unique(DF1_tr_inval$restarts.presence)  # no patterns
# So derivative with O(h^4) accuracy degree, DSK-Powell method and stop criteria 
# with norma occurs NA values while initial point is trivial.
# As we cat see only better methods occurs problems when point is trivial.
# It is probably might be due to gradient values wich are extra-close to zero.

# Check for percent of uncorrect minimums in valid records
sum(DF1_tr_valid$accuracy < DF1_tr_valid$reached.accuracy)/nrow(DF1_tr_valid) * 100
# 0% of uncorrect values, so with trivial points method works almost correct

######    PART 2

# Let's then analyse records for non-trivial initial point
# Check for NA presence in records for trivial pt.
NA %in% DF1_nm$function.value
# Got TRUE, so need to reject such rows
DF1_nm_valid <- DF1_nm[!is.na(DF1_nm$function.value),]

# What the percent of valid values there?
nrow(DF1_nm_valid)/nrow(DF1_nm) * 100
# - Slightly more than 92.6%

# Check for percent of uncorrect minimums in valid records
sum(DF1_nm_valid$accuracy < DF1_nm_valid$reached.accuracy)/nrow(DF1_nm_valid) * 100
# - Almost 27.1%! It's pretty much. More than quarter!

# Create variables for both subsets of correct and uncorrect values records
DF1_nm_cor <- DF1_nm[DF1_nm_valid$accuracy >= DF1_nm_valid$reached.accuracy,]
DF1_nm_unc <- DF1_nm[DF1_nm_valid$accuracy < DF1_nm_valid$reached.accuracy,]
N_unc <- nrow(DF1_nm_unc)

# Let's look on histograms and ratios to find out what affects
# Histoframs for scale variables
# Derivative step -- almost no influence
hist(log10(DF1_nm_unc$derivative.step))
# Onedimesional search accuracy -- no influence
hist(log10(DF1_nm_unc$onedim.accuracy))
# Alpha parameter in Sven method -- no influence
hist(log10(DF1_nm_unc$alpha.in.Sven))
# Ratios for factor variables
# Derivative method -- no influence
100*sum(as.character(DF1_nm_unc$derivative.method)=="O(h^2)")/N_unc
# Onedimensional search method -- no influence
100*sum(as.character(DF1_nm_unc$onedim.method)=="GR")/N_unc
# Stop criteria --  no influence
100*sum(as.character(DF1_nm_unc$criteria.of.stop)=="norm")/N_unc
# Restarts presence --  no influence
100*sum(!DF1_nm_unc$restarts.presence)/N_unc

# As we can't use information from trivial point because of almost no 
# calculations performed, so next way - look on load with number of function calls

# Derivative step - calls -- looks like it is some dependence, also it't notably on
# vicinity with zero
plot(log10(DF1_nm_cor$derivative.step), DF1_nm_cor$calls.number, cex=0.4)
indcs <- DF1_nm_cor$calls.number < 100000
plot(log10(DF1_nm_cor$derivative.step)[indcs], DF1_nm_cor$calls.number[indcs], cex=0.4)
# As we can see density is greater near zero as derivative step less, 
# so we can replace such steps as 1, 0.5, 1e-2 to step 1e-5

# Onedimesional search accuracy - calls -- very similar situation
plot(log10(DF1_nm_cor$onedim.accuracy), DF1_nm_cor$calls.number, cex=0.4)
# We need to look near zero
plot(log10(DF1_nm_cor$onedim.accuracy)[indcs], DF1_nm_cor$calls.number[indcs], cex=0.4)
# OK, it might be not bad to remove accuracies like 0.1 and 1e-3 and add accuracy 1e-6

# Sven alpha - calls -- looks like there is some dependence, also it't notably on
# vicinity with zero
plot(log10(DF1_nm_cor$alpha.in.Sven), DF1_nm_cor$calls.number, cex=0.4)
# Couldn't say something about dependencies. We should look near zero
plot(log10(DF1_nm_cor$alpha.in.Sven)[indcs], DF1_nm_cor$calls.number[indcs], cex=0.4)
# No beneficial information got. 

# We also can look on correlations, but it won't be beneficial
cor(log10(DF1_nm_cor$derivative.step), DF1_nm_cor$calls.number)
cor(log10(DF1_nm_cor$onedim.accuracy), DF1_nm_cor$calls.number)
cor(log10(DF1_nm_cor$alpha.in.Sven), DF1_nm_cor$calls.number)
# Just as I said no profit

# And last -- let's look on dependence from accuracy
plot(log10(DF1_nm_cor$accuracy), DF1_nm_cor$calls.number, cex=0.4)
# Pretty logical dependence. Rejections are noticeable.