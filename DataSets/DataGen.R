source('Methods/DFP.R', echo=FALSE)
source('Counter/Counter.R', echo=FALSE)

# Function to be researched
rosenbrock <- function(x) (1-x[1])^2 + 100*(x[2]-x[1]^2)^2

# Link counter
f <- function(x) {
  increment()
  rosenbrock(x)
}

# Data frame types for columns
{
init_point = matrix(numeric(), nrow=0, ncol=2)
last_point = matrix(numeric(), nrow=0, ncol=2)
func_value = numeric()
accuracy = numeric()
got_accuracy = numeric()
function_calls = numeric()
deriv_step = numeric()
deriv_method = factor(x=NULL, levels=c(deriv_single_step, deriv_double_step), labels=c('O(h^2)', 'O(h^4)'))
onedim_method = factor(x=NULL, levels=c(onedim_GRM, onedim_DSKP), labels=c('GR', 'DSK-P'))
onedim_eps = numeric()
sven_alpha = numeric()
stop_criteria = factor(x=NULL, levels=c(stop_absolute, stop_gradient), labels=c('norm', 'grad'))
restarts = logical()
}

# Initial structure for Data Frame
DF_init <- data.frame(
  "x0"     = init_point,
  "x*"        = last_point,
  "function value"    = func_value,
  "accuracy"          = accuracy,
  "reached accuracy"  = got_accuracy,
  "calls number"      = function_calls,
  "derivative step"   = deriv_step,
  "derivative method" = deriv_method,
  "onedim method"     = onedim_method,
  "onedim accuracy"   = onedim_eps,
  "alpha in Sven"     = sven_alpha,
  "criteria of stop"  = stop_criteria,
  "restarts presence" = restarts
)

# Data Frame to be filled on each iteration and be merged after each DFP run
DF_row <- data.frame(
  "x0"     = rbind(init_point, c(NA,NA)),
  "x*"        = rbind(last_point, c(NA,NA)),
  "function value"    = c(func_value,NA),
  "accuracy"          = c(accuracy,NA),
  "reached accuracy"  = c(got_accuracy,NA),
  "calls number"      = c(function_calls,NA),
  "derivative step"   = c(deriv_step,NA),
  "derivative method" = factor(x=F, levels=c(deriv_single_step, deriv_double_step), labels=c('O(h^2)', 'O(h^4)')),
  "onedim method"     = factor(x=F, levels=c(onedim_GRM, onedim_DSKP), labels=c('GR', 'DSK-P')),
  "onedim accuracy"   = c(onedim_eps, NA),
  "alpha in Sven"     = c(sven_alpha, NA),
  "criteria of stop"  = factor(x=F, levels=c(stop_absolute, stop_gradient), labels=c('norm', 'grad')),
  "restarts presence" = F
)


################  Dataset 1 generation

# Parameters sets for first Dataset
start_points <- list(c(1,1), c(0,0))
accuracies <- c(1e0, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5)
der_steps <- c(1, 5e-1, 1e-1, 1e-2, 1e-3)
der_methods <- factor(c(deriv_single_step, deriv_double_step), levels=c(deriv_single_step, deriv_double_step), labels=c('O(h^2)', 'O(h^4)'))
onedim_methods <- factor(c(onedim_GRM, onedim_DSKP), levels=c(onedim_GRM, onedim_DSKP), labels=c('GR', 'DSK-P'))
onedim_accuracies <- c(1e-1, 1e-2, 1e-3, 1e-4)
sven_alphas <- c(1, 1e-1, 1e-2, 1e-3)
criterias <- factor(c(stop_absolute, stop_gradient), levels=c(stop_absolute, stop_gradient), labels=c('norm', 'grad'))
with_restarts <- c(restart, norestart)

# Initialization of Data Frame wich will be filled
DF <- DF_init
# Start brut force 
for (pt0 in start_points){
  DF_row$x0.1 <- pt0[1]
  DF_row$x0.2 <- pt0[2]
  
  for (eps in accuracies){
    DF_row$accuracy <- eps
    
    for (der_h in der_steps){
      DF_row$derivative.step <- der_h
      
      for (der_meth in der_methods){
        DF_row$derivative.method <- der_meth
        
        for (ods_meth in onedim_methods){
          DF_row$onedim.method <- ods_meth
          
          for (ods_eps in onedim_accuracies){
            DF_row$onedim.accuracy <- ods_eps
            
            for (alpha in sven_alphas){
              DF_row$alpha.in.Sven <- alpha
              
              for (criter in criterias){
                DF_row$criteria.of.stop <- criter
                
                for (rest in with_restarts){
                  DF_row$restarts.presence <- rest
                  
                  dfp_res <- DFP( f, 
                                  pt0,
                                  eps,
                                  der_h,
                                  ifelse(as.character(der_meth)=="O(h^4)", T, F),
                                  ifelse(as.character(ods_meth)=="DSK-P", T, F),
                                  ods_eps,
                                  alpha,
                                  ifelse(as.character(criter)=="grad", T, F),
                                  rest)
                  if (!is.list(dfp_res)){
                    DF_row$x..1 <- NA
                    DF_row$x..2 <- NA
                    DF_row$function.value <- NA
                    DF_row$reached.accuracy <- NA
                  }
                  else{
                    DF_row$x..1 <- dfp_res$minimum[1]
                    DF_row$x..2 <- dfp_res$minimum[2]
                    DF_row$function.value <- dfp_res$objective
                    DF_row$reached.accuracy <- vnorm(c(1,1)-dfp_res$minimum)
                  }
                  DF_row$calls.number <- discharge()
                  DF <<- rbind.data.frame(DF, DF_row)
                }
              }
            }
          }
        }
      }
    }
  }
}
# Writing got results into file
write.csv2(DF, paste("DataSets/DS1.csv", sep=''))



################  Dataset 2 generation

# Parameters sets for second Dataset
start_points <- list(c(3,-1), c(-1,3), c(15, 1), c(1, 15), c(-1,-1))
accuracies <- c(1e0, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5)
der_steps <- c(1e-1, 1e-3, 1e-5)
der_methods <- factor(c(deriv_single_step, deriv_double_step), levels=c(deriv_single_step, deriv_double_step), labels=c('O(h^2)', 'O(h^4)'))
onedim_methods <- factor(c(onedim_GRM, onedim_DSKP), levels=c(onedim_GRM, onedim_DSKP), labels=c('GR', 'DSK-P'))
onedim_accuracies <- c(1e-2, 1e-4, 1e-6)
sven_alphas <- c(1, 1e-1, 1e-2, 1e-3)
criterias <- factor(c(stop_absolute, stop_gradient), levels=c(stop_absolute, stop_gradient), labels=c('norm', 'grad'))
with_restarts <- c(restart, norestart)

# Initialization of Data Frame wich will be filled
DF <- DF_init
# Start brut force
for (pt0 in start_points){
  DF_row$x0.1 <- pt0[1]
  DF_row$x0.2 <- pt0[2]
  
  for (eps in accuracies){
    DF_row$accuracy <- eps
    
    for (der_h in der_steps){
      DF_row$derivative.step <- der_h
      
      for (der_meth in der_methods){
        DF_row$derivative.method <- der_meth
        
        for (ods_meth in onedim_methods){
          DF_row$onedim.method <- ods_meth
          
          for (ods_eps in onedim_accuracies){
            DF_row$onedim.accuracy <- ods_eps
            
            for (alpha in sven_alphas){
              DF_row$alpha.in.Sven <- alpha
              
              for (criter in criterias){
                DF_row$criteria.of.stop <- criter
                
                for (rest in with_restarts){
                  DF_row$restarts.presence <- rest
                  
                  dfp_res <- DFP( f, 
                                  pt0,
                                  eps,
                                  der_h,
                                  ifelse(as.character(der_meth)=="O(h^4)", T, F),
                                  ifelse(as.character(ods_meth)=="DSK-P", T, F),
                                  ods_eps,
                                  alpha,
                                  ifelse(as.character(criter)=="grad", T, F),
                                  rest)
                  if (!is.list(dfp_res)){
                    DF_row$x..1 <- NA
                    DF_row$x..2 <- NA
                    DF_row$function.value <- NA
                    DF_row$reached.accuracy <- NA
                  }
                  else{
                    DF_row$x..1 <- dfp_res$minimum[1]
                    DF_row$x..2 <- dfp_res$minimum[2]
                    DF_row$function.value <- dfp_res$objective
                    DF_row$reached.accuracy <- vnorm(c(1,1)-dfp_res$minimum)
                  }
                  DF_row$calls.number <- discharge()
                  DF <<- rbind.data.frame(DF, DF_row)
                }
              }
            }
          }
        }
      }
    }
  }
}
# Writing got results into file
write.csv2(DF, paste("DataSets/DS2.csv", sep=''))


################  Dataset 3 generation

# Parameters sets for third Dataset
start_points <- list(c(20,20), c(-20,-20))
accuracies <- c(1e-1, 1e-3, 1e-5)
der_steps <- c(1e-1, 1e-3, 1e-5)
der_methods <- factor(c(deriv_single_step, deriv_double_step), levels=c(deriv_single_step, deriv_double_step), labels=c('O(h^2)', 'O(h^4)'))
onedim_methods <- factor(c(onedim_GRM, onedim_DSKP), levels=c(onedim_GRM, onedim_DSKP), labels=c('GR', 'DSK-P'))
onedim_accuracies <- c(1e-2, 1e-4, 1e-6)
sven_alphas <- c(1, 1e-1, 1e-2, 1e-3)
criterias <- factor(c(stop_gradient), levels=c(stop_absolute, stop_gradient), labels=c('norm', 'grad'))
with_restarts <- c(restart)

# Initialization of Data Frame wich will be filled
DF <- DF_init
# Start brut force
for (pt0 in start_points){
  DF_row$x0.1 <- pt0[1]
  DF_row$x0.2 <- pt0[2]
  
  for (eps in accuracies){
    DF_row$accuracy <- eps
    
    for (der_h in der_steps){
      DF_row$derivative.step <- der_h
      
      for (der_meth in der_methods){
        DF_row$derivative.method <- der_meth
        
        for (ods_meth in onedim_methods){
          DF_row$onedim.method <- ods_meth
          
          for (ods_eps in onedim_accuracies){
            DF_row$onedim.accuracy <- ods_eps
            
            for (alpha in sven_alphas){
              DF_row$alpha.in.Sven <- alpha
              
              for (criter in criterias){
                DF_row$criteria.of.stop <- criter
                
                for (rest in with_restarts){
                  DF_row$restarts.presence <- rest
                  
                  dfp_res <- DFP( f, 
                                  pt0,
                                  eps,
                                  der_h,
                                  ifelse(as.character(der_meth)=="O(h^4)", T, F),
                                  ifelse(as.character(ods_meth)=="DSK-P", T, F),
                                  ods_eps,
                                  alpha,
                                  ifelse(as.character(criter)=="grad", T, F),
                                  rest)
                  if (!is.list(dfp_res)){
                    DF_row$x..1 <- NA
                    DF_row$x..2 <- NA
                    DF_row$function.value <- NA
                    DF_row$reached.accuracy <- NA
                  }
                  else{
                    DF_row$x..1 <- dfp_res$minimum[1]
                    DF_row$x..2 <- dfp_res$minimum[2]
                    DF_row$function.value <- dfp_res$objective
                    DF_row$reached.accuracy <- vnorm(c(1,1)-dfp_res$minimum)
                  }
                  DF_row$calls.number <- discharge()
                  DF <<- rbind.data.frame(DF, DF_row)
                }
              }
            }
          }
        }
      }
    }
  }
}
# Writing got results into file
write.csv2(DF, paste("DataSets/DS3.csv", sep=''))
