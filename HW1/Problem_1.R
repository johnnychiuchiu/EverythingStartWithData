my_anova <- function(data, group){
  ### It is a function that calculate one-way anova, which is a technique that can be used to compare means of two or more samples ###
  
  # calculate SS_Total
  SS_Total = sum(data^2)-(sum(data)^2)/length(data)
  
  # calculate SS_Treatment
  anova_df= data.frame(resp=data, group=group)
  cal_SS_Treatment_df = anova_df %>% 
    group_by(group) %>% 
    dplyr::summarise(sum_resp=sum(resp), count=n()) %>%
    dplyr::mutate(sst=sum_resp^2/count)
  
  SS_Treatment = sum(cal_SS_Treatment_df$sst)-(sum(data)^2)/length(data)
  
  # calculate SS_Error
  SS_Error = SS_Total-SS_Treatment
  
  # calculate MS_Treatment
  k=length(unique(group))
  MS_Treatment = SS_Treatment/(k-1)
  
  # calculate MS_Error
  N=length(data)
  MS_Error = SS_Error/(N-k)
  
  # calculate F-statistics 
  f_statistics = MS_Treatment/MS_Error
  
  # calculate p value of F-statistics 
  p_value= 1-pf(f_statistics, df1=(k-1), df2=(N-k))
  
  result = ifelse(p_value<0.01,
                  paste('Reject null hypothesis at significance level 0.01 with p-value',p_value),
                  paste('Do not reject null hypothesis at significance level 0.01',p_value))
  return(result)
}

# an example using data “Tensile.txt”
tensile <-read.table("Everything Starts with Data/HW/HW1/Tensile.txt", header=T)
resp <- c(t(as.matrix(tensile)))
treats <- c("HC5","HC10","HC15","HC20")
k <- 4
n <- 6
tm <- gl(k,1,n*k,factor(treats))

print(my_anova(resp, tm))
