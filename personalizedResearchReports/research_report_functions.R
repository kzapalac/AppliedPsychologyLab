### Research Report Functions ###

# baseline_summary ------------------------
# purpose: creates a sentence summarizing one variable's baseline results
# descr: Provides the average score on a questionnaire for all of a teacher's students
  # that passed the attention check and compares it to the total sample average
# args:
  # x = variable name as a string
  # max = maximum score possible on the questionnaire
  # questionnaire name = (str) name for the questionnaire to use in the sentence
# returns: sentence summarzing the baseline results for the specified variable
# notes:
  # the dfs are specified within the function, so make sure your dfs are named the same
  # (which they should be since that's the name the script gives those dfs)
  # You can include the 2 dfs as arguments if you want to specify that in the future
  # for example, if you want to create a baseline summary for all of the students,
  # even those that didn't pass the attention check
baseline_summary <- function(x, max, questionnaire_name){
  total_avg <- mean(pre_data_pass[[x]], na.rm=T) %>% round(digits=2)
  teacher_avg <- mean(teacher_pre_pass[[x]], na.rm=T) %>%  round(digits=2)
  result <- paste("Your students' average score on the ", questionnaire_name, " questionnaire was ", teacher_avg, " out of ", max, ". In comparison the average score from the total sample this semester was ", total_avg, ".",   sep="")
  return(result)
}


# ttest_results ----------------------------------------------------------------
# purpose: runs two-sided, paired t-test and outputs a summary sentence with the results
# descr: 
# args:
  # post = matched_pre_and_post_data_pass$varColumnName_post 
  # pre = matched_pre_and_post_data_pass$varColumnName_pre
  # variable name = (str) questionnaire name that you want in the summary sentence
# returns: sentence summarizing the t-test results for the specified variable
# notes:
  # access the column you need from the df you want with the $ accessor like this
  # matched_pre_and_post_data_pass$ERQ_post_avg
  # used the matched_pre_and_post_df in my code, so t only looks at students that passed both att checks
  # you can change the sentence returned depending on the significance level
  # right now, the summary sentence will say the result was marginally significant
  # if 0.05 < p < 0.1 and significant if p < 0.05
ttest_result <- function(post, pre, variable_name){
  ttest_output <- t.test(post, pre, paired=T)
  ttest_t <- ttest_output$statistic %>% round(digits=2)
  ttest_p <- ttest_output$p.value %>% round(digits=3)
  ttest_df <- ttest_output$parameter
  if (ttest_p < .001 & ttest_t > 0){
    result <- paste("We found a significant increase in ",variable_name,", t(", ttest_df, ") = ",ttest_t, ", p < .001.", sep="")
  } else if (ttest_p < .05 & ttest_t > 0){
    result <- paste("We found a significant increase in ",variable_name,", t(", ttest_df, ") = ",ttest_t, ", p = ",ttest_p,".", sep="")
  } else if (ttest_p < .1 & ttest_t >0){
    result <- paste("We found a marginally significant increase in ",variable_name,", t(", ttest_df, ") = ",ttest_t, ", p = ",ttest_p,".", sep="")
  } else if (ttest_p < .001 & ttest_t < 0){
    result <- paste("We found a significant decrease in ",variable_name,", t(", ttest_df, ") = ",ttest_t, ", p < .001.", sep="")
  } else if (ttest_p < .05 & ttest_t < 0){
    result <- paste("We found a significant decrease in ",variable_name,", t(", ttest_df, ") = ",ttest_t, ", p = ",ttest_p,".", sep="")
  } else if (ttest_p < .1 & ttest_t < 0){
    result <- paste("We found a marginally significant decrease in ",variable_name,", t(", ttest_df, ") = ",ttest_t, ", p = ",ttest_p,".", sep="")
  } else {
    result <- paste("We did not find a significant change in ",variable_name,", t(", ttest_df, ") = ",ttest_t, ", p = ",ttest_p,".", sep="")
  }
  return(result)
}


# df_for_graphing --------------------------------------------------------------
# purpose: Create a summary df for one variable, which can then be used to graph changes across time
# descr:  The df includes counts, means, sds, ses, and cis for the pre and post data. 
  # Can be used to create a graph of the pre and post mean with 95% confidence interval
# args: 
  # df = matched_pre_and_post_data_pass
  # var = (str) overarching questionnaire abbreviation (ex: "ERQ")
  # pre_append = (pre-set, but you can change) abbreviation at the end of the pre composite columns
  # post_append = (pre-set, but you can change) abbreviation at the end of the post composite columns
df_for_graphing <- function(df, var, pre_append="_pre_avg", post_append="_post_avg"){
  # automatically set up for the composites, but can be set up for individual variables
  # pre
  parameter_name <- paste(var, pre_append, sep="")
  df2 <- data.frame(parameter_name = df[[parameter_name]])
  df2 <- df2 %>%  drop_na()
  pre_sum <- df2 %>% 
    dplyr::summarise(name="Pre", count=n(), means=mean(parameter_name), sds=sd(parameter_name))
  pre_sum <- mutate(pre_sum, ses = sds/sqrt(count))
  ciMult <- qt(.95/2 + .5, pre_sum$count-1)
  pre_sum <- mutate(pre_sum, cis = ciMult * ses)
  pre_sum <- mutate(pre_sum, lower = means - cis)
  pre_sum <- mutate(pre_sum, upper = means + cis)
  # post
  parameter_name <- paste(var, post_append, sep="")
  df2 <- data.frame(parameter_name = df[[parameter_name]])
  df2 <- df2 %>%  drop_na()
  post_sum <- df2 %>% 
    dplyr::summarise(name="Post", count=n(), means=mean(parameter_name), sds=sd(parameter_name))
  post_sum <- mutate(post_sum, ses = sds/sqrt(count))
  ciMult <- qt(.95/2 + .5, post_sum$count-1)
  post_sum <- mutate(post_sum, cis = ciMult * ses)
  post_sum <- mutate(post_sum, lower = means - cis)
  post_sum <- mutate(post_sum, upper = means + cis)
  # combine pre and post
  pre_post_sum <- rbind(pre_sum, post_sum)
  return(pre_post_sum)
}


# graph ------------------------------------------------------------------------
# purpose: graphs the changes from pre to post
# descr: Pre and post means are surrounded by 95% CIS and the graph axes are automatically adjusted
  # so that the change from pre to post is apparent
# args:
  # data = graphing df containing counts, means, sds, ses, and cis (created using df_for_graphing function)
  # ylab = (str, optional) customized label for the y-axis
  # title = (str, optional) customized graph title (if you're combining multiple graphs in a panel
    # you may not want to inlcude a title)
# returns: a graph displaying the changes from pre to post for a specified variable
# notes: 
  # the graphs may look funny in your code, but they will look okay when you knit the document
  # if you adjust their size
graph <- function(data, ylab=NULL, title=NULL){
  y_ax_min = min(data['lower'])*.95 # automatically adjusts the y-axis minimum
  y_ax_max = max(data['upper'])*1.05 # automatically adjusts the y-axis maximum
  graph <- ggplot(data) +
    geom_bar(aes(x=factor(name, level=c("Pre", "Post")), y=means, fill=name), stat="identity", alpha=0.7, show.legend=F) + 
    geom_errorbar(aes(x=name, ymin = lower, ymax = upper, 
                      width = 0.2)) +
    ylab(ylab) + 
    xlab("") +
    ggtitle(title) + coord_cartesian(ylim = c(y_ax_min, y_ax_max)) +
    scale_fill_grey() +
    theme_bw() +
    theme(plot.margin = unit(c(1,1,1,1), "lines")) +
    theme(axis.text=element_text(size=13), axis.title=element_text(size=13)) +
    theme(plot.title = element_text(hjust = 0.5))
  return(graph)
}