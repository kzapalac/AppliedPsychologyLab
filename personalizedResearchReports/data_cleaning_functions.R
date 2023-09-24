##### Data Processing Functions ####


# load_data -------------------------------------------------------
# purpose: load the data
# descr: load, remove extra headings, and combine the completed and in progress (optional) survey responses into one df
# note: You must download 2 csv files from Qualtrics (Recorded and In Progress) unless it has been >1 week since the last response was recorded
# args: 
  # completed_file = str indicating the file location
  # inProgress_file = (optional) str indicating the file location
# returns: a dataframe containing all of the pre or post data
load_data <- function(completed_file=NULL, inProgress_file=NULL){
  if (is.null(inProgress_file)) {
    # load the data
    completed <- read_csv(completed_file, show_col_types=FALSE)
    # remove the extra headings
    completed <- completed[-(1:2),] 
    # combine the complete and in progress responses
    data <- completed
  } else {
    # load the data
    completed <- read_csv(completed_file, show_col_types=FALSE)
    inProgress <- read_csv(inProgress_file, show_col_types=FALSE)
    # remove the extra headings
    completed <- completed[-(1:2),] 
    inProgress <- inProgress[-(1:2),]
    # combine the complete and in progress responses
    data <- rbind(completed, inProgress)
  }
  return(data)
}

# rename_cols -----------------------------------------------------------------
# purpose: rename questionnaires that don't follow the same naming scheme
# descr: You should try to consistently name questions when creating the survey,
  # both within the survey and across surveys, as this will create less work for you
  # If one of the questionnaires wasn't named properly though, you can change it
  # This function works best for changing substrings in multiple columns, 
  # otherwise you can just use the dplyr 'rename' function
# note: As an example, all the questions from the emotion regulation questionnaire 
  # should be titled 'ERQ1_pre', 'ERQ2_post', etc. but this may not be the case
  # They could be titled 'ERCA1_pre', 'ERCA2_pre', etc.
  # You will need to change these column names so that the code below works
# args: 
  # data = pre or post dataframe
  # oldSubString = str within the column name common to all the questions from that questionnaire
  # newSubString = new str you want within the column names corresponding to a scale
# returns: a dataframe with new column names
rename_cols <- function(data, oldSubString, newSubString){
  names(data) <- gsub(oldSubString, newSubString, names(data))
  return(data)
}

# remove_col_spaces -----------------------------------------------------------
# purpose: remove column spaces
# descr: it's best practice not to have space in column names, so remove spaces just in case a column name includes a space
# args:
  # data = pre or post dataframe
# returns: a dataframe without spaces in column names
remove_col_spaces <- function(data){
  names(data) <- gsub(" ", "", names(data))
  return(data)
}

# remove_extra_columns -------------------------------------------------
# purpose: remove the extra columns that don't contain data we will use
# descr: removes the extra data collected by Qualtrics that we don't need and makes the spreadsheet look nicer
# note: this step is not necessary, and the extraneous columns may not be named the same
  # If there any additional columns you would like to get rid of, add them to the vars list below
# args:
  # data = pre or post dataframe
# returns: a dataframe without extraneous columns
remove_extra_cols <- function(data){
  vars = c("Status", "IPAddress", "RecordedDate", "ResponseId",
           "RecipientLastName", "RecipientFirstName", 
           "RecipientEmail", "ExternalReference",
           "LocationLatitude", "LocationLongitude", "DistributionChannel",
           "UserLanguage", "student_ID2", "Q4_Browser", "Q4_Version",
           "Q4_OperatingSystem", "Q4_Resolution",
           "Q202_FirstClick", "Q202_LastClick", "Q202_PageSubmit",
           "Q202_ClickCount", "Q199_FirstClick", "Q199_LastClick",
           "Q199_PageSubmit", "Q199_ClickCount", "Q194_Browser", "Q194_Version",
           "Q194_OperatingSystem", "Q194_Resolution", "Q207_Browser", 
           "Q207_Version", "Q207_Resolution", "Q207_OperatingSystem")
  data <- data %>% select(-any_of(vars))
  return(data)
}

#  create_teacher_col -------------------------------------------------
# purpose: creates a column with the teacher name
# descr: pulls the teacher name the from the Q_URL column
# args: 
  # data = pre or post dataframe
# returns a dataframe with a column containing teacher names
create_teacher_col <- function(data){
  data$Q_URL <- sub(".*\\?", "", data$Q_URL)
  colnames(data)[which(names(data)=='Q_URL')] <- 'teacher'
  return(data)
}


# reformat_bday --------------------------------------
# purpose: convert bday months to numbers and ensure consistent formatting
# descr: convert months written in short or long form to numbers, remove leading 
  # 0's and ordinal appendix (st, nd, rd) from day
# args: 
  # data = pre or post df
# returns: a df with consistently formatted bdays (month and day represented as 
  # numbers without leading zeros or ordinal appendix)
reformat_bday <- function(data){
  if ('birthday_1' %in% colnames(data) & 'birthday_2' %in% colnames(data)){
    # turn months into numerical representation
    data$birthday_1 <- sub("(J|j)an.*", 1, data$birthday_1)
    data$birthday_1 <- sub("01", 1, data$birthday_1)
    data$birthday_1 <- sub("(F|f)eb.*", 2, data$birthday_1)
    data$birthday_1 <- sub("02", 2, data$birthday_1)
    data$birthday_1 <- sub("(M|m)ar.*", 3, data$birthday_1)
    data$birthday_1 <- sub("03", 3, data$birthday_1)
    data$birthday_1 <- sub("(A|a)pr.*", 4, data$birthday_1)
    data$birthday_1 <- sub("04", 4, data$birthday_1)
    data$birthday_1 <- sub("05", 5, data$birthday_1)
    data$birthday_1 <- sub("(M|m)ay", 5, data$birthday_1)
    data$birthday_1 <- sub("(J|j)une", 6, data$birthday_1)
    data$birthday_1 <- sub("06", 6, data$birthday_1)
    data$birthday_1 <- sub("(J|j)uly", 7, data$birthday_1)
    data$birthday_1 <- sub("07", 7, data$birthday_1)
    data$birthday_1 <- sub("(A|a)ug.*", 8, data$birthday_1)
    data$birthday_1 <- sub("08", 8, data$birthday_1)
    data$birthday_1 <- sub("(S|s)ep.*", 9, data$birthday_1)
    data$birthday_1 <- sub("09", 9, data$birthday_1)
    data$birthday_1 <- sub("(O|o)ct.*", 10, data$birthday_1)
    data$birthday_1 <- sub("(N|n)ov.*", 11, data$birthday_1)
    data$birthday_1 <- sub("(D|d)ec.*", 12, data$birthday_1)
    # removes ordinal appendix and leading 0 from day
    data$birthday_2 <- sub("st", "", data$birthday_2)
    data$birthday_2 <- sub("nd", "", data$birthday_2)
    data$birthday_2 <- sub("rd", "", data$birthday_2)
    data$birthday_2 <- sub("th", "", data$birthday_2)
    data$birthday_2 <- sub("(0)(?=\\d)", "", data$birthday_2, perl=TRUE) # replace 
    # 0 when it is followed by a digit
  } else {
    print("birthday_1 and birthday_2 don't exist as columns, so they can't be reformatted")
  }
  return(data)
}


# convert_dtypes -------------------------------------------
# purpose: convert column data types
# descr: convert the columns to the proper dtype
# args: 
# data = pre or post df
# returns a df with proper dtypes
convert_dtypes <- function(data){
  data <- type.convert(data, as.is=T) # infers the proper type
  data$student_ID <- as.character(data$student_ID) # need to keep this as character in case there are any letters in someone's id
  return(data)
}


# remove_previews --------------------------------------------------------------
# purpose: remove team previews
# descr: remove previews with the specified IDs (would be good to only use "test" as the preview ID in the future for ease of removal)
  # or those indicated by "preview" in the Q_URL
# args: 
  # data = pre or post dataframe
  # preview_IDs = (optional) list of IDs that were used by the team when previewing the survey
# returns: a df without team previews
remove_previews <- function(data, preview_IDs=c("alissa", "kennedy", "cmhp2222", "cmhp", "test")){
  data <- filter(data, !(student_ID %in% preview_IDs))
  data <- data[!grepl("preview", data$teacher),]
  data <- data[!grepl("cmhp", data$teacher),]
  return(data)
}


#  remove_blank ----------------------------------------------------------------
# purpose: remove blank responses
# descr: remove survey responses if there is no answers beyond the survey ID
# note: the total missing columns of data per row will not be greater than ncols-6
  # because there are 6 columns that will always have data. Used ncols-7 because
  # someone may have only filled in their ID and the rest of their answers are blank
# args:
  # data = pre or post df
# returns: a df without completely blank responses
remove_blank <- function(data){
  data <- data[!rowSums(is.na(data)) >= ncol(data)-7,]
  return(data)
}


# select duplicates ------------------------------------------------------------
# purpose: select duplicates to keep based on the rules below
# descr: pulls all the duplicates from the df and removes duplicates according to 
  # pre-determined duplicated rules, which are:
    # 1. Keep the row in which the student passed the attention check
    # 2. If they didn't pass the attention check, keep the one with higher completion
    # 3. If they didn't pass the attention check and had equal completion, keep 
      # the one that was started earlier
# arg:
  # data = pre or post dataframe
  # pre = TRUE or FALSE (changes what the column name is for the ATT_CHECK)
# returns: a new df with the duplicates to keep
select_duplicates <- function(data, pre){
  if (pre==TRUE){
    # create a table of all the unique student IDs and their frequencies
    n_student_ID <- data.frame(table(data$student_ID)) 
    # pulls any rows where the IDs are duplicates
    duplicates <- data[data$student_ID %in% n_student_ID$Var1[n_student_ID$Freq > 1],] 
    # sort so that the first duplicated survey will be the one we want to keep
    duplicates <- duplicates %>% arrange(student_ID, desc(ATT_CHECK_pre), desc(Progress), StartDate)
    # keep the first occurrence of any duplicate after sorting
    duplicates_keep <- duplicates %>% distinct(student_ID, .keep_all = T)
    return(duplicates_keep)
  } else {
    # create a table of all the unique student IDs and their frequencies
    n_student_ID <- data.frame(table(data$student_ID)) 
    # pulls any rows where the IDs are duplicates
    duplicates <- data[data$student_ID %in% n_student_ID$Var1[n_student_ID$Freq > 1],]
    # sort so that the first duplicated survey will be the one we want to keep
    duplicates <- duplicates %>% arrange(student_ID, desc(ATT_CHECK_post), desc(Progress), StartDate) 
    # keep the first occurrence of any duplicate after sorting
    duplicates_keep <- duplicates %>% distinct(student_ID, .keep_all = T)
    return(duplicates_keep)
  }
}


# remove_duplicates -----------------------------------------------------------
# purpose: remove duplicates
# descr: remove duplicates from the main df so that you can add the ones you are 
  # keeping back in the next step
# args:
  # data = pre or post dataframe
# returns: a df without the duplicates to be combined with df returned by select_duplicates
remove_duplicates <- function(data){
  # create a table of all the unique student IDs and their frequencies
  n_student_ID <- data.frame(table(data$student_ID)) 
  # remove the duplicates
  data <- data %>% filter(!(data$student_ID %in% n_student_ID$Var1[n_student_ID$Freq > 1]))
  return(data)
}


# create_composite ------------------------------------------------------------
# purpose: create a composite when given a variable name
# descr: given the pre or post df, questionnaire name, and number of questions, create a composite variable (mean)
# args:
  # data = pre or post df
  # Q_name = str indicating the questionnaire name
  # Q_number = number of questions
  # pre = T or F (changes the column names used below)
# notes:
  # STANDARDIZE VARIABLE NAMES TO MAKE CREATING COMPOSITES EASIER 
  # current code written for variables written like this: QuestionnaireName_#_pre or QuestionnaireName_#_post
  # could customize to use mean or sum in different cases
  # You likely won't be using this function since it's nested in the next one
# returns: a df with a new composite column
create_composite <- function(data, Q_name, Q_number, pre){
  if (pre==T){
    col_names = c()
    for (q in 1:Q_number) {
      col_name <- paste(Q_name, q, "_pre", sep="")
      col_names <- append(col_names, col_name)
    }
    comp_col_name <- paste(Q_name, "_pre_avg", sep="")
    data[[comp_col_name]] <- data %>% 
    select(all_of(col_names)) %>% 
      rowMeans()
  } else {
    col_names = c()
    for (q in 1:Q_number) {
      col_name <- paste(Q_name, q, "_post", sep="")
      col_names <- append(col_names, col_name)
    }
    comp_col_name <- paste(Q_name, "_post_avg", sep="")
    data[[comp_col_name]] <- data %>% 
      select(all_of(col_names)) %>% 
      rowMeans()
  }
  return(data)
}

# create_composites ------------------------------------------------------------
# purpose: create all the composites at once
# descr: When given a list of the questionnaire names and of how many questions there
  # are per questionnaire, will create all the composite columns for a dataset
# args:
  # data = pre or post df
  # Q_names = a character vector containing the overarching questionnaire variable names (ex: ERQ for emotion regulation questionnaire) 
  # Q_numbers: numerical vector containing the number of questions for each questionnaire
  # pre = T or F (indicates if this is the pre or post df and changes variable names accordingly)
# notes:
  # Make sure that the variables are titled properly or else this function won't work
  # For example, all the emotion regulation questions should be titled like this:
  # ERQ1_pre, ERQ2_pre, etc. or ERQ1_post, ERQ2_post, etc.
  # Have the overarching test come first, then the question #, and then _pre or _post

create_composites <- function(data, Q_names, Q_numbers, pre){
  for (q in 1:length(Q_names)){
    data <- create_composite(data=data, Q_name=Q_names[q], Q_number=Q_numbers[q], pre=pre)
  }
  return(data)
}

# match_IDs ----------------------------------------------------------------
# purpose: match pre and post responses by IDs and teacher
# descr: this is only one of the functions necessary to combine pre and post data
# args:
  # pre_data = pre df
  # post_data = post df
# returns: df with the matched surveys using student IDs and teacher name
match_IDs <- function(pre_data, post_data){
  matched_IDs <- merge(pre_data, post_data, by=c('student_ID', 'teacher'))
  return(matched_IDs)
}

# match_birthday_teacher -----------------------------------------------------
# purpose: match by students' birthdays and their teacher
# descr: this will match some of the remaining pre and post responses that don't have matching IDs
# args:
  # pre_data = pre df
  # post_data = post_df
# returns
match_birthday_teacher <- function(pre_data, post_data){
  # remove surveys with ID and teacher matches
  pre_without_post <- pre_data %>% filter(!(student_ID %in% post_data$student_ID))
  post_without_pre <- post_data %>% filter(!(student_ID %in% pre_data$student_ID))
  # match the remaining students based on birthday and teacher
  matched_birthday_teacher <- merge(pre_without_post, post_without_pre, 
                                    by=c('teacher', 'birthday_1', 'birthday_2', 'birthday_3'),
                                    all.x=FALSE, all.y=FALSE)
  return(matched_birthday_teacher)
}

# combine_pre_post -------------------------------------------------------------
# purpose: combine the pre and post dataset using IDs or birthdays and teacher
# descr: this does everything under the hood to merge the pre and post data
  # if you want to know more about the matching process, extract the code and explore
  # how many students weren't matched from each dataset (can create a function to do this though)
# args: 
  # pre_data = pre df
  # post_data = post df
# returns: a df with all the pre and post surveys that could be matched
combine_pre_post <-  function(pre_data, post_data){
  matched_IDs <- match_IDs(pre_data=pre_data, post_data=post_data)
  matched_birthday_teacher <- match_birthday_teacher(pre_data=pre_data, post_data=post_data)
  # append prep
  # remove y birthday columns amd rename x birthday columns
  matched_IDs <- matched_IDs %>% select(-c('birthday_1.y', 'birthday_2.y', 'birthday_3.y'))
  matched_IDs <- matched_IDs %>% rename(
    birthday_1 = birthday_1.x,
    birthday_2 = birthday_2.x,
    birthday_3 = birthday_3.x
  )
  matched_birthday_teacher <- matched_birthday_teacher %>% select(!('student_ID.y')) # removes the second student ID
  # rename the first student ID
  matched_birthday_teacher <- matched_birthday_teacher %>% rename(student_ID = student_ID.x)
  # append the birthday & teacher matches to the ID matches
  matched_pre_and_post_data <- rbind(matched_IDs, matched_birthday_teacher)
  return(matched_pre_and_post_data)
}


# match_similar_IDs ------------------------------------------------------------
# I didn't have time to build this function, but this would be useful to have to 
# retain as much data as possible. I think the best method would be to
# use a combination of str comparison functions that are already built and ascii codes
# I didn't see a function that did exactly what we are interested in, so you will have
# to build your own
# purpose: match people with IDs differing by 1 digit (1 different digit, swapped digits, and extra digit)
# stringsim might be a useful function for this, although it's not exactly what we need
# https://search.r-project.org/CRAN/refmans/stringdist/html/stringsim.html
# returns a value between 0 and 1
# 1 indicates identical strings and 0 indicates more disimilar strings
# based on how many changes are made to n elements
# could subtract from and multiply by n to determine how many changes have to be made to the string
# find n of the longest string using nchar()
# try matching IDs where only 1 digit differs (using default osa method)
#stringsim('12345', '12346')
#stringsim('123456', '123458')
#try matching ids where there is one digit less
#stringsim('12345', '1234')
#stringsim('1234', '12345') # same
# match ids where the characters are flipped
#stringsim('13245', '12345')
# 2 changes

