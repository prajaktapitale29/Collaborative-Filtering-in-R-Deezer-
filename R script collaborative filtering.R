library("tidyr")
library("recommenderlab")
library("reshape2")
library("arules")
library("dplyr")

setwd("D:\\MSBA\\Data Science Game")

stringsAsfactors = FALSE
#read data - read respective files by changing the file name
agg <- read.csv("train.csv")

agg_data <- aggregate(agg$is_listened ~ agg$user_id + agg$media_id, data = agg, mean)

# =============================================================================
# reshaping - usign aggregate data
g = reshape2::acast(agg, user_id ~ media_id, value.var = "is_listened")
R = as.matrix(g)
r = as(R, "realRatingMatrix")

# build the model =================================================
rec= recommenderlab::Recommender(r[1:nrow(r)],method="UBCF",
                                 param=list(method="Jaccard"))


###impute missing ratings ==============================================
recom <- predict(rec, r[1:nrow(r)], type="ratings")
#Pred_model = as(recom,"matrix") 

# other things to do here===========================================
# Convert all your recommendations to list structure
rec_list = as(recom, "list")


# ================ TESTING =================================

########## Create submission File from model #######################
# format test set same way
# select file and choose only certain columns
test <- read.csv("test.csv", header = TRUE)
testage = subset(test, user_age >= agemin & user_age < agemax)

# test columns to use
test1 = testage[,c(13,4)]

# Get ratings list and check ratings is blank
ratings<-NULL
u = NULL
u1 = NULL
x= NULL

# For all lines in test file, one by one ================================
for (u in 1:length(test1[,2]))
{
  # Read user_id and media_p from columns 1 and 2 of test data
  user_id = test1[u,1]
  media_id = test1[u,2]
  
  # Get as list & then convert to data frame all recommendations for user: userid
  u1 = as.data.frame(rec_list[[user_id==user_id]])
  
  # Create a (second column) column-id in the data-frame u1 and populate it with row-names
  # Remember (or check) that rownames of u1 contain are by media-ids
  # We use row.names() function
  u1$id<-row.names(u1)
  
  # Now access movie ratings in column 1 of u1
  x= u1[u1$id==media_id,1]
  
  # If no ratings were found, assign 0. You could also assign user-average
  if (length(x)==0)
  { 
    ratings[u] = 0
  }
  else
  {
    ratings[u] = x 
  }
}

final_test <-cbind(test1[,1],round(ratings))

# Write to a csv file: submitfile.csv in your folder
write.table(final_test,file="age21.csv",row.names=FALSE,col.names=FALSE,sep=',')




