# main code
# replace data_path with the path to the csv file
# Assumes that the first column on the data file is
# named Candidate.Question, which is then removed
# author: Divyanshu Vats


library(dplyr)
library(magrittr)
library(mirt)
source("rank_items.r")

# return irt parameters
get_irt_params <- function(mdl) {
    cc <- coef(mdl)
    scale <- lapply(cc, function(c) c[1]) %>% unlist
    diff <- lapply(cc, function(c) c[2]) %>% unlist
    diff <- diff[1:length(diff)-1]
    ability <- fscores(mdl, full.scores=TRUE, method='MAP')

    list(ability=ability, diff=diff, scale=scale)
}

# set seed
set.seed(1)

# insert data path
data_path <- ""

# read csv file
tryCatch({
   Y <- read.csv(data_path) %>% select(-Candidate.Question)
}, error = function(err) {
   print(err)
})

# replace -1 with NA
Y[Y == -1] <- NA

# drop columns with no values
cm <- colMeans(Y, na.rm=TRUE)
Y_new <- Y[, !is.nan(cm)]
column_names <- names(Y_new)

# Apply mIRT to Y_new
mdl <- mirt(Y_new, model=1, itemtype='Rasch', 
            verbose=TRUE, technical=list(NCYCLES=10000))

# get irt parameters
irt_params <- get_irt_params(mdl)

# difficulty parameters
diff <- as.data.frame(irt_params$diff)

# rank items in Y_new
item_ranking <- rank_items(mdl, m=0) %>% as.data.frame
names(item_ranking) <- c("rank")
item_ranking$item.name <- column_names[item_ranking$rank]

# write to csv file
write.csv(item_ranking, "item_ranking.csv", row.names=FALSE)
write.csv(diff, "item_difficulty.csv", row.names=TRUE)
