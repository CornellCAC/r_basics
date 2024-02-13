require(dplyr)
# I added these libraries
require(tictoc)
require(bench)
require(data.table)

tic("load data")
set.seed(123)

# Helper function to create a function 
# to generate sequence expression observations 
make_gene_sampler = function() {
  sd_val = rexp(1)
  mean_val = rexp(1)
  return(function(x) rnorm(x, mean=mean_val, sd=sd_val))
}

# Simulates loading a large data file
num_samples = 1200
num_geneIDs = 30000 #60000

#initialize containers for results
col_names = paste("sample", 1:num_samples, sep="_")
expression_matrix = matrix(nrow = num_geneIDs, ncol = num_samples, dimnames=list(NULL, col_names))
row_names = vector("character", length = num_geneIDs) 

num_drawn = 1
while(num_drawn <= num_geneIDs) {
  num_reps = if (runif(1) < .9) 1 else ceiling(rlnorm(1, sd=1.1)) #rbinom(1,10,.2) + 1. 
  rand_name = paste(sample(letters, 4), collapse='')
  value_gen = make_gene_sampler()
  # generate data
  for (num_reps in 1:num_reps) {
    row_names[num_drawn] = rand_name
    expression_matrix[num_drawn,] = value_gen(num_samples)
    num_drawn = num_drawn + 1
    if (num_drawn > num_geneIDs)
      break
  }
}

expression_df = data.frame(genes=row_names, expression_matrix)
rm(expression_matrix)
#expression_df = expression_df[sample(1:nrow(expression_df)),]
toc()
# End simulation of loading large data file

tic("group_by")
consolidated_df = expression_df %>% group_by(genes) %>% summarise_all(list(sum))
toc()



#bench::mark(
#consolidated_df = expression_df %>% group_by(genes) %>% summarise_all(list(sum))
#)

#setDT(expression_df)
#bench::mark(
#  consolidated_dt = expression_df[base::order(genes), lapply(.SD, sum), by = .(genes)]
#)

#tic("data.table")
#setDT(expression_df)
#consolidated_dt = expression_df[base::order(genes), lapply(.SD, sum), by = .(genes)]
#toc()

