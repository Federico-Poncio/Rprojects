# Significance test permutation
dataset <- read.csv("dataset_regla1_conVOs_yNAs.csv")

dataset.na <-dataset
dataset.na[dataset.na == 9] <- NA
dataset$norm <- rnorm(dataset$EDAD_AGRUP, mean = dataset$EDAD_AGRUP)
dt <- setDT(dataset.na)

gen_original_diff <- function(colname){
  group_vector = dataset$EDAD_AGRUP
  dataset.na <-dataset
  dataset.na[dataset.na == 9] <- NA
  dt <- setDT(dataset.na)
  A <- dt[,lapply(.SD,weighted.mean,w=pesos_finales, na.rm=TRUE), 
          by = group_vector, .SDcols = colname]
  setnames(A, colname, "question")
  #categorías: 2, 3, 4, 5, 6
  # DIFERENCIAS: 2-3, 3-4, 4-5, 5-6, 2-4, 2-5, 2-6, 3-5, 3-6, 4-6
  A<- A[order(group_vector)]
  
  original_difs <- c(A[1,question]-A[2,question],
                     A[2,question]-A[3,question],
                     A[3,question]-A[4,question],
                     A[4,question]-A[5,question],
                     A[1,question]-A[3,question],
                     A[1,question]-A[4,question],
                     A[1,question]-A[5,question],
                     A[2,question]-A[4,question],
                     A[2,question]-A[5,question],
                     A[3,question]-A[5,question])
  return(original_difs)
}
gen_permutations <- function(reps,colname){
  shuffle_difs <- c()
  dataset.na <-dataset
  dataset.na[dataset.na == 9] <- NA
  dt <- setDT(dataset.na)
  for(i in 1:reps){
    group_vector_shuffled <- sample(group_vector, replace = FALSE)
    A <- dt[,lapply(.SD,weighted.mean,w=pesos_finales, na.rm=TRUE), 
            by = group_vector_shuffled, .SDcols = colname]
    setnames(A, colname, "question")
    A<- A[order(group_vector_shuffled)]
    shuffle_difs <- rbind(shuffle_difs,
                          c(A[1,question]-A[2,question],
                       A[2,question]-A[3,question],
                       A[3,question]-A[4,question],
                       A[4,question]-A[5,question],
                       A[1,question]-A[3,question],
                       A[1,question]-A[4,question],
                       A[1,question]-A[5,question],
                       A[2,question]-A[4,question],
                       A[2,question]-A[5,question],
                       A[3,question]-A[5,question]))
    }
  return(shuffle_difs)
}
get_proportions <- function(reference, observ){
  ref_abs <- abs(reference)
  obs_abs <- abs(observ)
  pvals <- c()
  for (colnum in 1:10){
    greater_than <- sum(obs_abs[,colnum]>=ref_abs[colnum])
    newval <- greater_than / length(obs_abs[,colnum])
    pvals <- c(pvals, newval)
  }
  return(pvals)
}

shuffle_difs <- gen_permutations(200, "P2")
original_difs <- gen_original_diff("P2")
get_proportions(original_difs, shuffle_difs)

proportions <- c()
varnames <- c()
for (col.name in colnames(dataset)[7:21]){ #P2 en adelante
  varnames <- c(varnames, col.name)
  shuffle_difs <- gen_permutations(900, col.name)
  original_difs <- gen_original_diff(col.name)
  props <- get_proportions(original_difs, shuffle_difs)
  proportions <- rbind(proportions, props)
  print(col.name)
}

prop.df <- as.data.frame(proportions, row.names = varnames)
colnames(prop.df) <- c("2-3", "3-4", "4-5", "5-6", "2-4", "2-5", "2-6", "3-5", "3-6", "4-6")
write.csv(prop.df, "pvalues_permutations.csv")


for (i in 1:4){
dataset$norm <- rnorm(dataset$EDAD_AGRUP, mean = dataset$EDAD_AGRUP,sd = i)
hist(dataset$norm,breaks = 300, main =paste("Histograma ejemplo permutaciones \n sigma = " , i))
shuffle_difs <- gen_permutations(200, "norm")
original_difs <- gen_original_diff("norm")
print(get_proportions(original_difs, shuffle_difs))
}

dataset$norm <- rnorm(dataset$EDAD_AGRUP, mean = dataset$EDAD_AGRUP,sd = 20)
hist(dataset$norm,breaks = 300, main =paste("Histograma ejemplo permutaciones \n sigma = " , i))
shuffle_difs <- gen_permutations(200, "norm")
original_difs <- gen_original_diff("norm")
print(get_proportions(original_difs, shuffle_difs))