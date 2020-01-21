fc <- FormalContext$new(cobre32, remove_const = TRUE)
fc$find_implications()

dataset <- as.matrix(t(fc$I))
implications <- fc$implications$clone()
supp <- implications$support()
implications <- implications[-which(supp == 0)]

S <- generate_subjects(n = 50, dataset = dataset,
                       parameters = list(implications = implications, 
                                         grades_set = fc$expanded_grades_set,
                                         number_seeds = 3, 
                                         forbidden_attrs = c("dx_ss", "dx_other")))

critiquing <- "compound" # "unit" # "none"
elicit_type <- "knowledge" # "zscore" # "importance" # "random"
class_attrs <- c("dx_ss", "dx_other")
subject <- SparseSet$new(attributes <- fc$attributes,
                         M = Matrix(S$valid[2, ], sparse = TRUE))

precedence <- elicit_precedence(dataset, 
                                subject = subject, 
                                type = elicit_type)
