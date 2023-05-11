test_that("calculate_pseudoabund works", {

  taxa <- c("taxa1", "taxa2", "taxa3", "taxa4")
   rel.abundances<-  c(runif(16, 0, 2000), runif(16, 0, 2000), runif(16, 0, 2000), runif(16, 0, 2000))
   data <- data.frame(taxa = rep(taxa, 16), rel.abundances, event = sort(rep(1:16,4)))
   abund_data <- data.frame(event = 1:16, tot.abund = runif(16, 2000, 5000))

   output <- calculate_pseudoabund(data, abund_data, rel_abund = rel.abundances, total_abund = tot.abund)

   # Check that the output has 5 columns
   expect_equal(ncol(output), 5)

})
