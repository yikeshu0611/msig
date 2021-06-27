
load('R/sysdata.rda')
version <- 'test'
msigdb <- msigdb[1:10,]
usethis::use_data(version,msigdb,internal = T,overwrite = T)

