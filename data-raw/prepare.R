## code to prepare `msigdb` dataset goes here


msigdb <- NewMsigDB(xml)


usethis::use_data(msigdb,internal = TRUE,overwrite = TRUE,compress = 'xz')


c("standard_name", "systematic_name",

  "collection", "sub_collection", "organism",

  "description_brief", "description_full",

  "ensembl", "GeneSymbol", "EntrezId",

  "exact_source", "external_details_url", "chip",

  "author", "contributor", "contributor_org")




