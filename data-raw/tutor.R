
# browse
# --- show
show_browse_collection()
# --- query
x <- msigdb_browse('immune')
x |>
    msigdb_filt('system') |>
    msigdb_view('process')

# search
email <- '15211290016@fudan.edu.cn'
# --- show
show_search_collection()
show_search_organism()
show_search_contributor()

# --- query
x <- msigdb_search('immune & response','C2')
x |>
    msigdb_filt('atll','adult') |>
    msigdb_view()


# detail
x <- related_geneset('DESCARTES_FETAL_PLACENTA_STROMAL_CELLS')
x |>
    msigdb_filt('fetal') |>
    msigdb_view()


x <- similarity_geneset('REACTOME_DEGRADATION_OF_AXIN')

x |>
    msigdb_filt('apo') |>
    msigdb_view('med')



# gene
x <- msigdb_gene('HP_STROKE_LIKE_EPISODE')
x |>
    msigdb_filt('mit') |>
    msigdb_view()


x <- msigdb_gene('HP_ISCHEMIC_STROKE','HP_STROKE_LIKE_EPISODE')
x |>
    msigdb_view('mit','mt')














