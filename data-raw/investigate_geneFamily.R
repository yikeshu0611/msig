


investigate_geneFamily <- function(genes,species='Human',email){
    geneList <- paste0(genes,collapse = ' ')
    url0 <- 'http://www.gsea-msigdb.org/gsea/msigdb/gene_families.jsp?geneList=%s&speciesName=%s'
    url <- sprintf(url0,geneList,species)
    g <- httr::GET(URLencode(url),httr::authenticate(email, "password"))
    t <- httr::content(g)

    # table
    tbl <- t |>
        rvest::html_nodes(xpath='//table[@class]') |>
        rvest::html_table()
    tb <- as.data.frame(tbl[[1]])
    colnames(tb) <- do::Replace0(colnames(tb),c('\\[.*','\n {0,}','contextIds'))
    tf <- do::Replace0(tb,c('\\[.*','\n {0,}','contextIds'))
    tf[do::Nchar(tf) == 0]=NA
    rownames(tf) <- tf[,1]
    tf <- tf[,-1]
    # common genes

    number <- t |>
        rvest::html_nodes(xpath='//table[@class]//tr//td/a') |>
        rvest::html_text()
    genes <- t |>
        rvest::html_nodes(xpath='//table[@class]//tr//td/form/input') |>
        rvest::html_attr('value')

    # list columns
    for (i in 1:nrow(tf)) {
        if (i==1) cb <- c()
        col_nms <- colnames(tf)[tf[i,] != 0 & !is.na(tf[i,])]
        if (length(col_nms) == 0) next(i)
        cp <- paste0(rownames(tf)[i],'<--->',col_nms)
        cb <- c(cb,cp)
    }
    bp <- paste0(cb,'<--->',number,'<--->',genes)
    df <- do::col_split(bp,'<--->',colnames = c('family_row','family_col','number','genes'))
    df$genes <- strsplit(df$genes,',')

    # link
    url0 <- 'http://www.gsea-msigdb.org/gsea/msigdb/annotate.jsp?geneList=%s&amp;speciesName=%s'

    gnl <- sapply(strsplit(genes,','),function(i) paste0(i,collapse = ' '))
    link <- sprintf(url0,gnl,species)


    # combination
    tx <- t(tf)
    tx[tx != 0 & !is.na(tx)] <- genes
    tt <- t(tx)



    for (i in 1:ncol(tt)) {
        if (i == 1) rt=list()
        ck <- tt[,i] != 0 & !is.na(tt[,i])
        if (sum(ck) == 0) next(i)
        gs <- unique(unlist(strsplit(tt[ck,i],',')))
        rti <- list(gs)
        names(rti) <- colnames(tt)[i]
        rt <- c(rt,rti)
    }
    prt <- paste0(paste0('rt[[',1:length(rt),']]'),collapse = ',')
    rtdf <- eval(parse(text=sprintf('set::combination(%s)',prt)))
    colnames(rtdf)[1:length(rt)] <- names(rt)
    colnames(rtdf)[ncol(rtdf)] <- 'Genes'
    rtdf$Genes <- strsplit(rtdf$Genes,';')
    venn_data <- rtdf[!is.na(rtdf[,ncol(rtdf)]),]

    # oncoplot
    lx <- lapply(1:length(rt), function(i) matrix(rep(1,length(rt[[i]])),
                                                  nrow = 1,
                                                  dimnames = list(names(rt)[i],rt[[i]])) |> as.data.frame())
    onco <- do.call(plyr::rbind.fill,lx)
    rownames(onco) <- names(rt)
    onco <- onco[,order(colSums(onco,TRUE),decreasing = TRUE)]


    onco <- onco[order(onco$CDX2),]

    pheatmap <- onco
    onco
    onco <- onco[order(rowSums(onco,TRUE),decreasing = TRUE),]


    list(tf=tf,
         df=df,
         link=link,
         UpSetR=UpSetR::fromList(rt),
         venn_data=venn_data,
         pheatmap=pheatmap)
}



