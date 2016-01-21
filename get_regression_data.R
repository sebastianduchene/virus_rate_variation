library(NELSI)


rooted_trees <-   dir('ml_trees/', pattern = '^rooted_')


for(i in 1:length(rooted_trees)){
    tr <- read.nexus(paste0('ml_trees/', rooted_trees[i]))
    split_names <-  strsplit(tr$tip.label, '_')
    dates <- as.numeric(sapply(split_names, function(x) x[length(x)]))
    tip_depth <- allnode.times(tr, tipsonly = T)
    out_frame <- data.frame(tr$tip.label, dates, tip_depth)
    write.table(out_frame, file = gsub('phy_phyml_tree', '.csv',  rooted_trees[i]), sep = ',', row.names = F)
}

plot(dates, tip_depth)
