library(NELSI)


rooted_trees <-   dir('ml_trees/', pattern = '^rooted_')
rooted_trees <- rooted_trees[-grep('[.]csv', rooted_trees)]

for(i in 1:length(rooted_trees)){
    print(rooted_trees[i])
    tr <- read.nexus(paste0('ml_trees/', rooted_trees[i]))


# OJO: only works for rooted trees, where the root index should be skipped
reg_stats <- matrix(NA, tr$Nnode -1, 3)
colnames(reg_stats) <- c('node', 'slope', 'rsquared')

if(F){
    for(n in (2:tr$Nnode)+length(tr$tip.label)){
        new_rooted <- root(tr, node = n, resolve.root = T)
        split_names <-  strsplit(new_rooted$tip.label, '_')
        dates <- as.numeric(sapply(split_names, function(x) x[length(x)]))
        allnode_heights <- allnode.times(new_rooted)
        root_height <- max(allnode_heights)
        tip_heights <- root_height - allnode_heights[1:length(new_rooted$tip.label)]
        reg_tree <- lm(tip_heights ~ dates)
        reg_stats[n - length(tr$tip.label)-1, ] <- c(n, reg_tree$coefficients[2], summary(reg_tree)$r.squared)
    }

    positive_slopes <- reg_stats[, 'slope'] > 0
    if(any(positive_slopes)){
        optimal_node <- reg_stats[which.min(reg_stats[positive_slopes, 'rsquared']), 'node']
    }else{
        optimal_node <- reg_stats[which.min(reg_stats[, 'rsquared']), 'node']
    }

    rerooted_tree <- root(tr, node = optimal_node, resolve.root = T)
}else{
    rerooted_tree <- tr
}
    split_names <-  strsplit(rerooted_tree$tip.label, '_')
    dates <- as.numeric(sapply(split_names, function(x) x[length(x)]))
    allnode_heights <- allnode.times(rerooted_tree)
    root_height <- max(allnode_heights)
    tip_heights <- root_height - allnode_heights[1:length(rerooted_tree$tip.label)]
    out_frame <- data.frame(rerooted_tree$tip.label, dates, tip_heights)
    write.table(out_frame, file = paste0('ml_trees/', gsub('[.]phy_phyml_tree', '.csv',  rooted_trees[i])), sep = ',', row.names = F)
}




