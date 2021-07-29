require(graphics)

# Average
d1 <- dist(protein[,-1])
#d1 <- dist(USArrests)
hc <- hclust(d1, "ave")
d2 <- cophenetic(hc)
cor.avg <- cor(d1, d2) # 0.7193
cor.avg

# Single
d1 <- dist(protein[,-1])
hc <- hclust(d1, "single")
d2 <- cophenetic(hc)
cor.sing <- cor(d1, d2) # 0.5609
cor.sing

# Complete
d1 <- dist(protein[,-1])
#d1 <- dist(USArrests)
hc <- hclust(d1, "complete")
d2 <- cophenetic(hc)
cor.comp <- cor(d1, d2) # 0.7671
cor.comp

metode_ave<-hclust(dist(scale(protein[,-1])),method="ave")
plot(metode_ave)
rect.hclust(metode_ave,3)

metode_sing<-hclust(dist(scale(protein[,-1])),method="single")
plot(metode_sing)
rect.hclust(metode_sing,2)

metode_comp<-hclust(dist(scale(protein[,-1])),method="complete")
plot(metode_comp)
rect.hclust(metode_comp,4)
