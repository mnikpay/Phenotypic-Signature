args <- commandArgs(TRUE)

a1=paste(args[1],sep="")


a2=paste(a1,".results.txt",sep="")

dat=read.table("psign_v1.1.dat", header=T, row.names=1)

geneX <- a1

x <- as.numeric(dat[geneX, ])


results <- lapply(rownames(dat), function(g){

y <- as.numeric(dat[g, ])

n <- sum(complete.cases(x, y))


tmp <- cor.test(x,y, method="spearman")

rho <- unname(tmp$estimate)

se <- sqrt((1 - rho^2) / (n - 2))

    data.frame(
        Gene = g,
        N= n,
        rho = tmp$estimate,
                SE=se,
                abs_rho = abs(tmp$estimate),
        P = tmp$p.value

    )


})

results <- do.call(rbind, results)


write.table(results, file = a2, quote = FALSE, sep=" ", row.names=F)

