kdiff <- function(Xppp, r, cr = cr){
    k1 <- Kest(Xppp[marks(Xppp) == "case"], r = r, correction = cr)
    k2 <- Kest(Xppp[marks(Xppp) == "control"], r = r, correction = cr)
    res <- data.frame(r = r, D = k1[[cr]] - k2[[cr]])
    return(fv(res, valu = "D", fname = "D"))
}
