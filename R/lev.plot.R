##' Plotting leverages against pearson residuals for linear mixed effect models
##'
##' This function plots the leverages (calculated by HLMdiag ::leverage) against pearson residuals from a linear mixed effect model fitted by lmer function.
##' @param mod a lmer model
##' @param type 'overall' plots the overall leverage, 'fixef' plots the leverage corresponding to the fixed effects, 'ranef' plots the leverage corresponding to the random effects. Default is 'overall'.
##' @examples
##' data(sleepstudy)
##' mod<-lmer(Reaction ~ Days + (Days|Subject), data=sleepstudy)
##' lev.plot(mod, type='fixef')
##' @author Yian Guo, Yuting Yang, Xiaoman Zhai
##' @export


lev.plot <- function(mod, type = "overall") {
    lev <- leverage(mod, level = 1)
    colnames(lev) <- c("overall leverage", "fixed effect leverage", "random effect leverage")
    if (type == "overall") {
        ggplot(data.frame(lev = lev[, 1], pearson = residuals(mod, type = "pearson")), aes(x = lev, y = pearson)) + geom_point() + theme_bw() + 
            ggtitle(colnames(lev)[1])
    } else if (type == "fixef") {
        ggplot(data.frame(lev = lev[, 2], pearson = residuals(mod, type = "pearson")), aes(x = lev, y = pearson)) + geom_point() + theme_bw() + 
            ggtitle(colnames(lev)[2])
    } else if (type == "ranef") {
        ggplot(data.frame(lev = lev[, 3], pearson = residuals(mod, type = "pearson")), aes(x = lev, y = pearson)) + geom_point() + theme_bw() + 
            ggtitle(colnames(lev)[3])
    }
}
