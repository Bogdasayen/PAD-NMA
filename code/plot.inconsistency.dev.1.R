plot.inconsistency.dev <- function(b.object, b.object.inconsistency) {
  dev.consistent <- b.object$summary[grepl("dev", rownames(b.object$summary)) &
                                       !grepl("resdev", rownames(b.object$summary)) &
                                       !grepl("deviance", rownames(b.object$summary)), "mean"]
  dev.inconsistent <- b.object.inconsistency$summary[grepl("dev", rownames(b.object.inconsistency$summary))&
                                                       !grepl("resdev", rownames(b.object.inconsistency$summary)) &
                                                       !grepl("deviance", rownames(b.object.inconsistency$summary)), "mean"]
  plot(x = dev.consistent, y = dev.inconsistent, xlab = "Dev consistency", ylab = "Dev inconsistency")
       #xlim = c(0, 10), ylim = c(0, 10))

}