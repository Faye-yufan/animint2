library(animint2)
library(data.table)
library(profvis)

n.vars <- 30
n.samples <- 500
n.group <- 100

var.list <- list()
for (var in 1:n.vars) {
  var.list[[paste("var", var, sep = ".")]] <- data.table(
    val = rnorm(n.samples)
  )
}

var.table <- data.table(do.call(cbind, var.list))
var.table <- data.table(
  var.table,
  n = seq_along(1:n.samples),
  group = seq_along(1:n.group)
)

var.names <- names(var.table)
var.names <- var.names[var.names != "n" & var.names != "group"]
var.name.dt <- data.table(
  var.main.name = rep(var.names, each = length(var.names)),
  var.second.name = var.names,
  y = seq_along(var.names)
)

long.main.var.table <- data.table(reshape2::melt(
  var.table,
  id.vars = c("n", "group"),
  variable.name = "var.main.name",
  value.name = "var.main.val"
))

long.second.var.table <- data.table(reshape2::melt(
  var.table,
  id.vars = c("n", "group"),
  variable.name = "var.second.name",
  value.name = "var.second.val"
))

combinations.data.table <- long.main.var.table[long.second.var.table, on = c("n"), allow.cartesian = TRUE]

text.plot1 <- ggplot() +
  geom_text(
    data = var.name.dt,
    aes(y = y, label = var.second.name),
    x = 0,
    showSelected = "var.main.name",
    clickSelects = "var.main.name"
  )

text.plot2 <- ggplot() +
  geom_text(
    data = var.name.dt,
    aes(y = y, label = var.second.name),
    x = 0,
    showSelected = "var.main.name",
    clickSelects = "var.second.name"
  )

scatter.plot <- ggplot() +
  geom_point(
    data = combinations.data.table,
    aes(x = var.main.val, y = var.second.val),
    showSelected = c("var.main.name", "var.second.name")
  )

grouped.scatter.plot <- ggplot() +
  geom_point(
    data = combinations.data.table,
    aes(x = var.main.val, y = var.second.val, key = var.main.val),
    clickSelects = "i.group",
    showSelected = c("var.main.name", "var.second.name")
  ) 

scatter.viz <- list()
scatter.viz$one <- text.plot1
scatter.viz$two <- text.plot2
scatter.viz$three <- scatter.plot

grouped.scatter.viz <- list()
grouped.scatter.viz$one <- text.plot1
grouped.scatter.viz$two <- text.plot2
grouped.scatter.viz$three <- grouped.scatter.plot
grouped.scatter.viz$duration <- list("var.main.val" = 1000, "var.second.val" = 1000)

scatter.viz.opt <- scatter.viz
grouped.scatter.viz.opt <- grouped.scatter.viz

animint2dir_opt(scatter.viz.opt, "/Users/faye/Desktop/TestScript/Animate2-ggplot/optimization/animint_out/scatter_test_opt", open.browser = FALSE)


profvis({
  animint2dir_opt(scatter.viz.opt, "/Users/faye/Desktop/TestScript/Animate2-ggplot/optimization/animint_out/scatter_test_opt", open.browser = FALSE)
  animint2dir(scatter.viz, "/Users/faye/Desktop/TestScript/Animate2-ggplot/optimization/animint_out/scatter_test", open.browser = FALSE)
  animint2dir_opt(grouped.scatter.viz.opt, "/Users/faye/Desktop/TestScript/Animate2-ggplot/optimization/animint_out/grouped_scatter_test_opt", open.browser = FALSE)
  animint2dir(grouped.scatter.viz, "/Users/faye/Desktop/TestScript/Animate2-ggplot/optimization/animint_out/grouped_scatter_test", open.browser = FALSE)
})

## Evolution.
data(generation.loci, package = "animint2")
## Example: 2 plots, 2 selectors.
generations <- data.frame(generation=unique(generation.loci$generation))
loci <- data.frame(locus=unique(generation.loci$locus))
first <- subset(generation.loci,generation==1)
ancestral <- do.call(rbind,lapply(split(first,first$locus),with,{
  stopifnot(all(frequency==frequency[1]))
  data.frame(locus=locus[1],ancestral=frequency[1])
}))
gl.list <- split(generation.loci,
                 with(generation.loci,list(generation,locus)))
generation.pop <- do.call(rbind,lapply(gl.list,with,{
  data.frame(generation=generation[1], locus=locus[1],
             estimated=mean(frequency))
}))
generation.pop$ancestral <- ancestral$ancestral[generation.pop$locus]
evolution <- 
  list(ts=ggplot()+
         geom_vline(aes(xintercept=generation),
                    clickSelects="generation",
                    data=generations, alpha=1/2, lwd=4)+
         geom_line(aes(generation, frequency, group=population),
                   showSelected="locus",
                   data=generation.loci),
       predictions=ggplot()+
         geom_point(aes(ancestral, estimated, key=locus),
                    showSelected="generation",
                    clickSelects="locus",               
                    data=generation.pop, size=4, alpha=3/4),
       loci=ggplot()+
         geom_vline(aes(xintercept=locus),
                    data=loci,
                    clickSelects="locus",
                    alpha=1/2, lwd=4)+
         geom_point(aes(locus, frequency, key=locus),
                    showSelected="generation",
                    data=generation.loci),
       duration=list(generation=1000),
       time=list(variable="generation",ms=2000))
animint2dir(evolution,"/Users/faye/Desktop/TestScript/Animate2-ggplot/optimization/animint_out/evolution")
