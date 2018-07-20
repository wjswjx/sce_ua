# NOT RUN {
#set.seed(1)
# generate example data from a function with three parameters
# with some random noise
library(rtop)
fun = function(x, pars) pars[1]*x+pars[2]

x = c(0.3384, 0.359, 0.3803, 0.39, 0.3998, 0.4099, 0.4202, 0.4307, 0.4415, 0.4525, 0.4637, 0.5329, 0.5543, 0.5766, 0.5816, 0.5861, 0.5902, 0.6028, 0.6157, 0.6289, 0.6424)

y = c(0.883205153, 0.909040249, 0.866611888, 0.866360841, 0.887193317, 0.846502052, 0.780187771, 0.860830119, 0.851054422, 0.969985745, 0.926635661, 0.906489757, 0.901945388, 0.895166271, 0.863414151, 0.885055069, 1.166311028, 1.093602719, 1.070705475, 1.267809705, 1.277302561)

plot(x,y)

# Objective function, summing up squared differences
OFUN = function(pars, x, yobs) {
  yvals = fun(x, pars)  
  sum((yvals-yobs)^2)
}

sceuares = sceua(OFUN, pars = c(0.1,0.1), lower = c(-10,-10), 
                 upper = c(10,10), maxn = 10000, pcento = 0.01,x = x, yobs = y)
sceuares
xx = seq(min(x), max(x), 0.1)
lines(xx, fun(xx, pars = sceuares$par))

# }
