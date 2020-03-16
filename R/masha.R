ent <-function(x) {
  x / sum(x) -> x
  x * log(x) -> y
  return(-sum(y[!is.nan(y)]))
}

seq(0, 10, 0.001) -> t
sin(2*pi*t) + cos(2*pi*4*t) -> x
plot(t, x, type='l')
fft(x) -> fx
abs(fx)[1:(length(fx) / 2)] -> fx2
ent(fx2)
