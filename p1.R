sd = 3.6
n = 100
alfa = .05

me = 17.5

x = round(rnorm(n, mean=me, sd=sd))

hist(x)

z = qnorm(1-alfa/2)
print(z)

xu = me + z * sd/sqrt(n)
print(paste('xu = ', xu))

xl = me - z * sd/sqrt(n)
print(paste('xl = ', xl))

# supongamos que es falsa: me <> 17.5

pot = c()
x = seq(15, 20, .1)
print(x)

for (xi in x) {
	muv = xi
	z1 = (xl - muv) * sqrt(n) / sd 
	z2 = (xu - muv) * sqrt(n) / sd
#print(paste('z1: ', z1))
#print(paste('z2: ', z2))
beta = pnorm(z2) - pnorm(z1)
#print (beta)
#
pot = c(pot, 1-beta)
#print(pot)
}

plot(x, pot, type='l')

