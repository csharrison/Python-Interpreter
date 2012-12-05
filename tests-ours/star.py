def a(*args):
	print(args)

a(1,2,3)
a("charlie","fucker")
a()

def b(x,*args):
	print(x)
	print(args)

b(1,2,3,4,5)

def test(x,y,z):
	print(x)
	print(y)
	print(z)

test(1,2,3)
test(1,*(2,3))
test(*(1,2,3))
test(1,2,*(3,))

test(1,2,3,*())