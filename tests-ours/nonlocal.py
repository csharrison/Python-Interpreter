x = 10
def a():
	global x
	x = x + 20
	return x
a()
___assertEqual(x,30)


y = 1000
def b():
	global a
	___assertEqual(a(), 50)