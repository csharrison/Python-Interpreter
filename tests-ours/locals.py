def a(x):
	y = 19
	z = locals();
	q = 100
	print(z)

a(10)


def f(x):
    def g(y):
        def h(z):
            return y + z
        w = x + y
        y += 3
        return locals()
    return g

d = f(2)(4)
print(d)



def f(x):
    def g(y):
        def h(z):
            x + y + z
            return locals()
        return h
    return g

print(f(1)(2)(3))