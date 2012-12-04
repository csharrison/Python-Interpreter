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
print("d should have h")
___assertIn('h', d)

del d['h']
print(d)