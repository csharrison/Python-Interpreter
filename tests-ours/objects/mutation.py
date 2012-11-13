class scott:
	x = 5


print("before constructor")
a = scott()
print("before assignment")
a.x = 100
print(a.x)

b = scott()
___assertEqual(a.x, b.x+95)
___assertEqual(b.x,scott.x)