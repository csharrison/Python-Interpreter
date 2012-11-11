class A():
	def __call__(self,x):
		if x <= 0:
			return 1
		return x*self.__call__(x-1)

a = A()
print(a(5))