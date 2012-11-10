class charlie(object):
	def __init__(self):
		print("hi im in the __init__ constructor")
		print("myself:")
		print(self)

print("HERE")
a = charlie()
print("HEREEE")
print(a.__init__)
a.__init__()