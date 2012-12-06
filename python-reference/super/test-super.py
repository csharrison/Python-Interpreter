class A:
    def f(self):
        return 'A'
    @classmethod
    def cm(cls):
        return (cls, 'A')

class G(A):
    pass

___assertEqual(A.cm(), (A, 'A'))
___assertEqual(A().cm(), (A, 'A'))
___assertEqual(G.cm(), (G, 'A'))
___assertEqual(G().cm(), (G, 'A'))

# Issue4360: super() did not work in a function that
# contains a closure
class EE(A):
    def f(self):
        def nested():
            self
        return super().f() + 'E'

___assertEqual(EE().f(), 'AE')
