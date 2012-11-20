The final project for CS173 - An interpreter for Python


design considerations:
we use a hoisting strategy for scoping, with js style rules. Our desugarer keeps track of whether identifiers are local or nonlocal.

we keep global stuff in a separate environment, but local and nonlocal ids are bound in a two part store (local/nonlocal) that changes at function application


Tests we pass:
	most of scope
	all of lists
	all the comparator stuff
	some of tuple (we're getting there)


note: we recently made a few sweeping changes, and soon we'll be able to pass a lot more tests.
we added *arg **kwarg functionality, which should make us able to extend our core language quite a bit.

we are also in the middle of our exceptions branch, and should be done with it soon. Unfortunately it isn't even ready to be compiled, so that's too bad

anyways enjoy our stuff

-charlie (and scott)