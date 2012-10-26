from os import popen2

if __name__ == "__main__":
	print "Welcome to Python! type 'exit()' to exit"
	while True:
		inp = raw_input(">>> ")
		if inp == "exit()" :
			print "Thanks for using Python!"
			break

		command =  "racket python-main.rkt --interp"
		stdin, stdout = popen2(command)
		stdin.write('%s' %inp)
		stdin.close() # CTRL-D
		print stdout.read()
		stdout.close()
