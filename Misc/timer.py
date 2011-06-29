def timer(reps, func, *args):
	import time
	start = time.clock()
	for i in xrange(reps):
		func(*args)
	return time.clock() - start
