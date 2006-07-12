.HLL "Python", "python_group"

.sub main :main
	new $P0, .PyInt
	new $P1, .PyBoolean
	new $P2, .PyString
	new $P3, .PyNone
#	new $P4, .PyLong
	new $P5, .PyFloat
	new $P6, .PyComplex
	new $P7, .PyTuple
	new $P8, .PyList
	new $P9, .PyDict
	set $P0, 42
	set $P1, 1
	set $P2, "Hello PyString!"
	# $P3 is none
#	set $P4, 489473 # longs working right?
	set $P5, 2.2
	# set $P6
	# set $P7
	# set $P8
	# set $P9
	
    print $P0
	print "\n"
	print $P1
	print "\n"
	print $P2
	print "\n"
	print $P3
	print "\n"
#	print $P4
#	print "\n"
	print $P5
	print "\n"
	print $P6
	print "\n"
	print $P7
	print "\n"
	print $P8
	print "\n"
	print $P9
	print "\n"
.end

