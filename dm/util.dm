
#define ceil(x) (-round(-(x)))
#define floor(x) round(x)
#define clamp(x, low, high) max((low),min((high),(x)))

#define BENCH(NAME, ITERS, CODE) \
	do{ \
		var/s = world.timeofday ;\
		for(var/i = 1 to (ITERS)) {\
			CODE ;\
		} ;\
		var/e = world.timeofday ;\
		world.log << "[NAME]: [e-s] ds" ;\
	} while(0)
#define BENCHK(NAME, ITERS, CODE) \
	do{ \
		var/s = world.timeofday ;\
		for(var/j = 1 to 1000) {\
		for(var/i = 1 to (ITERS)) {\
			CODE ;\
		} ;\
		} ;\
		var/e = world.timeofday ;\
		world.log << "[NAME]: [e-s] ds" ;\
	} while(0)
#define BENCHM(NAME, ITERS, CODE) \
	do{ \
		var/s = world.timeofday ;\
		for(var/j = 1 to 1000000) {\
		for(var/i = 1 to (ITERS)) {\
			CODE ;\
		} ;\
		} ;\
		var/e = world.timeofday ;\
		world.log << "[NAME]: [e-s] ds" ;\
	} while(0)

/proc/fold() return foldl(arglist(args))
/proc/foldl(proc, list/L, initial=L)
	var/x = initial
	var/start = 1
	if(x == L)
		x = L[1]
		start = 2

	for(var/i = start to L.len)
		x = call(proc)(x, L[i])

	return x

/proc/foldr(proc, list/L, initial=L)
	var/x = initial
	var/start = L.len
	if(x == L)
		x = L[L.len]
		start = L.len - 1

	for(var/i = start to 1 step -1)
		x = call(proc)(x, L[i])

	return x

/proc/map(proc, list/L)
	for(var/x = 1 to L.len)
		L[x] = call(proc)(L[x])
	return L

/proc/filter(proc, list/L)
	var/x = 1
	while(x <= L.len)
		if(call(proc)(L[x]))
			x++
		else
			L.Cut(x, x+1)
	return L
