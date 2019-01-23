
#define enc json_encode
#define dec json_decode

#define ceil(x) (-round(-(x)))
#define floor(x) round(x)
#define clamp(x, low, high) max((low),min((high),(x)))

#define BENCHT(NAME, ITERS, CODE) \
	do{ \
		var/s = world.tick_usage ;\
		for(var/i = 1 to (ITERS)) {\
			CODE ;\
		} ;\
		var/e = world.tick_usage ;\
		world.log << "[NAME]: [(e-s) * world.tick_lag] ms" ;\
	} while(0)
#define BENCHTK(NAME, ITERS, CODE) \
	do{ \
		var/s = world.tick_usage ;\
		for(var/j = 1 to 1000) {\
		for(var/i = 1 to (ITERS)) {\
			CODE ;\
		} ;\
		} ;\
		var/e = world.tick_usage ;\
		world.log << "[NAME]: [(e-s) * world.tick_lag] ms" ;\
	} while(0)
#define BENCHTM(NAME, ITERS, CODE) \
	do{ \
		var/s = world.tick_usage ;\
		for(var/j = 1 to 1000000) {\
		for(var/i = 1 to (ITERS)) {\
			CODE ;\
		} ;\
		} ;\
		var/e = world.tick_usage ;\
		world.log << "[NAME]: [(e-s) * world.tick_lag] ms" ;\
	} while(0)
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

/proc/ffold() return ffoldl(arglist(args))
/proc/ffoldl(proc, list/L, initial=L)
	var/x = initial
	var/start = 1
	if(x == L)
		x = L[1]
		start = 2

	for(var/i = start to L.len)
		x = call(proc)(x, L[i])

	return x

/proc/ffoldr(proc, list/L, initial=L)
	var/x = initial
	var/start = L.len
	if(x == L)
		x = L[L.len]
		start = L.len - 1

	for(var/i = start to 1 step -1)
		x = call(proc)(x, L[i])

	return x

/proc/fmap(proc, list/L)
	for(var/x = 1 to L.len)
		L[x] = call(proc)(L[x])
	return L

/proc/ffilter(proc, list/L)
	var/x = 1
	while(x <= L.len)
		if(call(proc)(L[x]))
			x++
		else
			L.Cut(x, x+1)
	return L

/proc/stars(n, pr)
	if (pr == null)
		pr = 25
	if (pr < 0)
		return null
	else
		if (pr >= 100)
			return n
	var/te = n
	var/t = ""
	n = length(n)
	var/p = null
	p = 1
	var/intag = 0
	while(p <= n)
		var/char = copytext(te, p, p + 1)
		if (char == "<") //let's try to not break tags
			intag = !intag
		if (intag || char == " " || prob(pr))
			t = text("[][]", t, char)
		else
			t = text("[]*", t)
		if (char == ">")
			intag = !intag
		p++
	return t

/proc/seq(lo, hi, st=1)
	if(isnull(hi))
		hi = lo
		lo = 1

	. = list()
	for(var/x in lo to hi step st)
		. += x

/proc/num2hex(num, padlength)
	var/global/list/hexdigits = list("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")

	. = ""
	while(num > 0)
		var/hexdigit = hexdigits[(num & 0xF) + 1]
		. = "[hexdigit][.]"
		num >>= 4 //go to the next half-byte

	//pad with zeroes
	var/left = padlength - length(.)
	while (left-- > 0)
		. = "0[.]"

//Randomize: Return the list in a random order
/proc/shuffle(var/list/L)
	if(!L)
		return

	L = L.Copy()

	for(var/i=1; i<L.len; i++)
		L.Swap(i, rand(i,L.len))
	return L

#define typeid(x) (length("\ref[x]") == 12 ? copytext("\ref[x]", 4, 6) : "0[copytext("\ref[x]", 4, 5)]")
