
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
