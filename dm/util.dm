#define NUM_E 2.71828183
#define NUM_SQRT2 1.41421356

#define PI						3.1415
#define SPEED_OF_LIGHT			3e8		//not exact but hey!
#define SPEED_OF_LIGHT_SQ		9e+16
#define INFINITY				1e31	//closer then enough

//atmos
#define R_IDEAL_GAS_EQUATION	8.31	//kPa*L/(K*mol)
#define ONE_ATMOSPHERE			101.325	//kPa
#define T0C						273.15	// 0degC
#define T20C					293.15	// 20degC
#define TCMB					2.7		// -270.3degC

#define SHORT_REAL_LIMIT 16777216

//"fancy" math for calculating time in ms from tick_usage percentage and the length of ticks
//percent_of_tick_used * (ticklag * 100(to convert to ms)) / 100(percent ratio)
//collapsed to percent_of_tick_used * tick_lag
#define TICK_DELTA_TO_MS(percent_of_tick_used) ((percent_of_tick_used) * world.tick_lag)
#define TICK_USAGE_TO_MS(starting_tickusage) (TICK_DELTA_TO_MS(TICK_USAGE_REAL - starting_tickusage))

#define PERCENT(val) (round(val*100, 0.1))
#define CLAMP01(x) (CLAMP(x, 0, 1))

//time of day but automatically adjusts to the server going into the next day within the same round.
//for when you need a reliable time number that doesn't depend on byond time.
#define REALTIMEOFDAY (world.timeofday + (MIDNIGHT_ROLLOVER * MIDNIGHT_ROLLOVER_CHECK))
#define MIDNIGHT_ROLLOVER_CHECK ( GLOB.rollovercheck_last_timeofday != world.timeofday ? update_midnight_rollover() : GLOB.midnight_rollovers )

#define SIGN(x) (x!=0 ? x / abs(x) : 0)

#define ATAN2(x, y) ( !x && !y ? 0 : (y >= 0 ? arccos(x / sqrt(x*x + y*y)) : -arccos(x / sqrt(x*x + y*y)) ) )

#define CEILING(x, y) (-round(-x / y) * y)

#define FLOOR(x, y) (round(x / y) * y)

#define CLAMP(CLVALUE,CLMIN,CLMAX) ( max( (CLMIN), min((CLVALUE), (CLMAX)) ) )

#define MODULUS(x, y) (x - y * round(x / y))

// Tangent
#define TAN(x) (sin(x) / cos(x))

// Cotangent
#define COT(x) (1 / TAN(x))

#define CSC(x) (1 / sin(x))

#define DEFAULT(a, b) (a ? a : b)

// Greatest Common Divisor - Euclid's algorithm
/proc/Gcd(a, b)
	return b ? Gcd(b, a % b) : a

// Least Common Multiple
#define Lcm(a, b) (abs(a) / Gcd(a, b) * abs(b))

#define INVERSE(x) (1/x)

#define INVERSE_SQUARE(initial_strength,cur_distance,initial_distance) (initial_strength*(initial_distance**2/cur_distance**2))

#define ISABOUTEQUAL(a, b, deviation) (deviation ? abs(a - b) <= deviation : abs(a - b) <= 0.1)

#define ISEVEN(x) (x % 2 == 0)

#define ISODD(x) (!ISEVEN(x))

// Returns true if val is from min to max, inclusive.
#define ISINRANGE(val, min, max) (min <= val && val <= max)

#define ISINTEGER(x) (round(x) == x)

#define ISMULTIPLE(x, y) (x % y == 0)

// Performs a linear interpolation between a and b.
// Note that amount=0 returns a, amount=1 returns b, and
// amount=0.5 returns the mean of a and b.
#define LERP(a, b, amount) (amount ? (a + (b - a) * amount) : (a + (b - a) * 0.5)

// Returns the nth root of x.
#define ROOT(n, x) (x ** (1 / n))

// secant
#define SEC(x) (1 / cos(x))

// The quadratic formula. Returns a list with the solutions, or an empty list
// if they are imaginary.
/proc/SolveQuadratic(a, b, c)
	ASSERT(a)
	. = list()
	var/d		= b*b - 4 * a * c
	var/bottom  = 2 * a
	if(d < 0)
		return
	var/root = sqrt(d)
	. += (-b + root) / bottom
	if(!d)
		return
	. += (-b - root) / bottom

#define TODEGREES(radians) (radians * 57.2957795)

#define TORADIANS(degrees) (degrees * 0.0174532925)

// Will filter out extra rotations and negative rotations
// E.g: 540 becomes 180. -180 becomes 180.
#define SIMPLIFYDEGREES(degrees) ((degrees*SIGN(DEGREES))%360)

// min is inclusive, max is exclusive
#define WRAP(val, min, max) (val - ((round(val - min) / (max - min)) * (max - min)))

#define NORM_ROT(rot) ((((rot % 360) + (rot - round(rot, 1))) >= 0) ? ((rot % 360) + (rot - round(rot, 1))) : (((rot % 360) + (rot - round(rot, 1))) + 360))

/proc/get_angle_of_incidence(face_angle, angle_in, auto_normalize = TRUE)
	var/angle_in_s = NORM_ROT(angle_in)
	var/face_angle_s = NORM_ROT(face_angle)
	var/incidence = face_angle_s - angle_in_s
	var/incidence_s = incidence
	while(incidence_s < -90)
		incidence_s += 180
	while(incidence_s > 90)
		incidence_s -= 180
	if(auto_normalize)
		return incidence_s
	else
		return incidence

//A logarithm that converts an integer to a number scaled between 0 and 1 (can be tweaked to be higher).
//Currently, this is used for hydroponics-produce sprite transforming, but could be useful for other transform functions.
//Kept for documentation reasons
///proc/TRANSFORMUSINGVARIABLE(input, inputmaximum, scaling_modifier = 0)
//
//		var/inputToDegrees = (input/inputmaximum)*180 //Converting from a 0 -> 100 scale to a 0 -> 180 scale. The 0 -> 180 scale corresponds to degrees
//		var/size_factor = ((-cos(inputToDegrees) +1) /2) //returns a value from 0 to 1
//
//		return size_factor + scaling_modifier //scale mod of 0 results in a number from 0 to 1. A scale modifier of +0.5 returns 0.5 to 1.5
#define TRANSFORM_USING_VARIABLE(input, max, scale) ( ((input/max)*180) + (-cos((input/max)*180)) )

//converts a uniform distributed random number into a normal distributed one
//since this method produces two random numbers, one is saved for subsequent calls
//(making the cost negligble for every second call)
//This will return +/- decimals, situated about mean with standard deviation stddev
//68% chance that the number is within 1stddev
//95% chance that the number is within 2stddev
//98% chance that the number is within 3stddev...etc
#define ACCURACY 10000
/proc/gaussian(mean, stddev)
	var/static/gaussian_next
	var/R1;var/R2;var/working
	if(gaussian_next != null)
		R1 = gaussian_next
		gaussian_next = null
	else
		do
			R1 = rand(-ACCURACY,ACCURACY)/ACCURACY
			R2 = rand(-ACCURACY,ACCURACY)/ACCURACY
			working = R1*R1 + R2*R2
		while(working >= 1 || working==0)
		working = sqrt(-2 * log(working) / working)
		R1 *= working
		gaussian_next = R2 * working
	return (mean + stddev * R1)
#undef ACCURACY

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
