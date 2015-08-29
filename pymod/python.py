
import re
import random
from erlport.erlterms import Atom

pairs = {}
replyrate = 0

def init():
	global pairs
	print("init")

	try:
		with open("markovstr.txt", "rb") as inf:
			for line in inf:
				match = re.search("([^ ]*) ([^ ]*) ([^ ]+)\\n", line.decode("utf-8"))
				if not match:
					print("failed to parse: " + line.decode("utf-8"))
				else:
					a = rd(match.group(1))
					b = rd(match.group(2))
					pairs[(a,b)] = int(match.group(3))
	except FileNotFoundError:
		pass

	print("init done")
	return Atom(b'ok')

def exit():
	global pairs
	print("exit")

	with open("markovstr.txt", "w") as ouf:
		for (a,b) in pairs.keys():
			ouf.write(wr(a) + " " + wr(b) + " " + str(pairs[(a,b)]) + "\n")

	return Atom(b'ok')

def rd(a):
	if a == "{}":
		return None
	else:
		return a

def wr(a):
	if a == None:
		return "{}"
	else:
		return a

def main(a, b, c, d):
	o = a.to_string()
	rt = b.to_string()
	p = c.to_string()
	params = d
	for i, v in enumerate(params):
		params[i] = v.to_string()

	rep = reply(rt, " ".join(params))
	print(" ".join(params) + " -> " + str(rep))
	return rep
#	return (Atom(b'irc'), (Atom(b'msg'), (rt, p + 'hello world')))

def markov(chan, msg):
	msg = msg.to_string().split(" ")
	for i, v in enumerate(msg):
		msg[i] = filter(v)

	if random.randint(0,100) < replyrate:
		return reply(chan, msg)
	else:
		add(msg)
		return Atom(b'ok')

def add(msg):
	global pairs

	incpair((None,msg[0]))
	for i in range(0, len(msg)-1):
		incpair((msg[i], msg[i+1]))
	incpair((msg[-1],None))

def markovreply(chan, msg):
	msg = msg.to_string().split(" ")
	for i, v in enumerate(msg):
		msg[i] = filter(v)
	return reply(chan, msg)

def reply(chan, msg):
	global pairs
	freq = {}
	for word in msg:
		if word in freq:
			freq[word] += calc_freq(word)
		else:
			n = calc_freq(word)
			if n != 0:
				freq[word] = n

	word = pick_inv_weight(freq)
	if word == None:
		return Atom(b'ok')

	reply = [word]

	for _ in range(0,10):
		next = get_word(reply[-1], lambda x,a,b: x==a, lambda a,b: b)
		if next == None:
			break
		else:
			reply.append(next)
			if next[-1] == '.':
				break
	for _ in range(0,10):
		prev = get_word(reply[0], lambda x,a,b: x==b, lambda a,b: a)
		if prev == None:
			break
		else:
			if prev[-1] == '.':
				break
			reply.insert(0, prev)

	print("markov replying with '" + str(reply) + "'")

	return (Atom(b'irc'), (Atom(b'msg'), (chan, " ".join(reply))))

def get_word(word, match = lambda x, a, b: x==a, select = lambda a, b: b):
	global pairs
	weight = {}
	for (a,b) in pairs.keys():
		if match(word, a, b):
			weight[(a,b)] = pairs[(a,b)]
	tup = pick_weight(weight)
	return select(tup[0], tup[1])

def calc_freq(word):
	global pairs
	x = 0
	for (a,b) in pairs.keys():
		if a == word or b == word:
			x += pairs[(a,b)]
	return x

def filter(word):
	return re.sub("[\"'\(\)\[\]\{\}]+", "", word)

def incpair(pair):
	global pairs
	if pair in pairs:
		pairs[pair] = pairs[pair] + 1
	else:
		pairs[pair] = 1

def pick_inv_weight(dict):
	return pick_weight(dict, lambda x: 1/x)

def pick_weight(dict, weighting = lambda x: x):
	total = 0
	for val in dict.values():
		total += weighting(val)
	picked = random.random() * total
	lastk = None
	for (k,val) in dict.items():
		picked -= weighting(val)
		if picked <= 0:
			return k
		lastk = k
	return lastk
