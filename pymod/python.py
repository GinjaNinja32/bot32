
import re
import random
from erlport.erlterms import Atom

def log(s):
	print("PYTHON: " + s)

pairs = {}
replyrate = 0

def init():
	global pairs
	log("init")

	try:
		with open("markovstr.txt", "rb") as inf:
			for line in inf:
				match = re.search(b"([^ ]*) ([^ ]*) ([^ ]+)\\n", line)
				if not match:
					log("failed to parse: " + line.decode("utf-8"))
				else:
					a = rd(match.group(1))
					b = rd(match.group(2))
					pairs[(a,b)] = int(match.group(3))
	except FileNotFoundError:
		log("failed to load file")
		pass

	log("init done with " + str(len(pairs)) + " pairs")
	return Atom(b'ok')

def exit():
	global pairs
	log("exiting with " + str(len(pairs)) + " pairs")

	i = 0
	s = 0
	with open("markovstr.txt", "wb") as ouf:
		for key in pairs.keys():
			i = i + 1
			wk0 = wr(key[0])
			wk1 = wr(key[1])
			if type(wk0) == bytes and type(wk1) == bytes:
				s = s + 1
				ouf.write(wk0 + b" " + wk1 + b" " + bytes(str(pairs[key]), 'utf-8') + b"\n")
			else:
				log("cannot write key " + str(key) + " at index " + str(i))

	log("exit done, wrote " + str(s) + " keys successfully of " + str(i) + " total")
	return Atom(b'ok')

def rd(a):
	if a == b"{}":
		return None
	else:
		return a

def wr(a):
	if a == None:
		return b"{}"
	else:
		return a

def contexts(chan, ping, word):
	global pairs
	word = bytes(filter(word.to_string()), 'utf-8')
	contexts = 0
	n = 0
	x = []
	for (a,b) in pairs.keys():
		if a == word or b == word:
			contexts = contexts + 1
			ab = pairs[(a,b)]
			n = n + ab
			if len(x) < 10:
				if a == None:
					a = b"[start]"
				if b == None:
					b = b"[end]"
				x.append("'" + a.decode("utf-8") + " " + b.decode("utf-8") + "' (" + str(ab) + ")")
	return (Atom(b'irc'), (Atom(b'msg'), (chan, ping.to_string() + "I have " + str(contexts) + " contexts totalling " + str(n) +" instances for '" + word.decode("utf-8") + "': '" + ", ".join(x) + "'.")))

def markov(chan, msg):
	words = msg.to_string().split(" ")
	for i, v in enumerate(words):
		filtered = filter(v)
		bytesed = bytes(filtered, 'utf-8')
		words[i] = bytesed

	if random.randint(0,100) < replyrate:
		return reply(chan, words)
	else:
		add(words)
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
		msg[i] = bytes(filter(v), 'utf-8')
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

	for _ in range(0,20):
		next = get_word(reply[-1], lambda x,a,b: x==a, lambda a,b: b)
		if next == None:
			continue
		else:
			reply.append(next)
			if next[-1] == '.':
				log("next (" + str(next) + ") has . at end, breaking")
				break
	for _ in range(0,20):
		prev = get_word(reply[0], lambda x,a,b: x==b, lambda a,b: a)
		if prev == None:
			continue
		else:
			if prev[-1] == '.':
				log("prev (" + str(prev) + ") has . at end, breaking")
				break
			reply.insert(0, prev)

	reply_str = fix_string(b" ".join(reply))

	log("markov replying with '" + str(reply_str) + "'")

	return (Atom(b'irc'), (Atom(b'msg'), (chan, reply_str)))

def fix_string(string):
	for pattern, replacement in [
				(b"(i)(m)",        b"\\1'\\2"),
				(b"([dw]on|(?:c|w|sh)ouldn|can|shan)(t)", b"\\1'\\2"),
				(b"(there)(s)",    b"\\1'\\2"),
				(b"(they|we)(re)", b"\\1'\\2"),
				(b"(s?he)(s)",     b"\\1'\\2")
			]:
		string = re.sub(b"\\b" + pattern + b"\\b", replacement, string, flags = re.IGNORECASE)
	return string
#	return re.sub(b"\\b([iI])([mM])\\b", b"\\1'\\2", string)

def get_word(word, match = lambda x, a, b: x==a, select = lambda a, b: b):
	global pairs
	weight = {}
	for (a,b) in pairs.keys():
		if match(word, a, b):
			weight[(a,b)] = pairs[(a,b)]
	tup = pick_weight(weight)
	if tup == None:
		return None
	else:
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
