
import re
import random
from erlport.erlterms import Atom
from erlport.erlang import call as ecall

def atom(a):
	return Atom(bytes(a, "utf-8"))

def log(s):
	print("PYTHON: %s" % (s))

pairs = {}
replyrate = 0

# Erlang-called functions

def initialise():
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

	ecall(atom('pymod'), atom('register_command'), ["markov", atom('markov'), atom('markov_cmd'), atom('user')])
	ecall(atom('pymod'), atom('register_command'), ["contexts", atom('markov'), atom('contexts_cmd'), atom('user')])

	log("init done with %s pairs" % (len(pairs)))
	return Atom(b'ok')

def deinitialise():
	global pairs
	log("exiting with %s pairs" % (len(pairs)))

	i = 0
	s = 0
	with open("markovstr.txt", "wb") as ouf:
		for key in pairs.keys():
			i += 1
			wk0 = wr(key[0])
			wk1 = wr(key[1])
			if type(wk0) == bytes and type(wk1) == bytes:
				s += 1
				ouf.write(wk0 + b" " + wk1 + b" " + bytes(str(pairs[key]), 'utf-8') + b"\n")
			else:
				log("cannot write key %s at index %s" % (key, i))

	ecall(atom('pymod'), atom('unregister_command'), ["markov", atom('user')])
	ecall(atom('pymod'), atom('unregister_command'), ["contexts", atom('user')])

	log("exit done, wrote %s keys successfully of %s total" % (s, i))
	return Atom(b'ok')

def markov_cmd(reply, ping, params):
	return markovreply(reply.to_string(), " ".join([x.to_string() for x in params]))

def contexts_cmd(reply, ping, params):
	return contexts(reply.to_string(), ping.to_string(), params[0].to_string())

def handle_event(type, params):
	if type == atom('msg_nocommand'):
		channel = params[1].to_string()
		msg = " ".join([x.to_string() for x in params[2]])
		if channel != ecall(atom('config'), atom('get_value'), [atom('config'), [atom('bot'), atom('nick')]]):
			if re.match("(?i)(^|[^a-z0-9])nti([^a-z0-9]|$)", msg):
				reply = markovreply(channel, msg)
				ecall(atom('erlang'), atom('!'), [atom('core'), reply])
			else:
				markov(channel, msg)
	return atom('ok')

# Other functions

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
	word = bytes(filter(word), 'utf-8')
	contexts = 0
	n = 0
	x = []
	for (a,b) in pairs.keys():
		if a == word or b == word:
			contexts += 1
			ab = pairs[(a,b)]
			n += ab
			if len(x) < 10:
				if a == None:
					a = b"[start]"
				if b == None:
					b = b"[end]"
				x.append("'%s %s' (%s)" % (a.decode("utf-8"), b.decode("utf-8"), ab))
	return (Atom(b'irc'), (Atom(b'msg'), (chan, "%sI have %s contexts totalling %s instances for '%s': '%s'." % (ping, contexts, n, word.decode("utf-8"), ", ".join(x)))))

def markov(chan, msg):
	words = msg.split(" ")
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
	msg = msg.split(" ")
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
				log("next (%s) has . at end, breaking" % (next))
				break
	for _ in range(0,20):
		prev = get_word(reply[0], lambda x,a,b: x==b, lambda a,b: a)
		if prev == None:
			continue
		else:
			if prev[-1] == '.':
				log("prev (%s) has . at end, breaking" % (prev))
				break
			reply.insert(0, prev)

	reply_str = fix_string(b" ".join(reply))

	log("markov replying with '%s'" % (reply_str))

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
		pairs[pair] += 1
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
