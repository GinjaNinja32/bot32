#! /usr/bin/env python3

import sys
import sympy
import re
import traceback
from sympy.parsing.sympy_parser import *

global_dict={}
exec("from sympy import *", global_dict)
global_dict['__builtins__'] = {}
if(re.search("__", sys.argv[1]) == None):
	try:
		print(parse_expr(sys.argv[1], global_dict=global_dict))
	except Exception:
		a, b, tb = sys.exc_info()
		traceback.print_exception(a, b, None, None, sys.stdout)
		traceback.print_tb(tb, 0, sys.stdout)
else:
	print("oi")
