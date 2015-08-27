#! /usr/bin/env python

import sys
import sympy
import re
from sympy.parsing.sympy_parser import *

global_dict={}
exec("from sympy import *", global_dict)
global_dict['__builtins__'] = {}
if(re.search("__", sys.argv[1]) == None):
	print(parse_expr(sys.argv[1], global_dict=global_dict))
else:
	print("oi")
