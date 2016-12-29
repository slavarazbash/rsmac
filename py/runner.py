from pyper import R
from pysmac.optimize import fmin
import sys

args = sys.argv[1]
r = R()
r('rParamsList <- eval(parse(text="%s"))' % args)
r('rObj <- rParamsList$objective')
# for parName in []:
  
# x = r['x']

def pyWrapObj(x):
  # x1 = x[0]
  # x2 = x[1]
  # ret  = a*(x2-b*x1**2+c*x1-r)**2+s*(1-t)*np.cos(x1)+s
  return r['x'] * 2  # works!!!!!!!!!!!!!!


xmin, fval = fmin(pyWrapObj, x0=(0,0), xmin=(-5, 0), xmax=(10, 15), max_evaluations=100)
# print xmin, fval


