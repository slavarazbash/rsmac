from pyper import R
from pysmac.optimize import fmin
import sys
# import os

# print os.getpid()  # to kill interactive process on Mac
args = sys.argv[1]
r = R()
r('rArgsList <- eval(parse(text="%s"))' % args.replace('[CRLF]', '\n'))
r('eval(rArgsList$init_rcode)')
r('rObjective <- rArgsList$objective')

def pyWrapObjective(x):
  r.x = x
  return r['do.call(rObjective, args=as.list(x))']

pysmacArgs = { 'objective': pyWrapObjective,
  'x0': [], 'xmin': [], 'xmax': [],
  'x0_int': [], 'xmin_int': [], 'xmax_int': [],
  'x_categorical': {} }

grid = r['rArgsList$grid']
gridNames = r['names(rArgsList$grid)']

for gridName in gridNames:
  gridVar = grid[gridName]
  if gridVar['type'] == 'continuous':
    pysmacArgs['x0'].append(float(gridVar['init']))
    pysmacArgs['xmin'].append(float(gridVar['min']))
    pysmacArgs['xmax'].append(float(gridVar['max']))
  elif gridVar['type'] == 'discrete':
    pysmacArgs['x0_int'].append(int(gridVar['init']))
    pysmacArgs['xmin_int'].append(int(gridVar['min']))
    pysmacArgs['xmax_int'].append(int(gridVar['max']))
  elif gridVar['type'] == 'categorical':
    raise Exception('Categorical not implemented yet. Use discrete type')
  else:
    raise Exception('No such type for grid var')
  
r('rArgsList[c("grid", "objective", "init_rcode")] <- NULL')
pysmacArgs.update(r['rArgsList'])  # merge the rest arguments

try:
  xmin, fval = fmin(**pysmacArgs)
  print xmin['x'], fval
except Exception as e:
  print e
