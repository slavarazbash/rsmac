from pyper import R
from pysmac.optimize import fmin
import sys

args = sys.argv[1]
r = R()
r('rParamsList <- eval(parse(text="%s"))' % args.replace('[CRLF]', '\n'))
r('rObjective <- rParamsList$objective')

def pyWrapObjective(x):
  r.x = x
  return r['do.call(rObjective, args=as.list(x))']

pysmacArgs = { 'objective': pyWrapObjective,
  'x0': [], 'xmin': [], 'xmax': [],
  'x0_int': [], 'xmin_int': [], 'xmax_int': [],
  'x_categorical': {} }

grid = r['rParamsList$grid']
gridNames = r['names(rParamsList$grid)']

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
  
r('rParamsList[c("grid", "objective")] <- NULL')
pysmacArgs.update(r['rParamsList'])  # merge the rest arguments

try:
  xmin, fval = fmin(**pysmacArgs)
  print xmin['x'], fval
except Exception as e:
  print e
