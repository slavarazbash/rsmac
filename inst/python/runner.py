from pyper import R
from pysmac.optimize import fmin
import sys


args = sys.argv[1]
r = R()
r('r_args_list <- eval(parse(text="%s"))' % args.replace('[CRLF]', '\n'))
r('eval(r_args_list$init_rcode)')
r('r_objective <- r_args_list$objective')
r('grid <- r_args_list$grid')

# kwargs examples
# {'x': array([ 5.22359753,  0.526101  ])}
# {'x': array([ 0.5]), 'x_int': array([4])}
def fill_objective_args(args_map):
  r('objective_args <- list()')
  pyr_type_map = {'x': 'continuous', 'x_int': 'discrete'}
  for key in args_map:
    assert key in pyr_type_map.keys()
    r.cur_r_type = pyr_type_map[key]
    r('cur_names <- names(Filter(function(x) x$type == cur_r_type, grid))')
    r.cur_values = args_map[key]
    r('cur_values <- as.list(cur_values)')
    r('names(cur_values) <- cur_names')
    r('objective_args <- append(objective_args, cur_values)')

def py_wrap_objective(**kwargs):
  fill_objective_args(kwargs)
  return r['do.call(r_objective, args=objective_args)']

pysmac_args = { 'objective': py_wrap_objective,
  'x0': [], 'xmin': [], 'xmax': [],
  'x0_int': [], 'xmin_int': [], 'xmax_int': [],
  'x_categorical': {} }

grid = r['r_args_list$grid']
grid_names = r['names(r_args_list$grid)']


for grid_name in grid_names:
  grid_var = grid[grid_name]
  
  if grid_var['type'] == 'continuous':
    pysmac_args['x0'].append(float(grid_var['init']))
    pysmac_args['xmin'].append(float(grid_var['min']))
    pysmac_args['xmax'].append(float(grid_var['max']))
  elif grid_var['type'] == 'discrete':
    pysmac_args['x0_int'].append(int(grid_var['init']))
    pysmac_args['xmin_int'].append(int(grid_var['min']))
    pysmac_args['xmax_int'].append(int(grid_var['max']))
  elif grid_var['type'] == 'categorical':
    raise Exception('Categorical not implemented yet. Use discrete type')
  else:
    raise Exception('No such type for grid var')
    
r('r_args_list[c("grid", "objective", "init_rcode")] <- NULL')
pysmac_args.update(r['r_args_list'])  # merge the rest arguments

try:
  best_pars, objective_val = fmin(**pysmac_args)
  fill_objective_args(best_pars)
  print '%+%'.join(["pysmac>>", str(objective_val), 
                    r["paste(deparse(objective_args), collapse='')"]])
except Exception as e:
  print e
