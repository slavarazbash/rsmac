from pyper import R
from pysmac.optimize import fmin
import sys


args = sys.argv[1]
r = R()
r('r_args_list <- eval(parse(text="%s"))' % args.replace('[CRLF]', '\n'))
r('eval(r_args_list$init_rcode)')
r('r_objective <- r_args_list$objective')

def py_wrap_objective(**kwargs):
  r('objective_ars <- list()')
  pyr_type_map = {'x': 'continuous', 'x_int': 'discrete'}
  for key in kwargs:
    assert key in pyr_type_map.keys()
    r.cur_r_type = pyr_type_map[key]
    r('cur_names <- names(Filter(function(x) x$type == cur_r_type, r_args_list$grid))')
    r.cur_values = kwargs[key]
    r('cur_values <- as.list(cur_values)')
    r('names(cur_values) <- cur_names')
    r('objective_ars <- append(objective_ars, cur_values)')
    
    #   grid <- list(
    #             max.depth = list(type='discrete', init=4, min=3, max=10),
    #             eta       = list(type='continuous', init=0.5, min=0.01, max=0.99))
    # {'x': array([ 5.22359753,  0.526101  ])}
    # {'x': array([ 0.5]), 'x_int': array([4])}
    
  print 222
  print r['str(objective_ars)']
  return r['do.call(r_objective, args=objective_ars)']

pysmac_args = { 'objective': py_wrap_objective,
  'x0': [], 'xmin': [], 'xmax': [],
  'x0_int': [], 'xmin_int': [], 'xmax_int': [],
  'x_categorical': {} }

# r('\\n'.join([
#   'obj_formals <- formals(r_objective)',
#   'formals(r_objective) <- NULL'])
grid = r['r_args_list$grid']
grid_names = r['names(r_args_list$grid)']

# ready_types = ['continuous', 'discrete']
# for grid_type in ready_types:
for grid_name in grid_names:
  grid_var = grid[grid_name]
  # if grid_var['type'] != grid_type and grid_var['type'] in ready_types:
    # continue
  
  # reorder objective arguments in signature
  # r(''.join(['formals(r_objective) <- ',
             # 'append(formals(r_objective), obj_formals["%s"])' % grid_name]))
  
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
  print best_pars['x'], objective_val
except Exception as e:
  print e
