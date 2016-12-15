# takes a name and a tuple defining one parameter, registers that with the parser
# and returns the corresponding string for the SMAC pcs file and the type of the
# variable for later casting
def process_single_parameter_definition(name, specification):
  """
A helper function to process a single parameter definition for further communication with SMAC.
"""

data_type_mapping = {'integer': int, 'real': float}

assert isinstance(specification, tuple), "The specification \"{}\" for {} is not valid".format(specification,name)
assert len(specification)>1, "The specification \"{}\" for {} is too short".format(specification,name)

if specification[0] not in {'real', 'integer', 'ordinal', 'categorical'}:
  raise ValueError("Type {} for {} not understood".format(specification[0], name))

string = '{} {}'.format(name, specification[0])

# numerical values
if specification[0] in {'real', 'integer'}:
  dtype = data_type_mapping[specification[0]]
if len(specification[1])!= 2:
  raise ValueError("Range {} for {} not valid for numerical parameter".format(specification[1], name))
if specification[1][0] >= specification[1][1]:
  raise ValueError("Interval {} not not understood.".format(specification[1]))
if not (specification[1][0] <= specification[2] and specification[2] <= specification[1][1]):
  raise ValueError("Default value for {} has to be in the specified range".format(name))

if specification[0] == 'integer':
  if (type(specification[1][0]) != int) or (type(specification[1][1]) != int) or (type(specification[2]) != int):
  raise ValueError("Bounds and default value of integer parameter {} have to be integer types!".format(name))

string += " [{0[0]}, {0[1]}] [{1}]".format(specification[1], specification[2])

if ((len(specification) == 4) and specification[3] == 'log'):
  if specification[1][0] <= 0:
  raise ValueError("Range for {} cannot contain non-positive numbers.".format(name))
string += " log"

# ordinal and categorical types
if (specification[0] in {'ordinal', 'categorical'}):
  
  if specification[2] not in specification[1]:
  raise ValueError("Default value {} for {} is not valid.".format(specification[2], name))

# make sure all elements are of the same type
if (len(set(map(type, specification[1]))) > 1):
  raise ValueError("Not all values of {} are of the same type!".format(name))

dtype = type(specification[1][0])
string += " {"+",".join(map(str, specification[1])) + '}' + ('[{}]'.format(specification[2]))

return string, dtype


def process_parameter_definitions(parameter_dict):
  """
A helper function to process all parameter definitions conviniently with just one call.
This function takes the parametr definitions from the user, converts
them into lines for SMAC's PCS format, and also creates a dictionary
later used in the comunication with the SMAC process.
:param paramer_dict: The user defined parameter configuration space
"""
pcs_strings = []
parser_dict={}

for k,v in list(parameter_dict.items()):
  line, dtype = process_single_parameter_definition(k,v)
parser_dict[k] = dtype
pcs_strings.append(line)

return (pcs_strings, parser_dict)