#!/usr/bin/env python
#
# Script to generate basic documentation for CCPP metadata
#

import logging

from common import decode_container

#################### Main program routine
def main():
    pass

###############################################################################

# DH* TODO: create a Python module metadata.py with a class Metadata
# and use this for ccpp_prebuild.py; create a to_html routine for it

def metadata_to_html(metadata, model, filename):
    """Create a metadata table for each variable recorded"""

    # Set debug to true if logging level is debug
    #debug = logging.getLogger().getEffectiveLevel() == logging.DEBUG

    shading = { 0 : 'darkgray', 1 : 'lightgray' }
    success = True

    html = '''<html>
<title>CCPP variables provided by model {model}</title>
<body>
<h1>CCPP variables provided by model {model}</h1>
<table columns="6" border="1" cellpadding="4">
<tr>
  <th bgcolor="{bgcolor}" align="left" >standard_name</th>
  <th bgcolor="{bgcolor}" align="left" >long_name    </th>
  <th bgcolor="{bgcolor}" align="left" >units        </th>
  <th bgcolor="{bgcolor}" align="right">rank         </th>
  <th bgcolor="{bgcolor}" align="left" >type         </th>
  <th bgcolor="{bgcolor}" align="left" >kind         </th>
  <th bgcolor="{bgcolor}" align="left" >source       </th>
  <th bgcolor="{bgcolor}" align="left" >{model} name </th>
</tr>
'''.format(model=model, bgcolor = shading[0])

    count = 0
    for var_name in sorted(metadata.keys()):
        for var in metadata[var_name]:
            # Alternate shading, count is 0 1 0 1 ...
            count = (count+1) % 2
            # ... create html row ...
            line = '''<tr>
  <td bgcolor="{bgcolor}" align="left" >{v.standard_name}</td>
  <td bgcolor="{bgcolor}" align="left" >{v.long_name}    </td>
  <td bgcolor="{bgcolor}" align="left" >{v.units}        </td>
  <td bgcolor="{bgcolor}" align="right">{rank}           </td>
  <td bgcolor="{bgcolor}" align="left" >{v.type}         </td>
  <td bgcolor="{bgcolor}" align="left" >{v.kind}         </td>
  <td bgcolor="{bgcolor}" align="left" >{container}      </td>
  <td bgcolor="{bgcolor}" align="left" >{v.local_name}   </td>
</tr>'''.format(v=var, rank=var.rank.count(':'), container = decode_container(var.container), bgcolor=shading[count])
            html += line
    with open(filename, 'w') as f:
        f.write(html)

    logging.info('Metadata table for model {0} written to {1}'.format(model, filename))
    return success

###############################################################################
if __name__ == "__main__":
    main()
