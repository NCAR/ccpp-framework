#!/usr/bin/env python

# Standard modules
import os

# Local modules
from common import execute
from parse_fortran import parse_subroutine_call
from parse_scheme_table import parse_scheme_tables
from process_tables import scheme_type, subroutine_type, parse_xml_files, process_arguments, find_longname_conflicts


separators = [
    {
     'user' : 'christopherwharrop',
     'branch' : 'harrop_gfs',
     'files' : ['sfc_nst.f'],
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'], # last update 20171114_1036
     'use_existing' : False,
    },
    { 
     'user' : 'davegill',
     'branch' : 'gfs_separator_SASS',
     'files' : ['mfdeepcnv.f'],
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'], # last update 20171114_1036
     'use_existing' : False,
    },
    { 
     'user' : 'grantfirl',
     'branch' : 'EDMF_table',
     'files' : ['moninedmf.f', 'GFS_PBL_generic.f90'], # last update 20171114_1036
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'],
     'use_existing' : False,
    },
    {
     'user' : 'gsketefian',
     'branch' : 'gsk_separator_work_gfs',
     'files' : ['gwdc.f', 'gwdps.f', 'rayleigh_damp.f', 'get_prs_fv3.f90', 'get_prs_fv3.f90', 'cnvc90.f', 'dcyc2.f', ], # last update 20171114_1036
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'],
     'use_existing' : False,
    },
    { 
     'user' : 'JulieSchramm',
     'branch' : 'gfs_separator_shalcnv',
     'files' : ['mfshalcnv.f'],
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'], # last update 20171114_1036
     'use_existing' : False,
    },
    #{
    # 'user' : 'kellylittleblackdog',
    # 'branch' : 'seaice_separator_work_gfs',
    # 'files' : ['sfc_sice.f'],
    # 'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'], # last update 20171114_1036
    # 'use_existing' : False,
    #},
    {
     'user' : 'lulinxue',
     'branch' : 'GFS_separate_Noah_LSM_master_gfsphysics',
     'files' : ['sfc_drv.f', 'sfc_diff.f', 'sfc_diag.f'], # last update 20171114_1036
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'],
     'use_existing' : True,
    },
    { 
     'user' : 'mzhangw',
     'branch' : 'masep',
     'files' : ['gscond.f', 'precpd.f', 'GFS_calpreciptype.f90', 'GFS_MP_generic_post.f90'], # last update 20171114_1036
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'],
     'use_existing' : False,
    },
    { 
     'user' : 'pedro-jm',
     'branch' : 'radiation2',
     'files' : ['radsw_main.f', 'radlw_main.f'],
     'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'], # last update 20171110 or earlier
     'use_existing' : True,
    },
    #{ 'user' : '', 'branch' : '', 'files' : [''], 'driverfiles' : ['GFS_physics_driver.F90', 'GFS_radiation_driver.F90'], 'use_existing' : False,},
    ]

SEPARATORS_INFO_HTML_TEMPLATE = """<p><h2>List of separators, branches and files with modifications</h2></p>

<table>
<tr>
<th align='left'>Separator</th><th align='left'>Branch</th><th align='left'>Files modified</th>
</tr>
{0}
</table>
"""

def main():
    basedir = os.getcwd()

    # For each of the separators, check out the branches listed above
    calling_var_set = set()
    all_schemes = []
    for separator in separators:
        os.chdir(basedir)
        user = separator['user']
        branch = separator['branch']
        clonedir = '{0}_{1}_gmtb-gfsphysics'.format(user, branch)
        use_existing = separator['use_existing']
        if use_existing and not os.path.isdir(clonedir):
            raise Exception('I was asked to use existing clone directory {0}, but it does not exist.'.format(clonedir))
        elif not use_existing and os.path.isdir(clonedir):
            print 'Clone directory {0} already exists, removing it.'.format(clonedir)
            cmd = 'rm -fr {0}'.format(clonedir)
            execute(cmd)

        if not use_existing:
            # Clone repository
            cmd = 'git clone https://github.com/{0}/gmtb-gfsphysics.git {1}'.format(user, clonedir)
            execute(cmd)
            # Checkout branch
            os.chdir(clonedir)
            cmd = 'git checkout {0}'.format(branch)
            execute(cmd, debug = True)

        # It is assumed that all modified files (except the driverfile) reside in subdirectory physics
        os.chdir(basedir)
        files = [ os.path.join(clonedir, 'physics', file) for file in separator['files'] ]
        # Test that all files exists
        for file in files:
            if not os.path.isfile(file):
                raise Exception('Input file {0} for user {1} and branch {2} not found.'.format(file, user, branch))
        xmlfiles = parse_scheme_tables(files)
        #print xmlfiles
        schemes = parse_xml_files(xmlfiles)
        #print schemes

        for driverfile in separator['driverfiles']:
            driverfilepath = os.path.join(clonedir, 'GFS_layer', driverfile)

            new_schemes = []
            for scheme in schemes:
                new_subroutines = []
                for subroutine in scheme.subroutines:

                    # Extract the argument list for every call to the routine in the driverfile
                    calls = parse_subroutine_call(driverfilepath, subroutine.name)
                    # Warn if function is not called?
                    if len(calls) == 0:
                        print "Found no calls to routine {0} in {1}, skip".format(subroutine.name, driverfile)
                    for call in calls:
                        print "Parsing call {0}/{1} for routine {2} in {3}".format(calls.index(call)+1, len(calls),
                                                                                       subroutine.name, driverfile)
                        new_tuple = subroutine_type(name = subroutine.name, arguments = subroutine.arguments, calling_vars = call)
                        #(success, subroutine) = process_arguments(new_tuple)
                        (success, new_subroutine) = process_arguments(new_tuple)
                        if success:
                            new_subroutines.append(new_subroutine)
                            for var in subroutine.arguments:
                                #only add the calling var to the set if it is not empty
                                if(var.calling_var):
                                    calling_var_set.add(var.calling_var)

                new_schemes.append(scheme_type(name=scheme.name, subroutines=new_subroutines))
            all_schemes += new_schemes

        del schemes

    print
    print "---"
    print
    
    # Create info on separators as html table
    html_text = ''
    for separator in separators:
        html_text += '<tr><td>{0}</td><td>{1}</td><td>{2}</td></tr>\n'.format(separator['user'],
                                                                                 separator['branch'],
                                                                                 ', '.join(separator['files']))
    separators_info = SEPARATORS_INFO_HTML_TEMPLATE.format(html_text)

    html = find_longname_conflicts(calling_var_set, all_schemes, separators_info)
    f = open('conflicts.html', 'w')
    f.write(html)
    f.close()
    print 'Write "conflicts.html"'

if __name__ == '__main__':
    main()
