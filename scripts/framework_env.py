#!/usr/bin/env python3

"""
Module to contain the runtime options for the CCPP Framework.
Function to parse arguments to the CCPP Framework and store them in an
object which allows various framework functions to access CCPP
Framework runtime information and parameter values.
"""

# Python library imports
import argparse
import os
# CCPP framework imports
from parse_tools import type_name

_EPILOG = '''
'''

###############################################################################
class CCPPFrameworkEnv:
###############################################################################
    """Object and methods to hold the runtime environment and parameter
    options for the CCPP Framework"""

    def __init__(self, logger, ndict=None, verbose=0, clean=False,
                 host_files=None, scheme_files=None, suites=None,
                 preproc_directives=[], generate_docfiles=False, host_name='',
                 kind_types=[], use_error_obj=False, force_overwrite=False,
                 output_root=os.getcwd(), ccpp_datafile="datatable.xml"):
        """Initialize a new CCPPFrameworkEnv object from the input arguments.
        <ndict> is a dict with the parsed command-line arguments (or a
           dictionary created with the necessary arguments).
        <logger> is a logger to be used by users of this object.
        """
        emsg = ''
        esep = ''
        if ndict and ('verbose' in ndict):
            self.__verbosity = ndict['verbose']
            del ndict['verbose']
        else:
            self.__verbosity = verbose
        # end if
        if ndict and ('clean' in ndict):
            self.__clean = ndict['clean']
            del ndict['clean']
        else:
            self.__clean = clean
        # end if
        if ndict and ('host_files' in ndict):
            self.__host_files = ndict['host_files']
            del ndict['host_files']
            if host_files and logger:
                wmsg = "CCPPFrameworkEnv: Using ndict, ignoring 'host_files'"
                logger.warning(wmsg)
            # end if
        elif host_files is None:
            emsg += esep + "Error: 'host_files' list required"
            esep = '\n'
        else:
            self.__host_files = host_files
        # end if
        if ndict and ('scheme_files' in ndict):
            self.__scheme_files = ndict['scheme_files']
            del ndict['scheme_files']
            if scheme_files and logger:
                wmsg = "CCPPFrameworkEnv: Using ndict, ignoring 'scheme_files'"
                logger.warning(wmsg)
            # end if
        elif scheme_files is None:
            emsg += esep + "Error: 'scheme_files' list required"
            esep = '\n'
        else:
            self.__scheme_files = scheme_files
        # end if
        if ndict and ('suites' in ndict):
            self.__suites = ndict['suites']
            del ndict['suites']
            if suites and logger:
                wmsg = "CCPPFrameworkEnv: Using ndict, ignoring 'suites'"
                logger.warning(wmsg)
            # end if
        elif suites is None:
            emsg += esep + "Error: 'suites' list required"
            esep = '\n'
        else:
            self.__suites = suites
        # end if
        if ndict and ('preproc_directives' in ndict):
            preproc_defs = ndict['preproc_directives']
            del ndict['preproc_directives']
        else:
            preproc_defs = preproc_directives
        # end if
        # Turn preproc_defs into a dictionary, start with a list to process
        if isinstance(preproc_defs, list):
            # Someone already handed us a list
            preproc_list = preproc_defs
        elif (not preproc_defs) or (preproc_defs == 'UNSET'):
            # No preprocessor definitions
            preproc_list = list()
        elif ',' in preproc_defs:
            # String of definitions, separated by commas
            preproc_list = [x.strip() for x in preproc_defs.split(',')]
        elif isinstance(preproc_defs, str):
            # String of definitions, separated by spaces
            preproc_list = [x.strip() for x in preproc_defs.split(' ') if x]
        else:
            wmsg = f"Error: Bad preproc list type, '{type_name(preproc_defs)}'"
            emsg += esep + wmsg
            esep = '\n'
        # end if
        # Turn the list into a dictionary
        self.__preproc_defs = {}
        for item in preproc_list:
            tokens = [x.strip() for x in item.split('=', 1)]
            if len(tokens) > 2:
                emsg += esep + "Error: Bad preproc def, '{}'".format(item)
                esep = '\n'
            else:
                key = tokens[0]
                if key[0:2] == '-D':
                    key = key[2:]
                # end if
                if len(tokens) > 1:
                    value = tokens[1]
                else:
                    value = None
                # end if
                self.__preproc_defs[key] = value
            # end if
        # end for
        if ndict and ('generate_docfiles' in ndict):
            self.__generate_docfiles = ndict['generate_docfiles']
            del ndict['generate_docfiles']
        else:
            self.__generate_docfiles = generate_docfiles
        # end if
        if ndict and ('host_name' in ndict):
            self.__host_name = ndict['host_name']
            del ndict['host_name']
        else:
            self.__host_name = host_name
        # end if
        self.__generate_host_cap = self.host_name != ''
        self.__kind_dict = {}
        if ndict and ("kind_type" in ndict):
            kind_list = ndict["kind_type"]
            del ndict["kind_type"]
        else:
            kind_list = kind_types
        # end if
        # Note that the command line uses repeated calls to 'kind_type'
        for kind in kind_list:
            kargs = [x.strip() for x in kind.strip().split('=')]
            if len(kargs) != 2:
                emsg += esep
                emsg += "Error: '{}' is not a valid kind specification "
                emsg += "(should be of the form <kind_name>=<kind_spec>)"
                emsg = emsg.format(kind)
                esep = '\n'
            else:
                kind_name, kind_spec = kargs
                # Do not worry about duplicates, just use last value
                self.__kind_dict[kind_name] = kind_spec
            # end if
        # end for
        # We always need a kind_phys so add a default if necessary
        if "kind_phys" not in self.__kind_dict:
            self.__kind_dict["kind_phys"] = "REAL64"
        # end if
        if ndict and ('use_error_obj' in ndict):
            self.__use_error_obj = ndict['use_error_obj']
            del ndict['use_error_obj']
        else:
            self.__use_error_obj = use_error_obj
        # end if
        if ndict and ('force_overwrite' in ndict):
            self.__force_overwrite = ndict['force_overwrite']
            del ndict['force_overwrite']
        else:
            self.__force_overwrite = force_overwrite
        # end if
        # Make sure we know where output is going
        if ndict and ('output_root' in ndict):
            self.__output_root = ndict['output_root']
            del ndict['output_root']
        else:
            self.__output_root = output_root
        # end if
        self.__output_dir = os.path.abspath(self.output_root)
        # Make sure we can create output database
        if ndict and ('ccpp_datafile' in ndict):
            self.__datatable_file = os.path.normpath(ndict['ccpp_datafile'])
            del ndict['ccpp_datafile']
        else:
            self.__datatable_file = ccpp_datafile
        # end if
        if not os.path.isabs(self.datatable_file):
            self.__datatable_file = os.path.join(self.output_dir,
                                                 self.datatable_file)
        # end if
        self.__logger = logger
        ## Check to see if anything is left in dictionary
        if ndict:
            for key in ndict:
                emsg += esep + "Error: Unknown key in <ndict>, '{}'".format(key)
                esep = '\n'
            # end for
        # end if
        # Raise an exception if any errors were found
        if emsg:
            raise ValueError(emsg)
        # end if

    @property
    def verbosity(self):
        """Return the <verbosity> property for this CCPPFrameworkEnv object."""
        return self.__verbosity

    @property
    def clean(self):
        """Return the <clean> property for this CCPPFrameworkEnv object."""
        return self.__clean

    @property
    def host_files(self):
        """Return the <host_files> property for this CCPPFrameworkEnv object."""
        return self.__host_files

    @property
    def scheme_files(self):
        """Return the <scheme_files> property for this
        CCPPFrameworkEnv object."""
        return self.__scheme_files

    @property
    def suites(self):
        """Return the <suites> property for this
        CCPPFrameworkEnv object."""
        return self.__suites

    @property
    def preproc_defs(self):
        """Return the <preproc_defs> property for this
        CCPPFrameworkEnv object."""
        return self.__preproc_defs

    @property
    def generate_docfiles(self):
        """Return the <generate_docfiles> property for this
        CCPPFrameworkEnv object."""
        return self.__generate_docfiles

    @property
    def host_name(self):
        """Return the <host_name> property for this CCPPFrameworkEnv object."""
        return self.__host_name

    @property
    def generate_host_cap(self):
        """Return the <generate_host_cap> property for this
        CCPPFrameworkEnv object."""
        return self.__generate_host_cap

    def kind_spec(self, kind_type):
        """Return the kind specification for kind type, <kind_type>
        for this CCPPFrameworkEnv object.
        If there is no entry for <kind_type>, return None."""
        kind_spec = None
        if kind_type in self.__kind_dict:
            kind_spec = self.__kind_dict[kind_type]
        # end if
        return kind_spec

    def kind_types(self):
        """Return a list of all kind types defined in this
        CCPPFrameworkEnv object."""
        return self.__kind_dict.keys()

    @property
    def use_error_obj(self):
        """Return the <use_error_obj> property for this
        CCPPFrameworkEnv object."""
        return self.__use_error_obj

    @property
    def force_overwrite(self):
        """Return the <force_overwrite> property for this
        CCPPFrameworkEnv object."""
        return self.__force_overwrite

    @property
    def output_root(self):
        """Return the <output_root> property for this
CCPPFrameworkEnv object."""
        return self.__output_root

    @property
    def output_dir(self):
        """Return the <output_dir> property for this CCPPFrameworkEnv object."""
        return self.__output_dir

    @property
    def datatable_file(self):
        """Return the <datatable_file> property for this
        CCPPFrameworkEnv object."""
        return self.__datatable_file

    @property
    def logger(self):
        """Return the <logger> property for this CCPPFrameworkEnv object."""
        return self.__logger

###############################################################################
def parse_command_line(args, description, logger=None):
###############################################################################
    """Create an ArgumentParser to parse and return a CCPPFrameworkEnv
    object containing the command-line arguments and related quantities."""
    ap_format = argparse.RawTextHelpFormatter
    parser = argparse.ArgumentParser(description=description,
                                     formatter_class=ap_format, epilog=_EPILOG)

    parser.add_argument("--host-files", metavar='<host files filename>',
                        type=str, required=True,
                        help="""Comma separated list of host filenames to process
Filenames with a '.meta' suffix are treated as host model metadata files
Filenames with a '.txt' suffix are treated as containing a list of .meta
filenames""")

    parser.add_argument("--scheme-files", metavar='<scheme files filename>',
                        type=str, required=True,
                        help="""Comma separated list of scheme filenames to process
Filenames with a '.meta' suffix are treated as scheme metadata files
Filenames with a '.txt' suffix are treated as containing a list of .meta
filenames""")

    parser.add_argument("--suites", metavar='<Suite definition file(s)>',
                        type=str, required=True,
                        help="""Comma separated list of suite definition filenames to process
Filenames with a '.xml' suffix are treated as suite definition XML files
Other filenames are treated as containing a list of .xml filenames""")

    parser.add_argument("--preproc-directives",
                        metavar='VARDEF1[,VARDEF2 ...]', type=str, default='',
                        help="Proprocessor directives used to correctly parse source files")

    parser.add_argument("--ccpp-datafile", type=str,
                        metavar='<data table XML filename>',
                        default="datatable.xml",
                        help="Filename for information on content generated by the CCPP Framework")

    parser.add_argument("--output-root", type=str,
                        metavar='<directory for generated files>',
                        default=os.getcwd(),
                        help="directory for generated files")

    parser.add_argument("--host-name", type=str, default='',
                        help='''Name of host model to use in CCPP API
If this option is passed, a host model cap is generated''')

    parser.add_argument("--clean", action='store_true', default=False,
                        help='Remove files created by this script, then exit')

    parser.add_argument("--kind-type", type=str, action='append',
                        metavar="kind_type", default=list(),
                        help="""Data size for real(<kind_type>) data.
Entry in the form of <kind_type>=<kind_val>
e.g., --kind-type "kind_phys=REAL64"
Enter more than one --kind-type entry to define multiple CCPP kinds.
<kind_val> SHOULD be a valid ISO_FORTRAN_ENV type""")

    parser.add_argument("--generate-docfiles",
                        metavar='HTML | Latex | HTML,Latex', type=str,
                        help="Generate LaTeX and/or HTML documentation")

    parser.add_argument("--use-error-obj", action='store_true', default=False,
                        help="""Host model and caps use an error object
instead of ccpp_error_message and ccpp_error_code.""")

    parser.add_argument("--force-overwrite", action='store_true', default=False,
                        help="""Overwrite all CCPP-generated files, even
if unmodified""")

    parser.add_argument("--verbose", action='count', default=0,
                        help="Log more activity, repeat for increased output")
    pargs = parser.parse_args(args)
    return CCPPFrameworkEnv(logger, vars(pargs))
