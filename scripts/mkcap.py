#!/usr/bin/env python
#
# Script to generate a cap module and subroutines
# from a scheme xml file.
#

from __future__ import print_function
import os
import sys
import getopt
import xml.etree.ElementTree as ET

#################### Main program routine
def main():
    args = parse_args()
    data = parse_scheme(args['scheme'])
    cap = Cap()
    cap.filename = args['output']
    cap.write(data)

#################### Parse the command line arguments
def parse_args():
    args = {}
    opts, rem = getopt.getopt(sys.argv[1:],
                              'hvo:',
                              ['help',
                               'verbose',
                               'output=',
                              ])
    for opt, arg in opts:
        if opt in ('-h', '--help'):
            lusage()
        elif opt in ('-v', '--verbose'):
            args['verbose'] = True
        elif opt in ('-o', '--output'):
            args['output'] = arg
        else:
            usage()

    if (not rem):
        eprint("Must specify an input scheme file")
        usage()

    if (os.path.isfile(rem[0])):
        args['scheme'] = rem[0]
    else:
        eprint("Unable to read input scheme file: {0}".format(rem[0]))
        usage()

    if (not 'output' in args):
        args['output'] = sys.stdout

    return args

#################### Parse the scheme xml file into a data dictionary
def parse_scheme(filename):

    data = {}

    tree = ET.parse(filename)
    root = tree.getroot()

    data['module'] = root.attrib.get('module')
    data['subs'] = {}

    for sub in root.findall('subroutine'):
        name = sub.attrib.get('name')
        data['subs'][name] = {}
        data['subs'][name]['vars'] = []

        for var in sub.findall('var'):
            v = Var()
            v.standard_name = var.find('standard_name').text
            #v.long_name     = var.find('long_name').text
            v.units         = var.find('units').text
            v.local_name    = var.find('local_name').text
            v.type          = var.find('type').text
            v.rank          = int(var.find('rank').text)
            data['subs'][name]['vars'].append(v)

    return data

#################### Print a usage statement
def usage():
    name = os.path.basename(__file__)
    eprint("Usage {0}: [-h] [-v] [-o output.f90] scheme.xml".format(name))
    sys.exit(1)

#################### Print a long usage statement
def lusage():
    pass

#################### Print a message to STDERR
def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


###############################################################################
class Var(object):

    def __init__(self, **kwargs):
        self._standard_name = None
        self._long_name     = None
        self._units         = None
        self._local_name    = None
        self._type          = None
        self._rank          = None
        self._container     = None
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    @property
    def standard_name(self):
        '''Get the name of the variable.'''
        return self._standard_name

    @standard_name.setter
    def standard_name(self, value):
        self._standard_name = value

    @property
    def long_name(self):
        '''Get the name of the variable.'''
        return self._long_name

    @long_name.setter
    def long_name(self, value):
        self._long_name = value

    @property
    def units(self):
        '''Get the units of the variable.'''
        return self._units

    @units.setter
    def units(self, value):
        self._units = value

    @property
    def local_name(self):
        '''Get the local variable name of the variable.'''
        return self._local_name

    @local_name.setter
    def local_name(self, value):
        self._local_name = value

    @property
    def type(self):
        '''Get the type of the variable.'''
        return self._type

    @type.setter
    def type(self, value):
        self._type = value

    @property
    def rank(self):
        '''Get the rank of the variable.'''
        return self._rank

    @rank.setter
    def rank(self, value):
        if not isinstance(value, int):
            raise TypeError('Invalid type for variable property rank, must be integer')
        if (value == 0):
            self._rank = ''
        else:
            self._rank = '('+ ','.join([':'] * value) +')'

    @property
    def intent(self):
        '''Get the intent of the variable.'''
        return self._intent

    @intent.setter
    def intent(self, value):
        if not value in ['none', 'in', 'out', 'inout']:
            raise ValueError('Invalid value {0} for variable property intent'.format(value))
        self._intent = value

    @property
    def optional(self):
        '''Get the optional of the variable.'''
        return self._optional

    @optional.setter
    def optional(self, value):
        if not value in ['T', 'F']:
            raise ValueError('Invalid value {0} for variable property optional'.format(value))
        self._optional = value

    @property
    def container(self):
        '''Get the container of the variable.'''
        return self._container

    @container.setter
    def container(self, value):
        self._container = value

    def compatible(self, other):
        # DH* 20171206 - removed long_name from mandatory items for compatibility
        #            and self.long_name == other.long_name \
        return self.standard_name == other.standard_name \
            and self.units == other.units \
            and self.type == other.type \
            and self.rank == other.rank

    def print_def(self):
        '''Print the definition line for the variable.'''
        str = "{s.type}, pointer     :: {s.local_name}{s.rank}"
        return str.format(s=self)

    def print_get(self):
        '''Print the data retrieval line for the variable.'''

        str='''
        call ccpp_fields_get(cdata, '{s.standard_name}', {s.local_name}, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve {s.standard_name}')
            return
        end if'''
        return str.format(s=self)

    def print_debug(self):
        '''Print the data retrieval line for the variable.'''
    
        # DH* 20171206 - removed long_name from mandatory items for compatibility
        str='''Contents of {s} (* = mandatory for compatibility):
        standard_name = {s.standard_name} *
        long_name     = {s.long_name}
        units         = {s.units} *
        local_name    = {s.local_name}
        type          = {s.type} *
        rank          = {s.rank} *
        intent        = {s.intent}
        optional      = {s.optional}
        container     = {s.container}'''
        return str.format(s=self)
    
    @classmethod
    def from_table(cls, columns, data):
        # DH* - workaround to use the existing table headers
        standard_name = data[columns.index('longname')]
        #standard_name = data[columns.index('standard_name')]
        long_name = data[columns.index('description')]
        #long_name = data[columns.index('long_name')]
        units = data[columns.index('units')]
        local_name = data[columns.index('local var name')]
        #local_name = data[columns.index('local_name')]
        type = data[columns.index('type')]
        rank = data[columns.index('rank')]
        intent = data[columns.index('intent')]
        optional = data[columns.index('optional')]
        # *DH
        return cls(standard_name = standard_name,
                   long_name = long_name,
                   units = units,
                   local_name = local_name,
                   type = type,
                   rank = rank,
                   intent = intent,
                   optional = optional,
                   )

    def to_xml(self, element):
        element.set('name', self._standard_name)
        sub_element = ET.SubElement(element, 'standard_name')
        sub_element.text = self._standard_name
        sub_element = ET.SubElement(element, 'long_name')
        sub_element.text = self._long_name
        sub_element = ET.SubElement(element, 'units')
        sub_element.text = self._units
        sub_element = ET.SubElement(element, 'local_name')
        sub_element.text = self._local_name
        sub_element = ET.SubElement(element, 'type')
        sub_element.text = self._type
        sub_element = ET.SubElement(element, 'rank')
        sub_element.text = self._rank
        sub_element = ET.SubElement(element, 'intent')
        sub_element.text = self._intent
        sub_element = ET.SubElement(element, 'optional')
        sub_element.text = self._optional
        sub_element = ET.SubElement(element, 'container')
        sub_element.text = self._container
        return element

###############################################################################
class Cap(object):

    header='''
!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Auto-generated cap module for the {module} scheme
!!
!
module {module}_cap

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr
    use            :: ccpp_types,                                      &
                      only: ccpp_t
    use            :: ccpp_fields,                                     &
                      only: ccpp_fields_get
    use            :: ccpp_errors,                                     &
                      only: ccpp_error
    use            :: {module}, &
                      only: {subroutines}
    implicit none

    private
    public :: {subroutine_caps}

    contains

'''

    sub='''
    subroutine {subroutine}_cap(ptr) bind(c)

        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer      :: cdata
        integer                    :: ierr
{var_defs}

        call c_f_pointer(ptr, cdata)

        {var_gets}

        call {subroutine}({args})
    end subroutine {subroutine}_cap
'''

    def __init__(self, **kwargs):
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, data):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        subs = ','.join(["{0}".format(s) for s in data['subs']])
        sub_caps = ','.join(["{0}_cap".format(s) for s in data['subs']])

        f.write(Cap.header.format(module = data['module'],
                                  subroutines = subs,
                                  subroutine_caps = sub_caps))
        for (k, v) in data['subs'].items():
            var_defs = "\n".join([" "*8 + x.print_def() for x in v['vars']])
            var_gets = "\n".join([x.print_get() for x in v['vars']])
            args = ','.join(["{0}={0}".format(x.local_name) for x in v['vars']])
            f.write(Cap.sub.format(subroutine=k,
                                   var_defs=var_defs,
                                   var_gets=var_gets,
                                   args=args))

        f.write("end module {module}_cap\n".format(module = data['module']))

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

###############################################################################
if __name__ == "__main__":
    main()

