#!/usr/bin/env python3

"""
Define the CCPPDatabaseObj object
Object definition and methods to provide information from a run of capgen.
"""

from host_model import HostModel
from ccpp_suite import API

class CCPPDatabaseObjError(ValueError):
    """Error class specific to CCPPDatabaseObj.
    All uses of this error should be internal (i.e., programmer error,
       not user error)."""

    def __init__(self, message):
        """Initialize this exception"""
        super().__init__(message)

class CCPPDatabaseObj:
    """Ojbect with data and methods to provide information from a run of capgen.
    """

    def __init__(self, run_env, host_model=None, api=None, database_file=None):
        """Initialize this CCPPDatabaseObj.
        If <database_file> is not None, all other inputs MUST be None and
           the object is created from the database table created by capgen.
        To initialize the object from an in-memory capgen run, ALL other
           inputs MUST be passed (i.e., not None) and it is an error to pass
           a value for <database_file>.
        """

        runtime_obj = all([host_model is not None, api is not None])
        self.__host_model = None
        self.__api = None
        self.__database_file = None
        if runtime_obj and database_file:
            emsg = "Cannot provide both runtime arguments and database_file."
        elif (not runtime_obj) and (not database_file):
            emsg = "Must provide either database_file or all runtime arguments."
        else:
            emsg = ""
        # end if
        if emsg:
            raise CCPPDatabaseObjError(f"ERROR: {emsg}")
        # end if
        if runtime_obj:
            self.__host_model = host_model
            self.__api = api
        else:
            self.db_from_file(run_env, database_file)
        # end if

    def db_from_file(self, run_env, database_file):
        """Create the necessary internal data structures from a CCPP
              datatable.xml file created by capgen.
        """
        metadata_tables = {}
        host_name = "cam"
        self.__host_model = HostModel(metadata_tables, host_name, run_env)
        self.__api = API(sdfs, host_model, scheme_headers, run_env)
        raise CCPPDatabaseObjError("ERROR: <database_file> not supported")

    def host_model_dict(self):
        """Return the host model dictionary for this CCPP DB object"""
        if self.__host_model is not None:
            return self.__host_model
        # end if
        raise CCPPDatabaseObjError("ERROR: <database_file> not supported")

    def suite_list(self):
        """Return a list of suites built into the API"""
        if self.__api is not None:
            return list(self.__api.suites)
        # end if
        raise CCPPDatabaseObjError("ERROR: <database_file> not supported")

    def constituent_dictionary(self, suite):
        """Return the constituent dictionary for <suite>"""
        return suite.constituent_dictionary()

    def call_list(self, phase):
        """Return the API call list for <phase>"""
        if self.__api is not None:
            return self.__api.call_list(phase)
        # end if
        raise CCPPDatabaseObjError("ERROR: <database_file> not supported")
