from ccpp_datafile import datatable_report, DatatableReport
import subprocess


class BaseTests:


    class TestHostDataTables:
        _SEP = ","

        def test_host_files(self):
            test_str = datatable_report(self.database, DatatableReport("host_files"), self._SEP)
            self.assertSetEqual(set(self.host_files), set(test_str.split(self._SEP)))

        def test_suite_files(self):
            test_str = datatable_report(self.database, DatatableReport("suite_files"), self._SEP)
            self.assertSetEqual(set(self.suite_files), set(test_str.split(self._SEP)))

        def test_utility_files(self):
            test_str = datatable_report(self.database, DatatableReport("utility_files"), self._SEP)
            self.assertSetEqual(set(self.utility_files), set(test_str.split(self._SEP)))

        def test_ccpp_files(self):
            test_str = datatable_report(self.database, DatatableReport("ccpp_files"), self._SEP)
            self.assertSetEqual(set(self.ccpp_files), set(test_str.split(self._SEP)))
        
        def test_process_list(self):
            test_str = datatable_report(self.database, DatatableReport("process_list"), self._SEP)
            self.assertSetEqual(set(self.process_list), set(test_str.split(self._SEP)))

        def test_module_list(self):
            test_str = datatable_report(self.database, DatatableReport("module_list"), self._SEP)
            self.assertSetEqual(set(self.module_list), set(test_str.split(self._SEP)))

        def test_dependencies_list(self):
            test_str = datatable_report(self.database, DatatableReport("dependencies"), self._SEP)
            self.assertSetEqual(set(self.dependencies), set(test_str.split(self._SEP)))

        def test_suite_list(self):
            test_str = datatable_report(self.database, DatatableReport("suite_list"), self._SEP)
            self.assertSetEqual(set(self.suite_list), set(test_str.split(self._SEP)))


    class TestHostCommandLineDataFiles:
        _SEP = ","

        def test_host_files(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--host-files"],
                                            capture_output=True,
                                            text=True)
            self.assertEqual(self._SEP.join(self.host_files), completedProcess.stdout.strip())

        def test_suite_files(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--suite-files"],
                                            capture_output=True,
                                            text=True)
            self.assertEqual(self._SEP.join(self.suite_files), completedProcess.stdout.strip())

        def test_utility_files(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--utility-files"],
                                            capture_output=True,
                                            text=True)
            self.assertEqual(self._SEP.join(self.utility_files), completedProcess.stdout.strip())

        def test_ccpp_files(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--ccpp-files"],
                                            capture_output=True,
                                            text=True)
            self.assertEqual(self._SEP.join(self.ccpp_files), completedProcess.stdout.strip())

        def test_process_list(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--process-list"],
                                            capture_output=True,
                                            text=True)
            actualOutput = {s.strip() for s in completedProcess.stdout.split(self._SEP)}
            self.assertSetEqual(set(self.process_list), actualOutput)

        def test_module_list(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--module-list"],
                                            capture_output=True,
                                            text=True)
            self.assertEqual(self._SEP.join(self.module_list), completedProcess.stdout.strip())

        def test_dependencies(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--dependencies"],
                                            capture_output=True,
                                            text=True)
            self.assertEqual(self._SEP.join(self.dependencies), completedProcess.stdout.strip())

        def test_suite_list(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--suite-list"],
                                            capture_output=True,
                                            text=True)
            self.assertEqual(self._SEP.join(self.suite_list), completedProcess.stdout.strip())


    class TestSuite:
        _SEP = ","

        def test_required_variables(self):
            test_str = datatable_report(self.database, DatatableReport("required_variables", value=self.suite_name), self._SEP)
            self.assertSetEqual(set(self.required_vars), set(test_str.split(self._SEP)))

        def test_input_variables(self):
            test_str = datatable_report(self.database, DatatableReport("input_variables", value=self.suite_name), self._SEP)
            self.assertSetEqual(set(self.input_vars), set(test_str.split(self._SEP)))

        def test_output_variables(self):
            test_str = datatable_report(self.database, DatatableReport("output_variables", value=self.suite_name), self._SEP)
            self.assertSetEqual(set(self.output_vars), set(test_str.split(self._SEP)))


    class TestSuiteCommandLine:
        _SEP = ","
    
        def test_required_variables(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--required-variables", self.suite_name],
                                            capture_output=True,
                                            text=True)
            actualOutput = {s.strip() for s in completedProcess.stdout.split(self._SEP)}
            self.assertSetEqual(set(self.required_vars), actualOutput)

        def test_input_variables(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--input-variables", self.suite_name],
                                               capture_output=True,
                                               text=True)
            actualOutput = {s.strip() for s in completedProcess.stdout.split(self._SEP)}
            self.assertSetEqual(set(self.input_vars), actualOutput)

        def test_output_variables(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--output-variables", self.suite_name],
                                               capture_output=True,
                                               text=True)
            actualOutput = {s.strip() for s in completedProcess.stdout.split(self._SEP)}
            self.assertSetEqual(set(self.output_vars), actualOutput)


    class TestSuiteExcludeProtected(TestSuite):
        def test_required_variables_excluding_protected(self):
            test_str = datatable_report(self.database, DatatableReport("required_variables", value="temp_suite"), self._SEP, exclude_protected=True)
            self.assertSetEqual(set(self.required_vars_excluding_protected), set(test_str.split(self._SEP)))

        def test_input_variables_excluding_protected(self):
            test_str = datatable_report(self.database, DatatableReport("input_variables", value="temp_suite"), self._SEP, exclude_protected=True)
            self.assertSetEqual(set(self.input_vars_excluding_protected), set(test_str.split(self._SEP)))


    class TestSuiteExcludeProtectedCommandLine(TestSuiteCommandLine):
        def test_required_variables_excluding_protected(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--exclude-protected", "--required-variables", self.suite_name],
                                               capture_output=True,
                                               text=True)
            actualOutput = {s.strip() for s in completedProcess.stdout.split(self._SEP)}
            self.assertSetEqual(set(self.required_vars_excluding_protected), actualOutput)

        def test_input_variables_excluding_protected(self):
            completedProcess = subprocess.run([self.datafile_script, self.database, "--exclude-protected", "--input-variables", self.suite_name],
                                               capture_output=True,
                                               text=True)
            actualOutput = {s.strip() for s in completedProcess.stdout.split(self._SEP)}
            self.assertSetEqual(set(self.input_vars_excluding_protected), actualOutput)
