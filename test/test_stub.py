from ccpp_datafile import datatable_report, DatatableReport
import subprocess

class BaseTests:

    class TestHostDataTables:
        _SEP = ";"

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
            actualOutput = {s.strip() for s in completedProcess.stdout.split(self._SEP)}
            self.assertSetEqual(set(self.host_files), actualOutput)

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
            completedProcess = subprocess.run([self.datafile_script, self.database, "--suite-list", "--sep=;"],
                                            capture_output=True,
                                            text=True)
            # actualOutput = {s.strip() for s in completedProcess.stdout.split(";")}
            self.assertEqual(";".join(self.suite_list), completedProcess.stdout.strip())
