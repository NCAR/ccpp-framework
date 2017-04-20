/**
 * @file ccpp_ipd.c
 *
 * Routines and functions for the IPD to call physics routines.
 *
 * @ingroup IPD
 * @{
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <dlfcn.h>
#include <err.h>
#include <sysexits.h>

#include "ccpp_ipd.h"

/** Shared library prefix and suffix for different platforms **/
static const char prefix[] = "lib";
#if __APPLE__
static const char suffix[] = ".dylib";
#elif __unix__
static const char suffix[] = ".so";
#endif

/**
 * IPD cap initialization routine.
 *
 * @param[in]  scheme    The scheme name to call.
 * @param[in]  lib       The library continaing the physics scheme.
 * @param[in]  ver       The library version number.
 * @param[out] shdl      The scheme function pointer handle.
 * @param[out] lhdl      The library handle.
 * @retval     0         If it was sucessful
 * @retval     1         If there was an error
 **/
int
ccpp_ipd_open(const char *scheme, const char *lib, const char *ver,
	      void **shdl, void **lhdl)
{
	int i = 0;
	int n = 0;
	const char cap[] = "_cap";
	char *library = NULL;
	char *scheme_cap = NULL;
	char *error = NULL;

	/* Generate the library name with the platform suffix */
	n = (strlen(prefix) + strlen(lib) + strlen(suffix) + strlen(ver) +2)
		*sizeof(char);
	library = malloc(n);
	memset(library, 0, n);
	if (strcmp(ver, "") != 0) {
		snprintf(library, n, "%s%s.%s%s", prefix, lib, ver, suffix);
	} else {
		snprintf(library, n, "%s%s%s", prefix, lib, suffix);
	}

	/* Generate the scheme cap function name */
	n = (strlen(scheme) +strlen(cap) +1)*sizeof(char);
	scheme_cap = malloc(n);
	memset(scheme_cap, 0, n);

	for (i=0; i < n; ++i) {
		scheme_cap[i] = tolower(scheme[i]);
	}

	strncat(scheme_cap, cap, n);

	/* Open a handle to the library */
	*lhdl = dlopen(library, RTLD_NOW);
	if (!*lhdl) {
		warnx("%s", dlerror());
		return(EXIT_FAILURE);
	}

	dlerror();
	/*
	*(void **)(&(*shdl)) = dlsym(*lhdl, scheme_cap);
	*/
	*(void **)shdl = dlsym(*lhdl, scheme_cap);
	if ((error = dlerror()) != NULL)  {
		warnx("%s", error);
		return(EXIT_FAILURE);
	}

	/* Free the library filename */
	if (library) {
		free(library);
		library = NULL;
	}

	/* Free the scheme cap function name */
	if (scheme_cap) {
		free(scheme_cap);
		scheme_cap = NULL;
	}

	return(EXIT_SUCCESS);
}

/**
 * IPD library finialization routine.
 *
 * @param[in] lhdl      The library handle.
 * @retval     0        If it was sucessful
 * @retval     1        If there was an error
 **/
int
ccpp_ipd_close(void **lhdl)
{
	char *error = NULL;

	dlerror();
	dlclose(lhdl);
	if ((error = dlerror()) != NULL)  {
		warnx("%s", error);
		return(EXIT_FAILURE);
	}

	return(EXIT_SUCCESS);
}

/**
 * IPD cap calling routine.
 *
 * @param[in] f_ptr     The scheme function pointer to call.
 * @param[in] data      The opaque ccpp_t data type to pass.
 * @retval     0        If it was sucessful
 * @retval     1        If there was an error
 **/
int
ccpp_ipd_cap(void **f_ptr, void **data)
{
	void (*fun)(void **) = *f_ptr;

	/* Call the schemes cap subroutine */
	fun(data);

	return(EXIT_SUCCESS);
}

/**
 * @}
 **/
