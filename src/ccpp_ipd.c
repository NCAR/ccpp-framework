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

#if 0
static const char libname[] = "libccpp.so";  /* Library name */
static const char version[] = "1";      /* Linux needs the ver num */
#endif

/**
 * IPD physics cap calling rouinte.
 *
 * @param[in] scheme    The physics scheme name to call.
 * @param[in] aip       The aip_t fortran type.
 * @retval     0        If it was sucessful
 * @retval     1        If there was an error
 **/
int
ccpp_ipd_phy_cap(const char *scheme, void *aip)
{
	void *hdl = NULL;
	void (*f_ptr)(void *) = NULL;
	int i = 0;
	int n = 0;
	char *scheme_cap = NULL;
	char *error = NULL;

	/* Generate the scheme cap function name */
	n = strlen(scheme);
	scheme_cap = malloc(n+5);
	memset(scheme_cap, 0, (n+5)*sizeof(char));

	for (i=0; i < n; ++i) {
		scheme_cap[i] = tolower(scheme[i]);
	}

	strncat(scheme_cap, "_cap", n+5);

	/* Open a handle to ourself */
	hdl = dlopen(NULL, RTLD_NOW);
	if (!hdl) {
		errx(EX_SOFTWARE, "%s", dlerror());
	}

	*(void **) (&f_ptr) = dlsym(hdl, scheme_cap);
	if ((error = dlerror()) != NULL)  {
		errx(EX_SOFTWARE, "%s", error);
	}

	/* Call the schemes cap subroutine */
	f_ptr(aip);

	/* Free the scheme cap function name */
	if (scheme_cap) {
		free(scheme_cap);
		scheme_cap = NULL;
	}

	dlclose(hdl);

	return(EXIT_SUCCESS);
}

/**
 * @}
 **/
