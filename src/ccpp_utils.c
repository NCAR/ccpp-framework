/**
 * @file ccpp_utils.c
 *
 * Utility routines that are commonly used in CCPP.
 *
 * @ingroup common
 * @{
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <err.h>

/**
 * Resolves the absolute path when given a relative path.
 *
 * @param[in]  rel    Relative path name
 * @param[out] abs    Absolute path name
 * @retval     0      If the path was resolved
 * @retval     1      If we were unable to resolve the path
 **/
int
ccpp_abs_path(const char *rel, char **abs)
{
	size_t bsize = 0;
	char *buf = NULL;
	struct stat sbuf = {0};

	/* make sure we were given a relative path */
	if (rel == NULL) {
		warn("Unable to resolve null relative filename");
		return(EXIT_FAILURE);
	}

	/* make sure the absolute path holder is null */
	if (*abs != NULL) {
		warn("Unable to write to non-null absolute filename pointer");
		return(EXIT_FAILURE);
	}

	/* make sure relative path actually exists */
	if (stat(rel, &sbuf) < 0) {
		warn("Unable to stat %s", rel);
		return(EXIT_FAILURE);
	}

	if ((bsize = pathconf(".", _PC_PATH_MAX)) < 0) {
		warn("Unable to obtain maximum size of pathname");
		return(EXIT_FAILURE);
	}

	buf = malloc((bsize + 1) * sizeof(char));

	/* find the absolute path */
	if (realpath(rel, buf) == NULL) {
		warn("Unable to resolve %s an error occurred at %s", rel, buf);
		free(buf);
		return(EXIT_FAILURE);
	}

	bsize = strlen(buf);
	/* malloc the absolute path */
	*abs = malloc((bsize + 1) * sizeof(char));
	strncpy(*abs, buf, bsize);
	(*abs)[bsize] = '\0';

	/* free up temporary stuff */
	free(buf);
	buf = NULL;

	return(EXIT_SUCCESS);
}
