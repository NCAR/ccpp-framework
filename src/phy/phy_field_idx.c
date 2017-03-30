/**
 * @file phy_field_idx
 *
 * @breif Routines and functions to generate and lookup
 *        fields/variables needed for the physics routines.
 *
 * @ingroup Physics
 * @{
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <sysexits.h>

#include "types.h"

struct idx {
	int  n;
	char name[STR_LEN];
};

/**
 * Comparison function.
 **/
static int
cmp(const void *f1, const void *f2)
{
	struct idx *f_1 = (struct idx *) f1;
	struct idx *f_2 = (struct idx *) f2;
	return strcmp(f_1->name, f_2->name);
}

/**
 * Initialization routine.
 **/
int
phy_field_idx_init(int *max, void **index)
{

	*index = malloc((*max) * sizeof(struct idx));
	if (*index == NULL) {
		errx(EX_SOFTWARE, "Unable to allocate field index");
	}

	return(EXIT_SUCCESS);
}

/**
 * Finialization routine.
 **/
int
phy_field_idx_fini(void **index)
{
	free(*index);
	*index = NULL;

	return(EXIT_SUCCESS);
}

/**
 * Add/Insert a field into the index.
 **/
int
phy_field_idx_add(const char *name, int *i, void **index)
{
	struct idx *f_index = (struct idx *)(*index);

	strncpy(f_index[*i-1].name, name, STR_LEN * sizeof(char));
	f_index[*i-1].n = *i;

	return(EXIT_SUCCESS);
}


/**
 * Find the index number of a field.
 **/
int
phy_field_idx_find(const char *name, int *n_fields, void **index)
{
	int n = 0;
	struct idx  key = {0};
	struct idx *res = NULL;

	n = strlen(name);
	strncpy(key.name, name, n);
	key.name[n] = '\0';

	res = bsearch(&key, *index, *n_fields, sizeof(struct idx), cmp);
	if (res == NULL) {
		errx(EX_SOFTWARE, "Unable to find in index: %s", name);
	}

	return(res->n);
}

/**
 * Sort the index.
 **/
int
phy_field_idx_sort(int *n_fields, void **index)
{
	qsort(*index, *n_fields, sizeof(struct idx), cmp);
	return(EXIT_SUCCESS);
}
