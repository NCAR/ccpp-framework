/**
 * @file ccpp_fields_idx.c
 *
 * @brief Routines and functions to generate and lookup fields/variables
 *        needed for the physics routines.
 *
 * @details The fields are stored in an array of C pointers within the
 *          ccpp_t type. There is also an index array in this type.
 *          We poppulate this index array with the standard name of
 *          each variable in the fields array. We use a binary search
 *          on the sorted index array to retreive the array index for
 *          the field witin the fields array.
 *
 * TODO 
 * - Test the sort and lookup times for qsort() and bsearch().
 * - Implement this as a hash-map instead.
 *
 * @ingroup Physics
 * @{
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <err.h>
#include <sysexits.h>
#include <assert.h>

#include "ccpp_fields_idx.h"

/**
 * Comparison function.
 *
 * Compares the name of two index elements using strcmp().
 * It returns an integer less than, equal to, or greater than
 * zero if the name in f1 is  found, respectively, to be less
 * than, to match, or be greater than the name in f2.
 *
 * @param[in] f1   The first field.
 * @param[in] f2   The second field.
 **/
static int
cmp(const void *f1, const void *f2)
{
	struct ccpp_field *f_1 = *(struct ccpp_field * const *) f1;
	struct ccpp_field *f_2 = *(struct ccpp_field * const *) f2;
	return strcmp(f_1->name, f_2->name);
}

/**
 * Initialization routine.
 *
 * Allocates an array for the field indices.
 *
 * @param[inout] index The index array.
 * @retval       0     If it was sucessful.
 * @retval       1     If there was an error.
 **/
int
ccpp_field_idx_init(void **index)
{

	struct ccpp_field_idx *f_index = NULL;
	int i = 0;

	*index = (struct ccpp_field_idx *)malloc(sizeof(struct ccpp_field_idx));
	if (*index == NULL) {
		warnx("Unable to allocate field index");
		return(EXIT_FAILURE);
	}

	f_index = (struct ccpp_field_idx *)(*index);

	f_index->sorted = 0;
	f_index->n      = 0;
	f_index->max    = CCPP_FIELD_IDX_MAX;
	f_index->fields = malloc(CCPP_FIELD_IDX_MAX *
				 sizeof(struct ccpp_field *));

	return(EXIT_SUCCESS);
}

/**
 * Finialization routine.
 *
 * Deallocates the field indices array.
 *
 * @param[inout] index The index array.
 * @retval       0     If it was sucessful.
 **/
int
ccpp_field_idx_fini(void **index)
{
	int i = 0;

	struct ccpp_field_idx *f_index = (struct ccpp_field_idx *)(*index);

	for (i = 0; i < f_index->n; ++i) {
		if (f_index->fields[i]->name) {
			free(f_index->fields[i]->name);
			f_index->fields[i]->name = NULL;
		}
		free(f_index->fields[i]);
		f_index->fields[i] = NULL;
	}
	free(f_index->fields);
	f_index->fields = NULL;

	free(f_index);
	f_index = NULL;

	return(EXIT_SUCCESS);
}

/**
 * Add/Insert a field into the index.
 *
 * @param[in]    name  The name to add to the index array.
 * @param[inout] index The index array.
 * @retval       > 0   The index location.
 * @retval       -1    If there was an error.
 **/
int
ccpp_field_idx_add(const char *name, void **index)
{
	struct ccpp_field_idx *f_index = (struct ccpp_field_idx *)(*index);
	int n = 0;
	int ierr = 0;
	size_t len = 0;

	n = f_index->n;
	if (n == f_index->max) {
		if (ccpp_field_idx_grow(index)) {
			warnx("Unable to grow field index array");
			return(-1);
		}
	}
	f_index->fields[n] = malloc(sizeof(struct ccpp_field));

	len = strlen(name);

	f_index->fields[n]->name = malloc((len + 1) * sizeof(char));

	strncpy(f_index->fields[n]->name, name, len * sizeof(char));
	f_index->fields[n]->name[len] = '\0';
	f_index->fields[n]->n = n+1;
	f_index->sorted = 0;
	f_index->n++;

	return(n+1);
}


/**
 * Find the index number of a field.
 *
 * @param[in]    name     The field name to find the index array.
 * @param[inout] index    The index array.
 * @retval       > 0      The position in the index array of the
 *                        requested field.
 * @retval       -1       If there was an error.
 **/
int
ccpp_field_idx_find(const char *name, void **index)
{
	int ierr = 0;
	int n = 0;
	struct ccpp_field  *key = NULL;
	struct ccpp_field **res = NULL;

	struct ccpp_field_idx *f_index = (struct ccpp_field_idx *)(*index);

	if (f_index->sorted == 0) {
		ccpp_field_idx_sort(index);
	}

	key = malloc(sizeof(struct ccpp_field));
	n = strlen(name);
	key->name = malloc((n+1) * sizeof(char));
	strncpy(key->name, name, n);
	key->name[n] = '\0';

	res = bsearch(&key, f_index->fields, f_index->n,
		      sizeof(struct ccpp_field *), cmp);
	if (*res == NULL) {
		warnx("Unable to find in index: %s", name);
		return(-1);
	}

	free(key->name);
	free(key);

	return((*res)->n);
}

/**
 * Sort the index by calling qsort() and using cmp() as the
 * comparison function.
 *
 * @param[inout] index    The index array.

 * @retval       0        If there was no error.
 **/
int
ccpp_field_idx_sort(void **index)
{
	struct ccpp_field_idx *f_index = (struct ccpp_field_idx *)(*index);

	qsort(f_index->fields, f_index->n, sizeof(struct ccpp_field *), cmp);
	f_index->sorted = 1;

	return(EXIT_SUCCESS);
}

/**
 * Grow the index field array.
 *
 * @param[inout] index    The index array.
 * @retval       0        If there was no error.
 **/
int
ccpp_field_idx_grow(void **index)
{
	struct ccpp_field_idx *f_index = (struct ccpp_field_idx *)(*index);
	struct ccpp_field **new = NULL;
	int new_max = 0;

	new_max = f_index->max * CCPP_FIELD_IDX_GROW;

	new = realloc(f_index->fields, new_max * sizeof(struct ccpp_field *));
	if (new == NULL) {
		warnx("Unable to expand the field index array");
		return(EXIT_FAILURE);
	}
	f_index->fields = new;
	f_index->max = new_max;

	return(EXIT_SUCCESS);
}

/**
 * Get the maximum number of fields the index array can hold.
 *
 * @param[inout] index    The index array.
 * @retval       >= 0     The maximum number of fields.
 **/
int
ccpp_field_idx_max(void **index)
{
	struct ccpp_field_idx *f_index = (struct ccpp_field_idx *)(*index);

	assert(f_index->max > 0);

	return(f_index->max);

}

/**
 * @}
 **/
