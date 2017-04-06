/**
 * A test to make sure the field array is growable.
 **/

#include <stdio.h>
#include <stdlib.h>

#include "ccpp_fields_idx.h"

int
main(int argc, char **argv)
{
	int i = 0;
	int n = 100;
	char f[10] = {0};
	void *cdata = NULL;

	if (ccpp_field_idx_init(&cdata)) {
		return(EXIT_FAILURE);
	}

	for (i = 0; i < n; ++i) {
		sprintf(f, "f_%d", i);
		if (ccpp_field_idx_add(f, &cdata) <= 0) {
			return(EXIT_FAILURE);
		}
	}

	i = ccpp_field_idx_find("f_90", &cdata);
	printf("%d\n", i);

	if (ccpp_field_idx_fini(&cdata)) {
		return(EXIT_FAILURE);
	}

	return(EXIT_SUCCESS);
}
