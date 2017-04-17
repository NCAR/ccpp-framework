/**
 * \file ccpp_utils.h
 *
 * CCPP utility functions.
 *
 * \ingroup CCPP
 * \{
 **/
#ifndef CCPP_UTILS_H
#define CCPP_UTILS_H

#ifdef __cplusplus
extern "C"
{
#endif

/** Resolves the absolute path when given a relative path. **/
int ccpp_abs_path(const char *, char **);

#ifdef __cplusplus
}                               /* extern "C" */
#endif

#endif                          /* CCPP_UTILS_H */

/**
 * \}
 **/
