/**
 * @file ccpp_ipd.h
 *
 * IPD physics call function mapping header.
 *
 * @ingroup IPD
 * @{
 **/
#ifndef CCPP_IPD_H
#define CCPP_IPD_H

#ifdef __cplusplus
extern "C"
{
#endif

/** IPD libaray and cap function initialization routine. **/
int ccpp_ipd_open(const char *, const char *, const char *, void **, void **);

/** IPD library finalization routine. **/
int ccpp_ipd_close(void **);

/** IPD physics cap function call. **/
int ccpp_ipd_cap(void **, void **);

#ifdef __cplusplus
}                               /* extern "C" */
#endif

#endif                          /* CCPP_IPD_PHY_H */

/**
 * @}
 **/
