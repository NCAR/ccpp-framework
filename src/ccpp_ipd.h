/*
 * This work (Common Community Physics Package), identified by NOAA, NCAR,
 * CU/CIRES, is free of known copyright restrictions and is placed in the
 * public domain.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

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
