////////////////////////////////////////////////////////////
//  horodateur source file 
//  implements timestamp functions
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 28/05/2025 - Security improvements
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xOsHorodateur.h"
#include "xOsMemory.h"
#include "xAssert.h"
#include <time.h>
#include <string.h>
#include <stdio.h>


static __thread char s_cTimeBuffer[XOS_HORODATEUR_BUFFER_SIZE];

// Forward declarations
static int validateTimeComponents(const struct tm *p_pttTimeComponent);
static int formatTimestamp(char *p_ptcBuffer, size_t p_iBufferSize, 
                                const struct tm *p_pttTimeComponent, long p_lMicroseconds);
static int getSystemTime(struct timespec *p_pttTimespec);

////////////////////////////////////////////////////////////
/// xHorodateurGetString 
////////////////////////////////////////////////////////////
const char* xHorodateurGetString(void)
{
    struct timespec l_tTimespec = {0};
    struct tm l_tTmResult = {0};
    
    int l_iRet = getSystemTime(&l_tTimespec);
    if (l_iRet != XOS_HORODATEUR_OK)
    {
        return NULL;
    }
    
    // Convert to local time in thread-safe manner
    struct tm *l_ptTmPtr = localtime_r(&l_tTimespec.tv_sec, &l_tTmResult);
    if (l_ptTmPtr == NULL)
    {
        return NULL;
    }
    
    X_ASSERT(l_ptTmPtr == &l_tTmResult);
    
    // Validate time components
    l_iRet = validateTimeComponents(&l_tTmResult);
    if (l_iRet != XOS_HORODATEUR_OK)
    {
        return NULL;
    }
    
    long l_lMicroseconds = l_tTimespec.tv_nsec / 1000L;
    
    l_iRet = formatTimestamp(s_cTimeBuffer, sizeof(s_cTimeBuffer), 
                                  &l_tTmResult, l_lMicroseconds);
    if (l_iRet != XOS_HORODATEUR_OK)
    {
        return NULL;
    }
    
    return s_cTimeBuffer;
}

////////////////////////////////////////////////////////////
/// getSystemTime 
////////////////////////////////////////////////////////////
static int getSystemTime(struct timespec *p_pttTimespec)
{
    X_ASSERT(p_pttTimespec != NULL);
    
    if (p_pttTimespec == NULL)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    // Clear structure first
    XOS_MEMORY_SANITIZE(p_pttTimespec, sizeof(*p_pttTimespec));
    
    int l_iClockRet = clock_gettime(CLOCK_REALTIME, p_pttTimespec);
    if (l_iClockRet != 0)
    {
        return XOS_HORODATEUR_ERROR;
    }
    
    X_ASSERT(p_pttTimespec->tv_sec > 0);
    X_ASSERT(p_pttTimespec->tv_nsec >= 0 && p_pttTimespec->tv_nsec < 1000000000L);
    
    return XOS_HORODATEUR_OK;
}

////////////////////////////////////////////////////////////
/// validateTimeComponents 
////////////////////////////////////////////////////////////
static int validateTimeComponents(const struct tm *p_pttTimeComponent)
{
    X_ASSERT(p_pttTimeComponent != NULL);
    
    if (p_pttTimeComponent == NULL)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    int l_iYear = p_pttTimeComponent->tm_year + 1900;
    int l_iMonth = p_pttTimeComponent->tm_mon + 1;
    int l_iDay = p_pttTimeComponent->tm_mday;
    int l_iHour = p_pttTimeComponent->tm_hour;
    int l_iMin = p_pttTimeComponent->tm_min;
    int l_iSec = p_pttTimeComponent->tm_sec;
    
    // Validate ranges
    if (l_iYear < XOS_HORODATEUR_MIN_YEAR || l_iYear > XOS_HORODATEUR_MAX_YEAR)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    if (l_iMonth < 1 || l_iMonth > 12)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    if (l_iDay < 1 || l_iDay > 31)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    if (l_iHour < 0 || l_iHour > 23)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    if (l_iMin < 0 || l_iMin > 59)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    if (l_iSec < 0 || l_iSec > 60)  // 60 for leap seconds
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    return XOS_HORODATEUR_OK;
}

////////////////////////////////////////////////////////////
/// formatTimestamp 
////////////////////////////////////////////////////////////
static int formatTimestamp(char *p_ptcBuffer, size_t p_iBufferSize, 
                                const struct tm *p_pttTimeComponent, long p_lMicroseconds)
{   
    X_ASSERT(p_ptcBuffer != NULL);
    X_ASSERT(p_pttTimeComponent != NULL);
    X_ASSERT(p_iBufferSize > 0);
    
    if (p_ptcBuffer == NULL || p_pttTimeComponent == NULL || p_iBufferSize == 0)
    {
        return XOS_HORODATEUR_INVALID;
    }
    
    // Clear buffer first
    XOS_MEMORY_SANITIZE(p_ptcBuffer, p_iBufferSize);
    
    int l_iYear = p_pttTimeComponent->tm_year + 1900;
    int l_iMonth = p_pttTimeComponent->tm_mon + 1;
    int l_iDay = p_pttTimeComponent->tm_mday;
    int l_iHour = p_pttTimeComponent->tm_hour;
    int l_iMin = p_pttTimeComponent->tm_min;
    int l_iSec = p_pttTimeComponent->tm_sec;
    
    // Validate microseconds
    if (p_lMicroseconds < 0 || p_lMicroseconds >= 1000000L)
    {
        p_lMicroseconds = 0;  // Fallback to 0
    }
    
    // Format timestamp: YYYY-MM-DD HH:MM:SS.microseconds
    int l_iWritten = snprintf(p_ptcBuffer, p_iBufferSize,
                             "%04d-%02d-%02d %02d:%02d:%02d.%06ld",
                             l_iYear, l_iMonth, l_iDay,
                             l_iHour, l_iMin, l_iSec,
                             p_lMicroseconds);
    
    if (l_iWritten < 0 || (size_t)l_iWritten >= p_iBufferSize)
    {
        // Clear buffer on error
        XOS_MEMORY_SANITIZE(p_ptcBuffer, p_iBufferSize);
        return XOS_HORODATEUR_ERROR;
    }
    
    X_ASSERT(l_iWritten == 26);  // Expected: "YYYY-MM-DD HH:MM:SS.microseconds"
    
    return XOS_HORODATEUR_OK;
}

////////////////////////////////////////////////////////////
/// xHorodateurGet 
////////////////////////////////////////////////////////////
uint32_t xHorodateurGet(void)
{
    struct timespec l_tTimespec = {0};
    
    int l_iRet = getSystemTime(&l_tTimespec);
    if (l_iRet != XOS_HORODATEUR_OK)
    {
        X_ASSERT(0);
        return 0;
    }
    
    X_ASSERT(l_tTimespec.tv_sec > 0);
    
    time_t l_tSeconds = l_tTimespec.tv_sec;
    
    // Validate range for uint32_t
    if (l_tSeconds < 0 || l_tSeconds > UINT32_MAX)
    {
        X_ASSERT(0);  
        return 0;
    }
    
    return (uint32_t)l_tSeconds;
}

////////////////////////////////////////////////////////////
/// xHorodateurGetMicroseconds 
////////////////////////////////////////////////////////////
uint64_t xHorodateurGetMicroseconds(void)
{
    struct timespec l_tTimespec = {0};
    
    int l_iRet = getSystemTime(&l_tTimespec);
    if (l_iRet != XOS_HORODATEUR_OK)
    {
        X_ASSERT(0);
        return 0;
    }
    
    X_ASSERT(l_tTimespec.tv_sec >= 0);
    X_ASSERT(l_tTimespec.tv_nsec >= 0 && l_tTimespec.tv_nsec < 1000000000L);
    
    uint64_t l_ullSeconds = (uint64_t)l_tTimespec.tv_sec;
    uint64_t l_ullNanoseconds = (uint64_t)l_tTimespec.tv_nsec;
    
    // Convert to microseconds: seconds * 1,000,000 + nanoseconds / 1,000
    uint64_t l_ullMicroseconds = (l_ullSeconds * 1000000ULL) + (l_ullNanoseconds / 1000ULL);
    
    X_ASSERT(l_ullMicroseconds > 0);
    
    return l_ullMicroseconds;
}
