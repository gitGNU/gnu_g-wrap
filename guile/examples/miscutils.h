#ifndef GW_MISCUTILS_H
#define GW_MISCUTILS_H

typedef struct {
    long long seconds;
    long int nanoseconds;
} Timespec64;

Timespec64 elapsed_time (Timespec64 start, Timespec64 finish);
char *     join_strings (const char *s1, const char *s2);
double     seconds_since_dow (unsigned int dow);

#endif
