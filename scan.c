#include<stdio.h>
#include<stdint.h>
#include<immintrin.h>

// Scans for a semi-colon; if none found, it returns -1, else the position
static int scan(const char* str, int n) {

    __m128i* pSrc1 = (__m128i *) str;           // init pointer to start of string
    const __m128i m0 = _mm_set1_epi8(';');      // vector of 16 `;` characters

    int i = 0;
    int ub = n - (n % 16);
    while(i < ub)
    {
        __m128i v0 = _mm_loadu_si128( pSrc1 );        // get 16 chars from string
        __m128i v1 = _mm_cmpeq_epi8(v0, m0);        // compare all 16 chars with ';'
        int vmask = _mm_movemask_epi8( v1 );          // get 16 comparison result bits
        if (vmask != 0) {                          // if any bit is 1
            int k = __builtin_ctzll(vmask);         // count trailing zeros
            return i + k;
        }
        pSrc1++;                                // next 16 characters...
        i += sizeof(__m128i);
    }

    // remainder loop
    while(i < n) {
        if (str[i] == ';')
            return i;
        i++;
    }
    return -1;
}

#if __GNUC__ > 7
typedef size_t fortran_charlen_t;
#else
typedef int fortran_charlen_t;
#endif

// Fortran wrapper
int scan_(char* str, fortran_charlen_t n) {
    return scan(str,n) + 1;
}

/*
int main(void) {

    int n;

    const char* r1 = "abc;";
    n = scan(r1,4);
    printf("n = %d, expected = %d\n",n, 3);

    const char* r2 = "abcsahfsjk;";
    n = scan(r2,11);
    printf("n = %d, expected = %d\n",n, 10);

    const char* r3 = "abcsahfsjka";
    n = scan(r3,11);
    printf("n = %d, expected = %d\n", n, -1);

    const char* r4 = "abcsahfsabcsahfsabcsahfsabcsah;s";
    n = scan(r4,32);
    printf("n = %d, expected = %d\n", n, 30);

    return 0;
}
*/