#include <fstream>      // std::ifstream
#include <ctime>
#include <string>
#include <vector>
#include <cctype>
#include <cmath>
#include <iostream>

typedef int (*fn)( int );

int main () {

  std::vector<double> times;
  times.reserve(12);

  static fn funcs[] = {iscntrl, isprint,isspace,isblank,isgraph,ispunct,isalnum,isalpha,isupper,
    islower,isdigit,isxdigit};


  for (int exp = 3; exp < 9; ++exp) {
    
    int sz = std::pow(10,exp);
    
    std::string filename = "chars-" + std::to_string(sz) + ".txt";
    std::ifstream is(filename, std::ifstream::binary);
    
    std::vector<char> buffer(sz);
    
    if (is) {

      // read data as a block:
      is.read(buffer.data(),sz);

      for (int j = 0; j < 12; ++j) {
        double duration = 0;
        for (int reps = 0; reps < 10; ++reps) {
          clock_t start = clock();
          for (int n = 0; n < sz; ++n) {
            int res = funcs[j](buffer[n]);
          }
          clock_t end = clock();
          duration += (end - start);
        }
        times[j] = duration / ((double)CLOCKS_PER_SEC) / 10;
      }
    }

    std::cout << sz << ' ';
    for (int i = 0; i < 12; ++i)
      std::cout << times[i] << ' ';
    std::cout << '\n';
  }

  return 0;
}

