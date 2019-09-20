#include <iostream>
#include <string>
#include "md5.h"
using namespace std;

const std::string KEY = "yzbqklnj";

// find_lowest finds the lowest number that, concatenated with the key, has an
// MD5 hash beginning with length zeroes.
long find_lowest(std::string key, int length) {
  long ans = 1;
  while (true) {
    std::string hash = md5(key + std::to_string(ans));
    if (hash.substr(0, length) == std::string(length, '0')) {
      return ans;
    }
    ans++;
  }
}

int main() {
  std::cout << "Part 1: \n" << find_lowest(KEY, 5) << std::endl;
  // Warning: Part 2 is slow (~10s).
  std::cout << "Part 2: \n" << find_lowest(KEY, 6) << std::endl;
}