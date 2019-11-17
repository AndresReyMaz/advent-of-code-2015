// Copyright 2019 Andrés Reynoso-Mazoy
#include <iostream>
#include <string>
#include "./md5.h"

const char KEY[] = "yzbqklnj";

// find_lowest finds the lowest number that, concatenated with the key, has an
// MD5 hash beginning with length zeroes.
int64_t find_lowest(std::string key, int length) {
  int64_t ans = 1;
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
