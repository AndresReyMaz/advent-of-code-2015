// Copyright 2019 Andrés Reynoso-Mazoy
#include <iostream>
#include <vector>

std::vector<char> readInput() {
  std::string x;
  std::cin >> x;
  std::vector<char> ans(x.begin(), x.end());
  return ans;
}

void printSeq(const std::vector<char>& seq) {
  for (auto x : seq)
    std::cout << x;
  std::cout << std::endl;
}

std::vector<char> generate(const std::vector<char>& seq) {
  std::vector<char> ans;
  for (int i = 0; i < seq.size(); ++i) {
    int cnt = 1;
    while (i < seq.size() - 1 && seq[i] == seq[i+1]) {
      cnt++;
      i++;
    }
    ans.push_back(cnt + '0');
    ans.push_back(seq[i]);
  }
  return ans;
}

int getSizeAfterNGenerations(std::vector<char> seq, int N) {
  for (int i = 0; i < N; ++i) {
    seq = generate(seq);
  }
  return seq.size();
}

int main() {
  auto x = readInput();
  std::cout << "Part 1: " << getSizeAfterNGenerations(x, 40) << std::endl;
  std::cout << "Part 2: " << getSizeAfterNGenerations(x, 50) << std::endl;
}
