#include <iostream>
#include <vector>
using namespace std;

vector<char> readInput() {
  string x;
  cin >> x;
  vector<char> ans(x.begin(), x.end());
  return ans;
}

void printSeq(const vector<char>& seq) {
  for (auto x : seq)
    cout << x;
  cout << endl;
}

vector<char> generate(const vector<char>& seq) {
  vector<char> ans;
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

int getSizeAfterNGenerations(vector<char> seq, int N) {
  for (int i = 0; i < N; ++i) {
    seq = generate(seq);
  }
  return seq.size();
}

int main() {
  auto x = readInput();
  cout << "Part 1: " << getSizeAfterNGenerations(x, 40) << endl;
  cout << "Part 2: " << getSizeAfterNGenerations(x, 50) << endl;
}