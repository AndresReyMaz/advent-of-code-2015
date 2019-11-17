// Copyright 2019 Andrés Reynoso-Mazoy
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#define ID 0
#define AND 1
#define OR 2
#define NOT 3
#define LSHIFT 4
#define RSHIFT 5
#define UNEVAL_ID 6


struct node {
  // The value that the numbers have.
  uint64_t value;
  // The operation number (ID means just take the value).
  int operation;
  // The left and right side of the op.
  std::string left, right;
};

uint64_t resolve(std::string target, std::map<std::string, node> *hashtable);

std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> result;
    std::stringstream ss(s);
    std::string item;

    while (getline(ss, item, delim)) {
        result.push_back(item);
    }

    return result;
}

std::map<std::string, node> process_info() {
  std::string line;
  std::map<std::string, node> hashtable;
  while (getline(std::cin, line)) {
    if (line.length() <= 1) break;
    auto splitLine = split(line, ' ');
    std::string outputWire = splitLine.back();
    if (splitLine[0] == "NOT") {
      node n;
      n.left = splitLine[1];
      n.right = "";
      n.operation = NOT;
      hashtable[outputWire] = n;
      continue;
    }
    if (splitLine.size() == 3) {
      node n;
      n.operation = UNEVAL_ID;
      n.left = splitLine[0];
      n.right = "";
      hashtable[outputWire] = n;
      continue;
    }
    node n;
    n.left = splitLine[0];
    n.right = splitLine[2];
    if (splitLine[1] == "AND") {
      n.operation = AND;
    } else if (splitLine[1] == "OR") {
      n.operation = OR;
    } else if (splitLine[1] == "LSHIFT") {
      n.operation = LSHIFT;
    } else if (splitLine[1] == "RSHIFT") {
      n.operation = RSHIFT;
    } else {
      std::cout << "Error: unknown instruction found: " << line << std::endl;
      exit(1);
    }
    hashtable[outputWire] = n;
  }
  return hashtable;
}

inline uint64_t resolveOrDigit(std::string wire,
                               std::map<std::string, node> *hashtable) {
  if (wire.length() == 0) {
    return 0;
  }
  if (isalpha(wire[0])) {
    return resolve(wire, hashtable);
  }
  // Else it's a digit and we can just convert.
  return std::stoi(wire);
}

// resolve gets the value of the target, figuring out any rules if necessary.
uint64_t resolve(std::string target, std::map<std::string, node> *hashtable) {
  if (hashtable->find(target) == hashtable->end()) {
    std::cout << "Error: wire " << target << " not found." << std::endl;
    exit(1);
  }
  node curNode = (*hashtable)[target];
  if (curNode.operation == ID) {
    return curNode.value;
  }
  uint64_t left = resolveOrDigit(curNode.left, hashtable);
  uint64_t right = resolveOrDigit(curNode.right, hashtable);
  switch (curNode.operation) {
    case AND:
      curNode.value = left & right;
      break;
    case OR:
      curNode.value = left | right;
      break;
    case NOT:
      curNode.value = ~left;
      break;
    case LSHIFT:
      curNode.value = left << right;
      break;
    case RSHIFT:
      curNode.value = left >> right;
      break;
    case UNEVAL_ID:
      curNode.value = left;
      break;
  }
  curNode.operation = ID;
  // Write back the curNode to the hashtable and return
  (*hashtable)[target] = curNode;
  return curNode.value;
}

uint64_t part1() {
  std::map<std::string, node> hashtable = process_info();
  return resolve("a", &hashtable);
}

uint64_t part2() {
  std::map<std::string, node> hashtable = process_info();
  hashtable["b"].operation = ID;
  hashtable["b"].value = 46065;
  return resolve("a", &hashtable);
}

int main() {
  // Uncomment one at a time, and redirect stdin.
  // std::cout << "Part 1:\n" << part1() << std::endl;
  std::cout << "Part 2:\n" << part2() << std::endl;
}
