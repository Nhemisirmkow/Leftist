#include <fstream>
#include <vector>
#include <random>
#include <queue>

constexpr size_t max_size = 5300300;

int main() {
  std::random_device rd;
  std::mt19937 gen(rd());
  std::discrete_distribution<> dist({5000, 10, 1});
  std::uniform_int_distribution<int32_t> random(-100000, 100000);
  std::priority_queue<int32_t, std::vector<int32_t>, std::greater<int32_t>> queue;
  std::ofstream input("test.in"), output("test.out");
  input << "is_empty\n";
  output << "TAK\n";
  while (queue.size() < max_size) {
    auto command = dist(gen);
    if(command == 2) {
      input << "is_empty\n";
	  output << (queue.empty() == true ? "TAK\n" : "NIE\n");
      continue;
     }
    if(command == 1 && queue.empty() == false) {
      input << "delete_min\n";
	  output << queue.top() << ".\n";
	  queue.pop();
      continue;
     }
    auto number = random(gen);
    input << "add\n" << number << ".0\n";
	queue.push(number);
   }
  while(queue.empty() == false) {
    input << "delete_min\n";
    output << queue.top() << ".\n";
    queue.pop();
   }
  input << "exit\n";
  output << "Correct exit.\n\n";
 }
