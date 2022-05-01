#include <string>
#include <iostream>
using namespace std;

int main() {
	const char *s = "1235";
	string *str = new string(s);
	cout << *str << endl;
	return 0;
}