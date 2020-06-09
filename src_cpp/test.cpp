/* test.cpp */
#include <vector>
#include <deque>

int foo(int x) {
  return x+1;
}

int bar(int y) {
  return y*2;
}



std::vector<int> square2(std::vector<int> listVec, std::vector<int> listVec2) {
  
    if(listVec.size() == listVec2.size()){
	    for(int i=0; i < listVec.size(); ++i) {
		listVec[i] = listVec[i]+listVec2[i];
	    } 
    }
    return listVec;
}

std::deque<int> push(std::deque<int> listDeq) {
	listDeq.push_back(10);
	listDeq.push_front(20);
	return listDeq;
}

