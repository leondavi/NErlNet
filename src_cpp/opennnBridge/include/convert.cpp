#include <iostream>
#include <eigen3/Eigen/Core>
#include "../../opennn/opennn/opennn.h"

//#include <unsupported/Eigen/CXX11/Tensor>

int main(){

// convert from Vector to Tensor start -------------------------------------------------------------------------------------------

Eigen::Tensor<Index,2> t(2,2) ;
t(0,0) = 1;
t(1,1) = 2;
t(0,1) = 3;
t(1,0) = 4;
Index i = t(1,1);
Eigen::Tensor<Index,1> t1(t.size());
for (int i =0 ; i< t.size(); i++){
    t1(i) = t(i,0);
}

//Eigen::Tensor<Index,1> t1 = t();
std::cout << "\n" ;
std::cout <<  t1(1);
std::cout << "\n" ;
std::cout << t1(0) ;
std::cout << "\n" ;
    return 0;
}
// convert from Vector to Tensor end -------------------------------------------------------------------------------------------



