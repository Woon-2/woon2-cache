#ifndef __random_object
#define __random_object

#include <random>

std::random_device rd;
std::mt19937 re( rd() );

#endif  // __random_object