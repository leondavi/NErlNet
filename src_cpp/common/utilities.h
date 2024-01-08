#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <regex>
#include <cassert>
#include <memory>

namespace nerlnet_utilities
{

template<typename T>
bool shared_ptr_uninitialized(std::shared_ptr<T> &in_ptr) {return !in_ptr;};
std::vector<std::string> split_strings_by_comma(std::string &str);
bool is_integer_number(const std::string &input_str);
std::vector<std::string> matchRegex(std::string &input, std::regex re);

} // namespace nerlutils