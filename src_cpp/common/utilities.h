#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <regex>
#include <cassert>

namespace nerlnet_utilities
{

std::vector<std::string> split_strings_by_comma(std::string str);
std::vector<std::string> matchRegex(std::string input, std::regex re);

} // namespace nerlutils