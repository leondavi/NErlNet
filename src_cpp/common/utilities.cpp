#include "utilities.h"

namespace nerlnet_utilities
{

std::vector<std::string> split_strings_by_comma(std::string &str) {
    std::vector<std::string> result;
    std::stringstream ss(str);
    std::string token;
    while (getline(ss, token, ','))
    {
        result.push_back(token);
    }

    return result;
}

// check if string is only an single integer number
bool is_integer_number(const std::string &input_str) {
    std::regex reg_num ("^\\d+$");
    if (std::regex_match(input_str,reg_num)) return true;
    return false;
}

// This function modifies the input string!
std::vector<std::string> matchRegex(std::string &input_str, std::regex re) {
    std::vector<std::string> result;
    std::smatch match;
    while (std::regex_search(input_str, match, re)) {
        result.push_back(match.str());
        input_str = match.suffix().str();
    }
    return result;
}

} // namespace nerlutils