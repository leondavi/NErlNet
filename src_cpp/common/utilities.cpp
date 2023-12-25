#include "utilities.h"

namespace nerlnet_utilities
{

std::vector<std::string> split_strings_by_comma(std::string str) {
    std::vector<std::string> result;
    std::stringstream ss(str);
    std::string token;
    while (getline(ss, token, ','))
    {
        result.push_back(token);
    }

    return result;
}

std::vector<std::string> matchRegex(std::string input, std::regex re) {
    std::vector<std::string> result;
    std::smatch match;
    while (std::regex_search(input, match, re)) {
        result.push_back(match.str());
        input = match.suffix().str();
    }
    return result;
}

} // namespace nerlutils