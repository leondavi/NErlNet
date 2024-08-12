#pragma once

#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <regex>
#include <cassert>
#include <memory>
#include <climits>

namespace nerlnet_utilities
{

template<typename T>
bool shared_ptr_uninitialized(std::shared_ptr<T> &in_ptr) {return !in_ptr;};
std::vector<std::string> split_strings_by_comma(std::string &str);
bool is_integer_number(const std::string &input_str);
std::vector<std::string> matchRegex(std::string &input, std::regex re);

inline bool substr_in_string(const std::string &input, const std::string &substr)
{
    return input.find(substr) != std::string::npos;
}

inline bool is_big_endian(void)
{
    union {
        uint32_t i;
        char c[4];
    } bint = {0x01020304};

    return bint.c[0] == 1;
}

template <typename T>
T swap_endian(T u)
{
    static_assert (CHAR_BIT == 8, "CHAR_BIT != 8");

    union
    {
        T u;
        unsigned char u8[sizeof(T)];
    } source, dest;

    source.u = u;

    for (size_t k = 0; k < sizeof(T); k++)
        dest.u8[k] = source.u8[sizeof(T) - k - 1];

    return dest.u;
}

} // namespace nerlutils