#include <ctre.hpp>
#include <string_view>
#include <algorithm>
#include <numeric>
#include <cctype>

static constexpr std::string_view raw_input = R"(
    1-3 a: abcde
    1-3 b: cdefg
    2-9 c: ccccccccc
)";

struct parse_result {
    int fst;
    int snd;
    char expected_char;
    std::string_view password;
};

static constexpr int int_from_char(const char ch) {
    return ch - '0';
}

static constexpr int to_int(std::string_view str) {
    return std::accumulate(
        std::cbegin(str), std::cend(str), 0,
        [](auto acc, auto current) { return (acc * 10) + int_from_char(current); }
    );
}

static constexpr std::string_view trim(const std::string_view str) {
    const auto not_space = [](auto c) { return c != ' ' && c != '\n' && c != '\t'; };
    const auto begin = std::find_if(std::cbegin(str), std::cend(str), not_space);
    const auto end = std::find_if(std::crbegin(str), std::crend(str), not_space);
    return std::string_view{begin, end.base()};
}

static constexpr parse_result parse_line(std::string_view line) {
        const auto match = ctre::match<"(\\d+)-(\\d+) ([a-z]): ([a-z]+)">(trim(line));

        const int fst = to_int(match.get<1>().view());
        const int snd = to_int(match.get<2>().view());
        const char expected_char = *match.get<3>().view().cbegin();
        const auto password = match.get<4>().view();

        return { fst, snd, expected_char, password };
}

static consteval int part1() {
    int valid_count = 0;

    const auto input = trim(raw_input);

    auto first = std::cbegin(input);
    const auto eof = std::cend(input);
    
    for (;;) {
        const auto eol = std::find(first, eof, '\n');
        const auto result = parse_line({first, eol});

        const int char_count = std::accumulate(
            std::cbegin(result.password),
            std::cend(result.password),
            0,
            [&](auto count, auto current) { return current == result.expected_char ? count + 1 : count; });
        
        if (char_count >= result.fst && char_count <= result.snd)
            ++valid_count;

        if (eol == eof) {
            break;
        }

        first = eol + 1;
    }

    return valid_count;
}

static consteval int part2() {
    int valid_count = 0;

    const auto input = trim(raw_input);

    auto first = std::cbegin(input);
    const auto eof = std::cend(input);
    
    for (;;) {
        const auto eol = std::find(first, eof, '\n');
        const auto result = parse_line({first, eol});

        const bool fst_matches = result.password[result.fst - 1] == result.expected_char;
        const bool snd_matches = result.password[result.snd - 1] == result.expected_char;
        
        if ((fst_matches ^ snd_matches))
            ++valid_count;

        if (eol == eof) break;

        first = eol + 1;
    }

    return valid_count;
}


int main() {
    //return part1();
    return part2();
}
