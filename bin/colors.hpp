#include <ostream>

namespace Color {

    enum Code {
        black        = 30,
        blue         = 34,
        cyan         = 36,
        dark_gray    = 90,
        def          = 39,
        green        = 32,
        light_blue   = 94,
        light_cyan   = 96,
        light_gray   = 37,
        light_green  = 92,
        light_magenta = 95,
        light_red    = 91,
        light_yellow = 93,
        magenta      = 35,
        red          = 31,
        white        = 97,
        yellow       = 33,
    };

    std::ostream& operator<<(std::ostream& os, Code code) {
        return os << "\033[" << static_cast<int>(code) << "m";
    }
}
