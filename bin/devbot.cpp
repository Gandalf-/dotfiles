#include <ctime>
#include <string>

#include "colors.hpp"
#include "apocrypha.hpp"

std::string translate_time(int& next) {

    const int seconds_in_minute = 60;
    const int seconds_in_hour = 60 * 60;
    const int seconds_in_day = 60 * 60 * 24;
    const int seconds_in_week = 60 * 60 * 24 * 7;

    std::string interval = "second";

    if (next > seconds_in_week) {
        next /= seconds_in_week;
        interval = "week";

    } else if (next > seconds_in_day) {
        next /= seconds_in_day;
        interval = "day";

    } else if (next > seconds_in_hour) {
        next /= seconds_in_hour;
        interval = "hour";

    } else if (next > seconds_in_minute) {
        next /= seconds_in_minute;
        interval = "minute";
    }

    if (next != 1) {
        interval += "s";
    }

    return interval;
}


/* get the string representation of the actions for this name
 */
std::string get_action(const json& events, const std::string& name) {

    std::string action{};
    json action_j = events[name]["action"];

    if (action_j.is_array()) {
        for (auto& e : action_j) {
            action += "\t" + e.get<std::string>() + "\n";
        }
    } else {
        action = "\t" + action_j.get<std::string>() + "\n";
    }

    return action;
}


/* show the same information as wizard devbot list
 *  name of command
 *  action items
 *  how often it runs, when it runs next
 *  how long the last run took
 */
void devbot_list(void) {
    Client client;

    std::time_t now = std::time(0);
    json data = client.get( {"devbot", "data"} );
    json events = client.get( {"devbot", "events"} );

    for (json::iterator it = data.begin(); it != data.end(); ++it) {

        std::string name = it.key();
        json& values = (*it);

        int when = std::stoi( values["when"].get<std::string>() );
        int next = when - now;

        std::string interval = translate_time(next);
        std::string action = get_action(events, name);

        std::string frequency;
        auto frequency_ = events[name]["interval"];
        if (frequency_.is_number()) {
            int t = frequency_.get<int>();
            frequency = translate_time(t);
            frequency = "every " + std::to_string(t) + " " + frequency;

        } else {
            frequency = frequency_;
        }

        std::string duration = "unknown";
        if (values.find("duration") != values.end()) {

            duration = values["duration"].get<std::string>();
            duration += " second";

            if (std::stoi(duration) != 1) {
                duration += "s";
            }
        }

        std::cout
            << Color::green
            << name
            << Color::def << std::endl

            << Color::blue
            << action
            << Color::def // << std::endl

            << "\t" << Color::cyan
            << frequency
            << Color::def << ", " << Color::yellow
            << "next in " << next << " " << interval
            << Color::def << std::endl;

        if (std::stoi(duration) > 0) {
            std::cout
                << Color::dark_gray
                << "\tlast run took " << duration
                << Color::def << std::endl;
        }

        std::cout << std::endl;
    }
}

int main() {

    devbot_list();
    return 0;
}
