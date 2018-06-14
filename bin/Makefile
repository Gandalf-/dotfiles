all: d devbot_list

d: d.c
	gcc -std=c99 -Wall -Wextra -o d d.c

devbot_list: json.hpp devbot.cpp
	clang++ -Wall -Wextra -std=c++11 devbot.cpp -o devbot_list

json.hpp:
	wget --quiet https://github.com/nlohmann/json/releases/download/v3.1.2/json.hpp

clean:
	@rm *.o d
	@echo Done
