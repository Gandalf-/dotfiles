all: d devbot_list

CFLAGS = -Wall -Wextra

devbot_list: json.hpp devbot.cpp devbot.hs Apocrypha/Network.hs
ifneq ("$(shell which ghc)","")
	ghc --make -Wall devbot.hs -o $@
else
	g++ -std=c++11 ${CFLAGS} devbot.cpp -o $@
endif

d: d.c d.hs Apocrypha/Network.hs
ifneq ("$(shell which ghc)","")
	ghc --make -Wall d.hs -o $@
else
	gcc -std=c99 ${CFLAGS} d.c -o $@
endif

json.hpp:
	wget --quiet https://github.com/nlohmann/json/releases/download/v3.1.2/json.hpp

clean:
	-rm -f *.o d devbot_list *.hi
	@echo Done
