all: d devbot_status

CFLAGS  = -Wall -Wextra


devbot_status:
	cp status.sh $@

d: d.c
	gcc -std=c99 ${CFLAGS} d.c -o $@

clean:
	-rm -f d devbot_status
	@echo Done
