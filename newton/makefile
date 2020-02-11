OBJS	= newton
CC	= gcc
CFLAGS	= -O3 -pthread -lm

.PHONY : clean

newton : newton.c
	$(CC) -o newton $(CFLAGS) newton.c

clean:
	rm -f $(OBJS)
