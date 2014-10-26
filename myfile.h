#ifndef EXAMPLE_H
#define EXAMPLE_H

#define DATA_MAX_NAME_LEN 64

struct bar_s
{
	char   name[DATA_MAX_NAME_LEN];
	int    type;
	double min;
	double max;
};

typedef struct bar_s bar_t;

struct foo_s
{
	char           name[DATA_MAX_NAME_LEN];
	int            bar_num;
	bar_t          *bar;
};
typedef struct foo_s foo_t;

#endif /* EXAMPLE_H */
