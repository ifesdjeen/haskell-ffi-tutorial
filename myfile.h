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

#define UNION_TYPE_STRING  0
#define UNION_TYPE_NUMBER  1
#define UNION_TYPE_BOOLEAN 2

struct weird_union_s
{
  union
  {
    char  *string;
    double number;
    int    boolean;
  } value;
  int type;
};

typedef struct weird_union_s weird_union_t;



#endif /* EXAMPLE_H */
