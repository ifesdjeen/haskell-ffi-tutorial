#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "myfile.h"
#include "HsFFI.h"

/* #ifdef __GLASGOW_HASKELL__ */
#include "Example_stub.h"
/* #endif */


void
example_init (void)
{
  hs_init (NULL, NULL);
}

void
example_exit (void)
{
  hs_exit ();
}

int main( int argc, char *argv[] )
{
  int bar_count = 3;
  foo_t *foo;
  foo = malloc(sizeof(*foo));
  memset(foo, 0, sizeof(*foo));

  char foo_name[] = "foo name";
  strncpy(foo->name, foo_name, sizeof(foo_name));
  foo->bar_num = bar_count;
  foo->bar = (bar_t *) malloc (bar_count * sizeof (bar_t));

  char first_foo_name[] = "first foo name";
  strncpy(foo->bar[0].name, first_foo_name, sizeof(first_foo_name));
  foo->bar[0].type = 1;
  foo->bar[0].min  = 0.1;
  foo->bar[0].max  = 100.0;

  char second_foo_name[] = "second foo name";
  strncpy(foo->bar[1].name, second_foo_name, sizeof(second_foo_name));
  foo->bar[1].type = 2;
  foo->bar[1].min  = 0.2;
  foo->bar[1].max  = 200.0;

  char third_foo_name[] = "third foo name";
  strncpy(foo->bar[3].name, third_foo_name, sizeof(third_foo_name));
  foo->bar[2].type = 3;
  foo->bar[2].min  = 0.3;
  foo->bar[2].max  = 300.0;

  hs_init (&argc, &argv);
  entrypoint(foo);
}
