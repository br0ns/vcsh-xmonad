#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main () {
  char *args1[] = {"slock", NULL};
  char *args2[] = {"pm-suspend", NULL};

  switch (fork()) {
  case -1:
    break;
  case 0:
    /* wait a short while to make sure the screen is locked */
    usleep(1000000);
    break;
  default:
    execvp(args1[0], args1);
    _exit(0);
  }

  if (fork() == 0) {
    setuid(0);
    setgid(0);
    execvp(args2[0], args2);
  }
}
