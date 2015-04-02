#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

int main () {
  char *args1[] = {"xscreensaver-command", "-lock", NULL};
  char *args2[] = {"pm-suspend", NULL};
  setuid(0);
  setgid(0);

  switch (fork() == 0) {
  case -1:
    break;
  case 0:
    /* wait until screen is locked */
    wait(NULL);
    /* a hack */
    usleep(2000000);
    break;
  default:
    execvp(args1[0], args1);
    _exit(0);
  }

  if (fork() == 0) {
    execvp(args2[0], args2);
  }
}
