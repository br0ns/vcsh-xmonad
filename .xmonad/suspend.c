#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* 1) `slock` may have to run as the current user and `pm-suspend` must run as
 * root.  2) `pm-suspend` must not run before `slock` has locked the screen.
 * This presents an issue.  `slock` can run a command after the screen is
 * locked, but since we have dropped privs it cannot be `pm-suspend`.  The
 * workaround used here is to touch a file and wait for the file_handleto appear
 * before running `pm-suspend`.  Is there a better way? */

#define LOCKED_FILE "/tmp/locked"

int main (void) {
  char *argv1[] = {"slock", "touch", LOCKED_FILE, NULL};
  char *argv2[] = {"pm-suspend", NULL};

  switch (fork()) {
  case -1:
    return EXIT_FAILURE;
  case 0:
    break;
  default:
    /* The X server may only authorize the current user. */
    seteuid(getuid());
    setegid(getgid());
    execvp(argv1[0], argv1);
    _exit(0);
  }

  for (int i = 0; i < 100; i++) {
    if (0 == access(LOCKED_FILE, F_OK)) {
      unlink(LOCKED_FILE);
      /* Need to be root to do this */
      setuid(0);
      setgid(0);
      execvp(argv2[0], argv2);
      _exit(0);
    }
    /* Wait for a while, then go look again. */
    usleep(100000);
  }

  return EXIT_SUCCESS;
}
