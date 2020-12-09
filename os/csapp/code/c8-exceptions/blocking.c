sigset_t mask, prev_mask;

Sigemptyset(&mask);
Sigaddset(&mask, SIGINT);

/* block sigint and save previous blocked set */
Sigprocmask(SIG_BLOCK, &mask, &prev_mask);

//code region that will not be interrupted by SIGINT

/* restore previous blocked set, unblocking sigint */
Sigprocmask(SIG_SETMASK, &prev_mask, NULL);
