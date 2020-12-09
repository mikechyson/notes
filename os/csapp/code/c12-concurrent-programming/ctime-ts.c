/* thread safe wrapper function for the C standard library ctime function */
char *ctime_ts(const time_t *timep, char *privatep)
{
    char *sharedp;

    P(&mutex);
    sharedp = ctime(timep);
    strcpy(privatep, sharedp);	/* copy string from shared to private */
    V(&mutex);
    return privatep;
}
