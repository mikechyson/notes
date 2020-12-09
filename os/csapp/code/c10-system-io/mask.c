#define DEF_MODE S_IRUSER|S_IWUSER|S_IRGPP|S_IWGPP|S_IROTH|S_IWOTH
#define DEF_UMASK S_IWGPP|S_IWOTH

umask(DEF_UMASK);
fd = open("foo.txt", O_CREATE|O_TRUNC|O_WRONLY, DEF_MODE);
