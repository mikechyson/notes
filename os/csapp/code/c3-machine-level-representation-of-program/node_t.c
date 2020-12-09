typedef enum { N_LEAF, N_INTERNAL } nodetype_t;

struct node_t {
  nodetype_t type;
  union {
    struct {
      struct node_t *left;
      struct node_t *right;
    } internal;
    double data[2];
  } info;
};
