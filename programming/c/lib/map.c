typedef struct {
  int key;
  int value;
} DataType;

typedef struct {
  DataType data;
  struct HashNode *next;
} HashNode;

typedef struct {
  int size;
  HashNode *table;
} HashMap, *hashmap;
