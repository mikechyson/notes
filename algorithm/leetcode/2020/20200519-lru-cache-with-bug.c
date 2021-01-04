/* 
   Design and implement a data structure for Least Recently Used (LRU) cache. It should support the following operations: get and put.

   get(key) - Get the value (will always be positive) of the key if the key exists in the cache, otherwise return -1.
   put(key, value) - Set or insert the value if the key is not already present. When the cache reached its capacity, it should invalidate the least recently used item before inserting a new item.

   The cache is initialized with a positive capacity.

   Follow up:
   Could you do both operations in O(1) time complexity?

   Example:

   LRUCache cache = new LRUCache(2);

   cache.put(1, 1);
   cache.put(2, 2);
   cache.get(1);       // returns 1
   cache.put(3, 3);    // evicts key 2
   cache.get(2);       // returns -1 (not found)
   cache.put(4, 4);    // evicts key 1
   cache.get(1);       // returns -1 (not found)
   cache.get(3);       // returns 3
   cache.get(4);       // returns 4
*/

#include <stdlib.h>
#include <stdio.h>

typedef struct LRUEntry {
    int key;			
    int value;

    struct LRUEntry *hashPrev; /* hash previous */
    struct LRUEntry *hashNext; /* hash next */
    
    struct LRUEntry *listPrev; /* previous element in data list */
    struct LRUEntry *listNext; /* next element in data list */
} Entry;


typedef struct {
    int capacity;		/* capacity of the cache */
    int size;			/* num of current elements in the cache */
    
    Entry **hashMap;

    Entry *head;		/* head of doubled linked list */
    Entry *tail;		/* tail of doubled linked list */
} LRUCache;


int getHashKey(LRUCache *cache, int key);
Entry *newEntry(int key, int value);
Entry *findEntry(LRUCache *cache, int key);
void removeEntryFromLRU(LRUCache *cache, Entry *entry);
void addEntryInHead(LRUCache *cache, Entry *ptr);
void removeEntryFromHash(LRUCache *cache, Entry *entry);
void addEntryToHash(LRUCache *cache, Entry *entry);
void addEntryInHead(LRUCache *cache, Entry *entry);
void removeEndEntryFromLRU(LRUCache *cache);

/* Create a LRU cache. */
LRUCache* lRUCacheCreate(int capacity)
{
    LRUCache *cache = (LRUCache *) malloc(sizeof(LRUCache)); /* pointer to cache */

    cache->capacity = capacity;
    cache->size = 0;		/* at start, there is no elements */

    
    cache->head = cache->tail = NULL;
    cache->hashMap = (Entry **) malloc(sizeof(Entry) * capacity); /* array of pointers pointing to entries */

    for (int i = 0; i < capacity; i++) /* init */
	cache->hashMap[i] = NULL;
    
    return cache;
}
	

	    
/* Get the value of the given key. */
int lRUCacheGet(LRUCache* obj, int key) {
    if (NULL == obj)		/* no cache */
	return -1;

    Entry *entry = findEntry(obj, key); /* find the entry according to the given key */
    if (NULL == entry)			/* no entry found */
	return -1;

    Entry *ptr = newEntry(entry->key, entry->value); /* create a new entry with the same key and value with the found entry */
    removeEntryFromLRU(obj, entry);		     /* remove the found entry */
    addEntryInHead(obj, ptr);                        /* add the new entry to the head of the linked list */

    return ptr->value;
}

void lRUCachePut(LRUCache* obj, int key, int value) {
    if (NULL == obj)		/* no cache, do nothing */
	return;

    Entry *entry = findEntry(obj, key); /* search for the entry with the given key */
    if (NULL != entry)
	removeEntryFromLRU(obj, entry); /* if there is an entry, remove the original entry first */

    entry = newEntry(key, value); /* create new entry */
    if (obj->size + 1 > obj->capacity) /* then cache is full */
	removeEndEntryFromLRU(obj);    /* remove the most not recently entry */

    addEntryInHead(obj, entry);
}

void lRUCacheFree(LRUCache* obj) {
    
}

/**
 * Your LRUCache struct will be instantiated and called as such:
 * LRUCache* obj = lRUCacheCreate(capacity);
 * int param_1 = lRUCacheGet(obj, key);
 
 * lRUCachePut(obj, key, value);
 
 * lRUCacheFree(obj);
 */


/* get hash key */
int getHashKey(LRUCache *cache, int key)
{
    return key % cache->capacity;
}


/* Find entry with the given key. */
Entry *findEntry(LRUCache *cache, int key)
{
    printf("key: %d, hash key: %d\n", key, getHashKey(cache, key));
    Entry *listHead = cache->hashMap[getHashKey(cache, key)];

    if (NULL == listHead)	/* no entry found */
	return NULL;

    Entry *ptr = listHead;
    while (NULL != ptr) {
	if (ptr->key == key)	/* have found the entry */
	    return ptr;
	ptr = ptr->hashNext;	/* look for the next entry in the hash linked list */
    }

    return NULL;
}

/* Create a cache entry. */
Entry *newEntry(int key, int value)
{
    Entry *entry = (Entry *) malloc(sizeof(Entry));
    entry->key = key;
    entry->value = value;

    entry->listNext = entry->listPrev = NULL;
    entry->hashNext = entry->hashPrev = NULL;

    return entry;
}


/* Remove a entry from the LRU cache. */
void removeEntryFromLRU(LRUCache *cache, Entry *entry)
{
    if (NULL == cache || NULL == entry)
	return;

    removeEntryFromHash(cache, entry);

    if (NULL == entry->listPrev && NULL == entry->listNext) { /* the only entry in the cache */
	free(entry);
	entry = NULL;
	cache->head = cache->tail = NULL;
    } else if (NULL == entry->listPrev) { /* at the head of the cache */
	cache->head = entry->listNext;
	cache->head->listPrev = NULL;
	free(entry);
	entry = NULL;
    } else if (NULL == entry->listNext) { /* at the tail of the cache */
	cache->tail = entry->listPrev;
	cache->tail->listNext = NULL;
	free(entry);
	entry = NULL;
    } else {			/* at the middle of the cache */
	entry->listPrev->listNext = entry->listNext;
	entry->listNext->listPrev = entry->listPrev;
	free(entry);
	entry = NULL;
    }

    cache->size -= 1;

}


/* Add entry in head of the LRU cache. */
void addEntryInHead(LRUCache *cache, Entry *entry)
{
    if (NULL == cache || NULL == entry)
	return ;

    if (NULL == cache->head)
	cache->head = cache->tail = entry;
    else {
	cache->head->listPrev = entry;
	entry->listNext = cache->head;
	cache->head = entry;
    }

    cache->size += 1;
    addEntryToHash(cache, entry);
}

    
/* Remove a entry from hash map.  */
void removeEntryFromHash(LRUCache *cache, Entry *entry)
{
    if (NULL == entry)
	return;

    if (entry->hashPrev != NULL) /* not the head */
	entry->hashPrev->hashNext = entry->hashNext;
    else {
	int idx = getHashKey(cache, entry->key);
	cache->hashMap[idx] = entry->hashNext;
	if (NULL != entry->hashNext)
	    entry->hashNext->hashPrev = cache->hashMap[idx];
    }
}


void addEntryToHash(LRUCache *cache, Entry *entry)
{
    if (cache == NULL || entry == NULL)
	return;

    Entry *hashHead = cache->hashMap[getHashKey(cache, entry->key)];
    if (NULL == hashHead) { /* not exist key yet */
	cache->hashMap[getHashKey(cache, entry->key)] = entry;
	entry->hashPrev = cache->hashMap[getHashKey(cache, entry->key)]; /* the only element in the hash map */
    } else {
	entry->hashNext = hashHead; /* add to the linked list of the head */
	hashHead->hashPrev = entry;

	cache->hashMap[getHashKey(cache, entry->key)] = entry;
    }
    	
}


void removeEndEntryFromLRU(LRUCache *cache)
{
    if (NULL != cache)
	removeEntryFromLRU(cache, cache->tail);
}


void exploit(LRUCache *cache)
{
    printf("capacity: %d\t size: %d\n", cache->capacity, cache->size);
    printf("head: %d\n", (int)cache->head);
}

int main(void)
{
    
   LRUCache *cache = lRUCacheCreate(2);
   exploit(cache);

   lRUCachePut(cache, 1, 1);
   exploit(cache);

   lRUCachePut(cache, 2, 2);
   exploit(cache);

   printf("get(1): %d\n", lRUCacheGet(cache, 1));
   exploit(cache);
   
   lRUCachePut(cache, 3, 3);
   exploit(cache);
   
   printf("get(2): %d\n", lRUCacheGet(cache, 2));
   exploit(cache);
   
   lRUCachePut(cache, 4, 4);
   exploit(cache);
   
   printf("get(1): %d\n", lRUCacheGet(cache, 1));
   exploit(cache);
   
   printf("get(3): %d\n", lRUCacheGet(cache, 3));
   exploit(cache);

   printf("get(4): %d\n", lRUCacheGet(cache, 4)); /* todo: there is a bug */
   exploit(cache);   




}
