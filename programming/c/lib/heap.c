#include <stdlib.h>
#include <limits.h>
#include <stdio.h>

typedef struct {
    int length;
    int size;
    int *array;
} Heap;

int parent(int i)
{
    return (i+1) / 2 - 1;
}

int left(int i)
{
    return 2 * i + 1;
}

int right(int i)
{
    return 2 * i + 2;
}

void exchange(int *array, int a, int b)
{
    int tmp;

    tmp = array[a];
    array[a] = array[b];
    array[b] = tmp;
}


void max_heapify(Heap *heap, int i)
{
    int largest;
    int l = left(i);
    int r = right(i);

    if (l <= heap->size-1 && heap->array[l] > heap->array[i])
	largest = l;
    else
	largest = i;

    if (r <= heap->size-1 && heap->array[r] > heap->array[largest])
	largest = r;

    if (largest != i) {
	exchange(heap->array, i, largest);
	max_heapify(heap, largest);
    }
}

void build_max_heap(Heap *heap)
{
    heap->size = heap->length;
    int i;
    for (i = heap->length/2-1; i >= 0; i--)
	max_heapify(heap, i);
}

  
int heap_maximum(Heap *heap)
{
    return heap->array[0];
}

int heap_extract_max(Heap *heap)
{
    int max;
  
    if (heap->size < 1) {
	printf("heap underflow");
	exit(1);
    }

    max = heap->array[0];
    heap->array[0] = heap->array[heap->size-1];
    heap->size -= 1;
    max_heapify(heap, 0);
    return max;
}

void heap_increase_key(Heap *heap, int i, int key)
{
    if (key < heap->array[i]) {
	printf("error: ney key: %d is smaller than current key: %d", key, heap->array[i]);
	exit(2);
    }

    heap->array[i] = key;

    while (i > 0 && heap->array[parent(i)] < heap->array[i]) {
	exchange(heap->array, i, parent(i));
	i = parent(i);
    }
}

void max_heap_insert(Heap *heap, int key)
{
    heap->size += 1;
    heap->array[heap->size-1] = INT_MIN;
    heap_increase_key(heap, heap->size-1, key);
}
      

int lastStoneWeight(int* stones, int stonesSize){
    int max1, max2;
    
    Heap *heap = (Heap *) malloc(sizeof(Heap));
    heap->length = stonesSize;
    heap->size = stonesSize;
    heap->array = stones;
    build_max_heap(heap);
    printf("heap size: %d\n", heap->size);
    
    while(heap->size >= 2) {
        max1 = heap_extract_max(heap);
        max2 = heap_extract_max(heap);

	if (max1 != max2)
            max_heap_insert(heap, max1 - max2);

	printf("max1: %d, max2: %d, heap size: %d\n", max1, max2, heap->size);
	int i;
	for (i = 0; i < heap->length; i++)
	    printf("%d", heap->array[i]);
	printf("\n");

    }

    printf("heap size: %d\n", heap->size);


    
    if (heap->size == 1)
        return heap->array[0];
    else 
        return 0;

}


int main(void)
{
    int array[] = {2, 7, 4, 1, 8, 1};
    int stonesSize = 6;
    int res = lastStoneWeight(array, stonesSize);
    printf("result: %d\n", res);
	
}
