class Node:
    def __init__(self, val=None, rnext=None):
        self.val = val
        self.rnext = rnext  # reference next

    def __str__(self):
        return str(self.val)


class MyLinkedList:

    def __init__(self):
        """
        Initialize your data structure here.
        """
        self.head = None
        self.length = 0

    def get(self, index: int) -> int:
        """
        Get the value of the index-th node in the linked list.
        If the index is invalid, return -1.
        """
        if self.length == 0 or index >= self.length:
            return -1

        node = self.head
        for i in range(index):
            node = node.rnext
        return node.val

    def addAtHead(self, val: int) -> None:
        """
        Add a node of value val before the first element of the linked list.
        After the insertion, the new node will be the first node of the linked list.
        """
        newNode = Node(val)
        newNode.rnext = self.head
        self.head = newNode
        self.length += 1

    def addAtTail(self, val: int) -> None:
        """
        Append a node of value val to the last element of the linked list.
        """
        newNode = Node(val)
        if self.head is None:
            self.head = newNode
            self.length += 1
            return
        rlast = self.head
        while rlast.rnext:
            rlast = rlast.rnext
        rlast.rnext = newNode
        self.length += 1

    def addAtIndex(self, index: int, val: int) -> None:
        """
        Add a node of value val before the index-th node in the linked list.
        If index equals to the length of linked list, the node will be appended to the end of linked list.
        If index is greater than the length, the node will not be inserted.
        """
        if index > self.length:
            return
        elif index == self.length:
            self.addAtTail(val)
        elif index == 0:
            self.addAtHead(val)
        else:
            newNode = Node(val)

            # locate
            node = self.head
            for i in range(1, index):
                node = node.rnext

            newNode.rnext = node.rnext
            node.rnext = newNode
            self.length += 1

    def deleteAtIndex(self, index: int) -> None:
        """
        Delete the index-th node in the linked list, if the index is valid.
        """
        if index >= self.length:
            return
        if index == 0:
            self.head = self.head.rnext
            self.length -= 1
            return

        node = self.head
        for i in range(1, index):
            node = node.rnext
        if node.rnext:
            node.rnext = node.rnext.rnext

        self.length -= 1

    def __str__(self):
        lst = []
        node = self.head
        lst.append(node.val)
        while node.rnext:
            node = node.rnext
            lst.append(node.val)
        return str(lst)


# Your MyLinkedList object will be instantiated and called as such:
# obj = MyLinkedList()
# param_1 = obj.get(index)
# obj.addAtHead(val)
# obj.addAtTail(val)
# obj.addAtIndex(index,val)
# obj.deleteAtIndex(index)

if __name__ == '__main__':
    obj = MyLinkedList()

    # param_1 = obj.get(0)
    # print(param_1)
    #
    # obj.addAtHead(2)
    # obj.addAtTail(10)
    # obj.addAtHead(1)
    # obj.addAtTail(100)
    # print(obj)
    #
    # print(obj.get(0))
    # print(obj.get(1))
    # print(obj.get(3))
    # print(obj.get(4))
    #
    # print(obj)
    #
    # obj.addAtIndex(0, 0)
    # print(obj)
    #
    # obj.addAtIndex(2, 2000)
    # print(obj)
    # obj.deleteAtIndex(5)
    # print(obj)
    # obj.deleteAtIndex(1)
    # print(obj)
    # obj.deleteAtIndex(2)
    # print(obj)

    # obj.addAtTail(1)
    # print(obj.get(0))

    obj.addAtHead(1)
    obj.addAtTail(3)
    obj.addAtIndex(1, 2)
    obj.get(1)
    obj.deleteAtIndex(1)
    obj.get(1)
