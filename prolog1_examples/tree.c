#include <stdio.h>

enum treetype {
    Node,
    Leaf
};

struct tree {
    enum treetype type;
    union {
        struct {
            struct tree *left;
            struct tree *right;
        } node;
        struct {
            int value;
        } leaf;
    } u;
};


int sum_tree(struct tree *tree)
{
    switch(tree->type) {
    case Leaf:
        return tree->u.leaf.value;
    case Node:
        return sum_tree(tree->u.node.left) + sum_tree(tree->u.node.right);
    }
}

int sum_tree2(struct tree *tree)
{
  int sum = 0; 

 repeat:
  switch(tree->type) {
  case Leaf:
    sum += tree->u.leaf.value;
    return sum;
  case Node:
    sum += sum_tree(tree->u.node.left);
    tree = tree->u.node.right;
    goto repeat;
  }
}


int main()
{
    struct tree n1, n2;
    struct tree l1, l2, l3;
    int sum;

    l1.type = l2.type = l3.type = Leaf;
    l1.u.leaf.value = 5;
    l2.u.leaf.value = 3;
    l3.u.leaf.value = 2;

    n1.type = n2.type = Node;
    n1.u.node.left = &l1;
    n1.u.node.right = &n2;
    n2.u.node.left = &l2;
    n2.u.node.right = &l3;
    
    sum = sum_tree(&n1);
    printf("sum is: %i\n", sum);
    
    return 0;
}
