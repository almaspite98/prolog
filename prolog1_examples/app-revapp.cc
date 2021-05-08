#define NULL 0

struct lnk
{
  char elem;
  lnk *next;
  lnk(char e): elem(e) {}
};

typedef lnk *list;

list append(list list1, list list2)
{ list list3, *lp = &list3;
  for (list pos = list1; pos; pos = pos->next) {
    list new_list = new lnk(pos->elem);
    *lp = new_list; lp = &new_list->next;
  }
  *lp = list2;
  return list3;
}

list revapp(list list1, list list2)
{ list list3 = list2;
  for (list pos = list1; pos; pos = pos->next) {
    list new_list = new lnk(pos->elem);
    new_list->next = list3; list3 = new_list;
  }
  return list3;
}

#include <iostream>
using namespace std;

ostream &operator<<(ostream &o, lnk const *list)
{
  for (; list; list = list->next) o << list->elem;
  return o;
}

list read(istream &i)
{
  char c;
  list lst, *lstp = &lst;
  while ((c = i.get()) != '\n') 
    *lstp = new lnk(c), lstp = &((*lstp)->next);
  *lstp = NULL;
  return lst;
}

int main()
{
  cout << "Please enter two newline-terminated strings.\n";

  list list1 = read(cin), list2 = read(cin);

  cout << "\n<list1> = " << list1
       << "\n<list2> = " << list2 << '\n';

  cout << "\n<list1><list2>           = " << list1 << list2 
       << "\nappend(<list1>, <list2>) = " << append(list1, list2)
       << "\nrevapp(<list1>, <list2>) = " << revapp(list1, list2) << '\n';
  
  return 0;
}
