#include "Python.h"

/*
 *  $Id$
 *
 *  Spike of how to use the python string object.
 *
 */

void mock_Py_Initialize(void) {
  PyThreadState_Swap(PyThreadState_New(PyInterpreterState_New()));
}

char* str(PyObject *o){
  return PyString_AsString(PyObject_Str(o));
}


int main(void) {
  PyObject *s, *t;
  mock_Py_Initialize();
  s = PyString_FromString("hello, world!\n");
  t = PyList_New(1);
  PyList_SetItem(t, 0, s);
  printf(str(t));
  printf("\n");
  return(0);
}
