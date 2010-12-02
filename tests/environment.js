load("../environment.js");

EnvironmentModule(this);

function test(pair) {
  var actual = pair[0];
  var expect = pair[1];
  var result = actual === expect;
  if (result) {
    print(".");
  } else {
    print("F");
    print("\nExpected " + expect +
          " but actual value is " + actual + "\n");
  }
  return result;
}

function test_array(arr) {
  var result = arr.every(test);
  if (result) {
    print("\nAll Tests Passed!\n");
  } else {
    print("\nSome tests failed.\n");
  }

  quit(!result);
}

test_array(
  [[a0(1), 1],
   [a0(1,2), 3],
   [a0(1,2,3,4), 10],
   [a0(), 0],

   [_0(1), -1],
   [_0(1, 3), -2],
   [_0(1, 3, 5), -7],

   [m0(1), 1],
   [m0(1,2), 2],
   [m0(1,4,5), 20],
   [m0(), 1],

   [d0(2), 1/2],
   [d0(2,3), 2/3],
   [d0(2,3,4,5), 2/3/4/5]]);