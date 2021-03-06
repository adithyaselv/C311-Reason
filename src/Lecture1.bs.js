// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';


function plus(a, b) {
  if (b !== 0) {
    return 1 + plus(a, b - 1 | 0) | 0;
  } else {
    return a;
  }
}

function star(a, b) {
  if (b !== 0) {
    return a + star(a, b - 1 | 0) | 0;
  } else {
    return 0;
  }
}

function exp(a, b) {
  if (b !== 0) {
    return Math.imul(a, exp(a, b - 1 | 0));
  } else {
    return 1;
  }
}

function sumList(l) {
  if (l) {
    return l.hd + sumList(l.tl) | 0;
  } else {
    return 0;
  }
}

function listLength(l) {
  if (l) {
    return 1 + listLength(l.tl) | 0;
  } else {
    return 0;
  }
}

console.log("Adding 5 + 6 = " + String(plus(5, 6)));

console.log("Multiplying 3 * 5 = " + String(star(3, 5)));

console.log("Exponential 3^5 = " + String(exp(3, 5)));

console.log("List sum [1, 2, 3, 4, 5] = " + String(sumList({
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: {
                      hd: 5,
                      tl: /* [] */0
                    }
                  }
                }
              }
            })));

console.log("Length of list [1, 2, 3, 4, 5] = " + String(listLength({
              hd: 1,
              tl: {
                hd: 2,
                tl: {
                  hd: 3,
                  tl: {
                    hd: 4,
                    tl: {
                      hd: 5,
                      tl: /* [] */0
                    }
                  }
                }
              }
            })));

exports.plus = plus;
exports.star = star;
exports.exp = exp;
exports.sumList = sumList;
exports.listLength = listLength;
/*  Not a pure module */
