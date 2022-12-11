int counter() {
  static int i;
  static int j = 1+1;
  return i++ + j++;
}