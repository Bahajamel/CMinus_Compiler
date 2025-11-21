int x;
int y;
int i; 
int add(int a, int b) { return a + b; }
int main() {
    x = 1;
    y = 3;

    x = add(x, y);
     {
        int z;
        z = x + y;
        y = z;
    }
     if (x > y) {
        x = x - 1;
    } else {
        y = y - 1;
    }
    while (x < 10) {
        x = x + 1;
    }
     do {
        y = y - 1;
    } while (y > 0);

   for (i = 0; i < 5; i = i + 1) {
        x = x + i;
    }
    int * p;
    

    
    return 0;
}

