class Test {
    void foo() {
        if(1 == 1 && true) {
            return 10 + (-10);
        }
        if(1 == 1 && (false && true)) {
            return 5 * ((false && true) == false ? 5 : 6);
        }
    }
}
