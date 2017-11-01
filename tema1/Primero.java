public class Primero {

    public static int primeroI(int x, int y) {
        return x;
    }

    public static char primeroC(char x, char y) {
        return x;
    }

    // Java: <T>
    // genericidad - T es un parámetro genérico que representa cualquier tipo
    //
    // Haskell: a
    // polimorfismo - a es una variable de tipo que representa cualquier tipo
    public static <T> T primero(T x, T y) {
        return x;
    }

    public static void main(String[] args) {
        System.out.println(primeroI(3, 5));
        System.out.println(primeroC('t', 'x'));
        System.out.println(primero(true, false));
    }
}
