public class Maximo {

    // Java: T extends Comparable<T>
    // restricción sobre el tipo T, debe implementar Comparable
    //
    // Haskell: Ord a
    // restricción sobre el tipo a, debe ser instancia de Ord

    public static <T extends Comparable<T>> T maximo(T x, T y) {
        T result;
        if (x.compareTo(y) >= 0)
            result = x;
        else
            result = y;
        return result;
    }

    public static void main(String[] args) {
        System.out.println(maximo(5, 8));
        System.out.println(maximo('r', 'f'));
    }
}
