public class Main {
    public static void main(String[] args) {
        System.out.println("Hello all");
        System.out.println("Arguments are");
        int v = -5;
        char c = 'п';
        long l = 4000000000L;
        for (int i = 0; i < args.length; ++i) {
            System.out.println(i + ": " + args[i]);
        }
    }
}
