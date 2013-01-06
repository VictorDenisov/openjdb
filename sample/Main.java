public class Main implements Runnable {

    public static int field1 = 10;

    private String[] args;

    public Main(String[] args) {
        this.args = args;
    }

    public static void main(String[] args) {
        new Thread(new Main(args)).start();
    }

    public void run() {
        System.out.println("Hello all");
        System.out.println("Arguments are");
        int v = -5;
        char c = 'п';
        long l = 4000000000L;
        methodA("argA");
        for (int i = 0; i < args.length; ++i) {
            System.out.println(i + ": " + args[i]);
        }
    }

    public static void methodA(String v) {
        System.out.println(v);
        methodB(48);
    }

    public static void methodB(int x) {
        System.out.println(x + "");
    }
}
