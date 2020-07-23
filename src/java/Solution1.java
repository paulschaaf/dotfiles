import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;


public class Solution1 {
    private static final int MAX_INDEX = 100000;

    public static boolean isPrime(long n) {
        return LongStream
                .rangeClosed(2, (long) Math.sqrt(n))
                .allMatch(i -> n % i != 0);
    }

    public static LongStream primeSequence() {
        return LongStream.range(2, Long.MAX_VALUE)
                .filter(Solution1::isPrime);
    }

    public static Stream<String> primeChars() {
        return primeSequence()
                .mapToObj(String::valueOf)
                .flatMapToInt(String::chars)
                .mapToObj(i -> String.valueOf((char) i));
    }

    public static String solution(int i) {
        return primeChars()
                .skip(i)
                .limit(5)
                .collect(Collectors.joining());
    }

    public static void main(String[] args) {
        int n = Integer.parseInt(args[0]);

//        var primes = primeSequence()
//                .limit(n)
//                .mapToObj(String::valueOf)
//                .collect(Collectors.joining(", "));
//        System.out.println("[ " + primes + " ]");
//
//        var primeChars = primeChars()
//                .limit(n)
//                .collect(Collectors.joining(", "));
//        System.out.println("[ " + primeChars + " ]");

        System.out.println(solution(n));
    }
}
