package net.ohloh.ohcount4j.scan;

import static net.ohloh.ohcount4j.Entity.BLANK;
import static net.ohloh.ohcount4j.Entity.CODE;
import static net.ohloh.ohcount4j.Entity.COMMENT;
import static net.ohloh.ohcount4j.Language.CLOJURE;

import org.testng.annotations.Test;

public class ClojureScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "\n");
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "     \n");
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "\t\n");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "(+ 1 2 3)\n");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), "; Comment\n");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";; Comment\n");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";;; Comment\n");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "#_(a : b) 1  ;;=> 1 (contrast to (comment a : b) which doesn't compile.)\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "     ");
        assertLine(CLOJURE, new Line(CLOJURE, BLANK), "\t");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "(ns com.clojurebook.broken-array-set)");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), "; Comment");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";; Comment");
        assertLine(CLOJURE, new Line(CLOJURE, COMMENT), ";;; Comment");
        assertLine(CLOJURE, new Line(CLOJURE, CODE), "(println (\"Hello World!\"))");
    }

    @Test
    public void helloWorld() {
        String code = "(defn array-set\n"
                + "  ; comment\n"
                + "  ;; comment\n"
                + "  ;;; comment\n"
                + "\n"
                + "  \"Creates an array-backed set containing the given values.\"\n"
                + "  [& vals]\n"
                + "  (into empty-array-set vals))";

        Line[] expected = {
                new Line(CLOJURE, CODE),
                new Line(CLOJURE, COMMENT),
                new Line(CLOJURE, COMMENT),
                new Line(CLOJURE, COMMENT),
                new Line(CLOJURE, BLANK),
                new Line(CLOJURE, CODE),
                new Line(CLOJURE, CODE),
                new Line(CLOJURE, CODE),
        };
        assertLines(CLOJURE, expected, code);
    }

}
