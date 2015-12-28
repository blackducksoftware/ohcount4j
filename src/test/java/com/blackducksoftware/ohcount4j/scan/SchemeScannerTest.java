package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class SchemeScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.SCHEME, new Line(Language.SCHEME, BLANK), "\n");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, BLANK), "     \n");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, BLANK), "\t\n");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, CODE), "(define eval-aplus50 (delay (+ a 50)))\n");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, COMMENT), "#| Block Comment |#\n");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, COMMENT), "; Line comment\n");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, CODE), "(define eval-aplus50 (delay (+ a 50))) ; Single line comment on code line\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.SCHEME, new Line(Language.SCHEME, BLANK), "     ");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, BLANK), "\t");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, CODE), "(define eval-aplus50 (delay (+ a 50)))");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, COMMENT), "#| Block Comment |#");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, COMMENT), "; Line comment");
        assertLine(Language.SCHEME, new Line(Language.SCHEME, CODE), "(define eval-aplus50 (delay (+ a 50))) ; Single line comment on code line");
    }

    @Test
    public void sampleTest() {
        String code = ";; Tabulation of Hofstadter's male and female sequences\n"
                + "#|\n"
                + "Sample Scheme Program found on Wikipedia\n"
                + "\t\n"
                + "|#\n"
                + "(letrec ((female (lambda(n)\n"
                + "                   (if (= n 0) 1\n"
                + "                       (- n (male (female (- n 1)))))))\n"
                + "         (male (lambda(n)\n"
                + "                 (if (= n 0) 0\n"
                + "                     (- n (female (male (- n 1))))))))\n"
                + "  (display \"i male(i) female(i)\")(newline)\n"
                + "  (do ((i 0 (+ i 1)))\n"
                + "      ((> i 8) #f)\n"
                + "    (display i) (display \"   \")(display (male i))(display \"         \")(display (female i))\n"
                + "    (newline)))";

        Line[] expected = {
                new Line(Language.SCHEME, COMMENT),
                new Line(Language.SCHEME, COMMENT),
                new Line(Language.SCHEME, COMMENT),
                new Line(Language.SCHEME, BLANK),
                new Line(Language.SCHEME, COMMENT),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE),
                new Line(Language.SCHEME, CODE)
        };
        assertLines(Language.SCHEME, expected, code);
    }

}
