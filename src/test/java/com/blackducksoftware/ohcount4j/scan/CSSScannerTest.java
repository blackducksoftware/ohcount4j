package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class CSSScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "\n");
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "     \n");
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "\t\n");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em;\n");
        assertLine(Language.CSS, new Line(Language.CSS, COMMENT), "/* comment */\n");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em; /* with comment */\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "     ");
        assertLine(Language.CSS, new Line(Language.CSS, BLANK), "\t");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em;");
        assertLine(Language.CSS, new Line(Language.CSS, COMMENT), "/* comment */");
        assertLine(Language.CSS, new Line(Language.CSS, CODE), "margin: 1em; /* with comment */");
    }

    @Test
    public void helloWorld() {
        String code = "/* Pure CSS Hello World */\n"
                + "\n"
                + "body:after {\n"
                + "  content:\"Hello, world!\";\n"
                + "}";

        Line[] expected = {
                new Line(Language.CSS, COMMENT),
                new Line(Language.CSS, BLANK),
                new Line(Language.CSS, CODE),
                new Line(Language.CSS, CODE),
                new Line(Language.CSS, CODE)
        };
        assertLines(Language.CSS, expected, code);
    }
}