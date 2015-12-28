package com.blackducksoftware.ohcount4j.scan;

import static com.blackducksoftware.ohcount4j.Entity.BLANK;
import static com.blackducksoftware.ohcount4j.Entity.CODE;
import static com.blackducksoftware.ohcount4j.Entity.COMMENT;

import org.testng.annotations.Test;

import com.blackducksoftware.ohcount4j.Language;

public class SmalltalkScannerTest extends AbstractBaseScannerTest {

    @Test
    public void basic() {
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "     \n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "\t\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), "^Student new name: aPerson name\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"Line comment\"\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"\"\n");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), " y := y + 7. // with comment\n");
    }

    @Test
    public void eofHandling() {
        // Note lack of trailing \n in all cases below
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "     ");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, BLANK), "\t");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), "^Student new name: aPerson name");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"Line comment\"");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, COMMENT), "\"\"");
        assertLine(Language.SMALLTALK, new Line(Language.SMALLTALK, CODE), " y := y + 7. // with comment");
    }

    @Test
    public void sampleTest() {
        String code = "\"Simple piece of a Smalltalk\n"
                + "			Program\n"
                + "\n"
                + "\"\n"
                + "name: aName address: adAddress\n"
                + "\"Set the receiver's name and address\n"
                + "to the specified values.\"\n"
                + "self name: aName.\n"
                + "self address: anAddress\n";

        Line[] expected = {
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, BLANK),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, CODE),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, CODE),
                new Line(Language.SMALLTALK, CODE)
        };
        assertLines(Language.SMALLTALK, expected, code);
    }

    @Test
    public void unterminatedBlockCommentCrash() {
        // This minimal case caused an Arrays.copyOfRange() crash
        String code = "\"\n\n\n";

        Line[] expected = {
                new Line(Language.SMALLTALK, COMMENT),
                new Line(Language.SMALLTALK, BLANK),
                new Line(Language.SMALLTALK, BLANK)
        };
        assertLines(Language.SMALLTALK, expected, code);
    }
}
